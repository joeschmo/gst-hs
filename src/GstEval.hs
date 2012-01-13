module GstEval where

import Control.Monad.Error
import Control.Monad
import System.IO

import GstError
import GstTypes
import GstEnv
import GstParser

-- STATICS

typeof :: Env -> Ctx -> Exp -> IOThrowsError Typ
typeof env cx ex =
    case ex of
         Z -> return Nat
         S e ->
            typeof env cx e >>= (\t ->
                case t of
                     Nat -> return Nat
                     _   -> throwError $ TypeMismatch Nat t ex)
         X v ->
            case lookup v cx of
                 Nothing -> getVar env v >>= typeof env cx
                 Just t  -> return t
         Lam t v e ->
            let
                cx' = (v, t) : cx
            in
                typeof env cx' e >>= (\t' -> return $ Arr t t')
         Natrec e e0 x y e1 ->
            do
                et <- typeof env cx e
                case et == Nat of
                     False -> throwError $ TypeMismatch Nat et e
                     True  ->
                        typeof env cx e0 >>= (\t ->
                            let
                                cx' = (x, Nat) : (y, t) : cx
                            in
                                do
                                    t' <- typeof env cx' e1
                                    case t == t' of
                                         True -> return t
                                         False ->
                                            throwError $ TypeMismatch t t' e1)
         Ap e1 e2 ->
            do
                et <- typeof env cx e1
                case et of
                     Arr t2 t ->
                        typeof env cx e2 >>= (\t0 ->
                            if t0 == t2 then return t
                            else throwError $ TypeMismatch t2 t0 e2)
                     _ -> throwError $ Default "Application expected a function"
         Set v e -> typeof env cx e
         Load _ -> return Nat

-- DYNAMICS

rebindVar :: Var -> Exp -> Exp -> Exp
rebindVar v ex r =
    case ex of
         Z -> Z
         S e -> S (rebindVar v e r)
         X x ->
            case v == x of
                 True -> r
                 False -> ex
         Lam t x e ->
            case x == v of
                 False -> Lam t x (rebindVar v e r)
                 True ->
                    let
                        lam' = Lam t (x++"#") (rebindVar x e (X (x++"#")))
                    in
                        rebindVar v lam' r
         Ap e1 e2 -> Ap (rebindVar v e1 r) (rebindVar v e2 r)
         Natrec e e0 x y e1 ->
            case x == v of
                 True ->
                    let
                        nat' = Natrec e e0 (x++"#") y (rebindVar x e (X (x++"#")))
                    in
                        rebindVar v nat' r
                 False ->
                    case y == v of
                         True ->
                            let
                                nat' = Natrec e e0 x (y++"#") (rebindVar y e1 (X (y++"#")))
                            in
                                rebindVar v nat' r
                         False ->
                            Natrec (rebindVar v e r) (rebindVar v e0 r) x y (rebindVar v e1 r)

-- IO Helper functions

load :: String -> IOThrowsError [Exp]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExpList

eval :: Env -> Exp -> IOThrowsError Exp
eval env ex =
    case ex of
         Z -> return Z
         S e -> eval env e >>= (\e' -> return $ S e')
         X v -> getVar env v
         Lam t v e -> return ex
         Ap e1 e2 ->
            do
                e1' <- eval env e1
                case e1' of
                     Lam t v e ->
                        let
                            e' = rebindVar v e e2
                        in
                            eval env e'
                     _ -> throwError $ Default "Application expected a function"
         Natrec e e0 x y e1 ->
            do
                e' <- eval env e
                case e' of
                     Z -> eval env e0
                     S ep ->
                        let
                            repX = rebindVar x e1 ep
                            repY = rebindVar y repX (Natrec ep e0 x y e1)
                        in
                            eval env repY
                     _ -> typeof env emptyCtx e' >>= (\t -> throwError $ TypeMismatch Nat t e')
         Set v e -> do
            e' <- eval env e
            defineVar env v e'
            return $ Set v e'
         Load f -> load f >>= liftM last . mapM (eval env)
