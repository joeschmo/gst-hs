module GstEval where

import Control.Monad.Error
import GstError
import GstTypes

-- STATICS

typeof :: Ctx -> Exp -> ThrowsError Typ
typeof cx ex =
    case ex of
         Z -> return Nat
         S e ->
            typeof cx e >>= (\t ->
                case t of
                     Nat -> return Nat
                     _   -> throwError $ TypeMismatch Nat t ex)
         X v ->
            case lookup v cx of
                 Nothing -> throwError $ UnboundVar "Unbound variable " v
                 Just t  -> return t
         Lam t v e ->
            let
                cx' = (v, t) : cx
            in
                typeof cx' e >>= (\t' -> return $ Arr t t')
         Natrec e e0 x y e1 ->
            do
                et <- typeof cx e
                case et == Nat of
                     False -> throwError $ TypeMismatch Nat et e
                     True  ->
                        typeof cx e0 >>= (\t ->
                            let
                                cx' = (x, Nat) : (y, t) : cx
                            in
                                typeof cx' e1)
         Ap e1 e2 ->
            do
                et <- typeof cx e1
                case et of
                     Arr t2 t ->
                        typeof cx e2 >>= (\t0 ->
                            if t0 == t2 then return t
                            else throwError $ TypeMismatch t2 t0 e2)
                     _ -> throwError $ Default "Application expected a function"

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
                                nat' = Natrec e e0 x (y++"#") (rebindVar y e (X (y++"#")))
                            in
                                rebindVar v nat' r
                         False ->
                            Natrec (rebindVar v e r) (rebindVar v e0 r) x y (rebindVar v e1 r)

eval :: Exp -> ThrowsError Exp
eval ex =
    case ex of
         Z -> return Z
         S e -> eval e >>= (\e' -> return $ S e')
         X v -> throwError $ UnboundVar "Unbound Variable" v
         Lam t v e -> return ex
         Ap e1 e2 ->
            do
                e1' <- eval e1
                case e1' of
                     Lam t v e ->
                        let
                            e' = rebindVar v e e2
                        in
                            eval e'
                     _ -> throwError $ Default "Application expected a function"
         Natrec e e0 x y e1 ->
            do
                e' <- eval e
                case e' of
                     Z -> eval e0
                     S ep ->
                        let
                            repX = rebindVar x e1 ep
                            repY = rebindVar y repX (Natrec ep e0 x y e1)
                        in
                            eval repY
                     _ -> typeof emptyCtx e' >>= (\t -> throwError $ TypeMismatch Nat t e')

evalWith :: Exp -> IO ()
evalWith ex =
    case typeof emptyCtx ex of
         Left err -> putStrLn $ show err
         Right t  ->
            case eval ex of
                 Left err' -> putStrLn $ show err'
                 Right e   -> putStrLn $ show e ++ " : " ++ show t
