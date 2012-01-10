module GstTypes where

data Typ = Nat | Arr Typ Typ
    deriving (Eq)

type Var = String

data Exp = X Var
         | Z
         | S Exp
         | Natrec Exp Exp Var Var Exp
         | Lam Typ Var Exp
         | Ap Exp Exp
    deriving (Eq)

type Ctx = [(Var, Typ)]

emptyCtx = []

instance Show Typ where
    show Nat = "nat"
    show (Arr t1 t2) =
        case t1 of
             Arr _ _ -> "("++(show t1)++")"++" -> "++(show t2)
             _       -> (show t1) ++ " -> " ++ (show t2)

instance Show Exp where
    show Z = "z"
    show (S e) = "s("++(show e)++")"
    show (X v) = v
    show (Natrec e e0 x y e1) = 
        "natrec "++(show e)++" {z => "++(show e0)++" | s("++x++") with "++y++" => "++(show e1)++"}"
    show (Lam t v e) = "fn("++v++" : "++(show t)++") "++(show e)
    show (Ap e1 e2) = (show e1)++"("++(show e2)++")"
