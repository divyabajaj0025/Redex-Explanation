module Expr where
import Data.Set
type Var = String
type Order = (Expr -> [Expr])
data Expr = Ref Var
            |App Expr Expr
            |Abs Var Expr
          deriving (Eq, Ord)

instance Show Expr where
    show (Ref v) = v
    show (App e1 e2) = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    show (Abs v e) = "\\" ++ v ++ " . " ++ show e

-------------------------------------------------------------------------------------------------------------------------------
--Smart Constructors
app :: Expr -> Expr -> Expr
app e1 e2 = App e1 e2

abs :: Var -> Expr -> Expr
abs v e = Abs v e

ref :: Var -> Expr
ref v = Ref v
