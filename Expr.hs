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
-- Example expression
e :: Expr
e = Abs "x" (App (Abs "y" (App (Abs "z" (Ref "z")) (Ref "x"))) (Ref "x"))

e1 :: Expr
e1 = App (App (Abs "y" (App (Ref "y") (Ref "y"))) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))

e2 :: Expr
e2 = App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))

e3 :: Expr
e3 = App (Abs "x" (App (App (Abs "y" (Ref "x")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z")))) (Abs "y" (Ref "z"))
