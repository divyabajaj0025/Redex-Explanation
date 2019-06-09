module Test where
import Expr
import Prelude hiding (ref, abs, app)

-- Example expression
e :: Expr
e = abs "x" (app (abs "y" (app (abs "z" (ref "z")) (ref "x"))) (ref "x"))
--e = Abs "x" (App (Abs "y" (App (Abs "z" (Ref "z")) (Ref "x"))) (Ref "x"))

e1 :: Expr
e1 = app (app (abs "y" (app (ref "y") (ref "y"))) (ref "z")) (app (abs "y" (ref "y")) (ref "z"))
--e1 = App (App (Abs "y" (App (Ref "y") (Ref "y"))) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))

e2 :: Expr
e2 = app (app (abs "y" (ref "y")) (ref "z")) (app (abs "y" (ref "y")) (ref "z"))
--e2 = App (App (Abs "y" (Ref "y")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z"))

e3 :: Expr
e3 = app (abs "x" (app (app (abs "y" (ref "x")) (ref "z")) (app (abs "y" (ref "y")) (ref "z")))) (abs "y" (ref "z"))
--e3 = App (Abs "x" (App (App (Abs "y" (Ref "x")) (Ref "z")) (App (Abs "y" (Ref "y")) (Ref "z")))) (Abs "y" (Ref "z"))
