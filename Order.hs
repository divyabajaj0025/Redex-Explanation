module Order where
import Views
--Template to define order

--type Order = Expr -> [Expr] --Redex to be choosen from an expression is given as output
orderRedex :: Order
orderRedex (App (Abs v e1) e2) = undefined
orderRedex (App e1 e2)         = undefined
orderRedex (Abs v e)           = undefined
orderRedex (Ref v)             = undefined
