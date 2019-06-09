module Order where
import Expr
import Views
import Normal
import Applicative
import NewViews
import Test

orderRedex :: Order
orderRedex = normalRedex
