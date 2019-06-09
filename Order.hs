module Order where
import Expr
import Views
import Normal
import Applicative
import NewViews

orderRedex :: F
orderRedex = normalRedex
