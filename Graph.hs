module Graph where
import Expr

-----------------------------------------------------------Nodes-----------------------------------------------------------------------
type Node = Int
type LNode = (Node, Expr)

----------------------------------------------------------Edges------------------------------------------------------------------------
data Relation = BetaReduction (Expr, Expr)
               |AlphaRename [(Var, Var)]
            deriving(Eq)
instance Show Relation where
  show (BetaReduction (e1, e2)) = "BetaReduction: " ++ show e1 ++ " => " ++ show e2
  show (AlphaRename vs)         = "AlphaRename: " ++ pretty vs
type LEdge = (Node, Node, Relation)
--------------------------------------------------------------------------------------------------------------------------------------
type G = ([LNode], [LEdge])

pretty :: [(Var, Var)] -> String
pretty [] = ""
pretty (v:vs) = case pretty vs of
                      "" ->  toString v
                      ss -> toString v ++ ", " ++ ss
        where toString :: (Var, Var) -> String
              toString (x,y) = x ++ "->" ++ y
