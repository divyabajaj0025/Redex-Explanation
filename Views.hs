module Views where
import Expr
import Graph
import Reduce
import Normal
import Applicative
import Test
data Direction = Up
                |Down
          deriving (Eq, Show)
-----------------------------------------------------------------------------------------------------------------------------------------------
--Creating views

views :: Expr -> G
views e = evaluate e possRedex
-----------------------------------------------------------------------------------------------------------------------------------------------
evaluate :: Expr -> Order -> G
evaluate e f = toGraph e f

-- Input to a Graph
input :: G -> Expr
input (((n,x):xs), ys) = x

-- Possible redexes
redexes :: Expr -> [Expr]
redexes = possRedex

-- Context -1 and +1
context :: Node -> Direction -> G -> G
context i Up g   = find i g inEdge
context i Down g = find i g outEdge

find :: Node -> G -> ([LEdge] -> Node -> [(Node, LEdge)]) -> G
find i (ns,es) f = (map ((existsNode ns) . fst) (f es i), map snd (f es i))

-- Finding edges into a node
inEdge :: [LEdge] -> Node -> [(Node, LEdge)]
inEdge [] i = []
inEdge (e@(x,y,xs):es) i
                  |y == i = (x, e) : inEdge es i
                  |otherwise = inEdge es i

--Finding edges out from a node
outEdge :: [LEdge] -> Node -> [(Node, LEdge)]
outEdge [] i = []
outEdge (e@(x,y,xs):es) i
                  |x == i = (y, e) : outEdge es i
                  |otherwise = outEdge es i

-- Finding a node for the index
existsNode :: [LNode] ->Node -> LNode
existsNode (n@(x,y) : ns) i
                  |x == i = n
                  |otherwise = existsNode ns i
