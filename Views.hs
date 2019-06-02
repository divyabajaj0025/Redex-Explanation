module Views where
import Expr
import Graph
import Reduce
import Normal
import Applicative

data Direction = Up
                |Down
          deriving (Eq, Show)
-----------------------------------------------------------------------------------------------------------------------------------------------
--Creating views

views :: Expr -> Order -> (G -> b) -> b
views e f g = g $ evaluate e f
-----------------------------------------------------------------------------------------------------------------------------------------------
evaluate :: Expr -> Order -> G
evaluate e = toGraph e

input :: G -> Expr
input (((n,x):xs), ys) = x

redexes :: Expr -> [Expr]
redexes = possRedex

{-path :: Node -> G -> [G]
path x (xs, ys) = undefined-}

context :: Node -> Direction -> G -> G
context i Up (ns,es)   = (map ((existsNode ns) . fst) (inEdge es i), map snd (inEdge es i))
context i Down (ns,es) = (map ((existsNode ns) . fst) (outEdge es i), map snd (outEdge es i))

{-confluence :: G -> Bool
confluence (_, es) = undefined

f :: [LEdge] -> [Node]
f [] = []
f ((x,y,r):es) = case outEdge es y of
                      [] -> y : f es
                      (x:xs) | map snd (x:xs) == (alphaEdge . map snd) (x:xs) -> y : f es
                             | otherwise -> f es
-}
alphaconfluence :: G -> [(Expr, Relation, Expr)]
alphaconfluence (ns, es) = [((snd . (existsNode ns))x, r, (snd . (existsNode ns))y)|(x,y,r) <- alphaEdge es]

alphaEdge :: [LEdge] -> [LEdge]
alphaEdge []                         = []
alphaEdge (a@(_,_,AlphaRename _):es) = a : alphaEdge es
alphaEdge ((_,_,_):es)               = alphaEdge es

inEdge :: [LEdge] -> Node -> [(Node, LEdge)]
inEdge [] i = []
inEdge (e@(x,y,xs):es) i = if y == i then (x, e) : inEdge es i else inEdge es i

outEdge :: [LEdge] -> Node -> [(Node, LEdge)]
outEdge [] i = []
outEdge (e@(x,y,xs):es) i = if x == i then (y, e) : outEdge es i else outEdge es i

existsNode :: [LNode] ->Node -> LNode
existsNode (n@(x,y) : ns) i = if x == i then n else existsNode ns i
