module NewViews where
import Expr
import Graph
import Reduce
import Normal
import Applicative
import Test
import Views
import Data.List(nub,intercalate)

view :: Expr -> IO()
view e = fun' $ views e

evaluate' :: Expr -> Order -> IO()
evaluate' e f = fun' $ evaluate e f

fun' :: G -> IO()
fun' (ns,es) = do
              putStrLn ("Nodes:")
              putStrLn (show ns)
              putStrLn ("Edges:")
              putStrLn (show es)

type  Graph = [[LEdge]]

fun :: Graph -> IO()
fun  = mapM_ (putStrLn.(intercalate ", ").(map show))

path :: Node -> G -> IO()
path x (ns,es) = fun $ pathTo 1 x es

pathTo :: Node -> Node -> [LEdge] -> Graph
pathTo x y g
    | x == y    = [[]]
    | otherwise = [edge:connect | (t, edge) <- outEdge g x, connect <- pathTo t y g]

confluence :: G -> Bool
confluence (ns, es) = case f es of
                         [] -> False
                         [x] -> True
                         (x:xs) -> confluent (x:xs) (ns,es)
f :: [LEdge] -> [Node]
f [] = []
f ((x,y,r):es) = case outEdge es y of
                       [] -> nub $ y : f es
                       (x:xs) -> nub $ f es

confluent :: [Node] -> G -> Bool
confluent [] (ns,es) = True
confluent (x:y:xs) (ns,es) = exists x y ls ns && confluent (y:xs) (ns,es)
    where ls = alphaEquivalent (ns,es)
          exists :: Node -> Node -> [(Expr,Relation,Expr)] -> [LNode] -> Bool
          exists x y [] ns = False
          exists x y ((e1,_,e2):es) ns = (expr ns x == e1 && expr ns y == e2) || (expr ns x == e2 && expr ns y == e1) || exists x y es ns

expr ns x = snd $ existsNode ns x

alphaEquivalent :: G -> [(Expr, Relation, Expr)]
alphaEquivalent (ns, es) = [(expr ns x, r, expr ns y)|(x,y,r) <- alphaEdge es]
     where alphaEdge :: [LEdge] -> [LEdge]
           alphaEdge []                         = []
           alphaEdge (a@(_,_,AlphaRename _):es) = a : alphaEdge es
           alphaEdge ((_,_,_):es)               = alphaEdge es

out :: Expr -> Int -> IO()
out e m = do
       putStrLn ("Reduction Graph:")
       view e
       putStrLn ("\nPath to:" ++ show m)
       path m (views e)
output :: IO()
output = do
          putStrLn ("Choose the expression to Reduce")
          putStrLn ("1."++ show e)
          putStrLn ("2."++ show e1)
          putStrLn ("3."++ show e2)
          putStrLn ("4."++ show e3)
          i <- getLine
          case (read i::Int) of
              1 -> out e 4
              2 -> out e1 4
              3 -> out e2 4
              otherwise -> out e3 5
