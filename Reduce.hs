module Reduce where
import Expr
import Graph
import Capture
import Eval
import Alpha
import qualified Data.Set as S
import qualified Data.Tuple as T

-----------------------------------------------------------------------------------------------------------------------------------------
-- Finding all the possible redexes of an expression
possRedex :: Order
possRedex e@(App (Abs _ e1) e2) = e : possRedex e1 ++ possRedex e2
possRedex (App e1 e2)           = possRedex e1 ++ possRedex e2
possRedex (Abs _ e)             = possRedex e
possRedex (Ref _)               = []
----------------------------------------------------------------------------------------------------------------------------------------------
toGraph ::  Expr -> Order -> G
toGraph e f = case catlist ys of
                (z : zs) -> let (e', ns, es) = nodups e ys [(1,e)] [] in let (ns', es') = eval' e' f (ns, es) in (reverse ns', (unique . alpha ns') es')
                []       -> let (ns, es) = eval' e f ([(1,e)], []) in (reverse ns, (unique . alpha ns) es)
      where xs = possRedex e
            ys = dups xs xs
------------------------------------------------------------------------------------------------------------------------------------
-- Creating Graph
eval' :: Expr -> Order -> G -> G
eval' e f (xs, ys) = graph ps f (ls, es ++ zipWith (\p r -> gedges e p r ls) ps rs)
     where (ps, ns, es, rs) = evalExpr e f xs ys
           ls = gnodes ns ps

graph :: [Expr] -> Order -> G -> G
graph [] f p       = p
graph (e : es) f p = graph es f (eval' e f p)

gnodes :: [LNode] -> [Expr] -> [LNode]
gnodes xs [] = xs
gnodes xs (e : es) = gnodes (equivalence e xs) es

gedges :: Expr -> Expr -> (Expr, Expr) -> [LNode] -> LEdge
gedges e1 e2 e' xs = let Just x = lookup e1 ns; Just y = lookup e2 ns
                             in (x, y, BetaReduction e')
      where ns = Prelude.map T.swap xs

unique :: [LEdge] -> [LEdge]
unique [] = []
unique (x:xs) | x `elem` xs = unique xs
              | otherwise   = x : unique xs

-------------------------------------------------------------------------------------------------------------------------------
-- Duplicate redex
nodups :: Expr -> [[Expr]] -> [LNode] -> [LEdge] -> (Expr, [LNode], [LEdge])
nodups e [] ns es          = (e, ns, es)
nodups e ([]:ys) ns es     = nodups e ys ns es
nodups e ((x:xs):ys) ns es = let (e', ss) = rename e x; ns' = equivalence e' ns
                                 in let ns'' = Prelude.map T.swap ns'
                                      in let (Just x, Just y) = (lookup e ns'', lookup e' ns'')
                                          in nodups e' (xs:ys) ns' (es ++ [(x, y, AlphaRename ss)])

rename :: Expr -> Expr -> (Expr, [(Var, Var)])
rename (Ref v)        _ = (Ref v, [])
rename (Abs v e)      _ = (Abs v e, [])
rename e@(App e1 e2) e' = if (e==e') then let (e'', ss) = vars e [] s s in (e'', dupString ss)
                              else case (e1 /= fst (rename e1 e')) of
                                                     True  -> let (e1', ss) = rename e1 e' in (App e1' e2, ss)
                                                     False -> let (e2', ss) = rename e2 e' in (App e1 e2', ss)
                                          where s = bound e S.empty

catlist :: [[a]] -> [[a]]
catlist []      = []
catlist ([]:ys) = catlist ys
catlist (x:ys) = x : catlist ys

-- check if duplicate entries in possRedex list
dups :: [Expr] -> [Expr] -> [[Expr]]
dups []     []     = []
dups (x:xs) (y:ys) = Prelude.filter (==x) ys : dups xs ys
