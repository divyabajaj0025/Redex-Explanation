module Eval where
import Expr
import Graph
import Capture
import qualified Data.Tuple as T hiding (map)
import qualified Data.Set as S hiding (map)

--------------------------------------------------------------------------------------------------------------------------------
-- Evaluating an expression
eval :: Expr -> [LNode] -> [LEdge] -> (Expr, [LNode], [LEdge])
eval (App (Abs v e1) e2) ns es
          |(s1 `S.disjoint` s2) = (substitute v e1 e2, ns, es)
          |otherwise = let (Just x, Just y) = (lookup e1 ns''', lookup e1' ns''')
                          in (substitute v e1' e2, ns'', es ++ [(x, y, AlphaRename (dupString ss))])
            where s1 = bound e1 S.empty
                  s2 = free e2
                  s3 = free e1
                  (e1', ss) = vars e1 [] (s1 `S.intersection` s2) (s3 `S.union` s2)
                  ns' = equivalence e1 ns
                  ns'' = equivalence e1' ns'
                  ns''' = map T.swap ns''

dupString :: [(Var, Var)] ->[(Var, Var)]
dupString [] = []
dupString xs = nodup xs

nodup :: [(Var, Var)] -> [(Var, Var)]
nodup []     = []
nodup (x:xs) | x `elem` xs =  nodup xs
             | otherwise   = x : nodup xs

substitute :: Var -> Expr -> Expr -> Expr
substitute v (Ref v') e    = if (v==v') then e else (Ref v')
substitute v (Abs v1 e1) e = Abs v1 (substitute v e1 e)
substitute v (App e1 e2) e = App (substitute v e1 e) (substitute v e2 e)
-------------------------------------------------------------------------------------------------------------------------------
-- Children for each redex
type Result = ([Expr], [LNode], [LEdge], [(Expr, Expr)])

evalExpr :: Expr -> Order -> [LNode] -> [LEdge] -> Result
evalExpr e f ns es = med e ps [] ns es []
     where ps = f e

med :: Expr -> [Expr] -> [Expr] -> [LNode] -> [LEdge] -> [(Expr, Expr)] -> Result
med e []     ps' ns es rs = (ps', ns, es, rs)
med e (p:ps) ps' ns es rs = let (p', ns', es', Just r) =  helper e ns es p
                            in med e ps (ps' ++ [p']) ns' es' (rs ++ [r])

helper :: Expr -> [LNode] -> [LEdge] -> Expr -> (Expr, [LNode], [LEdge], Maybe (Expr, Expr))
helper (Ref v)       ns es e'    = (Ref v, ns, es, Nothing)
helper (Abs v e)     ns es e'    = let (e1, ns', es', p) = helper e ns es e'
                                    in (Abs v e1, ns', es ++ es', p)
helper e@(App e1 e2) ns es e'
                     | e == e'   = let (e'', ns', es') = eval e' ns es
                                    in (e'', ns', es', Just (e', e''))
                     | otherwise = let (e1', ns', es', p) = helper e1 ns es e'
                                    in let (e2', ns'', es'', p') = helper e2 ns' es' e'
                                        in (App e1' e2', ns'', es'', matchJust p p')
                  where matchJust :: Maybe a -> Maybe a -> Maybe a
                        matchJust Nothing m = m
                        matchJust m Nothing = m
-----------------------------------------------------------------------------------------------------------------------------------------------
equivalence :: Expr -> [LNode] -> [LNode]
equivalence e [] = [(1,e)]
equivalence e es@((n,x):xs) = case lookup e (map T.swap es) of
                                   Nothing -> (n+1, e) : es
                                   Just _  -> es
