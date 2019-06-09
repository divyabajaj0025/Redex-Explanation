module Alpha(alpha) where
import Expr
import Graph
import qualified Data.Tuple as T hiding (map)

----------------------------------------------------------------------------------------------------------------------------------
-- Checking Alpha-equivalence
alpha :: [LNode] -> [LEdge] -> [LEdge]
alpha []  es             = es
alpha ns@((n,x) : xs) es = let es' = edge n ls es in alpha xs es'
  where ls =  alphahelper x xs

alphahelper :: Expr -> [LNode] -> ([(LNode, [(Var, Var)])])
alphahelper _ []           = []
alphahelper e ((m, n): ns) = case alphaeq e n [] of
                                   (True, ss) -> ((m, n),ss) : alphahelper e ns
                                   (False, _) -> alphahelper e ns

alphaeq :: Expr -> Expr -> [(Var, Var)] -> (Bool, [(Var, Var)])
alphaeq (App e1 e2) (App e11 e21) vs = let (b1, v1) = alphaeq e1 e11 vs;
                                           (b2, v2) = alphaeq e2 e21 vs
                                          in (b1 && b2, v1++v2)
alphaeq (Abs v e1) (Abs v1 e11) vs   = let vs' = (v,v1) : vs in alphaeq e1 e11 vs'
alphaeq (Ref v) (Ref v1) vs          = case (lookup v vs,lookup v1 (map T.swap vs)) of
                                            (Nothing, Nothing) -> (True, (v,v1):vs)
                                            (Just x, Just y) | x==v1&&y==v -> (True,vs)
                                                             | otherwise   -> (False, [])
alphaeq _ _ _ = (False, [])

---------------------------------------------------------------------------------------------------------------------------------------
-- Creating AlphaEquivalent Edges
edge :: Node -> [(LNode, [(Var, Var)])] -> [LEdge] -> [LEdge]
edge i [] es  = es
edge i (((m,n), ss): ls) es = case ((elem' (i, m, AlphaRename []) es) || (elem' (m, i, AlphaRename []) es)) of
                                    True -> edge i ls es
                                    False -> let es' = (i, m, AlphaRename ss) : (m, i, AlphaRename (map T.swap ss)) : es
                                                      in edge i ls es'

elem' :: LEdge -> [LEdge] -> Bool
elem' _ []                     = False
elem' e@(m,n,_) ((m1,n1,_):es) = ((m==m1) && (n==n1)) || elem' e es
