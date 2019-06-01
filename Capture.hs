module Capture where

import qualified Data.Set as S
import Expr

-- Capture-avoiding substitution
vars :: Expr -> [(Var, Var)] -> S.Set Var -> S.Set Var -> (Expr, [(Var, Var)])
vars (Ref v) ss s1 s2     = if (v `S.member` s1) then let v' = replace v s1 s2 in ((Ref v'), ((v, v'):ss)) else ((Ref v), ss)
vars (Abs v e) ss s1 s2   = if (v `S.member` s1) then let v' = replace v s1 s2; (e1', ss') = vars e ss s1 s2 in (Abs v' e1', (v, v'):(ss'++ss)) else let (e',ss') = vars e ss s1 s2 in (Abs v e', ss')
vars (App e1 e2) ss s1 s2 = let (e1',ss') = vars e1 ss s1 s2; (e2',ss'') = vars e2 ss s1 s2
                                 in ((App e1' e2'), ss'++ss'')

replace :: Var -> S.Set Var -> S.Set Var -> Var
replace v s1 s2 = fun v (S.toList s1) (list s2)

list :: S.Set Var -> [Var]
list s = Prelude.map (\v -> [v]) (Prelude.filter (\v -> S.notMember [v] s) ['a'..'z'])

fun :: Var -> [Var] -> [Var] -> Var
fun v (x : xs) (y : ys) = if (v == x) then y else fun v xs ys
-----------------------------------------------------------------------------------------------------------------------------
-- Free and bound variables
bound :: Expr -> S.Set Var -> S.Set Var
bound (Ref _) s = s
bound (App e1 e2) s = let s1 = bound e1 s
                         in bound e2 s1
bound (Abs v e) s = let s1 = S.insert v s
                         in bound e s1

free :: Expr -> S.Set Var
free e = S.difference (helper' e S.empty) (bound e S.empty)

helper' :: Expr -> S.Set Var -> S.Set Var
helper' (Abs v e) s = helper' e s
helper' (App e1 e2) s = let s1 = helper' e1 s
                          in helper' e2 s1
helper' (Ref v) s = S.insert v s
