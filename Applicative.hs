module Applicative where
import Expr

------------------------------------------------------------------------------------------------------------------------------------------
--Order for evaluation
applicativeRedex :: Order
applicativeRedex e@(App (Abs _ e1) e2) = case applicativeRedex e1 of
                                                 [] -> case applicativeRedex e2 of
                                                               [] -> [e]
                                                               (x:xs) -> applicativeRedex e2
                                                 (x:xs) -> applicativeRedex e1
applicativeRedex (App e1 e2) = case (applicativeRedex e1) of
                               []       -> applicativeRedex e2
                               (x : xs) -> applicativeRedex e1
applicativeRedex (Abs v e) = applicativeRedex e
applicativeRedex (Ref _) = []
