module Applicative where
import Expr

------------------------------------------------------------------------------------------------------------------------------------------
--Order for evaluation
applicativeRedex :: Order
applicativeRedex e@(App (Abs _ e1) e2) = case (applicativeRedex e1, applicativeRedex e2) of
                                              ( [], [] )    -> [e]
                                              ( [], (e:es)) -> (e:es)
                                              ( (e:es), _)  -> (e:es)
applicativeRedex (App e1 e2) = case (applicativeRedex e1) of
                                    []       -> applicativeRedex e2
                                    (e : es) -> (e:es)
applicativeRedex (Abs v e) = applicativeRedex e
applicativeRedex (Ref _) = []
