module Normal where
import Expr

------------------------------------------------------------------------------------------------------------------------------------------
--Order for evaluation
normalRedex :: F
normalRedex e@(App (Abs _ _) _) = [e]
normalRedex (App e1 e2) = case (normalRedex e1) of
                               []       -> normalRedex e2
                               (x : xs) -> normalRedex e1
normalRedex (Abs v e) = normalRedex e
normalRedex (Ref _) = []
