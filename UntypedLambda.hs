module UntypedLambda (LamTerm(..)) where 

data LamTerm a =  Var a
				| Lam a (LamTerm a)
				| App (LamTerm a) (LamTerm a)
	deriving (Eq, Show, Read)


betaReduce :: String -> LamTerm String -> LamTermString -> LamTerm String
betaReduce fromVar toTerm (Var x) =
betaReduce fromVar toTerm (Lam x t) = 
betaReduce fromVar toTerm (App x y) = 