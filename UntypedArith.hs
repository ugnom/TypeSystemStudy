

data Term = TmTrue
		|	TmFalse
		|	IfThenElse Term Term Term
		|	Zero
		|	Succ Term
		|	Pred Term
		| 	IsZero Term
	deriving (Show, Read, Eq, Ord)

