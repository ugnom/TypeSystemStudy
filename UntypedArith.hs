

data Term = TmTrue
		|	TmFalse
		|	TmIf Term Term Term
		|	TmZero
		|	TmSucc Term
		|	TmPred Term
		| 	TmIsZero Term
	deriving (Show, Read, Eq, Ord)

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc n) = isNumVal n
isNumVal _ = False

isValue :: Term -> Bool
isValue TmTrue = True
isValue TmFalse = True
isValue n = isNumVal n



eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) = (eval1 t1) >>= (\x -> return $ TmIf x t2 t3)
--eval1 ()
eval1 _ = Nothing

parse :: [String] -> Term
parse ["true"] = TmTrue 
parse ["false"] = TmFalse
parse ("if" : xs) = parseIf xs

parseIf :: [String] -> Term
parseIf xs = 
	let
		(tmCond, tmThen, tmElse) = findIfTerms xs
	in 
		TmIf (parse tmCond) (parse tmThen) (parse tmElse)

findIfTerms :: [String] -> ([String], [String], [String])
findIfTerms xs = 
	let 
		(tmCond, condRest) = findCond xs
		(tmThen, tmElse) = findThen condRest
	in (tmCond, tmThen, tmElse)

findCond :: [String] -> ([String], [String]) 
findCond xs = 
	let
		thenIndex = findThenIndex xs
	in 
		((take thenIndex xs) , (drop (thenIndex + 1) xs))

	where 
		findThenIndex = findIndexOf "then" 0 

findThen :: [String] -> ([String], [String])
findThen xs = 
	let 
		elseIndex = findElseIndex xs 
	in 
		((take elseIndex xs), (drop (elseIndex + 1) xs))
	where
		findElseIndex = findIndexOf "else" 0


findIndexOf :: String -> Int -> [String] -> Int
findIndexOf s n (x:xs) 
	| x == "if" = 1 + findIndexOf s (n+1) xs
	| s == x && n == 0 = 0
	| s == x && n /= 0 = 1 + findIndexOf s (n-1) xs
	| s /= x  = 1 + findIndexOf s 0 xs
	 


