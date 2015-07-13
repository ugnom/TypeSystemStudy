import Data.Tree

data Term = TmTrue
		|	TmFalse
		|	TmIf Term Term Term
		|	TmZero
		|	TmSucc Term
		|	TmPred Term
		| 	TmIsZero Term
		|	TmWrong
	deriving (Show, Read, Eq, Ord)

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc n) = isNumVal n
isNumVal _ = False

isValue :: Term -> Bool
isValue TmTrue = True
isValue TmFalse = True
isValue n = isNumVal n

isBadNat :: Term -> Bool
isBadNat TmWrong = True
isBadNat TmTrue = True
isBadNat TmFalse = True
isBadNat _ = False

isBadBool :: Term -> Bool
isBadBool TmWrong = True
isBadBool t = isNumVal t

eval :: Term -> Term
eval t =
	case (eval1 t) of
		Just x -> eval x
		Nothing -> t

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t2 _) = Just t2
eval1 (TmIf TmFalse _ t3) = Just t3
eval1 (TmIf t1 t2 t3) = 
	if isBadBool t1 then do
		return TmWrong
	else do
		t1' <- eval1 t1
		return $ TmIf t1' t2 t3
eval1 (TmSucc t1) = 
	if isBadNat t1 then do
		return TmWrong
	else do
		t1' <- eval1 t1
		return (TmSucc t1')
eval1 (TmPred TmZero) = Just TmZero
eval1 (TmPred (TmSucc nv1)) = Just nv1
eval1 (TmPred t) = if (isBadNat t) then Just TmWrong else Nothing
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc _)) = Just TmFalse
eval1 (TmIsZero t1) = 
	if isBadNat t1 then do
		return TmWrong
	else do 
		t1' <- eval1 t1
		return (TmIsZero t1')
eval1 _ = Nothing

parse :: [String] -> Maybe Term
parse ["true"] = Just TmTrue 
parse ["false"] = Just TmFalse
parse ("if" : xs) = parseIf xs
	where 
		parseIf :: [String] -> Maybe Term
		parseIf xs = do
			(tmCond, tmThen, tmElse) <- findIfTerms xs
			parsedCond <- parse tmCond
			parsedThen <- parse tmThen
			parsedElse <- parse tmElse
			return  $ TmIf (parsedCond) (parsedThen) (parsedElse)

		findIfTerms :: [String] -> Maybe ([String], [String], [String])
		findIfTerms xs = do 
			(tmCond, ifRest) <- findCond xs
			(tmThen, tmElse) <- findThen ifRest
			return (tmCond, tmThen, tmElse)

		findCond :: [String] -> Maybe ([String], [String]) 
		findCond xs = do
			thenIndex <- findThenIndex xs
			return ((take thenIndex xs), (drop (thenIndex + 1) xs))
			where 
				findThenIndex = findIndexOf "then" 0 

		findThen :: [String] -> Maybe ([String], [String])
		findThen xs = do
			elseIndex <-findElseIndex xs
			return ((take elseIndex xs), (drop (elseIndex + 1) xs))
			where
				findElseIndex = findIndexOf "else" 0


		findIndexOf :: String -> Int -> [String] -> Maybe Int
		findIndexOf _ _ [] = Nothing
		findIndexOf s n (x:xs) 
			| x == "if" = findIndexOf s (n+1) xs >>= return . (+1)
			| s == x && n == 0 = Just 0
			| s == x && n /= 0 = findIndexOf s (n-1) xs >>= return . (+1)
			| s /= x  = findIndexOf s n xs >>= return . (+1)
parse ["0"] = Just TmZero
parse ("succ" : xs) = do
	x <- parse xs
	return (TmSucc x)
parse ("pred" : xs) = do
	x <- parse xs
	return (TmPred x)
parse ("iszero" : xs) = do
	x <- parse xs
	return (TmIsZero x)
parse _ = Nothing

main :: IO ()
main = do
	putStr "Atith Expr >>"
	str <- getLine
	case (parse . words) str of
		Just x -> do 
			res <- return . eval $ x
			putStrLn . show $ res
		Nothing -> 
			putStrLn "Illegal String"

test1 = "if iszero 0 then succ 0 else pred 0"
test2 = "iszero if true then 0 else succ 0"
test3 = "iszero if false then 0 else succ 0"

