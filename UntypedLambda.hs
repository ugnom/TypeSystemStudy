import Control.Monad.State


data LamTerm a =  Var a
				| Lam a (LamTerm a)
				| App (LamTerm a) (LamTerm a)
	deriving (Eq, Show, Read)

-- |x.x y --> App (Lam "x" (Var "x")) Var "y"
-- |x.(z x) (|y.(w v) k)
-- x y
parse :: String -> Maybe (LamTerm String)
parse xs = do
	dividedStr <- divideToBlocks xs
	if length dividedStr == 1 then do
		case (head . head) dividedStr of
			'|' -> parseLambda . head $ dividedStr
			_ -> return $ Var (head dividedStr) 
	else do
	parsedDividedStr <-  mapM parse dividedStr
	return . applyApp $ parsedDividedStr

parseLambda :: String -> Maybe (LamTerm String)
parseLambda xs = undefined

applyApp :: [LamTerm String] -> LamTerm String
applyApp (x:[]) = x
applyApp (x:y:xs) = applyApp ((App x y) : xs)

{-
parse (' ' : xs) = parse xs
parse all@('(' : xs) = do
	(f, rest) <- getFirstBlock all
	f' <- parse f
	if rest == [] then do
		return f'
	else do
		(s, rest') <- getFirstBlock rest
		s' <- parse s
		if rest' == [] then do 
			return $ App f' s'
		else do
			rest'' <- parse rest'
			return $ App (App f' s') rest''


parse ('|' : xs) = do
	(x, abt, apt, rest) <- findAbs xs
	if apt == [] then do 
		lamAbt <- parse abt
		return (Lam x lamAbt)
	else do
		lamAbt <- parse abt
		lamApt <- parse apt
		if rest == [] then do
			return (App (Lam x lamAbt) lamApt)
		else do  
			lamRest <- parse rest
			return (App (App (Lam x lamAbt) lamApt) lamRest) 
parse all@(a : xs) = do
	(v, rest) <- getFirstBlock all
	if rest == [] then do
		return (Var v)
	else do
		lamRest <- parse rest

		return (App (Var v) lamRest)
parse [] = Nothing
-}



findAbs :: String -> Maybe (String, String, String, String)
findAbs xs = do 
	n <- return . (findIndexOf '.') $ xs
	x <- (return . (take n)) xs
	--nn <- return . findBlockEndIndex $ ((drop (n+1)) xs)
	(abt, rest) <- getFirstBlock $ (drop (n+1) xs)
	--abt <- return $ take nn ((drop (n+1)) xs)
	--rest <- return $ drop (nn+ 2 + (length x)) xs
	(apt, rest') <- getFirstBlock rest
	return (x, abt, apt, rest')

divideToBlocks :: String -> Maybe [String]
divideToBlocks [] = Just []
divideToBlocks xs = do
	(f, rest) <- getFirstBlock xs
	ys <- divideToBlocks rest
	return (f : ys)


findIndexOf :: Char -> String -> Int
findIndexOf _ [] = 0
findIndexOf s (x:xs) 
	| s == x = 0
	| s /= x = 1 + findIndexOf s xs

findBlockEndIndex :: String -> Maybe Int
findBlockEndIndex [] = Just 0
findBlockEndIndex all@('(':xs) = findBracketEnd all
findBlockEndIndex all@('|':xs) = findLambdaEnd all
findBlockEndIndex (' ':xs) = do 
	n <- (findBlockEndIndex xs)
	return (n+1)
findBlockEndIndex xs = (return . (findIndexOf ' ')) xs

findLambdaEnd :: String -> Maybe Int
findLambdaEnd ('|':xs) = do
	n <- return . (findIndexOf '.') $ xs
	m <- findBlockEndIndex (drop (n+1) xs)
	return (n + m + 1 + 1)


findBracketEnd :: String -> Maybe Int 
findBracketEnd [] = Just 0
findBracketEnd ('(':xs) = inFindBracketEnd 1 xs
	where 
		inFindBracketEnd _ [] = Nothing 
		inFindBracketEnd n ('(':xs) = do
			m <- inFindBracketEnd (n+1) xs
			return (m+1)
		inFindBracketEnd 1 (')':xs) = do	
			return 1
		inFindBracketEnd n (')':xs) = do
			m <- inFindBracketEnd (n-1) xs
			return (m+1)
		inFindBracketEnd n (x:xs) = do
			m <- inFindBracketEnd n xs
			return (m+1)
findBracketEnd xs = Just 0

getFirstBlock :: String -> Maybe (String, String)
getFirstBlock xs = do
	n <- findBlockEndIndex xs
	return (((trim " ") . (take (n+1))) xs , ((trim " ") . (drop (n+1))) xs)

removeFirstBracket :: String -> Maybe String
removeFirstBracket all@('(':xs) = do
	n <- findBracketEnd all
	if (n + 1) == (length all) then 
		(removeFirstBracket . reverse . tail . reverse . tail) all
	else 
		Just all
removeFirstBracket xs = Just xs

trimSpace :: String -> String
trimSpace = trim " "

trim :: [Char] -> String -> String
trim _ [] = []
trim [] xs = xs
trim xs ys = 
	let
		res' = removeHeads xs ys
		res'' = removeHeads xs (reverse res')
	in (reverse res'') 
		where 
			removeHeads xs [] = []
			removeHeads xs all@(y:ys) = 
				if (y `elem` xs) then (removeHeads xs ys) else all















