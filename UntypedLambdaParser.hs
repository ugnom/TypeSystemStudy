import UntypedLambda

-- |x.x y --> App (Lam "x" (Var "x")) Var "y"
-- |x.(z x) (|y.(w v) k)
-- x y
parse :: String -> Maybe (LamTerm String)
parse all@('(':xs) = do 
	trimedStr <- removeFirstBracket all
	parse trimedStr
parse xs = do
	dividedStr <- divideToBlocks xs
	if length dividedStr == 1 then do
		case (head . head) dividedStr of
			'|' -> parseLambda . head $ dividedStr
			_ -> return $ Var (head dividedStr) 
	else do
	trimedDividedStr <- mapM removeFirstBracket dividedStr
	parsedDividedStr <- mapM parse trimedDividedStr
	return . applyApp $ parsedDividedStr
	where 
		parseLambda :: String -> Maybe (LamTerm String)
		parseLambda ('|':xs) = do
			let n = findIndexOf '.' xs
			let l = take n xs
			t <- parse (drop (n+1) xs)
			return (Lam l t)
		parselambda xs = Nothing

		applyApp :: [LamTerm String] -> LamTerm String
		applyApp (x:[]) = x
		applyApp (x:y:xs) = applyApp ((App x y) : xs)

divideToBlocks :: String -> Maybe [String]
divideToBlocks [] = Just []
divideToBlocks xs = do
	(f, rest) <- getFirstBlock xs
	ys <- divideToBlocks rest
	return (f : ys)
	where 	
		getFirstBlock :: String -> Maybe (String, String)
		getFirstBlock xs = do
			n <- findBlockEndIndex xs
			return (((trim " ") . (take (n+1))) xs , ((trim " ") . (drop (n+1))) xs)

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

removeFirstBracket :: String -> Maybe String
removeFirstBracket all@('(':xs) = do
	n <- findBracketEnd all
	if (n + 1) == (length all) then 
		(removeFirstBracket . reverse . tail . reverse . tail) all
	else 
		Just all
removeFirstBracket xs = Just xs

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

findIndexOf :: Char -> String -> Int
findIndexOf _ [] = 0
findIndexOf s (x:xs) 
	| s == x = 0
	| s /= x = 1 + findIndexOf s xs

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















