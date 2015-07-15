


data LamTerm a =  Var a
				| Lam a (LamTerm a)
				| App (LamTerm a) (LamTerm a)
	deriving (Eq, Show, Read)

-- |x.x y --> App (Lam "x" (Var "x")) Var "y"
-- |x.(z x) (|y.(w v) k)
-- x y
parse :: String -> Maybe (LamTerm String)
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
	(v, rest) <- findVar all
	if rest == [] then do
		return (Var v)
	else do
		lamRest <- parse rest

		return (App (Var v) lamRest)
parse [] = Nothing

findAbs :: String -> Maybe (String, String, String, String)
findAbs xs = do 
	n <- return . (findIndexOf '.') $ xs
	x <- (return . (take n)) xs
	--nn <- return . findBlockEndIndex $ ((drop (n+1)) xs)
	(abt, rest) <- getFirstBlock $ (drop (n+1) xs)
	--abt <- return $ take nn ((drop (n+1)) xs)
	--rest <- return $ drop (nn+ 2 + (length x)) xs
	(apt, rest') <- findApAbs rest
	return (x, abt, apt, rest')

findVar :: String -> Maybe (String, String)
findVar = getFirstBlock
--findVar xs = do
	--n <- return . (findIndexOf ' ') $  xs
	--x <- return $ take n xs
	--rest <- return $ drop (n+1) xs
	--return (x , rest)

findApAbs :: String -> Maybe (String, String)
findApAbs = getFirstBlock
{-
findApAbs xs = do
	n <- return . (findIndexOf ' ') $ xs
	x <- return $ take n xs
	rest <- return $ drop ((length x) + 1) xs
	return (x, rest)
-}
findIndexOf :: Char -> String -> Int
findIndexOf _ [] = 0
findIndexOf s (x:xs) 
	| s == x = 0
	| s /= x = 1 + findIndexOf s xs

findBlockEndIndex :: String -> Maybe Int
findBlockEndIndex [] = Just 0
findBlockEndIndex all@('(':xs) = findBracketEnd all
findBlockEndIndex xs = (return . (findIndexOf ' ')) xs
		

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
	return ((removeFirstBracket (take (n+1) xs)), (removeFirstBracket (drop (n+1) xs)))

removeFirstBracket :: String -> String
removeFirstBracket all@('(':xs) = 
	if ((head . reverse) xs) == ')' then 
		(removeFirstBracket . reverse . tail . reverse . tail) all
	else 
		all
removeFirstBracket xs = xs

trim :: [Char] -> String -> String
trim xs ys = 
	let
		res' = removeHeads xs ys
		res'' = removeHeads xs (reverse res')
	in (reverse res'') 
		where 
			removeHeads xs [] = []
			removeHeads xs all@(y:ys) = 
				if (y `elem` xs) then (removeHeads xs ys) else all















