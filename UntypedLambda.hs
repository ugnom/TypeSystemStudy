


data LamTerm a =  Var a
				| Lam a (LamTerm a)
				| App (LamTerm a) (LamTerm a)
	deriving (Eq, Show, Read)

-- |x.x y --> App (Lam "x" (Var "x")) Var "y"
-- |x.(z x) (|y.(w v) k)
-- x y
parse :: String -> Maybe (LamTerm String)
--parse ('(' : xs) + do
parse ('(' : xs) = do 
	res <- (parse . (trim "() ")) xs
	return res
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
	(abt, rest) <- return . getFirstBlock $ (drop (n+1) xs)
	--abt <- return $ take nn ((drop (n+1)) xs)
	--rest <- return $ drop (nn+ 2 + (length x)) xs
	(apt, rest') <- findApAbs rest
	return (x, abt, apt, rest')

findVar :: String -> Maybe (String, String)
findVar = return . getFirstBlock
--findVar xs = do
	--n <- return . (findIndexOf ' ') $  xs
	--x <- return $ take n xs
	--rest <- return $ drop (n+1) xs
	--return (x , rest)

findApAbs :: String -> Maybe (String, String)
findApAbs = return . getFirstBlock
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

findBlockEndIndex :: String -> Int
findBlockEndIndex [] = 0
findBlockEndIndex all@(x:xs) = 
	if x == '(' then
		findIndexOf ')' all
	else
		findIndexOf ' ' all

getFirstBlock :: String -> (String, String)
getFirstBlock xs = 
	let 
		n = findBlockEndIndex xs
	in ((trim "() " (take n xs)), (trim "() " (drop n xs)))

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















