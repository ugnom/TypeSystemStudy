


data LamTerm a =  Var a
				| Lam a (LamTerm a)
				| App (LamTerm a) (LamTerm a)
	deriving (Eq, Show, Read)

-- |x.x y --> App (Lam "x" (Var "x")) Var "y"
-- |x.(z x) (|y.(w v) k)
-- x y
parse :: String -> Maybe (LamTerm String)
parse ('|' : xs) = do
	(x, abt, rest) <- findAbs xs
	if rest == [] then do 
		lamAbt <- parse abt
		return (Lam x lamAbt)
	else do
		lamAbt <- parse abt
		lamRest <- parse rest
		return (App (Lam x lamAbt) lamRest)
parse all@(a : xs) = do
	(v, rest) <- findVar all
	if rest == [] then do
		return (Var v)
	else do
		lamRest <- parse rest
		return (App (Var v) lamRest)

findAbs :: String -> Maybe (String, String, String)
findAbs xs = do 
	n <- findIndexOf '.' xs
	x <- return . (take n) xs
	nn <- findIndexOf ' ' ((drop n+1) xs)
	abt <- take nn ((drop n+1) xs)
	rest <- drop (nn+1) xs
	return (x, abt, rest)

findVar :: String -> Maybe (String, String)
findVar xs = do 
	n <- findIndexOf " "
	x <- take n xs
	rest <- drop (n+1) xs
	return (x , rest)

findIndexOf :: Char -> String -> Maybe Int
findIndexOf _ [] = Nothing
findIndexOf s (x:xs) 
	| s == x = Just 0
	| s /= x = do
		n <- findIndexOf xs
		return (n + 1)