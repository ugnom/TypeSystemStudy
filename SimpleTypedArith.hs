import Data.Set
import Control.Monad.Writer

data Term = TTrue 
		|	TFalse 
		| 	Zero
		| 	Succ Term
		|	Pred Term
		|	IsZero Term
		|	IfThenElse Term Term Term
		|	Wrong
	deriving (Ord, Eq, Show, Read)
	-- Set requires Ord for the performance...

data Type = T | TNat | TBool
	deriving (Eq, Show, Read)

type TypedTerm = (Term, Type)

getType :: Term -> (Maybe Type)
getType TTrue = Just TBool
getType TFalse = Just TBool
getType Zero = Just TNat
getType (Succ n) = do
	r <- getType n
	if (r == TNat) then (return r) else Nothing
getType (Pred n) = do
	r <- getType n 
	if (r == TNat) then (return r) else Nothing
getType (IsZero n) = do
	r <- getType n
	if (r == TNat) then (return TBool) else Nothing
getType (IfThenElse x y z) = do
	r <- getType x
	if (r == TNat) 
		then Nothing
		else do
			ry <- getType y
			rz <- getType z
			if (ry == rz) then return ry else Nothing



eval1 :: Term -> Term 
--eval1 (IfThenElse TTrue t1 _) = t1
--eval1 (IfThenElse TFalse _ t2) = t2
eval1 (IfThenElse t t1 t2) 
	| t == TTrue = t1
	| t == TFalse = t2
	| (isBadBool t) == True = Wrong
	| otherwise 			= IfThenElse (eval1 t) t1 t2
eval1 (Succ t) 
	| (isBadNat t) 	= Wrong
	| otherwise 	= Succ $ eval1 t
eval1 (Pred Zero) = Zero
eval1 (Pred (Succ t)) = t
eval1 (Pred t) 
	| (isBadNat t) == True 	= Wrong
	| otherwise 			= Pred $ eval1 t
eval1 (IsZero Zero) = TTrue
eval1 (IsZero (Succ _)) = TFalse
eval1 (IsZero t) 
	| (isBadNat t) == True 	= Wrong
	| otherwise 			= IsZero $ eval1 t




isValue :: Term -> Bool
isValue Wrong = True
isValue TTrue = True
isValue TFalse = True
isValue t
	| isNum t == True = True
	| otherwise = False

isNum :: Term -> Bool
isNum Zero = True
isNum (Succ n) = isNum n
isNum _ = False

isBadNat :: Term -> Bool
isBadNat Wrong = True
isBadNat TTrue = True
isBadNat TFalse = True
isBadNat _ = False

isBadBool :: Term -> Bool
isBadBool Wrong = True
isBadBool t
	| isNum t == True = True
	| otherwise	= False 

eval :: Term -> Term
eval t
	| isValue t == True = t
	| otherwise 		= eval $ eval1 t

eval' :: Term -> WriterT [Term] IO Term
eval' t = do 
	tell [t]
	if isValue t == True 
		then return t
		else do
			t' <- return . eval1 $ t
			eval' t'


evalMain :: IO()
evalMain =  
	do 
		putStr "TERM > "
		t <- getLine
		(result, logs) <- runWriterT (eval' (read t))
		mapM_ (putStrLn . show) logs
		evalMain

checkType :: Term -> WriterT [Term] IO Term
checkType t = do
	let checkRes = getType t
	if (checkRes == Nothing) 


evalMain' :: String -> IO()
evalMain' s =
	do 
		(result, logs) <- runWriterT (eval' (read s))
		mapM_ (putStrLn . show) logs






