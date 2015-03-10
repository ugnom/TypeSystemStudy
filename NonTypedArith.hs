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

consts :: Term -> Set Term
consts TTrue = singleton TTrue
consts TFalse = singleton TFalse
consts Zero = singleton Zero
consts (Succ x) = consts x
consts (Pred x) = consts x
consts (IsZero x) = consts x
consts (IfThenElse x y z) = unions [(consts x), (consts y), (consts z)]

termIndex :: Int -> Set Term
termIndex 0 = empty
termIndex n =
	let
		prevTerm = elems $ termIndex (n-1)
		termSingle = [TTrue, TFalse, Zero]
		termSucc = [Succ x |x <- prevTerm]
		termPred = [Pred x |x <- prevTerm]
		termIsZero = [IsZero x |x <- prevTerm]
		termIfThenElse = [IfThenElse x y z | x <- prevTerm, y <- prevTerm, z <- prevTerm]
	in
		fromList (termSingle ++ termSucc ++ termPred ++ termIsZero ++ termIfThenElse)

term n = unions (Prelude.map termIndex [0..n])

tSize :: Term -> Int
tSize TTrue = 1
tSize TFalse = 1
tSize Zero = 1
tSize (Succ t) = (tSize t) + 1
tSize (Pred t) = (tSize t) + 1
tSize (IsZero n) = (tSize n) + 1
tSize (IfThenElse x y z) = (tSize x) + (tSize y) + (tSize z)

tDepth :: Term -> Int
tDepth TTrue = 1
tDepth TFalse = 1
tDepth Zero = 1
tDepth (Succ t) = (tDepth t) + 1
tDepth (Pred t) = (tDepth t) + 1
tDepth (IsZero t) = (tDepth t) + 1
tDepth (IfThenElse x y z) = (maximum [tDepth x, tDepth y, tDepth z]) + 1 



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

evalMain' :: String -> IO()
evalMain' s =
	do 
		(result, logs) <- runWriterT (eval' (read s))
		mapM_ (putStrLn . show) logs






