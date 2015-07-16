module UntypedLambda (LamTerm(..)) where 

import QueueAndStack
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

data LamTerm a =  Var a
				| Lam a (LamTerm a)
				| App (LamTerm a) (LamTerm a)
	deriving (Eq, Show, Read)



betaReduce :: String -> (LamTerm String) -> (LamTerm String) -> (LamTerm String)
betaReduce v t (Var w)
    | v == w    = t
    | otherwise = Var w
betaReduce v tout (Lam w tin)
    | v == w    = Lam w tin
    | otherwise = Lam w (betaReduce v tout tin)
betaReduce v tout (App tin1 tin2) = App (betaReduce v tout tin1) (betaReduce v tout tin2)

findFreeVariables :: LamTerm String -> [String]
findFreeVariables tm =
	where 
		runFind :: LamTerm String -> StateT [a] Maybe (LamTerm String)
		runFind (Lam s t) = do 
			push s
			

alphaConvert :: String -> String -> LamTerm String -> LamTerm String
alphaConvert x y tm = undefined

eval :: (LamTerm String) -> (LamTerm String) 
eval (App (Lam v t1) t2) = eval (betaReduce v t2 t1)
eval (App (App t1 t2) (App t3 t4)) = eval (App (eval (App t1 t2)) (eval (App t3 t4)))
eval (App (App t1 t2) t) = eval (App (eval (App t1 t2)) t)  
eval (App t (App t1 t2)) = eval (App t (eval (App t1 t2)))
eval t = t


isZero :: (LamTerm String) -> Bool
isZero (Lam f (Lam x (Var y))) = if (x == y) then True else False
isZero _ = False

success :: (LamTerm String) -> Maybe (LamTerm String)
success (Lam f (Lam x n)) = Just $ Lam f (Lam x (App (Var f) n))
success _ = Nothing

convert :: (LamTerm String) -> Int
convert = undefined


zero = Lam "f" (Lam "x" (Var "x"))
one = Lam "f" (Lam "x" (App (Var "f") (Var "x")))
two = Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var ("x")))))

true = Lam "t" (Lam "f" (Var "t")) 
true' = Lam "u" (Lam "g" (Var "u"))
true'' = Lam "s" (Lam "h" (Var "s"))
false = Lam "t" (Lam "f" (Var "f"))
false' = Lam "u" (Lam "g" (Var "g"))
false'' = Lam "s" (Lam "h" (Var "h"))

andTerm = Lam "b" (Lam "c" (App (App (Var "b") (Var "c")) (false'')))
land x y = eval (App (eval (App andTerm x)) y)

orTerm = Lam "d" (Lam "e" (App (Var "d") (App (Var "e") (true''))))
lor x y = eval (App (eval (App orTerm x)) y)


deBruijnIndex :: LamTerm String -> LamTerm Int
deBruijnIndex = undefined