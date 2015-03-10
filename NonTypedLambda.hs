
import Data.Char

data Term a = Var a | Lam a (Term a) | App (Term a) (Term a)
    deriving (Eq, Show, Read)

nextString :: String -> String
nextString (x:[]) = chr (ord (x) + 1) : [] 


isZero :: Eq a => (Term a) -> Bool
isZero (Lam f (Lam x (Var y))) = if (x == y) then True else False
isZero _ = False

success :: Eq a => (Term a) -> Maybe (Term a)
success (Lam f (Lam x n)) = Just $ Lam f (Lam x (App (Var f) n))
success _ = Nothing

convert :: Eq a => (Term a) -> Int
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

betaReduct :: Eq a => a -> (Term a) -> (Term a) -> (Term a)
betaReduct v t (Var w)
    | v == w    = t
    | otherwise = Var w
betaReduct v tout (Lam w tin)
    | v == w    = Lam w tin
    | otherwise = Lam w (betaReduct v tout tin)
betaReduct v tout (App tin1 tin2) = App (betaReduct v tout tin1) (betaReduct v tout tin2)

reduct :: Eq a => (Term a) -> (Term a) 
reduct (App (Lam v t1) t2) = reduct (betaReduct v t2 t1)
reduct (App (App t1 t2) (App t3 t4)) = reduct (App (reduct (App t1 t2)) (reduct (App t3 t4)))
reduct (App (App t1 t2) t) = reduct (App (reduct (App t1 t2)) t)  
reduct (App t (App t1 t2)) = reduct (App t (reduct (App t1 t2)))
reduct t = t


andTerm = Lam "b" (Lam "c" (App (App (Var "b") (Var "c")) (false'')))
land x y = reduct (App (reduct (App andTerm x)) y)

orTerm = Lam "d" (Lam "e" (App (Var "d") (App (Var "e") (true''))))
lor x y = reduct (App (reduct (App orTerm x)) y)