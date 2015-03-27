module NonTypedArith where

open import Data.Bool

data Term : Set where
  Ttrue : Term
  Tfalse : Term
  Tif_then_else_ : Term → Term → Term → Term
{--
size : Term → ℕ
size true = 1
size false = 1
size if x then y else z = (size x) + (size y) + (size z)
--}

isValue : Term → Bool
isValue Ttrue = true
isValue Tfalse = true
isValue (Tif x then y else z) = (isValue x) ∧ (isValue y) ∧ (isValue z)


eval1 : Term → Term
eval1 (Tif x then y else z) = {!!} 
eval1 Ttrue = Ttrue
eval1 Tfalse = Tfalse 
