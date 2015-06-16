module UntypedArith where

open import Relation.Unary
open import Data.Empty
open import Data.Unit
open import Data.Nat
open import Data.Product renaming (_×_ to _∧_)
open import Data.Sum renaming (_⊎_ to _∨_)
open import Relation.Nullary.Core
open import Relation.Binary.Core renaming (_≡_ to _==_)

data Term : Set where
  tmtrue : Term
  tmfalse : Term
  tmzero : Term
  tmif_then_else_ : Term → Term → Term → Term
  tmsucc : Term → Term
  tmpred : Term → Term
  tmiszero : Term → Term
  wrong : Term


data Tau : Term → Set where
  t11 : tmtrue ∈ Tau
  t12 : tmfalse ∈ Tau
  t13 : tmzero ∈ Tau
  t21 : ∀{t₁ : Term} → t₁ ∈ Tau → tmsucc t₁ ∈ Tau
  t22 : ∀{t₁ : Term} → t₁ ∈ Tau → tmpred t₁ ∈ Tau
  t23 : ∀{t₁ : Term} → t₁ ∈ Tau → tmiszero t₁ ∈ Tau
  t3 : ∀{t₁ t₂ t₃ : Term} → (t₁ ∈ Tau) ∧ (t₂ ∈ Tau) ∧ (t₃ ∈ Tau) → tmif t₁ then t₂ else t₃ ∈ Tau

data s : ℕ → Term → Set where
  s11 : ∀ {n} → tmtrue ∈ s (suc n)
  s12 : ∀ {n} → tmfalse ∈ s (suc n)
  s13 : ∀ {n} → tmzero ∈ s (suc n)
  s21 : ∀ {t n} → t ∈ s n → tmsucc t ∈ s (suc n)
  s22 : ∀ {t n} → t ∈ s n → tmpred t ∈ s (suc n)
  s23 : ∀ {t n} → t ∈ s n → tmiszero t ∈ s (suc n)
  s3 : ∀ {t₁ t₂ t₃ n} → (t₁ ∈ s n) ∧ (t₂ ∈ s n) ∧ (t₃ ∈ s n) → tmif t₁ then t₂ else t₃ ∈ s (suc n)

data S : Term → Set where 
  SS : ∀ {n} → s n ⊆ S

-------------------------------------------------------------------------------------------------------------------
--3.2.4 Number of elements of (s 3)

_^_ : ℕ → ℕ → ℕ
x ^ 0 = 1
x ^ (suc n) = x * (x ^ n)

calculateElemNum : ℕ → ℕ
calculateElemNum 0 = 0
calculateElemNum (suc n) = 3 + (calculateElemNum n) * 3 + (calculateElemNum n) ^ 3 

-------------------------------------------------------------------------------------------------------------------
--3.2.5 Proof (s i) is accumlative. (s n) ⊆ (s (suc n))

LemmaSubset : Set
LemmaSubset = ∀ {n} → (s n) ⊆ (s (suc n))

lemmaSubset : LemmaSubset
lemmaSubset {zero} () 
lemmaSubset {suc n} s11 = s11
lemmaSubset {suc n} s12 = s12
lemmaSubset {suc n} s13 = s13
lemmaSubset {suc n} (s21 x) = s21 (lemmaSubset {n} x)
lemmaSubset {suc n} (s22 x) = s22 (lemmaSubset {n} x)
lemmaSubset {suc n} (s23 x) = s23 (lemmaSubset {n} x)
lemmaSubset {suc n} (s3 x) = s3 (  lemmaSubset {n} (proj₁ x) ,
                                                           lemmaSubset {n} (proj₁ (proj₂  x)) , 
                                                           lemmaSubset {n}  (proj₂ (proj₂ x) )  )

-------------------------------------------------------------------------------------------------------------------
--3.2.6 Tau ≡ S

_≡_  :  ∀ {ℓ₁ ℓ₂} {a} {A : Set a} → Pred A ℓ₁ → Pred A ℓ₂ → Set _ 
P ≡ Q = (P ⊆ Q) ∧ (Q ⊆ P) 
 
LemmaA10 : Set
LemmaA10 = ∀{n t} → t ∈ (s n) → t ∈ S

lemmaA10 : LemmaA10
lemmaA10 = SS

LemmaA11 : Set
LemmaA11 = ∀{t} → t ∈ S → ∃ λ n → t ∈ (s n)

lemmaA11 : LemmaA11
lemmaA11 (SS {suc n} s11) = suc n , s11
lemmaA11 (SS {suc n} s12) = suc n , s12
lemmaA11 (SS {suc n} s13) = suc n , s13
lemmaA11 (SS {suc n} (s21 x)) = suc n , s21 x
lemmaA11 (SS {suc n} (s22 x)) = suc n , s22 x
lemmaA11 (SS {suc n} (s23 x)) = suc n , s23 x
lemmaA11 (SS {suc n} (s3 x)) = suc n , s3 x

LemmaA112 : Set
LemmaA112 = ∀{t} → t ∈ S → (tmsucc t) ∈ S

lemmaA112 : LemmaA112 
lemmaA112 {t} x = {!!}

LemmaA12 : Set
LemmaA12 = ∀ {t} → t ∈ Tau → ∃ λ n → t ∈ (s n)

lemmaA12 : LemmaA12
lemmaA12 t11 = {!!}
lemmaA12 t12 = {!!}
lemmaA12 t13 = {!!}
lemmaA12 (t21 x) = {!!}
lemmaA12 (t22 x) = {!!}
lemmaA12 (t23 x) = {!!}
lemmaA12 (t3 x) = {!!}

LemmaEqual : Set
LemmaEqual = Tau ≡ S

lemmaEqual : LemmaEqual
lemmaEqual = aux1 , aux2
  where 
    aux1 : Tau ⊆ S
    aux1 x = SS {!LemmaA12 x!}
    aux2 : S ⊆ Tau
    aux2 = {!!}




