clmodule UntypedArith where

open import Relation.Unary
open import Data.Empty
open import Data.Unit
open import Data.Nat
open import Data.Product renaming (_×_ to _∧_)
open import Data.Sum renaming (_⊎_ to _∨_)
open import Relation.Nullary.Core
open import Relation.Binary.Core renaming (_≡_ to _==_)
open import Data.List

-------------------------------------------------------------------------------------------------------------------
--3.  Untyped Arithmetic Expressions
--3.1. Introduction 

data Term : Set where
  tmtrue : Term
  tmfalse : Term
  tmzero : Term
  tmif_then_else_ : Term → Term → Term → Term
  tmsucc : Term → Term
  tmpred : Term → Term
  tmiszero : Term → Term
  wrong : Term

--3.2. Sentence Constructure
--Definition 3.2.1. Inductive Term Definition 

data Tau : Term → Set where
  t11 : tmtrue ∈ Tau
  t12 : tmfalse ∈ Tau
  t13 : tmzero ∈ Tau
  t21 : ∀{t₁ : Term} → t₁ ∈ Tau → tmsucc t₁ ∈ Tau
  t22 : ∀{t₁ : Term} → t₁ ∈ Tau → tmpred t₁ ∈ Tau
  t23 : ∀{t₁ : Term} → t₁ ∈ Tau → tmiszero t₁ ∈ Tau
  t3 : ∀{t₁ t₂ t₃ : Term} → (t₁ ∈ Tau) ∧ (t₂ ∈ Tau) ∧ (t₃ ∈ Tau) → tmif t₁ then t₂ else t₃ ∈ Tau

--Definition 3.2.3. Concrete Term Definition

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
--Exercise 3.2.4. Number of elements of (s 3)

_^_ : ℕ → ℕ → ℕ
x ^ 0 = 1
x ^ (suc n) = x * (x ^ n)

calculateElemNum : ℕ → ℕ
calculateElemNum 0 = 0
calculateElemNum (suc n) = 3 + (calculateElemNum n) * 3 + (calculateElemNum n) ^ 3 

-------------------------------------------------------------------------------------------------------------------
--Exercise 3.2.5. Proof (s i) is accumlative. (s n) ⊆ (s (suc n))

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
--Proposition 3.2.6. Tau ≡ S

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
{--
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
--}
-------------------------------------------------------------------------------------------------------------------
--3.3. Induction related to Terms

--Definition 3.3.1. Consts(t) - A set of values which appears in term t
data Consts : (Term) → (Term → Set) where
  constsTrue : tmtrue ∈ Consts(tmtrue)
  constsFalse : tmfalse ∈ Consts(tmfalse)
  constsZero : tmzero ∈ Consts(tmzero)
  constsSucc : ∀{t u : Term} → t ∈ Consts u → t ∈ Consts (tmsucc u)
  constsPred : ∀ {t u : Term} → t ∈ Consts u → t ∈ Consts (tmpred u)
  constsIsZero : ∀ {t u : Term} → t ∈ Consts u →  t ∈ Consts (tmiszero u)
  constsIf : ∀{t t1 t2 t3 : Term} → t ∈ ((Consts t1) ∪ Consts t2 ∪ Consts t3) → t ∈ Consts (tmif t1 then t2 else t3)

Lemma331-1 : Set
Lemma331-1 = tmtrue ∈ Consts tmtrue

lemma331-1 : Lemma331-1
lemma331-1 = constsTrue

Lemma331-2 : Set
Lemma331-2 = tmzero ∈ Consts (tmsucc tmzero)

lemma331-2 : Lemma331-2
lemma331-2 = constsSucc constsZero

Lemma331-3 : Set
Lemma331-3 = tmtrue ∉ Consts (tmsucc tmzero)

lemma331-3 : Lemma331-3
lemma331-3 (constsSucc ())

Lemma331-4 : Set
Lemma331-4 = (tmsucc tmzero) ∉ Consts (tmsucc tmzero)

lemma331-4 : Lemma331-4
lemma331-4 (constsSucc ())

--Definition 3.3.2. size(t) , depth(t)
size : Term → ℕ
size tmtrue = 1
size tmfalse = 1
size tmzero = 1
size (tmsucc t) = (size t) + 1
size (tmpred t) = (size t) + 1
size (tmiszero t) = (size t) + 1
size (tmif t₁ then t₂ else t₃ ) = (size t₁) + (size t₂) + (size t₃) + 1
size wrong = 0


maximum : List ℕ → ℕ
maximum [] = 0
maximum (x ∷ [] ) = x
maximum (x ∷ xs) = x ⊔ (maximum xs) 

depth : Term → ℕ 
depth tmtrue = 1
depth tmfalse = 1
depth tmzero = 1
depth (tmsucc t1) = (depth t1) + 1
depth (tmpred t1) = (depth t1) + 1
depth (tmiszero t1) = (depth t1) + 1
depth (tmif t₁ then t₂ else t₃) = maximum ((depth t₁) ∷  (depth t₂)  ∷ (depth t₃) ∷ [] ) + 1
depth wrong = 0

--Lemma 3.3.3. number of t's value is equal to or less than size of t

sizeOfConsts : Term → ℕ
sizeOfConsts = {!!}
  where Term → List

--Lemma333 : Set
--Lemma333 = 

