module UntypedArith where

open import Relation.Unary
open import Data.Empty
open import Data.Unit
open import Data.Nat
open import Data.Product renaming (_×_ to _∧_)
open import Data.Sum renaming (_⊎_ to _∨_)
open import Relation.Nullary.Core

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
  t21 : {t₁ : Term} → t₁ ∈ Tau → tmsucc t₁ ∈ Tau
  t22 : {t₁ : Term} → t₁ ∈ Tau → tmpred t₁ ∈ Tau
  t23 : {t₁ : Term} → t₁ ∈ Tau → tmiszero t₁ ∈ Tau
  t3 : {t₁ t₂ t₃ : Term} → (t₁ ∈ Tau) ∧ (t₂ ∈ Tau) ∧ (t₃ ∈ Tau) → tmif t₁ then t₂ else t₃ ∈ Tau

{--
s : ℕ → Term → Set
s 0 = ∅
s (suc n) = defineS
  where 
    defineS : Term → Set
    defineS tmtrue = ⊤
    defineS tmfalse = ⊤
    defineS tmzero = ⊤
    defineS (tmsucc x) = (s n) x
    defineS (tmpred x) = (s n) x
    defineS (tmif x then y else z) = ((s n) x) ∧ ((s n) y) ∧ ((s n) z) 
    defineS _ = ⊥
--}

data s : ℕ → Term → Set where
  s11 : ∀ {n} → tmtrue ∈ s (suc n)
  s12 : ∀ {n} → tmfalse ∈ s (suc n)
  s13 : ∀ {n} → tmzero ∈ s (suc n)
  s21 : ∀ {t n} → t ∈ s n → tmsucc t ∈ s (suc n)
  s22 : ∀ {t n} → t ∈ s n → tmpred t ∈ s (suc n)
  s23 : ∀ {t n} → t ∈ s n → tmiszero t ∈ s (suc n)
  s3 : ∀ {t₁ t₂ t₃ n} → (t₁ ∈ s n) ∧ (t₂ ∈ s n) ∧ (t₃ ∈ s n) → tmif t₁ then t₂ else t₃ ∈ s (suc n)


data S : Term → Set where 
  --S1' : ∀ {t n} → t ∈ s n → t ∈ S
  S1 : ∀ {n} → s n ⊆ S

LemmaSubset : Set
LemmaSubset = ∀ {n} → (s n) ⊆ (s (suc n))




_≡_  :  ∀ {ℓ₁ ℓ₂} {a} {A : Set a} → Pred A ℓ₁ → Pred A ℓ₂ → Set _ 
P ≡ Q = (P ⊆ Q) ∧ (Q ⊆ P) 
  
LemmaEqual : Set
LemmaEqual = Tau ≡ S

lemmaEqual : LemmaEqual
lemmaEqual = aux1 , aux2
  where 
    aux1 : Tau ⊆ S
    aux1 t11 = S1 s11
    aux1 t12 = S1 s12
    aux1 t13 = S1 s13
    aux1 (t21 x) = {!S1 s21 !}
    aux1 (t22 x) = {!!}
    aux1 (t23 x) = {!!}
    aux1 (t3 x) = {!!}
    aux2 : S ⊆ Tau
    aux2 = {!!}

Lemma1 : Set
Lemma1 = tmtrue ∈ Tau

lemma1 : Lemma1
lemma1 = t11

Lemma2 : Set
Lemma2 = (tmsucc tmzero) ∈ Tau

lemma2 : Lemma2 
lemma2 = t21 t13

Lemma3 : Set
Lemma3 = wrong ∉ Tau

lemma3 : Lemma3
lemma3 ()

Lemma4 : Set
Lemma4 = (tmsucc wrong) ∉ Tau

lemma4 : Lemma4
lemma4 (t21 ())


data N : ℕ → Set where 
  nn : ∀ {n : ℕ} → n ∈ N

data A : ℕ → Set where
  a0 : 0 ∈ A
  a1 : 1 ∈ A
  a2 : 2 ∈ A

Lemma01 : Set
Lemma01 = 0 ∈ A

lemma01 : Lemma01
lemma01 = a0

Lemma02 : Set
Lemma02 = A ⊆ N

lemma02 : Lemma02
lemma02 a0 = nn
lemma02 a1 = nn
lemma02 a2 = nn

Lemma03 : Set
Lemma03 = 3 ∉ A

lemma03 : Lemma03
lemma03 ()

Lemma04 : Set
Lemma04 = ¬ (N ⊆ A)
