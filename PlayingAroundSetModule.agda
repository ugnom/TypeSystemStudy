module PlayingAroundSetModule where

open import Data.Nat
open import Relation.Unary
open import Relation.Nullary.Core
open import Data.Product

data N : ℕ → Set where 
  nn : ∀ {n : ℕ} → n ∈ N

data A : ℕ → Set where
  a0 : 0 ∈ A
  a1 : 1 ∈ A
  a2 : 2 ∈ A

data B : ℕ → Set where
  b2 : 2 ∈ B

data Even : ℕ → Set where
  e0 : 0 ∈ Even
  ess : ∀{n} → n ∈ Even → suc (suc n) ∈ Even 

Lemma01 : Set
Lemma01 = 0 ∈ A

lemma01 : Lemma01
lemma01 = a0

Lemma02 : Set
Lemma02 = A ⊆ N

lemma02 : Lemma02
lemma02 _ = nn

Lemma03 : Set
Lemma03 = 3 ∉ A

lemma03 : Lemma03
lemma03 ()

Lemma04 : Set
Lemma04 = ¬ N ⊆ A

lemma04 : Lemma04
lemma04 x = lemma03 (x nn)

Lemma05 : Set
Lemma05 = ∀(n : ℕ) → (n * 2) ∈ Even

lemma05 : Lemma05
lemma05 zero = e0
lemma05 (suc n) = ess (lemma05 n)

Lemma06 : Set
Lemma06 = B ⊆ A

lemma06 : Lemma06
lemma06 b2 = a2

Lemma07 : Set 
Lemma07 = ¬ A ⊆ B

lemma07 : Lemma07
lemma07 x = aux07 (x a0)
  where 
    aux07 : 0 ∉ B
    aux07 () 

Lemma08 : Set
Lemma08 = ∃ λ n → n ∈ B

lemma08 : Lemma08
lemma08 = (2 , b2)
