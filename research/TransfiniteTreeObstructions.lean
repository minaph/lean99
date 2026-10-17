import Lean

/-!
# Obstructions to naive transfinite extensions of rational trees
Lean 4.34.1; core library only.
These theorems concern concrete models, not the full surreal-number field.
No `sorry`, `admit`, or user-defined axioms.
-/

namespace TransfiniteTree

inductive Direction where
  | L
  | R
  deriving Repr, DecidableEq

abbrev OmegaPath := Nat → Direction

/-- The constantly-right infinite path. -/
def allRight : OmegaPath := fun _ => .R

/-- Append the old path after a new leading R. -/
def prependRight (p : OmegaPath) : OmegaPath
  | 0 => .R
  | n + 1 => p n

/-- Ordinal absorption at the path level: R^(1+ω) = R^ω. -/
theorem right_absorption : prependRight allRight = allRight := by
  funext n
  cases n with
  | zero => rfl
  | succ n => rfl

/-- The finite rule V(Rp)=V(p)+1 cannot hold on every ω-path. -/
theorem no_global_right_translation
    (value : OmegaPath → Int)
    (h : ∀ p, value (prependRight p) = value p + 1) : False := by
  have hR := h allRight
  rw [right_absorption] at hR
  omega

/-- Calkin-Wilf child transformations on positive numerator/denominator pairs. -/
def cwStep (p : Nat × Nat) : Direction → Nat × Nat
  | .L => (p.1, p.1 + p.2)
  | .R => (p.1 + p.2, p.2)

/-- Exact pair after n left steps from a/b. -/
def leftTailPair (a b n : Nat) : Nat × Nat := (a, n * a + b)

/-- One more left edge preserves the explicit all-left-tail formula. -/
theorem left_tail_successor (a b n : Nat) :
    leftTailPair a b (n + 1) = cwStep (leftTailPair a b n) .L := by
  simp [leftTailPair, cwStep, Nat.succ_mul, Nat.add_assoc,
    Nat.add_left_comm]

/-- Illustrative Calkin-Wilf finite computations. -/
example : cwStep (1, 1) .L = (1, 2) := by decide
example : cwStep (1, 2) .R = (3, 2) := by decide

/-- Degree-2 jets, representing first two formal coefficients of a tail. -/
structure Jet2 where
  coefficientOne : Rat
  coefficientTwo : Rat
  deriving Repr, DecidableEq

/-- Formal first two coefficients of t/(1+c*t): t - c*t^2 + O(t^3). -/
def leftTailJet (c : Rat) : Jet2 := ⟨1, -c⟩

def jetAdd (x y : Jet2) : Jet2 :=
  ⟨x.coefficientOne + y.coefficientOne,
   x.coefficientTwo + y.coefficientTwo⟩

/-- The restricted left-tail family is not closed under addition,
    already at its first nonzero coefficient. -/
theorem sum_of_two_left_tails_not_one_tail (c d e : Rat) :
    jetAdd (leftTailJet c) (leftTailJet d) ≠ leftTailJet e := by
  intro h
  have hc := congrArg Jet2.coefficientOne h
  simp [jetAdd, leftTailJet] at hc

/-- Even the doubling of one represented tail is absent from this family. -/
theorem double_tail_not_representable (c e : Rat) :
    jetAdd (leftTailJet c) (leftTailJet c) ≠ leftTailJet e := by
  exact sum_of_two_left_tails_not_one_tail c c e

#print axioms TransfiniteTree.right_absorption
#print axioms TransfiniteTree.no_global_right_translation
#print axioms TransfiniteTree.left_tail_successor
#print axioms TransfiniteTree.sum_of_two_left_tails_not_one_tail
end TransfiniteTree
