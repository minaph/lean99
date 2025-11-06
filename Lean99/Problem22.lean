
def range_ (m n : Int) (current : Nat): List Int :=
  match current with
  | 0 => []
  | c+1 => (n-c) :: range_ m n (c)

def range (m n : Int) : List Int := range_ m n (n-m+1).toNat

#eval range 4 9

-- The following codes are for test and you should not edit these.

example : range 4 9 = [4, 5, 6, 7, 8, 9] := by rfl

example : range (-1) 2 = [-1, 0, 1, 2] := by rfl

example : range (-2) (-1) = [-2, -1] := by rfl
