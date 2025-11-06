variable {α : Type}

def insertAt (e : α) (l : List α) (i : Nat) : List α :=
  match l, i with
  | [], _ => [e]
  | _, 0 => panic! "Unexpected"
  | x::xs, 1 => e::(x::xs)
  | x::xs, n => x:: insertAt e xs (n-1)

#eval insertAt "X" ["1", "2", "3", "4"] 2

-- The following codes are for test and you should not edit these.

example : insertAt "X" ["1", "2", "3", "4"] 2 = ["1", "X", "2", "3", "4"] := rfl

example : insertAt "X" ["1", "2", "3", "4"] 1 = ["X", "1", "2", "3", "4"] := rfl

example : insertAt "X" [] 1 = ["X"] := rfl
