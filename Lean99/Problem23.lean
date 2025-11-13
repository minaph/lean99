import Lean

variable {α : Type} [Inhabited α]

def rndSelect (l : List α) (n : Nat) : IO (List α) :=
  match n with
  | 0 => pure ([] : List α)
  | m+1 => rndSelect l m >>= fun l' =>  IO.rand 0 (l.length - 1) >>= fun r => (pure (l[r]!::l'))

-- The following codes are for test and you should not edit these.

def runTest [BEq α] [ToString α] (l : List α) (n : Nat) : IO Unit := do
  let result ← rndSelect l n
  let check := result.length == n
    |> (· && result.all l.contains)
  if check then
    IO.println s!"ok!"
  else
    throw <| .userError s!"failed: rndSelect {l} {n} = {result}"

example : rndSelect [1, 2, 3] 0 = pure ([] : List Nat) := rfl
#eval runTest [1, 2, 3] 0

#eval runTest ['a', 'b'] 1

#eval runTest [1, 2, 3, 4, 5] 2

#eval runTest [1, 1, 1] 2

#eval runTest [2, 2, 2] 12

#eval runTest (List.range 5200) 1897
