import Lean

/-- List of natural numbers from `1` to `n` -/
def List.nrange (n : Nat) : List Nat :=
  match n with
  | 0 => []
  | 1 => [1]
  | n + 1 => nrange n ++ [n + 1]

example : List.nrange 5 = [1, 2, 3, 4, 5] := rfl

-- You can use this function
#check List.nrange

def diffSelect (count range : Nat) (nrange: List Nat := List.nrange range) : IO (List Nat) :=
  match count, nrange with
  | _, [] => pure []
  | 0, _ => pure []
  | _+1, [x] => pure [x]
  | n+1, xs => IO.rand 0 (xs.length -1) >>= fun r =>
    diffSelect n range (xs.eraseIdx r) >>= fun l =>
      pure (xs[r]! :: l)


-- The following codes are for test and you should not edit these.

def runTest (count range : Nat) : IO Unit := do
  let result ← diffSelect count range
  let check := result.eraseDups.length == count
    |> (· && result.all (List.nrange range).contains)
  if check then
    IO.println "ok!"
  else
    throw <| .userError s!"failed: diffSelect {count} {range} = {result}"

#eval runTest 3 3

#eval runTest 1 1

#eval runTest 2 2

#eval runTest 6 49

#eval runTest 1998 1999

#eval runTest 5668 5998
