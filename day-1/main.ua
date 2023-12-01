FilterDigits ← (▽×≥@0:≤@9..)
CalibrationValues ← (parse ⇌[⊢⇌:⊢⊔.])
FirstSolve ← /+∵CalibrationValues∵⍜⊔ FilterDigits

Input ← &fras "input.txt"

InputToArray ← ⊜□≠ @\n .

FirstSolve InputToArray Input
