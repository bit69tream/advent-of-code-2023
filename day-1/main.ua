FilterDigits ← (▽×≥@0:≤@9..)
CalibrationValues ← (⇌[⊢⇌:⊢⊔.])

FromRawInput ← ⊜□≠ @\n .

Input ← FromRawInput &fras "input.txt"

/+∵(parse CalibrationValues)∵⍜⊔ FilterDigits Input
{"part 1" ;.}

Input
:×1∵(□⍣⌕ (□ 0 ;;;) "one" ⊔.)
:×2∵(□⍣⌕ (□ 0 ;;;) "two" ⊔.)
:×3∵(□⍣⌕ (□ 0 ;;;) "three" ⊔.)
:×4∵(□⍣⌕ (□ 0 ;;;) "four" ⊔.)
:×5∵(□⍣⌕ (□ 0 ;;;) "five" ⊔.)
:×6∵(□⍣⌕ (□ 0 ;;;) "six" ⊔.)
:×7∵(□⍣⌕ (□ 0 ;;;) "seven" ⊔.)
:×8∵(□⍣⌕ (□ 0 ;;;) "eight" ⊔.)
×9∵(□⍣⌕ (□ 0 ;;;) "nine" ⊔)
++++++++

×:-@0:×≥@0:≤@9.. Input
+
∵(□ ▽ >0 . ⊔)
/+≡parse +@0∵CalibrationValues
{"part 2" ;.}
