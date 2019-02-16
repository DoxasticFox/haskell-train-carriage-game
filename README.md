# Haskell Train Carriage Game

This is a solver for the train carriage game. It's written in Haskell. The
solver prints the solutions to every possible game, from 0000 to 9999, if the
game is winnable:

```
$ ghc -O2 -threaded Main.hs && ./Main +RTS -N4 | tail
9992    9 - ((9 ÷ 9) - 2)
9993    9 - floor(9 × ((-9)^-3))
9994    9 - floor(9 × ((-4)^-9))
9995    9 - floor(9 × ((-9)^-5))
9996    9 - floor(-(9 × (9 ÷ 6!)))
9997    9 - floor(9 × ((-9)^-7))
9998    9 + (9 × (8! ÷ 9!))
9999    9 - floor(9 × ((-9)^-9))

9951 of 10000 carriage numbers from 0000 to 9999 make 10.
```

## What's The Train Carriage Game?

The aim of the train carriage game is to to produce a mathematical expression
which evaluates to 10 by using:

* The digits in a train carriage number; and
* A set of mathematical operations.

For example, given the train carriage number `5590`, one can produce the
expression `5 + 5 + 9 × 0` using addition and multiplication.

The set of allowed mathematical operations depends on the preferences of the
player(s), but typically includes addition, subtraction, multiplication and
division at the least. Other commonly used operations include `floor`,
`ceiling`, `factorial`, and even trigonometric functions, or the concatenation
of digits.

This particular program produces solutions which use only the following
operations:

* Binary
  * Addition
  * Division
  * Exponentiation
  * Multiplication
  * Subtraction
* Unary
  * Arithmetic negation
  * Ceiling
  * Factorial
  * Floor

## Future Work

* Comments in the code
* Dependency management with cabal or stack. For now you just need to try
  compiling it and address `ghc`'s complaints if any arise. YOLO.
