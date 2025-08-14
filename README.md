# DiceScript

A safe and fast dice expression evaluator base on Idris2.

## What is DiceScript?

DiceScript is a superset of Mathematical (Arithmetic) Expression, it supports many advanced features, such as:

- Lambdas (Anonymous functions): `\x -> x+1` (Refers to Haskell lambda syntax)
- Array types: `[1, 2, 3]`
- Infix operaters: Dice `d`, Range `..`, Random Integer `~`, Concatenation `++`, Colon `:`
- More math functions: `round`, `floor`, `ceil`, `abs`, `sqrt`...
- List comprehension (Similar to Mathematical Set): `[x*2 | x <- [1..5]]`
- Math

> DiceScript is not a programming language, it is a DSL (Domain Specific Language) of base expression, an legal programm of DiceScript has only a expression.

## What is Idris2?

Idris2 is a dependently typed and functional programming language, it powerfully ensures type and program safety.

<!-- $$
\begin{aligned}
\text{Given: } & \text{A function } f(x) = x^2 + 3x + 2 \\
\text{Find: } & \text{The roots of the function.} \end{aligned}
$$ -->
