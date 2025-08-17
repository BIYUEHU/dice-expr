# DiceScript

A safe and fast dice expression evaluator base on Idris2.

## What is DiceScript?

DiceScript is a superset of Mathematical (Arithmetic) Expression, it supports many advanced features, such as:

- Lambdas (Anonymous functions): `\x -> x+1` (Refers to Haskell lambda syntax)
- Array types: `[1, 2, 3]`
- Infix operaters: Dice `d`, Range `..`, Random Integer `~`, Concatenation `++`, Colon `:`
- More math functions: `round`, `floor`, `ceil`, `abs`, `sqrt`...
- Array functions: `map`, `filter`, `fold`, `reduce`, `flatMap`, `every`, `some`, `length`, `reverse`, `sort`, `include` ...
- Set functions: `union`, `intersection`, `difference` ...
- Random functions: `int`, `real`, `bool`, `shuffle`, `pick` ...
- List comprehension (Similar to Mathematical Set): `[x*2 | x <- [1..5]]`
- Math

> DiceScript is not a programming language, it is a DSL (Domain Specific Language) of base expression, an legal programm of DiceScript has only a expression.

## What is Idris2?

Idris2 is a dependently typed and functional programming language, it powerfully ensures type and program safety.

## License

Under the **BAN-ZHINESE-USING** License.

## Basic Syntax

DiceScript expressions follow these BNF grammar rules:

```bnf
<expr> ::= <lambda_expr> | <prefix_expr> | <infix_expr> | <call_expr> | <literal> | <ident> | "(" <expr> ")"
<lambda_expr> ::= "\\" <param_list> "->" <expr>
<param_list> ::= <ident> | <ident> "," <param_list>
<call_expr> ::= [a-z]+ "(" <expr> ("," <expr>)* ")"
<infix_expr> ::= <expr> <infix_op> <expr>
<infix_op> ::= "+" | "-" | "*" | "/" | "%" | "^" | "==" | "!=" | ">" | ">=" | "<" | "<=" | "&&" | "||" | "d" | "++" | ":" | "~" | ".."
<prefix_expr> ::= <prefix_op> <expr>
<prefix_op> ::= "-" | "!"
<literal> ::= <bool> | <number> | "[" <expr> ("," <expr>)* "]" | "[]"
<number> ::= [0-9]+ | [0-9]+ "." [0-9]+ | "e" | "p"
<bool> ::= "T" | "F"
```

## Types

DiceScript is dynamically strongly typed with the following types:

- **Bool**: Boolean values (`T` or `F`)
- **Double**: Floating-point numbers
- **[Any]**: Arrays of any type
- **Lambda**: Function values

### Type Coercion

The only implicit type conversion allowed is for **non-empty pure numeric arrays** in arithmetic operations - they are automatically summed to a number before calculation.

## Infix Operators

### `(d): Int -> Int -> [Double]`

Dice rolling operator. Generates `x` random integers from 1 to `y` (inclusive). Returns an array of results, not automatically summed.

Requirements: `x` must be a natural number, `y` must be a positive integer greater than 1.

```ts
> 2d6
[3, 5]
> 0d6
[]
```

### `(..): Int -> Int -> [Double]`

Range operator. Generates all integers from `x` to `y` (inclusive). Both operands must be integers.

```ts
> 1..5
[1, 2, 3, 4, 5]
> 5..1
[]
```

### `(~): Int -> Int -> Double`

Random integer generator. Generates a random integer in range `[x, y)`. Requires `x < y`.

```ts
> 1~10
7
```

### `(++): [Any] -> [Any] -> [Any]`

Array concatenation operator. Both operands must be arrays.

```ts
> [1, 2] ++ [3, 4]
[1, 2, 3, 4]
```

### `(:): Any -> [Any] -> [Any]`

Cons operator. Prepends an element to an array.

```ts
> 1 : [2, 3]
[1, 2, 3]
```

### Arithmetic Operators

- `(+): Double... -> Double` - Addition (variadic)
- `(-): Double... -> Double` - Subtraction (requires at least one argument)
- `(*): Double... -> Double` - Multiplication (variadic)
- `(/): Double... -> Double` - Division (requires at least one argument)
- `(%): Double -> Double -> Double` - Modulo
- `(^): Double... -> Double` - Exponentiation (requires at least one argument)

```ts
> 1 + 2 + 3
6
> 10 / 2
5
```

### Comparison Operators

- `(==): Double... -> Bool` - Equality (all arguments must be equal)
- `(!=): Double... -> Bool` - Inequality (not all arguments are equal)
- `(<): Double... -> Bool` - Less than (strictly increasing)
- `(>): Double... -> Bool` - Greater than (strictly decreasing)
- `(<=): Double... -> Bool` - Less than or equal
- `(>=): Double... -> Bool` - Greater than or equal

```ts
> 1 < 2 < 3
T
> 1 == 1 == 1
T
```

### Logical Operators

- `(&&): Bool... -> Bool` - Logical AND (variadic)
- `(||): Bool... -> Bool` - Logical OR (variadic)
- `(!): Bool -> Bool` - Logical NOT (prefix)

```ts
> T && F
F
> !F
T
```

## Lambda Expressions

Lambda expressions follow Haskell-style syntax:

```ts
> (\x -> x + 1)(5)
6
> (\x, y -> x * y)(3, 4)
12
```

## Internal Functions

### Mathematical Functions

#### `abs: Double -> Double`

Returns the absolute value of a number.

```ts
> abs(-5)
5
```

#### `sin: Double -> Double`

#### `cos: Double -> Double`

#### `tan: Double -> Double`

Trigonometric functions (input in radians).

```ts
> sin(0)
0
> cos(0)
1
```

#### `asin: Double -> Double`

#### `acos: Double -> Double`

#### `atan: Double -> Double`

Inverse trigonometric functions.

#### `sinh: Double -> Double`

#### `cosh: Double -> Double`

#### `tanh: Double -> Double`

Hyperbolic functions.

#### `asinh: Double -> Double`

#### `acosh: Double -> Double`

#### `atanh: Double -> Double`

Inverse hyperbolic functions.

#### `log: Double -> Double`

Natural logarithm.

```ts
> log(2.718)
1
```

#### `exp: Double -> Double`

Exponential function (e^x).

```ts
> exp(1)
2.718
```

#### `sqrt: Double -> Double`

Square root.

```ts
> sqrt(16)
4
```

#### `cbrt: Double -> Double`

Cube root.

```ts
> cbrt(27)
3
```

#### `ceil: Double -> Double`

Ceiling function (round up).

```ts
> ceil(3.2)
4
```

#### `floor: Double -> Double`

Floor function (round down).

```ts
> floor(3.8)
3
```

#### `round: Double -> Double`

Round to nearest integer.

```ts
> round(3.6)
4
```

#### `trunc: Double -> Double`

Truncate decimal part.

```ts
> trunc(3.9)
3
```

#### `max: Double... -> Double`

Returns the maximum of given numbers.

```ts
> max(1, 5, 3)
5
```

#### `min: Double... -> Double`

Returns the minimum of given numbers.

```ts
> min(1, 5, 3)
1
```

#### `hypot: Double... -> Double`

Returns the square root of the sum of squares of arguments.

```ts
> hypot(3, 4)
5
```

#### `imul: Double -> Double -> Double`

Integer multiplication (truncates inputs to integers).

```ts
> imul(3.7, 2.9)
6
```

### Logical Functions

#### `and: Bool... -> Bool`

Logical AND of multiple boolean values.

```ts
> and(T, T, F)
F
```

#### `or: Bool... -> Bool`

Logical OR of multiple boolean values.

```ts
> or(T, F, F)
T
```

#### `not: Bool -> Bool`

Logical NOT.

```ts
> not(T)
F
```

#### `if: Bool -> Any -> Any -> Any`

Conditional expression.

```ts
> if(T, 1, 2)
1
```

### Array Functions

#### `array: Any... -> [Any]`

Creates an array from arguments.

```ts
> array(1, 2, 3)
[1, 2, 3]
```

#### `length: [Any] -> Double`

Returns the length of an array.

```ts
> length([1, 2, 3])
3
```

#### `slice: [Any] -> Double -> Double -> [Any]`

Extracts a slice from an array (start index, count).

```ts
> slice([1, 2, 3, 4], 1, 2)
[2, 3]
```

#### `sum: [Double] -> Double`

Sums all numbers in an array.

```ts
> sum([1, 2, 3])
6
```

#### `fill: [Any] -> Any -> [Any]`

Fills an array with a given value.

```ts
> fill([1, 2, 3], 0)
[0, 0, 0]
```

#### `include: [Any] -> Any -> Bool`

Checks if an array contains an element.

```ts
> include([1, 2, 3], 2)
T
```

### Set Operations

#### `union: [Any] -> [Any] -> [Any]`

Returns the union of two arrays.

```ts
> union([1, 2], [2, 3])
[1, 2, 3]
```

#### `intersection: [Any] -> [Any] -> [Any]`

Returns the intersection of two arrays.

```ts
> intersection([1, 2, 3], [2, 3, 4])
[2, 3]
```

#### `difference: [Any] -> [Any] -> [Any]`

Returns elements in the first array but not in the second.

```ts
> difference([1, 2, 3], [2, 4])
[1, 3]
```

#### `contain: [Any] -> [Any] -> Bool`

Checks if the first array contains all elements of the second array.

```ts
> contain([1, 2, 3], [1, 2])
T
```

### Random Functions

#### `shuffle: [Any] -> [Any]`

Randomly shuffles an array.

```ts
> shuffle([1, 2, 3, 4])
[3, 1, 4, 2]
```

#### `pick: [Any] -> Double -> [Any]`

Randomly picks a specified number of elements from an array.

```ts
> pick([1, 2, 3, 4], 2)
[2, 4]
```

#### `int: Double -> Double -> Double`

Generates a random integer in range `[start, end)`.

```ts
> int(1, 10)
7
```

#### `real: Double -> Double -> Double`

#### `real: Double -> Double`

Generates a random real number. With two arguments: `[start, end)`. With one argument: `[0, end)`.

```ts
> real(1, 2)
1.42
> real(10)
7.31
```

#### `bool: Double -> Bool`

Generates a random boolean with given probability (0.0 to 1.0).

```ts
> bool(0.7)
T
```

## Constants

- `e`: Euler's number (≈ 2.718)
- `p`: Pi (≈ 3.14159)

```ts
> e
2.718
> p
3.14159
```
