# haskell-reducer2

## Overview
This project is a simple interpreter and visualizer for combinatory logic expressions written in a restricted subset of Haskell syntax.

It parses user-defined combinators, builds an internal representation, and performs step-by-step reduction using normal-order evaluation.

---

## Features
- Parsing a subset of Haskell using `haskell-src`
- Custom internal AST representation
- Safe substitution with variable renaming (avoids name capture)
- Step-by-step reduction tracing
- Validation of input program correctness

---

## Supported Syntax
The program supports a limited subset of Haskell:
- Function definitions
- Function application
- Variables and constants
- No type system

```hs
data Def = Def Name [Pat] Expr
data Expr = Var Name | Expr :$ Expr
type Pat = Name
type Name = String
newtype Prog = Prog {progDefs :: [Def]}
```

Example:

```haskell
s x y z = x z (y z)
k x y = x
main = s k k x
```

---

## Build Instructions (Cabal)

This project is configured as a Cabal executable.

### Build
```bash
cabal build
```

### Run
```bash
cabal run haskell-reducer2 -- path/to/input.hs
```

### Help
```bash
cabal run haskell-reducer2 -- --help
```

---

## Usage

The program expects a **file containing combinator definitions** as input.

Example:
```bash
cabal run haskell-reducer2 -- example.hs
```

Where `example.hs` contains:
```haskell
S x y z = x z (y z)
k x y = x
main = S k k x
```

---

## Input Requirements

A valid program must:
- Define each combinator exactly once
- Have unique parameter names within each definition
- Include a `main` definition with **no arguments**

---

## How It Works

1. **Parsing**
   - The input file is parsed using `Language.Haskell.Parser`
   - The AST is converted into a simplified internal representation

2. **Definition Map**
   - All combinators are stored in a map for fast lookup

3. **Reduction**
   - Uses **normal order evaluation** (outermost, leftmost)
   - Reduces only when enough arguments are available
   - Performs safe substitution with renaming

4. **Output**
   - Prints all definitions
   - Prints a separator line
   - Prints up to 30 reduction steps of `main`

---

## Example Output

```
S x y z = x z (y z)
k x y = x
main = S k k x
------------------------------------------------------------
S k k x
k x (k x)
x
```

---

## Project Structure

- `app/Main.hs` – main program
- Parser utilities – convert Haskell AST
- Reduction engine – evaluation logic
- Validation – program correctness checks

---

## Notes
- Reduction is limited to 30 steps
- Undefined names are treated as constants
- Arguments are not reduced prematurely
- Created as assignment for [Functional Programming in Haskell Course @ MIMUW](https://github.com/mbenke/pf26)
