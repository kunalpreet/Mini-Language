
# Mini-Language

This project showcases the culmination of our journey through developing language components and type-checking rules for a simple programming language. The objective is to integrate all the concepts learned and build a Haskell-based implementation of a programming language with the following features:

---

## Compiling and Execution:
Use GHCi, open your terminal, type ghci to start the interactive environment, load Haskell file with :load mini-language.hs, run functions by typing their names (e.g., run func), and exit with :quit.

---

## Language Features

- **Booleans**: Includes `true` and `false` as defined in the Typed Arithmetic Expression language.
- **Naturals**: Supports natural numbers as defined in the Typed Arithmetic Expression language.
- **Simply Typed Pure λ-Calculus**: Implements the core λ-calculus with type annotations.
- **Unit Data Type and Value**: Supports the `unit` data type, representing a single value.
- **Let Bindings**: Enables local variable bindings for enhanced expressiveness.
- **Sequencing Operator**: Allows for sequential execution of expressions.
- **Reference Operations**: Adds support for mutable state through references.

---

## Key Components

### `ssos` Function
Implements the **small-step operational semantics (SSOS)** for the language, defining how terms evaluate step by step.

### `typeCheck` Function
Verifies the type correctness of terms, ensuring they adhere to the language's type-checking rules.

---

## Implementation Notes

- **Haskell-Based Development**: The project leverages the functional programming capabilities of Haskell to implement the language features.
- **Provided Template**: The project starts with a provided `template.hs` file containing predefined data types. These types form the basis for implementing `ssos` and `typeCheck`.
- **Standardized Data Types**: While there may be slight variations from previous projects, this implementation adheres strictly to the provided data types for consistency.

---

## Summary

This project demonstrates the integration of theoretical concepts with practical programming, resulting in a functional and type-safe implementation of a simple programming language.



