---
title: "Design Document"
weight: 40
---

# Design Document: Racket-to-Luau Compiler for Roblox

## 1. Introduction & Motivation

Roblox game development primarily uses Luau, a gradually typed language derived from Lua. While powerful, developers coming from other ecosystems might miss features from their preferred languages. Racket is a powerful, multi-paradigm language from the Lisp family, known for its strong macro system, functional programming capabilities, and optional static type system (Typed Racket).

This document outlines the design for a compiler that translates Racket (and Typed Racket) code into Luau, enabling developers to leverage Racket's features for building Roblox games and experiences. The primary goal is to combine Racket's expressiveness and safety (especially with Typed Racket) with the ability to target the popular Roblox platform.

**Target Audience:** Racket developers interested in Roblox development, and Roblox developers looking for alternative language options with strong functional programming and metaprogramming features.

## 2. Goals

*   Compile a practical subset of Racket and Typed Racket into functional, readable Luau code.
*   Support core Racket features: functions (including closures), `let`/`let*`/`letrec` bindings, basic data structures (lists, vectors, structs), modules (`require`/`provide`).
*   Leverage Typed Racket for static analysis, translating type annotations to Luau type hints where possible.
*   Provide mechanisms for interoperating with Roblox APIs (Luau functions, Instance manipulation, events).
*   Generate Luau code compatible with the Roblox engine environment.
*   Maintain reasonable performance, although parity with handwritten Luau is not a primary goal initially.
*   Preserve tail call optimization where feasible, as Luau supports proper tail calls.

## 3. Non-Goals

*   Complete Racket standard library support. The focus will be on core language features and Roblox interoperability.
*   Support for advanced Racket features like full continuation support (`call/cc`), complex contracts, or highly sophisticated procedural macros that are difficult to map to Luau.
*   Generating the absolute most performant Luau code possible; readability and correctness are prioritized initially.
*   Full macro compatibility; focus on macros that expand to core forms.

## 4. Architecture

The compiler will adopt a multi-stage approach:

```text
Racket/Typed Racket Code
       |
       V
+--------------------+
|   Racket Frontend  |  (Leverages Racket's tools)
|--------------------|
| - Reader (S-expr)  |
| - Expander (Macros)|
| - Type Checker (TR)|
+--------------------+
       |
       V
 Core Racket AST / IR
 (Simplified Racket)
       |
       V
+--------------------+
|      Compiler      |
|--------------------|
| - IR -> Luau AST   |  (Main translation logic)
| - Luau Code Gen    |
| - (Opt.) Optimizer |
+--------------------+
       |
       V
  Luau Source Code
 (+ Runtime Library)
       |
       V
   Roblox Engine
 (Luau VM)
```

*   **Frontend (Racket-based):**
    *   **Parsing:** Utilize Racket's built-in `read` function to parse S-expressions into Racket data structures.
    *   **Expansion:** Use Racket's `expand` function to handle macro expansion, converting macro uses into core Racket forms. This simplifies the compiler by offloading complex macro logic to the Racket system itself.
    *   **Type Checking:** For Typed Racket files (`#lang typed/racket`), invoke the Typed Racket type checker. Extract type information from the typed AST for potential use in Luau type annotation generation and ensuring type safety.
*   **Intermediate Representation (IR):**
    *   Define a simplified IR representing core Racket semantics (e.g., lambda calculus with bindings, primitive operations, conditionals). This IR should be easier to translate directly to Luau constructs than the full Racket AST. It might strip away Racket-specific features already handled (like complex macros) and represent types explicitly if derived from Typed Racket.
*   **Backend (Luau Generation):**
    *   **IR -> Luau AST:** Translate the compiler's IR into an Abstract Syntax Tree representing Luau code. This involves mapping Racket concepts to Luau equivalents (see Section 5).
    *   **Luau Code Generation:** Pretty-print the Luau AST into valid Luau source code string(s).
    *   **Optimization (Optional):** Perform basic optimizations on the Luau AST or generated code, such as constant folding or simple dead code elimination. More advanced Luau-specific optimizations could be added later.

## 5. Language Feature Mapping

| Racket Feature        | Luau Target                                                                 | Notes                                                                                                   |
| :-------------------- | :-------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------ |
| Numbers               | `number`                                                                    | Potential precision differences for non-integers.                                                     |
| Strings               | `string`                                                                    | Generally straightforward mapping.                                                                      |
| Booleans (`#t`, `#f`) | `true`, `false`                                                             | Direct mapping.                                                                                         |
| Symbols               | `string`                                                                    | Use strings. Ensure runtime handles symbol equality correctly if needed (e.g., interning via a table). |
| Lists (`cons`, `car`, `cdr`) | Luau Tables (arrays) + Runtime Library                                      | Requires runtime functions (`rt.cons`, `rt.car`, `rt.cdr`, `rt.isNull`). Likely represented as `{car_val, cdr_val}`. |
| Vectors               | Luau Tables (arrays)                                                        | Direct mapping using 1-based indexing adjustments.                                                      |
| Structs               | Luau Tables + Metatables                                                    | Use tables for fields. Metatables can mimic struct predicates and potentially methods.                  |
| `define` (top-level)  | Luau global/module variable                                                 |                                                                                                         |
| `define` (internal)   | Luau `local` variable                                                       |                                                                                                         |
| `lambda`              | Luau `function`                                                             | Handle closures correctly (Luau supports this).                                                         |
| `let`, `let*`         | Luau `local` variables, nested `do..end` blocks                             | Straightforward lexical scoping.                                                                        |
| `letrec`              | Luau `local` variables, potentially forward declaration for functions       | Luau supports recursive local functions directly. Mutual recursion needs care.                        |
| `if`, `cond`          | Luau `if`/`elseif`/`else`                                                   | `cond` expands into nested `if` statements.                                                             |
| `begin`               | Luau `do ... end` block or sequential statements                            | Direct mapping.                                                                                         |
| Modules (`require`/`provide`) | Luau `ModuleScript` and `require`                                   | Map `provide`d names to the returned table of the `ModuleScript`. Map `require` to Luau `require`.        |
| Typed Racket Types    | Luau Type Annotations (`:`), Runtime Checks (optional)                      | Map base types (Number, String, Boolean) directly. Complex types (structs, unions, generics) might require runtime representation or loss of specificity. |
| Tail Calls            | Luau Tail Calls                                                             | Aim to generate Luau code that preserves tail calls where Racket uses them (Luau VM supports TCO).        |
| Exceptions            | Luau `error()`, `pcall()`/`xpcall()`                                        | Map Racket's exception system to Luau's error handling mechanisms.                                      |
| FFI (Roblox APIs)     | Special forms/functions -> Direct Luau calls                                | Define a mechanism (e.g., `(roblox-api "ServiceName" "Method" arg1 ...)` ) translated to `game:GetService("ServiceName"):Method(arg1, ...)` |

## 6. Runtime System (Luau)

A crucial component is a runtime library written in Luau (`racket_runtime.luau` or similar), which must be included in the Roblox project. This library will provide:

*   Implementations for core Racket functions not built into Luau (e.g., `cons`, `car`, `cdr`, `null?`, `list`, `vector->list`, `apply`, symbol handling).
*   Helper functions for struct creation and type checking (predicates).
*   Support for the module system linkage.
*   Potential implementations for parts of the numeric tower if needed beyond standard Luau numbers.

## 7. Roblox Integration

*   **FFI:** Introduce specific Racket forms or functions (e.g., `(import-roblox [GetService : (-> string Instance)])`) that the compiler recognizes and translates into direct calls to Roblox globals (`game`, `workspace`, etc.) or specific API methods. Type annotations from Typed Racket could help ensure correct API usage.
*   **Event Handling:** Map Roblox events (e.g., `Instance.Event:Connect(...)`) to Racket functions. This might involve generating Luau closures that call back into the compiled Racket code. Example: `(roblox-connect some-instance "Touched" (lambda (hit) ...))`

## 8. Build Process & Tooling

*   The compiler itself will likely be a Racket program, runnable from the command line: `racket compile.rkt --input game.rkt --output CoreScripts/game.luau`
*   Integrate with tools like Rojo: The compiler could watch Racket source files and automatically output corresponding Luau `ModuleScript`s into a Rojo-managed directory structure.
*   **Debugging:** This is challenging. Initially, debugging might have to occur at the Luau level. Future work could involve generating source maps to map Luau execution points back to the original Racket code.

## 9. Challenges

*   **Semantic Mismatches:** Bridging the gap between Racket's features (e.g., exact numeric tower, macros, contracts) and Luau's capabilities.
*   **Performance:** Generated code might be less performant than idiomatic Luau due to abstraction layers or the runtime library overhead.
*   **Standard Library:** Deciding on and implementing the necessary subset of the Racket standard library in the Luau runtime.
*   **Macros:** While relying on Racket's expander helps, ensuring macros expand to translatable core forms is key. Complex procedural macros interacting heavily with the compiler internals might not be supportable.
*   **Error Reporting:** Mapping runtime errors in Luau back to the original Racket source location effectively.
*   **Luau Typing:** Fully leveraging Luau's gradual type system when translating from Typed Racket, especially for complex types.

## 10. Future Work

*   Expand the subset of supported Racket features and libraries.
*   Implement more sophisticated optimizations targeting Luau.
*   Develop better debugging tools (source maps).
*   Explore support for more advanced Racket features if feasible (e.g., limited contract support).
*   Performance profiling and optimization of the runtime library and generated code.
*   Enhanced Roblox API bindings generation. 