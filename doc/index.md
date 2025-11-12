---
title: Welcome to mlang
---

# mlang

mlang is a multi-paradigm programming language that targets WebAssembly. The
compiler front-end is written in Rust and exposes several backends so that the
same source code can run on the command line, in native runtimes, and inside the
browser. This site collects the core documentation for the language and the
surrounding tooling.

- [Getting started](./getting-started.md)
- [Language tour](./language-tour.md)
- [Web playground](#web-playground)

## Key features

- **Two syntax styles** – mlang accepts both S-expression style and C-style
  function calls. The lexer distinguishes between `foo (...)` and `foo(...)` so
  the parser can treat them differently without backtracking.
- **Hybrid parsing strategy** – The main parser is an LL(1) implementation that
  handles statements and function declarations, while mathematical expressions
  inside `$$ ... $$` blocks are parsed through a Pratt parser dedicated to
  infix notation.
- **Strong typing** – The compiler performs semantic analysis on a typed AST.
  Variable mutability, function signatures, and expression types are validated
  before code generation.
- **WebAssembly-first** – The primary backend emits WAT that can be executed
  through Wasm runtimes or embedded directly in the browser playground.

## Architecture overview

The compilation pipeline is split into distinct phases:

1. **Declaration collection** – A pre-pass records all function declarations and
   their signatures so that definitions can reference one another.
2. **Parsing** – The parser converts the raw token stream into a *Raw AST* that
   captures the syntactic structure without assigning meaning yet.
3. **Semantic analysis** – The compiler transforms the Raw AST into a *Typed
   AST*, resolving identifiers, enforcing mutability rules, and running type
   checks.
4. **Code generation** – The Wasm backend produces WAT by traversing the typed
   nodes and lowering them to WebAssembly instructions.

This separation keeps the parser lightweight while giving the compiler enough
context to emit efficient code.

## Web playground

The [web playground](../playground/) packages the compiler as WebAssembly and
runs it entirely in the browser. The interface lets you:

- Edit mlang code with instant feedback.
- Compile to WAT and inspect the generated text.
- Run the program and view its stdout output.
- Step through execution to diagnose control flow issues.

The **Documentation** link in the playground header points back to this site. If
you host the playground on GitHub Pages alongside the documentation, place the
compiled Vite build into `playground/dist` and upload it to a subdirectory (for
example `/playground/`). The relative link will take visitors back to the docs
at the site root.

## Contributing

We welcome pull requests that improve the compiler, language runtime, or
documentation. To contribute:

1. Fork the repository and create a topic branch.
2. Make your changes and add tests where applicable.
3. Run `cargo test` to ensure the Rust crates pass their checks.
4. For UI changes, run `npm run lint` inside `playground/`.
5. Open a pull request that explains the motivation and design.

See the [Getting started guide](./getting-started.md) for more detailed setup
instructions.
