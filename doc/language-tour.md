---
title: Language tour
---

# Language tour

This document introduces the most important language constructs. The examples
are based on the `sample.mlang` program and the compiler design notes.

## Functions

Declare functions with the `fn` keyword. Function arguments and return types are
annotated in the signature:

```mylang
fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

Functions can be referenced before they are defined thanks to the declaration
collection pre-pass.

## Expressions and syntax styles

mlang accepts S-expression style calls as well as familiar C-style calls.
Spacing determines which variant the parser recognizes.

```mylang
let x = add (sub 5 3) $1+2$;   // S-expression: space before `(`
let y = add(sub 5 3, $1+2$);   // C-style: no space before `(`
```

- **S-expression calls** treat each element as a positional argument. They are
  flexible for composing pipelines of expressions.
- **C-style calls** use commas to separate arguments and support trailing
  parentheses directly after the identifier.

## Variables and mutability

Use `let` to bind names. Bindings are immutable by default. Add the `mut`
modifier to allow reassignment.

```mylang
let greeting = "Hello";
let mut counter = 0;

counter = counter + 1;      // ok: `counter` is mutable
greeting = "Hi";            // error: cannot reassign immutable binding
```

The semantic analyzer enforces mutability and type rules before code generation.
Assignment statements evaluate to the unit type `()` so they can only appear in
statement positions.

## Control flow

### If expressions

`if` expressions evaluate to the last expression of the taken branch. Conditions
must evaluate to `bool`.

```mylang
fn abs(value: i32) -> i32 {
    if value < 0 {
        -value
    } else {
        value
    }
}
```

### While loops

`while` loops execute a block while a boolean condition evaluates to `true`.
Variables declared inside the loop body are scoped to the loop.

```mylang
fn countdown(start: i32) {
    let mut current = start;
    while current > 0 {
        println current;
        current = current - 1;
    }
}
```

The loop itself has the unit type `()` and therefore cannot be used where a
value is expected.

## Blocks and scope

Blocks `{ ... }` introduce new lexical scopes. The value of a block is the value
of its final expression. This allows functions to return early by placing the
resulting expression at the end of a block.

```mylang
fn greet(name: string) -> string {
    let message = {
        let prefix = "Hello, ";
        string_concat prefix name
    };

    message
}
```

## Interoperability with WebAssembly

The compiler emits WebAssembly Text (WAT). Standard library functions such as
`println` lower to imported Wasm functions that write to the console. The web
playground bundles a small runtime using `wasmi` to execute the generated
modules entirely in the browser.

## Math blocks

Wrap infix math expressions in `$$ ... $$` to opt in to the Pratt parser. This is
useful for arithmetic-heavy code.

```mylang
let value = add 1 $$ 2 * 3 $$;
```

Inside math blocks you can use `+`, `-`, `*`, `/`, and parentheses with the
expected precedence rules.

## Next steps

Continue experimenting in the [web playground](../playground/) or inspect the
[Getting started guide](./getting-started.md) for build instructions. The
[repository README](../README.md) lists the available crates and entry points.
