---
title: Getting started
---

# Getting started

This guide walks through setting up a development environment for mlang and
explains the workflows for the command line compiler and the browser-based
playground.

## Prerequisites

- **Rust** – Install the latest stable toolchain via [rustup](https://rustup.rs/).
- **Node.js and npm** – Required for building the playground UI. Node.js 18 or
  newer is recommended.
- **wasm-pack** *(optional)* – If you want to rebuild the `mylang-playground`
  package, install [`wasm-pack`](https://rustwasm.github.io/wasm-pack/installer/).

Clone the repository:

```bash
git clone https://github.com/<your-github-username>/mlang.git
cd mlang
```

## Command line compiler

Build the CLI crate:

```bash
cargo build -p mylang-cli
```

Compile the sample program:

```bash
cargo run -p mylang-cli -- sample.mlang
```

The compiler prints the generated WAT to stdout. Redirect the output to a file
if you want to inspect it later:

```bash
cargo run -p mylang-cli -- sample.mlang > output.wat
```

Run the Rust tests to ensure everything still passes:

```bash
cargo test
```

## Browser playground

The `mylang-playground` crate exports the compiler pipeline to WebAssembly. The
`playground/` Vite project loads that Wasm module and provides a UI with source
editing, compilation, execution, and step-by-step debugging.

Install the npm dependencies and start the dev server:

```bash
cd playground
npm install
npm run dev
```

By default the playground opens at `http://localhost:5173/`. When you build a
production bundle, Vite writes static assets to `playground/dist`:

```bash
npm run build
```

Upload the contents of `playground/dist` to your hosting provider. When using
GitHub Pages, place the built files into a `playground/` folder alongside the
published documentation so the relative **Documentation** link keeps working.

### Configuring the documentation link

Create a `.env` file in the `playground/` directory if you want to customize the
documentation URL during development:

```
VITE_DOCUMENTATION_URL=http://localhost:4000/
```

This is useful when you run `bundle exec jekyll serve` inside the `doc/`
directory and want the playground link to open the locally served site.

## Rebuilding the Wasm package

The Wasm bindings under `pkg/` are typically checked into source control after
they are generated. If you need to rebuild them:

```bash
cd mylang-playground
wasm-pack build --target web --out-dir ../playground/pkg --out-name mlang_playground
```

This command compiles the Rust crate and places the generated JS glue code into
`playground/pkg/`, which is what the Vite app imports.

## Troubleshooting

- **`wasm-bindgen` version mismatch** – Delete the old `pkg/` directory and run
  `wasm-pack build` again to ensure the JS bindings and Wasm module match the
  crate version.
- **Playground shows a blank page** – Check the browser console for errors. Most
  issues stem from serving the static files at the wrong base path; rebuild the
  Vite project with `npm run build -- --base=/playground/` if you deploy the UI
  under a subdirectory.
- **`cargo test` fails** – Run `cargo test -p <crate-name>` to narrow down the
  failing package and inspect the offending unit tests.

Once your environment is ready, continue with the [language tour](./language-tour.md)
for an overview of syntax and semantics.
