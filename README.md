# mlang

mlang is an experimental programming language that compiles to WebAssembly. The
project is split into multiple crates that take care of lexing, parsing, type
checking, and code generation so that the compiler can target native Wasm
runtimes as well as the browser.

## Repository layout

- `mylang-core/` – language frontend and semantic analysis implemented in Rust.
- `mylang-cli/` – command line compiler that reads `.mlang` programs and writes
  the generated WebAssembly Text (WAT).
- `mylang-llvm-codegen/` – optional backend that lowers the typed abstract
  syntax tree into LLVM IR for native compilation.
- `mylang-wasm-codegen/` – WebAssembly backend used by both the CLI and the web
  playground.
- `mylang-playground/` – Rust crate that exposes the compiler to JavaScript
  through `wasm-bindgen`.
- `playground/` – Vite application that provides an in-browser playground for
  experimenting with the language.
- `doc/` – documentation that is meant to be published through GitHub Pages.

## Getting started

### Requirements

- Rust 1.75 or newer and Cargo
- Node.js 18+ and npm (only required for the web playground)

### Building the CLI compiler

```bash
cargo build -p mylang-cli
```

Run the compiler against the sample program to emit WAT:

```bash
cargo run -p mylang-cli -- sample.mlang
```

### Running the test suite

The core language crates ship with unit tests. Run them all with:

```bash
cargo test
```

### Language server support

An experimental Language Server Protocol (LSP) implementation is available in the
`mylang-lsp` crate. It provides diagnostics, go-to-definition, hover tooltips, and
semantic highlighting.

First, build the language server executable:

```bash
cargo build -p mylang-lsp
```

Editors can communicate with the server over standard input/output. A Visual
Studio Code extension is provided in `editors/vscode/mlang-lsp` and a Zed
extension in `editors/zed/mlang`. Configure the extension settings to point to
the `mylang-lsp` executable if it is not available in your `PATH`.

If you change the playground frontend you can run the UI checks with:

```bash
cd playground
npm install
npm run lint
```

### Working on the web playground

The `playground/` directory contains a Vite project. After installing the npm
dependencies you can start a development server:

```bash
cd playground
npm install
npm run dev
```

The server prints a local URL (usually <http://localhost:5173>) where you can
edit, compile, and run mlang snippets in the browser. The UI calls into the
`mylang-playground` Wasm package for compilation and execution.

## Documentation and GitHub Pages

The `doc/` directory contains Markdown sources for the project documentation.
GitHub Pages can be configured to publish this content:

1. Open the repository settings on GitHub and navigate to **Pages**.
2. Set the **Source** to the branch you want to publish (for example `main`).
3. Choose **/doc** as the publishing folder and save the configuration.

GitHub Pages will serve the documentation from
`https://<your-github-username>.github.io/<repository-name>/`. The web
playground links to this URL (see the header in `playground/index.html`). During
local development you can override the link by adding a `.env` file next to
`playground/package.json`:

```
VITE_DOCUMENTATION_URL=http://localhost:4000/
```

If you run a local static-site server—such as `bundle exec jekyll serve` in the
`doc/` directory—the playground link will open that URL instead.

## License

This repository does not currently declare a license. Please make sure to add
one before distributing binaries.
