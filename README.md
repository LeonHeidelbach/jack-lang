# jack-lang

[![build](https://img.shields.io/badge/build-local-brightgreen)](#) [![license](https://img.shields.io/badge/license-GPL--3.0-blue)](LICENSE)  

A multi-component implementation of the Jack toolchain (compiler, VM translator, assembler) implemented as a Rust workspace.

This repository contains several independent components that together implement a full Jack development toolchain inspired by the Nand2Tetris project. The Jack language and higher-level toolchain design are the intellectual property of the Nand2Tetris authors Noam Nisan and Shimon Schocken; this project is an independent implementation and learning exercise built on those ideas.

## Table of Contents

- [Repository layout](#repository-layout)
- [Quickstart](#quickstart)
  - [Prerequisites](#prerequisites)
  - [Build everything](#build-everything)
  - [End-to-end example (full pipeline)](#end-to-end-example-full-pipeline)
- [Running individual components](#running-individual-components)
  - [Hack assembler](#hack-assembler)
  - [Jack compiler](#jack-compiler)
  - [VM translator](#vm-translator)
  - [Shared utilities](#shared-utilities)
  - [Editor support](#editor-support)
- [Testing and examples](#testing-and-examples)
  - [Running tests](#running-tests)
- [Additional features (beyond standard jack-compiler requirements)](#additional-features-beyond-standard-jack-compiler-requirements)
- [License](#license)
- [Acknowledgements](#acknowledgements)

## Repository layout

- hack_assembler/: Hack assembler (converts Hack assembly to binary .hack)
- jack_compiler/: Jack compiler (parses .jack, generates VM or assembly)
- vm/: VM translator (translates VM commands to Hack assembly)
- shared/: Shared libraries and utilities used across components
- examples/: Small runnable example inputs for each component

## Quickstart

### Prerequisites

- Rust and Cargo (stable) installed: https://www.rust-lang.org/tools/install
- Supported toolchain: Rust 1.70+ is recommended (the workspace uses the Rust 2021 edition). Use rustup to install and manage the toolchain.
- All tooling required to run the tests and helper workflows is included in the `tools/` directory at the repository root; no external Nand2Tetris emulators or simulators are required.

### Build everything

Run from the repository root:

```console
cargo build --workspace --release
```

### End-to-end example (full pipeline)

The repository contains a small set of example inputs in `examples/` so you can run the toolchain locally. For complex examples the recommended layout is to put each example's files into its own subdirectory (the repository now includes `examples/jack_compiler/Square/`, `examples/jack_compiler/Pong/`, and `examples/jack_compiler/ComplexArrays/`). When files for an example are grouped in one directory, you can compile the example directory directly without listing individual files.

Preferred workflow (compile an example directory):

```console
# compile a grouped example directory to a final .hack file
cargo run -p jack_compiler -- --compile examples/jack_compiler/Square/ --output out/Square.hack
# result: out/Square.hack (ready to run in a Hack emulator)
```

Three-stage pipeline (preferred, directory mode):

```console
# 1) compile the example directory and emit VM/assembly into a directory
cargo run -p jack_compiler -- --compile examples/jack_compiler/Square/ --output out_vm/
# 2) translate the produced VM files to a single Hack assembly file
cargo run -p vm -- --compile out_vm/ --output out.asm
# 3) assemble the assembly file into a .hack ROM
cargo run -p hack_assembler -- --compile out.asm --out-dir out_final/

# results:
# - out_vm/: intermediate .vm files
# - out.asm: single Hack assembly file
# - out_final/*.hack: final Hack ROM images produced by the assembler
```

Alternative: if you prefer not to group files into subdirectories, list the files that belong to the example explicitly when invoking the compiler:

```console
cargo run -p jack_compiler -- --compile examples/jack_compiler/Square/Main.jack examples/jack_compiler/Square/Square.jack examples/jack_compiler/Square/SquareGame.jack --output out/Square.hack
```

Tips:
- Prefer compiling grouped example directories for convenience when running the whole example without supplying every file.
- Output directories used in these examples are created by the tools; inspect `out/`, `out_vm/`, and `out_final/` in the repository root to review generated artifacts.

## Running individual components

Each component can be built and run independently with Cargo from the repository root. Example invocations below assume each component exposes a CLI binary named after its directory; adjust bin names if different. Each component binary supports a `--help` or `-h` flag to display usage information and available options.

### Hack assembler

```console
cargo run -p hack_assembler -- --compile <file.asm> --out-dir <path>
# short form:
cargo run -p hack_assembler -- -c <file.asm> -o <path>
```

The assembler accepts one or more .asm files and writes compiled .hack files into the output directory (or current directory if none is specified).

### Jack compiler

```console
cargo run -p jack_compiler -- --compile <file.jack|dir> --output <dir|file.hack>
# example - compile a single file into a directory:
cargo run -p jack_compiler -- --compile Main.jack --output out/
# example - compile a whole directory into out/:
cargo run -p jack_compiler -- --compile src_dir/ --output out/
# optional: generate AST XML files:
cargo run -p jack_compiler -- --compile Main.jack --xml xml_out_dir
```

The compiler accepts .jack files or directories and produces VM/assembly or a final .hack output depending on the output target and flags.

### VM translator

```console
cargo run -p vm -- --compile <file.vm|dir> --output <dir|file.asm>
# short form:
cargo run -p vm -- -c <file.vm> -o <path>
```

The VM translator compiles .vm files into Hack assembly; use the output flag to control the destination file or directory.

### Shared utilities

The shared/ folder contains common parsing, AST, and code-generation helpers used by more than one component; include it in the build via the workspace configuration when required.

## Editor support

A Vim syntax file for the Jack language is included at the repository root as `jack.vim`. Install it to your local Vim setup to get syntax highlighting for `.jack` files. Example installation steps:

```console
# copy syntax file into your Vim syntax directory
mkdir -p ~/.vim/syntax
cp jack.vim ~/.vim/syntax/
# optionally add a filetype detection rule to ~/.vim/filetype.vim or your vimrc
# to associate .jack files with the "jack" filetype
```

Attribution: the `jack.vim` syntax file was authored by Zachary Stigall (zirrostig <at> lanfort.org), dated 10 Nov 2012.

## Testing and examples

- Example programs and helper scripts are provided in the `examples/` directory. Each component has a subdirectory under `examples/` (hack_assembler, jack_compiler, vm) containing a few sample input files you can use to try the toolchain.

Run a component on an example:

```console
cargo run -p jack_compiler -- --compile examples/jack_compiler/Square_Main.jack --output out/
cargo run -p vm -- --compile examples/vm/BasicTest.vm --output out.asm
cargo run -p hack_assembler -- --compile examples/hack_assembler/Add.asm --out-dir out/
```

### Running tests

All tests can be run with Cargo's test runner from the repository root:

```console
cargo test
```

To run tests for a specific package/module in the workspace use the -p flag:

```console
cargo test -p <package_name>
```

Examples:

```console
cargo test
cargo test -p jack_compiler
cargo test -p hack_assembler
cargo test -p vm
```

To run tests in release mode:

```console
cargo test --release
```

## Additional features (beyond standard jack-compiler requirements)

This implementation includes a number of useful extensions that make working with Jack significantly more pleasant. The following features are highlighted:

- Full type checker
  - A complete type checker is implemented. To preserve compatibility with existing Jack code the type system has been relaxed in certain cases; the following implicit type coercions are allowed but reported as warnings:
    - Primitive types may be implicitly converted to any other primitive type.
    - Primitive types may be implicitly converted to the complex array type (arrays have no additional type annotations).
    - The complex array type may be coerced to primitive and to complex types.
  - Strict rules: a primitive type can never be coerced to a complex type (e.g., `int` -/-> `Game`) and a complex type can never be coerced to a primitive type (e.g., `String` -/-> `char`).

- Character literals
  - Support for character literals, e.g. `let character = 'a'`.
  - Full escaping for character literals is implemented, including common escapes such as `'
'`, `'\\'`, `'\''`, and `'"'`.
  - Character escaping is also supported inside string literals (e.g., `let string = "\"Hello world\"`).

- Multi-class declarations
  - A single .jack file may contain an arbitrary number of class declarations; the file is processed sequentially and integrated correctly into the project.

- Extended 'this' syntax
  - Class-variable shadowing: class variables can be explicitly referenced using `this.<class_var_name>`. If a local variable with the same name exists in the current scope, an unqualified name references the local variable; only `this.<name>` unambiguously references the class variable. Example: `let x = ...` (local) vs. `let this.x = ...` (class variable).
  - Subroutine calls: calls like `this.<class_var_name>.<subroutine_name>()` are allowed and improve readability without changing semantics.

- Local scoping in subroutines
  - If and while statements define their own block scope. Scopes inherit declarations from outer scopes and any class variables the subroutine can access.
  - Variable declarations no longer need to appear only at the start of a subroutine; declarations may appear anywhere in the body.

- Enhanced error and warning reporting
  - Main class & main function check: verifies that a valid entry class and main function exist.
  - Control-flow analysis: return values are checked across all control-flow paths so that return types are enforced or warned about correctly.
  - Redundant operators: redundant unary operators (for example `+-3`, `----3`, `~~true`) are flagged as warnings but are treated so the compiler still produces expected behavior (for example `---3` ≠ `--3`); redundant operators are ignored as long as their presence does not affect the semantic result.
  - Syntax and semantic errors are reported with file, line and column locations and highlight the exact source range in the source file.
  - Color output: compiler output supports RGB, 8-color and no-color modes depending on the terminal's capabilities; the compiler detects supported escape codes for best presentation.

- Other ergonomic improvements
  - Improved diagnostics with precise hints and suggested fixes.
  - Support for multi-file/project workflows, incremental builds/caching and optional generation of source maps for debugging.

This list describes the implemented extensions; remove or adjust items if necessary to match the code.

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0). See the LICENSE file for full terms.

## Acknowledgements

- Jack language and the Nand2Tetris project by Noam Nisan and Shimon Schocken (https://www.nand2tetris.org) for the original Jack design and course materials.
