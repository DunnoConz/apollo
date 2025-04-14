---
title: "Project Structure"
weight: 50
---

# Apollo Compiler Project Structure

## Overview

Apollo is a compiler that transforms Racket code into Luau (the scripting language used by Roblox). The project follows a modular architecture adhering to Racket's best practices for library organization.

## Directory Structure (Post-Reorganization)

```
.
├── cmd/                    # Command-line executables
│   ├── apollo/             # Main compiler CLI
│   │   └── main.rkt        # Entry point for `apollo` executable
│   └── apollo-rojo/        # Rojo integration CLI
│       └── main.rkt        # Entry point for `apollo-rojo` executable
├── dist/                   # Distribution output (generated)
├── docs-hugo/              # Hugo documentation site source
│   ├── content/
│   ├── layouts/
│   ├── static/
│   ├── themes/
│   └── hugo.toml
├── examples/
│   └── ...                 # Example Racket/Rojo projects
├── scripts/
│   ├── create-dist.sh    # Script to build distribution package
│   └── apollo-rojo.sh    # Wrapper script for the Rojo tool
├── src/
│   ├── apollo/             # Core library source (`apollo` package)
│   │   ├── compiler/       # Core compiler modules (parser, ir, codegen, types)
│   │   ├── rojo/           # Rojo integration logic
│   │   │   └── integration.rkt
│   │   ├── private/        # Private implementation details
│   │   │   └── utils.rkt
│   │   ├── main.rkt        # Main library module (re-exports public API)
│   │   └── info.rkt        # Collection info for `apollo` package
│   └── scribblings/        # Racket scribble documentation (for API)
│       └── apollo.scrbl
├── tests/
│   └── ...                 # Unit and integration tests
├── .gitignore
├── CONTRIBUTING.md
├── LICENSE
└── README.md               # Minimal README pointing to docs
```

## Module Descriptions

### Core Library (`src/apollo/`)

-   **`main.rkt`**: The main library entry point (for `require`ing `apollo`). Re-exports the public API from compiler and potentially Rojo modules.
-   **`compiler/`**: Contains the core stages of the compilation pipeline:
    -   `parser.rkt`: Racket code parsing (String -> Racket AST).
    -   `ir.rkt`: Intermediate Representation definition and transformation (Racket AST -> IR).
    -   `codegen.rkt`: Luau code generation (IR -> Luau AST -> Luau String).
    -   `types.rkt`: Core data structures (AST, IR nodes).
-   **`rojo/integration.rkt`**: Logic for handling Rojo project compilation (directory traversal, dependency management for Rojo).
-   **`private/utils.rkt`**: Internal utility functions not part of the public API.
-   **`info.rkt`**: Metadata for the Racket package system.

### Command Line Tools (`cmd/`)

-   **`cmd/apollo/main.rkt`**: Entry point for the main `apollo` executable. Handles CLI argument parsing (single file, `--rojo` flag, `--watch`), orchestrates compilation using library modules.
-   **`cmd/apollo-rojo/main.rkt`**: Entry point for the dedicated `apollo-rojo` executable. Handles CLI arguments specific to Rojo compilation (`-v`, `--watch`), uses the `apollo/rojo` library module.

### Scripts (`scripts/`)

-   **`create-dist.sh`**: Builds the executables (`apollo`, `apollo-rojo`) and packages them with necessary files (scripts, docs, examples) into the `dist/` directory.
-   **`apollo-rojo.sh`**: A simple wrapper to execute `racket cmd/apollo-rojo/main.rkt`, making it easier to run the Rojo tool directly from the source tree or when installed.

### Documentation (`docs-hugo/` and `src/scribblings/`)

-   **`docs-hugo/`**: Source files for the Hugo-based documentation website (guides, tutorials, conceptual overviews).
-   **`src/scribblings/apollo.scrbl`**: Source for the Racket Scribble-based API reference documentation, generated using `raco docs`.

## Compilation Pipeline

The Apollo compiler transforms Racket code into Luau through the following stages (primarily within `src/apollo/compiler/`):

1.  **Parsing**: Racket code is parsed into an AST (`parser.rkt`).
2.  **IR Generation**: The Racket AST is transformed into an intermediate representation (`ir.rkt`).
3.  **Luau AST Generation**: The IR is converted to a Luau AST (`codegen.rkt`).
4.  **Code Generation**: The Luau AST is transformed into Luau code (`codegen.rkt`).

When compiling Rojo projects, the `src/apollo/rojo/integration.rkt` module coordinates this pipeline for each Racket file within the project structure, handling dependencies and generating appropriate Luau `ModuleScript` wrappers. 