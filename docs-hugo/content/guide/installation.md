---
title: "Installation"
weight: 10
---

# Installation

## Prerequisites

- Racket 8.0 or higher (download from [racket-lang.org](https://racket-lang.org/))
- `raco`, the Racket command-line tool (comes with Racket)
- `git` (for installing from source)

## Installing from Source

This is the recommended method for developers or those wanting the latest version.

```bash
# 1. Clone the repository
git clone https://github.com/yourusername/apollo.git # Replace with actual URL

# 2. Navigate into the directory
cd apollo

# 3. Install the package locally
# This makes the `apollo` library available to Racket
# and builds necessary components.
raco pkg install
```

## Installing from Racket Package Catalog (Future)

Once published, you will be able to install directly using `raco`:

```bash
# This command is not yet active
raco pkg install apollo 
```

## Installing the Executable

After installing from source, you can create a distributable package containing the command-line executables (`apollo` and `apollo-rojo`).

1.  **Build the distribution package:**

    ```bash
    ./create-dist.sh
    ```

    This script performs the following steps:
    *   Builds the `apollo` executable.
    *   Builds the `apollo-rojo` executable.
    *   Copies the executables, wrapper scripts, runtime files (if any), documentation, and examples into the `./dist` directory.
    *   Creates an installation script `./dist/install.sh`.

2.  **Install the executables system-wide (optional):**

    ```bash
    cd dist
sudo ./install.sh 
    # You can optionally provide an installation prefix, e.g.:
    # sudo ./install.sh /opt/custom_location
    ```

    This copies the `apollo` and `apollo-rojo` executables (and the `apollo-rojo.sh` wrapper) to `/usr/local/bin` (or the specified prefix) and supporting files to `/usr/local/share/apollo`. You can then run `apollo` and `apollo-rojo` directly from your terminal.

## Verifying Installation

-   **Library:** Open a Racket REPL (`racket`) and run `(require apollo)`. If it succeeds without error, the library is installed.
-   **Executable (if installed):** Run `apollo --help` and `apollo-rojo --help` in your terminal. 