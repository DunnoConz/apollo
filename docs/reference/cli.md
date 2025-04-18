# CLI Reference

This document provides a complete reference for the Apollo command-line interface.

## Command Structure

```bash
apollo [command] [options] [arguments]
```

## Commands

### `compile`

Compiles a Racket file to Luau.

```bash
apollo compile <input.rkt> [options]
```

#### Options

*   `-o, --output <file>`: Specify output file (default: input name with .luau extension)
*   `-v, --verbose`: Enable verbose output
*   `--no-optimize`: Disable optimizations
*   `--target <version>`: Specify Luau target version (default: latest)

### `check`

Type checks a Racket file without generating output.

```bash
apollo check <input.rkt> [options]
```

#### Options

*   `-v, --verbose`: Enable verbose output
*   `--strict`: Enable strict type checking

## Global Options

*   `-h, --help`: Show help
*   `--version`: Show version information
*   `--config <file>`: Specify config file

## Examples

```bash
# Basic compilation
apollo compile program.rkt

# Compile with specific output
apollo compile program.rkt -o out.luau

# Type check only
apollo check program.rkt

# Verbose compilation
apollo compile program.rkt -v
```

## Configuration File

Apollo can read options from a configuration file (default: `apollo.config.json`):

```json
{
  "target": "latest",
  "optimize": true,
  "strict": false
}
``` 