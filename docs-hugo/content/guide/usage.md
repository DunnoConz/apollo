---
title: "Usage"
weight: 20
---

# Usage

Apollo can be used both as a command-line tool and as a Racket library.

## Command-Line Usage (Executable)

If you have built or installed the executables (see [[Installation]]({{< relref "installation.md" >}})), you can use `apollo` for single files/directories and `apollo-rojo` for dedicated Rojo project workflows.

### Compiling Single Files

```bash
# Compile a single file (output defaults to input.luau)
apollo input.rkt

# Specify output file
apollo input.rkt -o output.luau

# Watch for changes
apollo input.rkt -o output.luau --watch
```

### Compiling Rojo Projects

**Using the main `apollo` tool:**

```bash
# Compile an entire Rojo project
apollo --rojo source_project -o output_project

# Watch a Rojo project for changes
apollo --rojo source_project -o output_project --watch
```

**Using the dedicated `apollo-rojo` tool:**

```bash
# Compile an entire Rojo project
apollo-rojo source_project -o output_project

# Watch a Rojo project for changes
apollo-rojo source_project -o output_project --watch

# Verbose mode
apollo-rojo source_project -o output_project -v --watch
```

### Command-Line Options (`apollo`)

-   `<input-path>`: The source Racket file or Rojo project directory.
-   `-o, --output <path>`: Output file or directory path.
-   `--rojo`: Compile as a Rojo project. Input must be a directory, and `-o` must specify an output directory.
-   `--watch`: Watch the input path for changes and recompile automatically.
-   `-h, --help`: Show help message.

### Command-Line Options (`apollo-rojo`)

-   `<source-directory>`: The source Rojo project directory.
-   `-o, --output <dir>`: Output directory path (required).
-   `--watch`: Watch the source directory for changes and recompile automatically.
-   `-v, --verbose`: Enable verbose output.
-   `-h, --help`: Show help message.

## Library Usage (in Racket)

Require the `apollo` library in your Racket code.

```racket
#lang racket

(require apollo)

;; -- Single String Compilation --

;; Compile a Racket code string to a Luau code string
(define luau-code-string
  (compile-racket-string-to-luau 
   "(define (add x y) (+ x y)) (add 1 2)"
   ;; Optional: Provide source file name for better error reporting
   #:source-file "my-snippet.rkt"))

(displayln luau-code-string)

;; -- File Compilation --

;; Compile a single Racket file to a Luau file
;; (Note: This function isn't explicitly provided yet, 
;;  but you can build it using the pipeline steps)
(define (compile-rkt-file->luau-file input.rkt output.luau)
  (with-handlers ([exn:fail? (lambda (e) (eprintf "Error: ~a\n" (exn-message e)))])
    (define code (file->string input.rkt))
    (define luau (compile-racket-string-to-luau code #:source-file input.rkt))
    (call-with-output-file output.luau
      (lambda (port) (display luau port))
      #:exists 'replace)))

; (compile-rkt-file->luau-file "path/to/input.rkt" "path/to/output.luau")

;; -- Rojo Project Compilation --

;; Require the specific Rojo integration module
(require apollo/rojo)

;; Compile an entire project
; (compile-rojo-project "path/to/source_project" "path/to/output_project")

;; -- Accessing Compiler Stages --

;; You can also access individual stages of the compiler pipeline
(define code-string "(define (sq x) (* x x))")

; (define ast (parse-racket-string code-string))
; (define ir (racket-to-ir ast))
; (define luau-ast (ir->luau ir))
; (define luau-string (luau-ast->string luau-ast))

; (displayln luau-string)
```

See the [API Reference](/api/) for details on available library functions. 