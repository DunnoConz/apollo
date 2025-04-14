#!/bin/bash

# Apollo compiler script
# Usage: ./apollo-compiler.sh input.rkt -o output.luau

# Run Apollo using the local source
racket src/main.rkt "$@"
