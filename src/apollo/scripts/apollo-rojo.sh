#!/bin/bash

# Apollo Rojo Integration Tool
# Wrapper script for compiling Rojo projects from Racket to Luau

# Get script directory (assuming this script is run from project root or installed)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Try to find the actual racket tool relative to this script OR in project root
APOLLO_ROJO_RKT="${SCRIPT_DIR}/../cmd/apollo-rojo/main.rkt"

if [ ! -f "$APOLLO_ROJO_RKT" ]; then
  # Fallback for running from project root if script is in scripts/
  APOLLO_ROJO_RKT="./cmd/apollo-rojo/main.rkt"
  if [ ! -f "$APOLLO_ROJO_RKT" ]; then
      echo "Error: Cannot find apollo-rojo Racket script (cmd/apollo-rojo/main.rkt)"
      exit 1
  fi
fi

# Check if racket is available
if ! command -v racket &>/dev/null; then
    echo "Error: Racket is not installed or not in PATH"
    echo "Please install Racket from https://racket-lang.org/"
    exit 1
fi

# Execute the Apollo Rojo tool, passing all arguments
echo "Executing: racket \"${APOLLO_ROJO_RKT}\" \"$@\""
racket "${APOLLO_ROJO_RKT}" "$@" 