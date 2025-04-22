#!/bin/bash

# Apollo compiler installation and binary creation
set -e  # Exit on any error
set -x  # Print each command for debugging

# Get the raco path (first check RACO_PATH, then look in PATH)
if [ -n "$RACO_PATH" ]; then
    RACO_CMD="$RACO_PATH"
elif command -v raco &> /dev/null; then
    RACO_CMD="raco"
else
    echo "Error: raco not found in PATH and RACO_PATH not set"
    echo "Please install Racket or set RACO_PATH environment variable"
    exit 1
fi

echo "Using raco from: $(which $RACO_CMD)"

echo "This script will create an Apollo compiler binary by:"
echo "1. Setting up package collection links"
echo "2. Installing Apollo as a local Racket package"
echo "3. Creating a simple wrapper script that uses raco apollo"

# Step 1: Set up collection links
echo "Setting up collection links..."
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"

# Debug: Print current directory structure
echo "Current directory structure:"
ls -la "$SCRIPT_DIR"
ls -la "$SCRIPT_DIR/src/apollo" || true

# Remove any existing apollo package and links
"$RACO_CMD" pkg remove --force apollo || true
"$RACO_CMD" link -r apollo || true

# Create a temporary directory for package installation
TEMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TEMP_DIR"' EXIT

# Create the package structure in the temporary directory
mkdir -p "$TEMP_DIR/apollo/compiler"
mkdir -p "$TEMP_DIR/apollo/rojo"
mkdir -p "$TEMP_DIR/apollo/std"
mkdir -p "$TEMP_DIR/apollo/dsls"
mkdir -p "$TEMP_DIR/apollo/ecs"
mkdir -p "$TEMP_DIR/apollo/scribblings"

# Copy main package files
cp "$SCRIPT_DIR"/info.rkt "$TEMP_DIR/apollo/"
cp "$SCRIPT_DIR"/main.rkt "$TEMP_DIR/apollo/"
cp "$SCRIPT_DIR"/installer.rkt "$TEMP_DIR/apollo/"
cp "$SCRIPT_DIR"/setup.rkt "$TEMP_DIR/apollo/"

# Copy source directories with proper error handling and maintain directory structure
echo "Copying compiler files..."
cp -r "$SCRIPT_DIR/src/apollo/compiler"/* "$TEMP_DIR/apollo/compiler/" || echo "No compiler files to copy"

echo "Copying rojo files..."
cp -r "$SCRIPT_DIR/src/apollo/rojo"/* "$TEMP_DIR/apollo/rojo/" || echo "No rojo files to copy"

echo "Copying std files..."
cp -r "$SCRIPT_DIR/src/apollo/std"/* "$TEMP_DIR/apollo/std/" || echo "No std files to copy"

echo "Copying dsls files..."
cp -r "$SCRIPT_DIR/src/apollo/dsls"/* "$TEMP_DIR/apollo/dsls/" || echo "No dsls files to copy"

echo "Copying ecs files..."
if [ -d "$SCRIPT_DIR/src/apollo/ecs" ] && [ -n "$(ls -A "$SCRIPT_DIR/src/apollo/ecs")" ]; then
    cp -r "$SCRIPT_DIR/src/apollo/ecs"/* "$TEMP_DIR/apollo/ecs/"
else
    echo "No ecs files to copy"
fi

echo "Copying scribblings files..."
if [ -d "$SCRIPT_DIR/src/apollo/scribblings" ]; then
    cp -r "$SCRIPT_DIR/src/apollo/scribblings"/* "$TEMP_DIR/apollo/scribblings/" || echo "No scribblings files to copy"
    # Comment out scribblings in info.rkt to avoid documentation build issues
    sed -i.bak 's|(define scribblings.*)|#;(define scribblings '\''(("scribblings/apollo.scrbl" ())))|' "$TEMP_DIR/apollo/info.rkt"
    rm -f "$TEMP_DIR/apollo/info.rkt.bak"
else
    echo "No scribblings directory found"
fi

# Fix paths in main.rkt
echo "Fixing paths in main.rkt..."
sed -i.bak 's|"src/apollo/compiler/main"|"compiler/main"|g' "$TEMP_DIR/apollo/main.rkt"
rm -f "$TEMP_DIR/apollo/main.rkt.bak"

# Fix DSL files
echo "Fixing DSL files..."
if [ -f "$TEMP_DIR/apollo/dsls/shout-dsl.rkt" ]; then
    sed -i.bak 's|(message|#;(message|g' "$TEMP_DIR/apollo/dsls/shout-dsl.rkt"
    rm -f "$TEMP_DIR/apollo/dsls/shout-dsl.rkt.bak"
fi

if [ -f "$TEMP_DIR/apollo/dsls/test_dsl.rkt" ]; then
    sed -i.bak 's|(in-syntax|#;(in-syntax|g' "$TEMP_DIR/apollo/dsls/test_dsl.rkt"
    rm -f "$TEMP_DIR/apollo/dsls/test_dsl.rkt.bak"
fi

# Debug: Print package directory structure
echo "Package directory structure:"
ls -la "$TEMP_DIR"
ls -la "$TEMP_DIR/apollo"

# Install base dependencies first
echo "Installing base dependencies..."
"$RACO_CMD" pkg install --batch --auto racket-doc scribble-lib syntax-color-lib || true

# Create a temporary info.rkt with required dependencies
cat > "$TEMP_DIR/apollo/info.rkt" << EOF
#lang info
(define collection "apollo")
(define deps '("base"
              "syntax-color-lib"
              "parser-tools-lib"))
(define build-deps '())
(define version "0.1.13")
EOF

# Install the package directly with linking
echo "Installing package..."
cd "$TEMP_DIR"
"$RACO_CMD" pkg install --link --batch --deps search-auto apollo || {
    echo "Error: Failed to install Apollo package"
    exit 1
}

# Get version from info.rkt
VERSION=$("$RACO_CMD" eval -e "(require setup/getinfo) (define info (get-info/full \"$SCRIPT_DIR\" #:namespace '(version))) (display (info 'version))")

# Step 3: Create a wrapper script
echo "Creating apollo-bin wrapper script..."
cat > "$SCRIPT_DIR/apollo-bin" << EOF
#!/bin/bash

# Apollo compiler wrapper v$VERSION
# Usage: ./apollo-bin input.rkt -o output.luau

# Get the directory where the script is located
SCRIPT_DIR="\$(cd "\$(dirname "\${BASH_SOURCE[0]}")" &> /dev/null && pwd)"

# Run Apollo using raco
if [ -n "\$RACO_PATH" ]; then
    "\$RACO_PATH" apollo "\$@"
elif command -v raco &> /dev/null; then
    raco apollo "\$@"
else
    echo "Error: raco not found in PATH and RACO_PATH not set"
    exit 1
fi
EOF

chmod +x "$SCRIPT_DIR/apollo-bin" || {
    echo "Error: Failed to make apollo-bin executable"
    exit 1
}

echo "Done! Apollo v$VERSION binary wrapper created."
echo "You can now use ./apollo-bin to compile Racket files to Luau."
