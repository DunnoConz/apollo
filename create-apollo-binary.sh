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
mkdir -p "$TEMP_DIR/apollo"

# Copy main package files
cp "$SCRIPT_DIR"/info.rkt "$TEMP_DIR/apollo/"
cp "$SCRIPT_DIR"/main.rkt "$TEMP_DIR/apollo/"
cp "$SCRIPT_DIR"/installer.rkt "$TEMP_DIR/apollo/"
cp "$SCRIPT_DIR"/setup.rkt "$TEMP_DIR/apollo/"

# Copy source directories with proper error handling and maintain directory structure
echo "Copying source files..."
if [ -d "$SCRIPT_DIR/src" ]; then
    cp -r "$SCRIPT_DIR/src"/* "$TEMP_DIR/"
else
    echo "Error: src directory not found"
    exit 1
fi

# Copy test files maintaining directory structure
echo "Copying test files..."
if [ -d "$SCRIPT_DIR/tests" ]; then
    cp -r "$SCRIPT_DIR/tests"/* "$TEMP_DIR/"
else
    echo "Warning: tests directory not found"
fi

# Fix DSL files
echo "Fixing DSL files..."
if [ -f "$TEMP_DIR/apollo/dsls/shout-dsl.rkt" ]; then
    cat > "$TEMP_DIR/apollo/dsls/shout-dsl.rkt" << 'EOF'
#lang racket

(require racket/syntax
         syntax/parse
         "../compiler/ir.rkt"
         "../compiler/ir-types.rkt")

(provide shout)

(define-syntax-parser shout
  [(_ msg:expr)
   #'(display (string-append (string-upcase (format "~a" msg)) "\n"))])
EOF
fi

if [ -f "$TEMP_DIR/apollo/dsls/test_dsl.rkt" ]; then
    cat > "$TEMP_DIR/apollo/dsls/test_dsl.rkt" << 'EOF'
#lang racket

(require racket/syntax
         syntax/parse
         "../compiler/ir.rkt"
         "../compiler/ir-types.rkt")

(provide test-dsl)

(define-syntax-parser test-dsl
  [(_ expr:expr ...)
   #'(begin
       (display "Running tests...\n")
       expr ...)])
EOF
fi

# Debug: Print package directory structure
echo "Package directory structure:"
ls -la "$TEMP_DIR"
ls -la "$TEMP_DIR/apollo"

# Install base dependencies first
echo "Installing base dependencies..."
"$RACO_CMD" pkg install --batch --auto racket-doc scribble-lib syntax-color-lib parser-tools-lib || true

# Get version from info.rkt (do this early as we'll use it multiple times)
VERSION=$(grep -o '"[0-9]\+\.[0-9]\+\.[0-9]\+"' "$SCRIPT_DIR/info.rkt" | tr -d '"')
if [ -z "$VERSION" ]; then
    echo "Error: Could not extract version from info.rkt"
    echo "Expected format: (define version \"X.Y.Z\") where X, Y, Z are numbers"
    exit 1
fi

echo "Building Apollo version $VERSION..."

# Create a temporary info.rkt with required dependencies
cat > "$TEMP_DIR/apollo/info.rkt" << EOF
#lang info
(define collection "apollo")
(define deps '("base"
              "syntax-color-lib"
              "parser-tools-lib"))
(define build-deps '())
(define version "$VERSION")
EOF

# Install the package directly with linking
echo "Installing package..."
cd "$TEMP_DIR"
"$RACO_CMD" pkg install --link --batch --deps search-auto apollo || {
    echo "Error: Failed to install Apollo package"
    exit 1
}

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
