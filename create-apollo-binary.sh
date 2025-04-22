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

# Remove any existing apollo package first
"$RACO_CMD" pkg remove --force apollo || true

# Create a local collections directory
COLLECTIONS_DIR="$SCRIPT_DIR/collections"
rm -rf "$COLLECTIONS_DIR"
mkdir -p "$COLLECTIONS_DIR"

# Create all the necessary directories
mkdir -p "$COLLECTIONS_DIR/apollo"
mkdir -p "$COLLECTIONS_DIR/apollo/compiler"
mkdir -p "$COLLECTIONS_DIR/apollo/rojo"
mkdir -p "$COLLECTIONS_DIR/apollo/std"
mkdir -p "$COLLECTIONS_DIR/apollo/dsls"
mkdir -p "$COLLECTIONS_DIR/apollo/ecs"
mkdir -p "$COLLECTIONS_DIR/apollo/scribblings"

# Copy main package files
cp "$SCRIPT_DIR"/info.rkt "$COLLECTIONS_DIR/apollo/"
cp "$SCRIPT_DIR"/main.rkt "$COLLECTIONS_DIR/apollo/"
cp "$SCRIPT_DIR"/installer.rkt "$COLLECTIONS_DIR/apollo/"
cp "$SCRIPT_DIR"/setup.rkt "$COLLECTIONS_DIR/apollo/"

# Copy source directories with proper error handling
echo "Copying compiler files..."
cp -r "$SCRIPT_DIR/src/apollo/compiler"/* "$COLLECTIONS_DIR/apollo/compiler/" || echo "No compiler files to copy"

echo "Copying rojo files..."
cp -r "$SCRIPT_DIR/src/apollo/rojo"/* "$COLLECTIONS_DIR/apollo/rojo/" || echo "No rojo files to copy"

echo "Copying std files..."
cp -r "$SCRIPT_DIR/src/apollo/std"/* "$COLLECTIONS_DIR/apollo/std/" || echo "No std files to copy"

echo "Copying dsls files..."
cp -r "$SCRIPT_DIR/src/apollo/dsls"/* "$COLLECTIONS_DIR/apollo/dsls/" || echo "No dsls files to copy"

echo "Copying ecs files..."
if [ -d "$SCRIPT_DIR/src/apollo/ecs" ] && [ -n "$(ls -A "$SCRIPT_DIR/src/apollo/ecs")" ]; then
    cp -r "$SCRIPT_DIR/src/apollo/ecs"/* "$COLLECTIONS_DIR/apollo/ecs/"
else
    echo "No ecs files to copy"
fi

echo "Copying scribblings files..."
if [ -d "$SCRIPT_DIR/src/apollo/scribblings" ] && [ -n "$(ls -A "$SCRIPT_DIR/src/apollo/scribblings")" ]; then
    cp -r "$SCRIPT_DIR/src/apollo/scribblings"/* "$COLLECTIONS_DIR/apollo/scribblings/"
else
    echo "No scribblings files to copy"
fi

# Debug: Print collections directory structure
echo "Collections directory structure:"
ls -la "$COLLECTIONS_DIR"
ls -la "$COLLECTIONS_DIR/apollo"

# Create collection links
"$RACO_CMD" link apollo "$COLLECTIONS_DIR/apollo"
"$RACO_CMD" link apollo/compiler "$COLLECTIONS_DIR/apollo/compiler"
"$RACO_CMD" link apollo/rojo "$COLLECTIONS_DIR/apollo/rojo"
"$RACO_CMD" link apollo/std "$COLLECTIONS_DIR/apollo/std"
"$RACO_CMD" link apollo/dsls "$COLLECTIONS_DIR/apollo/dsls"
"$RACO_CMD" link apollo/ecs "$COLLECTIONS_DIR/apollo/ecs"
"$RACO_CMD" link apollo/scribblings "$COLLECTIONS_DIR/apollo/scribblings"

# Step 2: Install the package
echo "Installing Apollo as a local package..."
cd "$SCRIPT_DIR"  # Make sure we're in the right directory
"$RACO_CMD" pkg install --copy --auto || {
    echo "Error: Failed to install Apollo package"
    rm -rf "$COLLECTIONS_DIR"
    exit 1
}

# Get version from info.rkt
VERSION=$("$RACO_CMD" eval -e "(require setup/getinfo) (define info (get-info/full \".\" #:namespace '(version))) (display (info 'version))")

# Step 3: Create a wrapper script
echo "Creating apollo-bin wrapper script..."
cat > apollo-bin << EOF
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

chmod +x apollo-bin || {
    echo "Error: Failed to make apollo-bin executable"
    rm -rf "$COLLECTIONS_DIR"
    exit 1
}

# Clean up
rm -rf "$COLLECTIONS_DIR"

echo "Done! Apollo v$VERSION binary wrapper created."
echo "You can now use ./apollo-bin to compile Racket files to Luau."
