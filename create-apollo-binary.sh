#!/bin/bash

# Apollo compiler installation and binary creation
set -e  # Exit on any error

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

# Remove any existing apollo package first
"$RACO_CMD" pkg remove --force apollo || true

# Create collection links
"$RACO_CMD" link apollo "$SCRIPT_DIR"
"$RACO_CMD" link apollo/compiler "$SCRIPT_DIR/src/apollo/compiler"
"$RACO_CMD" link apollo/rojo "$SCRIPT_DIR/src/apollo/rojo"
"$RACO_CMD" link apollo/std "$SCRIPT_DIR/src/apollo/std"
"$RACO_CMD" link apollo/dsls "$SCRIPT_DIR/src/apollo/dsls"
"$RACO_CMD" link apollo/ecs "$SCRIPT_DIR/src/apollo/ecs"
"$RACO_CMD" link apollo/scribblings "$SCRIPT_DIR/src/apollo/scribblings"

# Step 2: Install the package
echo "Installing Apollo as a local package..."
"$RACO_CMD" pkg install --copy --auto || {
    echo "Error: Failed to install Apollo package"
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
    exit 1
}

echo "Done! Apollo v$VERSION binary wrapper created."
echo "You can now use ./apollo-bin to compile Racket files to Luau."
