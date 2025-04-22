#!/bin/bash

# Apollo compiler installation and binary creation
set -e  # Exit on any error

# Get the raco path (same as in installer.rkt)
RACO_PATH=${RACO_PATH:-"/opt/homebrew/bin/raco"}

# Verify raco exists
if ! command -v "$RACO_PATH" &> /dev/null; then
    echo "Error: raco not found at $RACO_PATH"
    echo "Please install Racket or set RACO_PATH environment variable"
    exit 1
fi

echo "This script will create an Apollo compiler binary by:"
echo "1. Installing Apollo as a local Racket package"
echo "2. Creating a simple wrapper script that uses raco apollo"

# Step 1: Install the package
echo "Installing Apollo as a local package..."
"$RACO_PATH" pkg install --copy --auto || {
    echo "Error: Failed to install Apollo package"
    exit 1
}

# Get version from info.rkt
VERSION=$("$RACO_PATH" eval -e "(require setup/getinfo) (define info (get-info/full \".\" #:namespace '(version))) (display (info 'version))")

# Step 2: Create a wrapper script
echo "Creating apollo-bin wrapper script..."
cat > apollo-bin << EOF
#!/bin/bash

# Apollo compiler wrapper v$VERSION
# Usage: ./apollo-bin input.rkt -o output.luau

# Get the directory where the script is located
SCRIPT_DIR="\$(cd "\$(dirname "\${BASH_SOURCE[0]}")" &> /dev/null && pwd)"

# Run Apollo using raco
"$RACO_PATH" apollo "\$@"
EOF

chmod +x apollo-bin || {
    echo "Error: Failed to make apollo-bin executable"
    exit 1
}

echo "Done! Apollo v$VERSION binary wrapper created."
echo "You can now use ./apollo-bin to compile Racket files to Luau."
