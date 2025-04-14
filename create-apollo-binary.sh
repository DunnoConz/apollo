#!/bin/bash

# Apollo compiler installation and binary creation

echo "This script will create an Apollo compiler binary by:"
echo "1. Installing Apollo as a local Racket package"
echo "2. Creating a simple wrapper script that uses raco apollo"

# Step 1: Install the package
echo "Installing Apollo as a local package..."
raco pkg install --copy --auto

# Step 2: Create a wrapper script
echo "Creating apollo-bin wrapper script..."
echo "#!/bin/bash

# Apollo compiler wrapper
# Usage: ./apollo-bin input.rkt -o output.luau

# Run Apollo using raco
raco apollo \"\$@\"" > apollo-bin
chmod +x apollo-bin

echo "Done! You can now use ./apollo-bin to compile Racket files to Luau."
