#!/bin/bash

# Create a distribution package for the Apollo compiler

# Ensure we're in the project root
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"

# Ensure the main build-exe is up to date
echo "Building main executable (apollo)..."
raco exe cmd/apollo/main.rkt -o build-apollo-exe # Build with specific name

# Build the Rojo integration CLI tool
echo "Building Rojo integration tool (apollo-rojo)..."
raco exe cmd/apollo-rojo/main.rkt -o build-apollo-rojo-exe # Build with specific name

# Create dist directory if it doesn't exist
mkdir -p dist/bin

# Remove existing binaries if they exist
rm -f dist/bin/apollo
rm -f dist/bin/apollo-rojo

# Copy the binaries to the dist directory
echo "Copying binaries to dist/bin..."
cp build-apollo-exe dist/bin/apollo # Copy from build output
cp build-apollo-rojo-exe dist/bin/apollo-rojo 2>/dev/null || echo "Rojo integration binary build failed or not available"

# Clean up temporary build files
rm -f build-apollo-exe build-apollo-rojo-exe

# Make sure the binaries are executable
chmod +x dist/bin/apollo
chmod +x dist/bin/apollo-rojo 2>/dev/null || true

# Copy wrapper scripts (assuming they are in scripts/)
echo "Copying wrapper scripts..."
cp scripts/apollo-rojo.sh dist/bin/ 2>/dev/null || echo "Rojo integration script not available"
chmod +x dist/bin/apollo-rojo.sh 2>/dev/null || true

# Copy required runtime files (if any)
echo "Copying runtime files..."
mkdir -p dist/runtime
cp -r src/apollo/runtime/* dist/runtime/ 2>/dev/null || echo "No runtime files found"

# Copy documentation (Hugo source and main README/LICENSE)
echo "Copying documentation..."
mkdir -p dist/docs/hugo-source
cp -r docs-hugo/* dist/docs/hugo-source/ 2>/dev/null || echo "No Hugo documentation source found"
cp README.md dist/docs/
cp LICENSE dist/docs/
cp CONTRIBUTING.md dist/docs/

# Create examples directory
echo "Copying examples..."
mkdir -p dist/examples
cp -r examples/* dist/examples/ 2>/dev/null || echo "No example files found"

# Create installation script
cat > dist/install.sh << 'EOF'
#!/bin/bash
# Apollo compiler installation script

# Determine installation directory (default to /usr/local)
INSTALL_DIR=${1:-/usr/local}
SHARE_DIR="$INSTALL_DIR/share/apollo"

# Create directories
echo "Creating directories in $INSTALL_DIR..."
mkdir -p "$INSTALL_DIR/bin"
mkdir -p "$SHARE_DIR"

# Copy files
echo "Copying files..."
cp bin/apollo "$INSTALL_DIR/bin/"
cp bin/apollo-rojo "$INSTALL_DIR/bin/" 2>/dev/null || true
cp bin/apollo-rojo.sh "$INSTALL_DIR/bin/" 2>/dev/null || true
cp -r runtime "$SHARE_DIR/" 2>/dev/null || true
cp -r docs "$SHARE_DIR/"
cp -r examples "$SHARE_DIR/"

# Make executables
echo "Setting permissions..."
chmod +x "$INSTALL_DIR/bin/apollo"
chmod +x "$INSTALL_DIR/bin/apollo-rojo" 2>/dev/null || true
chmod +x "$INSTALL_DIR/bin/apollo-rojo.sh" 2>/dev/null || true

# Create symbolic link for apollo-rojo script
if [ -f "$INSTALL_DIR/bin/apollo-rojo.sh" ]; then
  ln -sf "$INSTALL_DIR/bin/apollo-rojo.sh" "$INSTALL_DIR/bin/apollo-rojo"
  echo "Rojo integration helper installed at $INSTALL_DIR/bin/apollo-rojo"
fi

echo ""
echo "Apollo compiler installed to $INSTALL_DIR"
echo "Executables: $INSTALL_DIR/bin/apollo $INSTALL_DIR/bin/apollo-rojo"
echo "Shared files (docs, examples): $SHARE_DIR"
echo "You can now run 'apollo' from the command line (and 'apollo-rojo' if applicable)"
EOF

chmod +x dist/install.sh

echo ""
echo "Distribution package created in './dist'"
echo "To install, run: cd dist && sudo ./install.sh" 