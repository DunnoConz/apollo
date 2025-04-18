#!/bin/bash

# Determine OS-specific output name
case "$(uname -s)" in
    Linux*)     OUTPUT_NAME=apollo-linux;;
    Darwin*)    OUTPUT_NAME=apollo-macos;;
    CYGWIN*|MINGW*|MSYS*) OUTPUT_NAME=apollo-windows.exe;;
    *)          OUTPUT_NAME=apollo-unknown;;
esac

# Create output directory
mkdir -p out

# Build binary
echo "Building Apollo binary for $(uname -s)..."
raco exe --gui -o "out/${OUTPUT_NAME}" src/apollo/main.rkt

# Create distributable
echo "Creating distributable..."
raco distribute "out/dist-${OUTPUT_NAME}" "out/${OUTPUT_NAME}"

echo "Build complete! Binary available at out/dist-${OUTPUT_NAME}" 