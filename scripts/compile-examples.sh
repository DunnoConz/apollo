#!/bin/bash
# Script to compile all Racket examples to Luau

# Text formatting
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=========================================${NC}"
echo -e "${BLUE}Compiling Racket Examples to Luau${NC}"
echo -e "${BLUE}=========================================${NC}"

# Check if racket is installed
if ! command -v racket &> /dev/null; then
    echo -e "${RED}Error: Racket is not installed or not in PATH${NC}"
    echo "Please install Racket to compile the examples"
    exit 1
fi

# Function to compile a Racket file to Luau
compile_rkt_to_lua() {
    local rkt_file=$1
    local lua_file="${rkt_file%.rkt}.luau"
    
    echo -e "\n${YELLOW}Compiling: ${rkt_file} -> ${lua_file}${NC}"
    
    # Compile the file using the Apollo compiler
    # Note: Replace this command with the actual command to compile Racket to Luau
    racket -l apollo -- "$rkt_file" -o "$lua_file"
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Compilation successful${NC}"
        return 0
    else
        echo -e "${RED}✗ Compilation failed${NC}"
        return 1
    fi
}

# Function to find and compile all Racket files in a directory
compile_directory() {
    local dir=$1
    local success_count=0
    local fail_count=0
    
    if [ -d "$dir" ]; then
        echo -e "\n${BLUE}=== Compiling in directory: ${dir} ===${NC}"
        
        for rkt_file in "$dir"/*.rkt; do
            if [ -f "$rkt_file" ]; then
                compile_rkt_to_lua "$rkt_file"
                if [ $? -eq 0 ]; then
                    ((success_count++))
                else
                    ((fail_count++))
                fi
            fi
        done
        
        echo -e "${BLUE}Directory summary: ${GREEN}${success_count} succeeded${NC}, ${RED}${fail_count} failed${NC}"
    else
        echo -e "${YELLOW}Directory not found: ${dir}${NC}"
    fi
}

# Compile examples in main directories
compile_directory "examples"
compile_directory "examples/01-basics"

# Compile examples in all other numbered directories
for dir in examples/0*; do
    if [ -d "$dir" ] && [ "$dir" != "examples/01-basics" ]; then
        compile_directory "$dir"
    fi
done

# Compile examples in special category directories
for dir in examples/basic examples/closures examples/factorial examples/list-operations; do
    compile_directory "$dir"
done

echo -e "\n${BLUE}Compilation complete!${NC}"
echo -e "${YELLOW}Note: Don't forget to run ./test-examples.sh to verify the compiled files.${NC}" 