#!/bin/bash
# Comprehensive test script for Racket-to-Luau compiler examples

# Text formatting
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=======================================${NC}"
echo -e "${BLUE}Testing Apollo Racket-to-Luau Examples${NC}"
echo -e "${BLUE}=======================================${NC}"

# Check if Lune is installed
if ! command -v lune &> /dev/null; then
    echo -e "${RED}Error: Lune is not installed or not in PATH${NC}"
    echo "Please install Lune to run Luau files: https://github.com/lune-org/lune"
    exit 1
fi

# Function to test a Luau file
test_luau_file() {
    local file=$1
    local expected_file="${file}.expected"
    local actual_output_file=$(mktemp)
    local diff_output
    local exit_code

    echo -e "\n${YELLOW}Testing: ${file}${NC}"

    # Run the luau file and capture output and exit code
    lune run "$file" > "$actual_output_file" 2>&1
    exit_code=$?

    if [ $exit_code -ne 0 ]; then
        echo -e "${RED}✗ Execution failed (Exit Code: $exit_code)${NC}"
        echo "--- Output --- "
        cat "$actual_output_file"
        echo "--------------"
        rm "$actual_output_file"
        return 1
    fi

    if [ ! -f "$expected_file" ]; then
        echo -e "${YELLOW}✓ Execution passed (Exit Code: 0)${NC}"
        echo -e "${YELLOW}  WARN: No expected output file found at ${expected_file}. Skipping output comparison.${NC}"
        rm "$actual_output_file"
        return 0 # Consider this success for now, but needs attention
    fi

    # Compare actual output with expected output
    diff_output=$(diff -u --strip-trailing-cr "$expected_file" "$actual_output_file")
    local diff_exit_code=$?

    if [ $diff_exit_code -eq 0 ]; then
        echo -e "${GREEN}✓ Test passed (Output matches expected)${NC}"
        rm "$actual_output_file"
        return 0
    else
        echo -e "${RED}✗ Test failed (Output mismatch)${NC}"
        echo "--- Diff --- (Expected vs Actual)"
        echo "$diff_output"
        echo "------------"
        rm "$actual_output_file"
        return 1
    fi
}

# Function to check if a Racket file has a corresponding Luau file
check_compiled_file() {
    local rkt_file=$1
    local lua_file="${rkt_file%.rkt}.luau"
    
    if [ -f "$lua_file" ]; then
        echo -e "${GREEN}✓ Found corresponding Luau file: ${lua_file}${NC}"
        return 0
    else
        echo -e "${RED}✗ Missing Luau file for: ${rkt_file}${NC}"
        return 1
    fi
}

# 1. Test Basic Examples
echo -e "\n${BLUE}=== Testing Basic Examples ===${NC}"
for file in examples/01-basics/*.luau; do
    if [ -f "$file" ]; then
        test_luau_file "$file"
    fi
done

# 2. Test Standalone Examples
echo -e "\n${BLUE}=== Testing Standalone Examples ===${NC}"
for file in examples/*.luau; do
    if [ -f "$file" ]; then
        test_luau_file "$file"
    fi
done

# 3. Test Advanced Examples (all numbered directories)
echo -e "\n${BLUE}=== Testing Advanced Examples ===${NC}"
for dir in examples/0*; do
    if [ -d "$dir" ] && [ "$dir" != "examples/01-basics" ]; then
        echo -e "\n${YELLOW}Directory: ${dir}${NC}"
        for file in "$dir"/*.luau; do
            if [ -f "$file" ]; then
                test_luau_file "$file"
            fi
        done
    fi
done

# 4. Test Special Category Examples
echo -e "\n${BLUE}=== Testing Special Category Examples ===${NC}"
for dir in examples/basic examples/closures examples/factorial examples/list-operations; do
    if [ -d "$dir" ]; then
        echo -e "\n${YELLOW}Directory: ${dir}${NC}"
        for file in "$dir"/*.luau; do
            if [ -f "$file" ]; then
                test_luau_file "$file"
            fi
        done
    fi
done

# 5. Check for Missing Luau Files
echo -e "\n${BLUE}=== Checking for Missing Luau Files ===${NC}"
missing_count=0
total_rkt=0

find_rkt_files() {
    local dir=$1
    if [ -d "$dir" ]; then
        for rkt_file in "$dir"/*.rkt; do
            if [ -f "$rkt_file" ]; then
                ((total_rkt++))
                check_compiled_file "$rkt_file"
                if [ $? -ne 0 ]; then
                    ((missing_count++))
                fi
            fi
        done
    fi
}

# Check all directories
find_rkt_files "examples"
find_rkt_files "examples/01-basics"
for dir in examples/0*; do
    if [ -d "$dir" ] && [ "$dir" != "examples/01-basics" ]; then
        find_rkt_files "$dir"
    fi
done
for dir in examples/basic examples/closures examples/factorial examples/list-operations; do
    find_rkt_files "$dir"
done

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
if [ $missing_count -eq 0 ]; then
    echo -e "${GREEN}All $total_rkt Racket files have corresponding Luau files.${NC}"
else
    echo -e "${YELLOW}Found $missing_count missing Luau files out of $total_rkt Racket files.${NC}"
fi

echo -e "\n${BLUE}Test script complete!${NC}" 