# Standard Library Reference

This document provides a complete reference for Apollo's standard library functions and their Luau equivalents.

## Core Functions

### Math Operations

| Racket | Luau | Description |
|--------|------|-------------|
| `+` | `+` | Addition |
| `-` | `-` | Subtraction |
| `*` | `*` | Multiplication |
| `/` | `/` | Division |
| `modulo` | `%` | Modulo |
| `expt` | `^` | Exponentiation |

### String Operations

| Racket | Luau | Description |
|--------|------|-------------|
| `string-append` | `..` | String concatenation |
| `string-length` | `#` | String length |
| `substring` | `string.sub` | Substring extraction |
| `string-upcase` | `string.upper` | Convert to uppercase |
| `string-downcase` | `string.lower` | Convert to lowercase |

### List Operations

| Racket | Luau | Description |
|--------|------|-------------|
| `cons` | `table.insert` | Add to front |
| `append` | `table.insert` | Add to end |
| `car` | `[1]` | First element |
| `cdr` | `{select(2, ...)}` | Rest of list |
| `length` | `#` | List length |

## Type Predicates

| Racket | Luau | Description |
|--------|------|-------------|
| `number?` | `type(x) == "number"` | Check for number |
| `string?` | `type(x) == "string"` | Check for string |
| `boolean?` | `type(x) == "boolean"` | Check for boolean |
| `list?` | `type(x) == "table"` | Check for list |
| `null?` | `x == nil` | Check for null |

## I/O Operations

| Racket | Luau | Description |
|--------|------|-------------|
| `display` | `print` | Print to console |
| `printf` | `string.format` | Formatted printing |
| `read` | `io.read` | Read input |

## Examples

```racket
; Racket
(define (sum-list lst)
  (foldl + 0 lst))

(define (greet name)
  (string-append "Hello, " name "!"))
```

```lua
-- Generated Luau
local function sumList(lst: {number}): number
    return table.fold(lst, function(acc, x) return acc + x end, 0)
end

local function greet(name: string): string
    return "Hello, " .. name .. "!"
end
```

## Notes

*   Some Racket functions may have slightly different behavior in Luau
*   Not all Racket standard library functions are supported
*   Custom implementations may be needed for some operations

## See Also

*   [Type System](../explanation/type-system.md)
*   [Error Messages](../reference/errors.md) 