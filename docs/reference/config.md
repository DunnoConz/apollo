# Configuration Reference

This document describes all configuration options for Apollo.

## Configuration File

Apollo can be configured using a JSON file (default: `apollo.config.json`):

```json
{
  "compiler": {
    "target": "roblox",
    "optimize": true,
    "strict": false,
    "sourceMaps": true
  },
  "typeChecking": {
    "level": "strict",
    "warnings": true
  },
  "output": {
    "directory": "out",
    "format": "luau"
  }
}
```

## Compiler Options

### Target

Specifies the target environment:

```json
{
  "compiler": {
    "target": "roblox"  // or "luau"
  }
}
```

### Optimization

Control compiler optimizations:

```json
{
  "compiler": {
    "optimize": true,  // Enable optimizations
    "inlineThreshold": 100,  // Function size threshold for inlining
    "constantFolding": true,  // Enable constant folding
    "deadCodeElimination": true  // Enable dead code elimination
  }
}
```

### Type Checking

Configure type checking behavior:

```json
{
  "typeChecking": {
    "level": "strict",  // or "moderate" or "loose"
    "warnings": true,  // Enable type warnings
    "inference": true,  // Enable type inference
    "runtimeChecks": true  // Add runtime type checks
  }
}
```

### Output

Control output generation:

```json
{
  "output": {
    "directory": "out",  // Output directory
    "format": "luau",  // Output format
    "sourceMaps": true,  // Generate source maps
    "comments": true  // Preserve comments
  }
}
```

## Command Line Overrides

All configuration options can be overridden via command line:

```bash
apollo compile input.rkt --target roblox --optimize --no-strict
```

## Environment Variables

Some options can be set via environment variables:

```bash
export APOLLO_TARGET=roblox
export APOLLO_OPTIMIZE=true
```

## Default Configuration

If no configuration file is found, Apollo uses these defaults:

```json
{
  "compiler": {
    "target": "luau",
    "optimize": false,
    "strict": false,
    "sourceMaps": false
  },
  "typeChecking": {
    "level": "moderate",
    "warnings": true
  },
  "output": {
    "directory": ".",
    "format": "luau"
  }
}
```

## Best Practices

1.  Use a configuration file for project-specific settings
2.  Enable source maps for debugging
3.  Use strict type checking in development
4.  Enable optimizations for production builds

## See Also

*   [CLI Reference](../reference/cli.md)
*   [Type System](../explanation/type-system.md)
*   [Debugging Guide](../how-to/debugging.md) 