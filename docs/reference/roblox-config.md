# Roblox Configuration Reference

This document describes Roblox-specific configuration options for Apollo.

## Project Configuration

Apollo can be configured for Roblox projects using a JSON file (default: `apollo.config.json`):

```json
{
  "roblox": {
    "project": {
      "name": "MyGame",
      "tree": {
        "$className": "DataModel",
        "ReplicatedStorage": {
          "$className": "ReplicatedStorage",
          "Shared": {
            "$path": "src"
          }
        },
        "ServerScriptService": {
          "$className": "ServerScriptService",
          "Server": {
            "$path": "server"
          }
        },
        "StarterPlayer": {
          "$className": "StarterPlayer",
          "StarterPlayerScripts": {
            "$className": "StarterPlayerScripts",
            "Client": {
              "$path": "client"
            }
          }
        }
      }
    },
    "compiler": {
      "target": "roblox",
      "strict": true,
      "sourceMaps": true,
      "optimize": true
    },
    "typeChecking": {
      "level": "strict",
      "warnings": true
    }
  }
}
```

## Roblox-Specific Options

### Project Structure

Configure how your Racket code maps to Roblox's hierarchy:

```json
{
  "roblox": {
    "project": {
      "name": "MyGame",
      "tree": {
        "ReplicatedStorage": {
          "Shared": {
            "$path": "src"
          }
        }
      }
    }
  }
}
```

### Compiler Options

Roblox-specific compiler settings:

```json
{
  "roblox": {
    "compiler": {
      "target": "roblox",
      "strict": true,
      "sourceMaps": true,
      "optimize": true,
      "runtimeChecks": true
    }
  }
}
```

### Type Checking

Configure type checking for Roblox:

```json
{
  "roblox": {
    "typeChecking": {
      "level": "strict",
      "warnings": true,
      "runtimeChecks": true
    }
  }
}
```

## Command Line Usage

Compile for Roblox using the command line:

```bash
apollo compile input.rkt --target roblox --strict --source-maps
```

## Environment Variables

Set Roblox-specific options via environment variables:

```bash
export APOLLO_ROBLOX_TARGET=roblox
export APOLLO_ROBLOX_STRICT=true
```

## Default Configuration

If no configuration is specified, Apollo uses these Roblox defaults:

```json
{
  "roblox": {
    "compiler": {
      "target": "roblox",
      "strict": false,
      "sourceMaps": false,
      "optimize": true
    },
    "typeChecking": {
      "level": "moderate",
      "warnings": true
    }
  }
}
``` 