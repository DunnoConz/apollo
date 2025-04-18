# Apollo: A Racket to Luau Compiler

<<<<<<< HEAD
<p align="center">
  <img src="the_logo.jpg" width="200" alt="Apollo Logo">
</p>

[![Build Status](https://github.com/yourusername/apollo/actions/workflows/ci.yml/badge.svg)](https://github.com/yourusername/apollo/actions/workflows/ci.yml)
=======
[![Build Status](https://github.com/dunnoconz/apollo/actions/workflows/ci.yml/badge.svg)](https://github.com/dunnoconz/apollo/actions/workflows/ci.yml)
>>>>>>> 3982852 (Update README.md)

Apollo is a powerful compiler that transforms Racket code into Luau, enabling Roblox game development with the elegance and power of Racket's functional programming paradigm.

## Features

- **Full Racket Language Support**: Compile most Racket language features to Luau
- **Optimized Code Generation**: Produces efficient Luau code with caching and fast paths
- **Module System**: Support for Racket's module system with proper namespace handling
- **CTFE (Compile-Time Function Evaluation)**: Evaluate expressions at compile time
- **Pattern Matching**: Convert Racket's pattern matching to efficient Luau code
- **Type Safety**: Maintain type safety through the compilation process

## Installation

### Prerequisites

- Racket 8.0 or later
- Roblox Studio (for testing compiled code)

### Installation Steps

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/apollo.git
   cd apollo
   ```

2. Install dependencies:
   ```bash
   raco pkg install
   ```

3. Build the compiler:
   ```bash
   raco make src/apollo/main.rkt
   ```

## Quick Start

1. Create a new Racket file (e.g., `game.rkt`):
   ```racket
   #lang racket/base
   
   (define (create-player name)
     (let ([player (game:create-player name)])
       (set-player-speed! player 16)
       player))
   
   (define (main)
     (let ([player (create-player "Hero")])
       (displayln "Game started!")))
   ```

2. Compile to Luau:
   ```bash
   raco apollo game.rkt -o game.luau
   ```

3. Use in Roblox Studio:
   - Create a new Script in Roblox Studio
   - Copy the contents of `game.luau` into the script
   - Run the game to test

## Documentation

### Online Documentation

Visit our [documentation website](https://yourusername.github.io/apollo/) for detailed guides and API references.

### Building Documentation Locally

1. Install [Hugo](https://gohugo.io/installation/)
2. Run the documentation server:
   ```bash
   cd docs-hugo
   hugo server
   ```
3. Open `http://localhost:1313` in your browser

### API Reference

Generate local API documentation:
```bash
raco docs apollo
```

## Project Structure

```
apollo/
├── src/
│   └── apollo/
│       ├── compiler/     # Core compiler components
│       │   ├── ir.rkt    # Intermediate Representation
│       │   ├── parser.rkt # Racket parser
│       │   └── codegen.rkt # Luau code generator
│       └── main.rkt      # Entry point
├── tests/               # Test suite
├── examples/           # Example projects
└── docs-hugo/         # Documentation
```

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details on:
- Setting up a development environment
- Submitting pull requests
- Reporting issues
- Code style guidelines

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The Racket team for creating such a powerful language
- The Roblox team for Luau and the Roblox platform
- All contributors who have helped make Apollo better

## Support

- [GitHub Issues](https://github.com/yourusername/apollo/issues) for bug reports and feature requests
- [Discord Server](https://discord.gg/your-invite) for community support
- [Documentation](https://yourusername.github.io/apollo/) for detailed guides 
