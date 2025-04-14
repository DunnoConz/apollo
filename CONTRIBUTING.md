# Contributing to Apollo

Thank you for your interest in contributing to Apollo! This document provides guidelines and instructions for contributing to the project.

## Code of Conduct

All contributors are expected to be respectful and constructive in their communications.

## Getting Started

1. Fork the repository on GitHub
2. Clone your fork to your local machine
3. Install dependencies: `raco pkg install --deps search-auto`
4. Run tests to verify everything is working: `raco test src/apollo`

## Project Structure

Before contributing, please familiarize yourself with the project structure outlined in [STRUCTURE.md](STRUCTURE.md).

## Development Workflow

1. Create a feature branch for your changes: `git checkout -b feature/my-feature`
2. Make your changes
3. Add tests for new functionality
4. Run the tests: `raco test src/apollo`
5. Format your code: `raco fmt <changed-files>`
6. Commit your changes with descriptive commit messages
7. Push your branch to your fork
8. Submit a pull request

## Pull Request Process

1. Ensure your code passes all tests
2. Update documentation if necessary
3. Describe your changes in the pull request description
4. Link to any related issues

## Coding Conventions

### Style Guidelines

- Follow Racket style conventions as outlined in [How to Program Racket](https://docs.racket-lang.org/style/index.html)
- Use meaningful variable and function names
- Add comments for complex code sections
- Keep functions small and focused on a single task

### Module Organization

- Keep related functionality in appropriate modules
- Follow the lib/test/doc pattern
- Place implementation details in private/ when appropriate

### Testing

- Write unit tests for new functionality
- Test edge cases
- Ensure existing tests continue to pass

## Adding Language Features

### Adding Racket Features

1. Identify which phase of compilation needs to be updated
2. Update the corresponding module(s)
3. Add tests for the new feature
4. Document the feature in the scribblings

### Adding Luau Features

1. Add necessary data structures in types.rkt
2. Update codegen.rkt to generate the appropriate code
3. Add tests for the new feature
4. Document the feature in the scribblings

## Documentation

- Update documentation when adding or changing features
- Use Scribble for formal documentation
- Add docstrings to public functions
- Keep README.md and STRUCTURE.md up to date

## License

By contributing to Apollo, you agree that your contributions will be licensed under the project's MIT license. 