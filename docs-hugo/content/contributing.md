---
title: "Contributing"
weight: 100 # Keep it lower in the main menu or guide
---

# Contributing to Apollo

Thank you for your interest in contributing to Apollo! We welcome contributions of all kinds, from bug fixes to new features and documentation improvements.

## Code of Conduct

All contributors are expected to be respectful and constructive in their communications. Please ensure your interactions align with promoting a positive and collaborative environment.

## Getting Started

1.  **Fork & Clone:** Fork the repository on GitHub and clone your fork locally.
    ```bash
    git clone https://github.com/YOUR_USERNAME/apollo.git # Use your username
    cd apollo
    ```
2.  **Install Dependencies:** Ensure you have Racket installed. Then, install the project package itself, which also installs dependencies.
    ```bash
    raco pkg install --deps search-auto .
    ```
3.  **Run Tests:** Verify your setup by running the existing tests.
    ```bash
    raco test src/apollo
    ```

## Project Structure

Before contributing, please familiarize yourself with the project structure outlined in the [[Project Structure]]({{< relref "guide/structure.md" >}}) document.

## Development Workflow

1.  **Create Branch:** Create a feature or bugfix branch.
    ```bash
    git checkout -b feature/my-cool-feature # or fix/address-bug-123
    ```
2.  **Code:** Make your changes.
3.  **Test:** Add tests for new functionality or bug fixes. Ensure all tests pass.
    ```bash
    raco test src/apollo
    ```
4.  **Format:** Format your code using Racket's standard formatter.
    ```bash
    raco fmt <path/to/changed/file.rkt> ... 
    ```
5.  **Document:** Update or add documentation (both Hugo guides and Racket Scribble API docs if applicable).
6.  **Commit:** Commit your changes with descriptive messages.
7.  **Push:** Push your branch to your fork.
    ```bash
    git push origin feature/my-cool-feature
    ```
8.  **Pull Request:** Submit a pull request (PR) to the main Apollo repository. Clearly describe the changes and link to any relevant issues.

## Pull Request Process

1.  Ensure your code passes all tests in the CI environment (GitHub Actions).
2.  Ensure code is properly formatted.
3.  Update documentation if necessary.
4.  Provide a clear and concise description of your changes in the PR.
5.  Link to any related issues (e.g., "Fixes #123").

## Coding Conventions

### Style Guidelines

-   Follow Racket style conventions as outlined in the [Racket Style Guide](https://docs.racket-lang.org/style/index.html).
-   Use meaningful variable and function names.
-   Add comments for complex or non-obvious code sections.
-   Keep functions focused on a single task.

### Testing

-   Write unit tests for new functionality using the `rackunit` framework.
-   Test edge cases and potential error conditions.
-   Ensure existing tests continue to pass after your changes.

## Documentation

-   **Guides/Conceptual Docs:** Update or add pages in the `docs-hugo/content/` directory (Markdown format).
-   **API Reference:** Update Scribble documentation in `src/scribblings/apollo.scrbl` for changes to the public library API. Use `raco setup apollo` to rebuild Racket docs locally.

## License

By contributing to Apollo, you agree that your contributions will be licensed under the project's MIT license. 