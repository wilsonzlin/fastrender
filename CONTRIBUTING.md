# Contributing to FastRender

Thank you for your interest in contributing to FastRender! This document provides guidelines and instructions for contributing.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Development Workflow](#development-workflow)
- [Code Style](#code-style)
- [Testing](#testing)
- [Pull Request Process](#pull-request-process)
- [Project Structure](#project-structure)
- [Common Tasks](#common-tasks)

## Getting Started

### Prerequisites

- **Rust**: Install from [rustup.rs](https://rustup.rs/)
  - Minimum version: 1.70
  - Recommended: Latest stable
- **just**: Command runner - `cargo install just`
- **Optional tools**: See [Development Tools](#development-tools)

### Clone the Repository

```bash
git clone https://github.com/fastrender/fastrender.git
cd fastrender
```

### Build and Test

```bash
# Build the project
cargo build

# Run tests
cargo test

# Or use just
just build
just test
```

If everything builds and tests pass, you're ready to contribute!

## Development Setup

### Editor Configuration

We recommend VS Code with the following extensions:
- **rust-analyzer**: Rust language server
- **CodeLLDB**: Debugging support
- **Even Better TOML**: TOML syntax highlighting

**VS Code Settings** (`.vscode/settings.json`):

```json
{
  "rust-analyzer.checkOnSave.command": "clippy",
  "editor.formatOnSave": true,
  "editor.rulers": [120],
  "[rust]": {
    "editor.defaultFormatter": "rust-lang.rust-analyzer"
  }
}
```

### Development Tools

Install recommended development tools:

```bash
just install-dev-tools
```

This installs:
- `cargo-watch`: Auto-rebuild on file changes
- `cargo-tarpaulin`: Code coverage
- `cargo-outdated`: Check for outdated dependencies
- `cargo-audit`: Security vulnerability scanning
- `tokei`: Line counter

## Development Workflow

### 1. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/your-bug-fix
```

**Branch naming conventions**:
- `feature/name` - New features
- `fix/name` - Bug fixes
- `refactor/name` - Code refactoring
- `docs/name` - Documentation updates
- `test/name` - Test additions/improvements

### 2. Make Changes

- Write code following our [code style guidelines](#code-style)
- Add tests for new functionality
- Update documentation as needed
- Run checks locally before committing
- Avoid no-op edits: ensure `git diff` shows meaningful changes before committing; whitespace-only or unchanged hunks add churn and may block pushes.

### 3. Run Pre-Commit Checks

Before committing, ensure all checks pass:

```bash
just check
```

This runs:
- `cargo fmt --check` - Format check
- `cargo clippy` - Linting
- `cargo test` - All tests

### 4. Commit Changes

Write clear, descriptive commit messages:

```bash
git add .
git commit -m "Add support for CSS grid auto-placement"
```

**Commit message guidelines**:
- Use imperative mood ("Add feature" not "Added feature")
- First line: 50 characters or less
- Body: Wrap at 72 characters
- Reference issues: "Fixes #123" or "Relates to #456"

Example:
```
Add CSS grid auto-placement algorithm

Implements the auto-placement algorithm from CSS Grid Level 1 spec.
This handles placing grid items that don't have explicit positions.

Fixes #123
```

### 5. Push and Create PR

```bash
git push origin feature/your-feature-name
```

Then create a pull request on GitHub.

## Code Style

### Formatting

All code must be formatted with `rustfmt`:

```bash
just fmt
```

Our formatting rules (`.rustfmt.toml`):
- **Max line width**: 120 characters
- **Tab width**: 4 spaces
- **Trailing commas**: Vertical
- **Import order**: Alphabetical, grouped by std/external/crate

### Linting

All code must pass clippy with no warnings:

```bash
just lint
```

We use strict clippy settings:
- `#![deny(clippy::all)]` - All clippy warnings are errors
- `#![warn(clippy::pedantic)]` - Pedantic lints enabled
- Some pedantic lints are allowed (see `src/lib.rs` for details)

### Naming Conventions

- **Types**: `PascalCase` - `BoxNode`, `ComputedStyle`
- **Functions/methods**: `snake_case` - `compute_layout`, `parse_css`
- **Constants**: `SCREAMING_SNAKE_CASE` - `DEFAULT_FONT_SIZE`
- **Modules**: `snake_case` - `geometry`, `text_shaping`

### Documentation

All public items must have rustdoc comments:

```rust
/// Computes the layout for a box tree
///
/// This function takes a box tree and viewport size, runs the layout
/// algorithm, and returns a positioned fragment tree.
///
/// # Arguments
///
/// * `box_tree` - The root of the box tree
/// * `viewport` - The viewport size in CSS pixels
///
/// # Returns
///
/// A positioned fragment tree ready for painting.
///
/// # Examples
///
/// ```
/// use fastrender::layout::compute_layout;
/// use fastrender::geometry::Size;
///
/// let viewport = Size::new(1024.0, 768.0);
/// let fragments = compute_layout(&box_tree, viewport);
/// ```
pub fn compute_layout(box_tree: &BoxNode, viewport: Size) -> FragmentTree {
    // ...
}
```

**Documentation requirements**:
- Summary line (first line)
- Detailed description
- Arguments (if any)
- Return value
- Examples (for public APIs)
- Panics (if the function can panic)
- Errors (if the function returns Result)

### Error Handling

- Use `Result<T, Error>` for recoverable errors
- Use `panic!` only for programmer errors (bugs)
- Provide helpful error messages
- Chain errors with `thiserror`

Example:

```rust
use crate::{Error, Result};

pub fn parse_color(input: &str) -> Result<Color> {
    csscolorparser::parse(input)
        .map_err(|e| Error::Parse(ParseError::InvalidColor {
            input: input.to_string(),
            source: e,
        }))
}
```

## Testing

### Writing Tests

- Write unit tests in the same file as the code
- Write integration tests in `tests/`
- Test public APIs and edge cases
- Use descriptive test names

Example:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_point_translate() {
        let p1 = Point::new(10.0, 20.0);
        let p2 = Point::new(5.0, 3.0);
        let result = p1.translate(p2);

        assert_eq!(result, Point::new(15.0, 23.0));
    }

    #[test]
    fn test_rect_intersection_non_overlapping() {
        let r1 = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
        let r2 = Rect::from_xywh(20.0, 20.0, 10.0, 10.0);

        assert_eq!(r1.intersection(r2), None);
    }
}
```

### Running Tests

```bash
# Run all tests
just test

# Run specific test
just test-one test_point_translate

# Run with output
just test-output

# Watch mode (auto-run on file changes)
just watch
```

**Guardrails**
- Guard tests:
  - `tests/readme_guard.rs` ensures the root `README.md` remains present.
  - `tests/readme_presence.rs` and `tests/style_regressions_presence.rs` ensure critical docs/regressions stay checked in.
  - `tests/fetch_and_render_exit_presence.rs` guards the fetch_and_render exit regression.
  Run them together via `just guard-tests` before pushing to avoid churn from accidental deletions.
  For a quick listing without running them, use `just guard-tests-quick`.
  The bundle also checks the README CLI example.

### Test Coverage

We aim for >80% code coverage:

```bash
just coverage
```

Open `coverage/index.html` to view the coverage report.

## Pull Request Process

### Before Submitting

1. ✅ All tests pass: `just test`
2. ✅ Code is formatted: `just fmt`
3. ✅ No clippy warnings: `just lint`
4. ✅ Documentation builds: `just doc-check`
5. ✅ All CI checks pass: `just ci`
6. 🔒 Guard rails: `just guard-tests` (especially if touching README/docs/regression files)

### PR Description Template

```markdown
## Summary

Brief description of changes.

## Motivation

Why is this change needed? What problem does it solve?

## Changes

- List of specific changes
- Made to the codebase

## Testing

How was this tested?
- [ ] Unit tests added
- [ ] Integration tests added
- [ ] Manual testing performed

## Checklist

- [ ] Tests pass locally
- [ ] Code is formatted
- [ ] No clippy warnings
- [ ] Documentation updated
- [ ] CHANGELOG.md updated (if applicable)

## Related Issues

Fixes #123
Relates to #456
```

### Review Process

1. **Automated checks**: CI must pass
2. **Code review**: At least one maintainer approval required
3. **Changes requested**: Address feedback and push updates
4. **Approval**: Maintainer approves and merges

## Project Structure

```
fastrender/
├── src/
│   ├── lib.rs              # Crate root, public API
│   ├── geometry.rs         # Geometric types
│   ├── error.rs            # Error types
│   ├── tree/               # DOM and box tree structures
│   ├── layout/             # Layout algorithms
│   ├── style/              # CSS cascade and styles
│   ├── text/               # Text shaping and fonts
│   └── paint/              # Painting and rasterization
├── tests/                  # Integration tests
├── benches/                # Benchmarks
├── docs/                   # Documentation
│   ├── plan/               # Architecture and planning docs
│   └── tasks/              # Task breakdown
├── .github/
│   └── workflows/
│       └── ci.yml          # GitHub Actions CI
├── Cargo.toml              # Package manifest
├── justfile                # Development commands
└── CONTRIBUTING.md         # This file
```

## Common Tasks

### Running the Renderer

```bash
# Build and run
cargo run --example basic

# Build release binary
just release
./target/release/fastrender
```

### Adding a New Module

1. Create module file: `src/modulename/mod.rs`
2. Add module declaration to `src/lib.rs`: `pub mod modulename;`
3. Re-export public types: `pub use modulename::PublicType;`
4. Add module documentation
5. Add tests

### Adding a Dependency

1. Add to `Cargo.toml` under appropriate section
2. Document why the dependency is needed
3. Run `cargo build` to update `Cargo.lock`
4. Commit both `Cargo.toml` and `Cargo.lock`

### Benchmarking

```bash
# Run all benchmarks
just bench

# Run specific benchmark
cargo bench --bench render_benchmark
```

### Debugging

Use `rust-lldb` or VS Code debugger:

```bash
# Set breakpoint in code
// ...
let result = compute_layout(&box_tree);
// Breakpoint here

# Run with debugger
rust-lldb target/debug/fastrender
```

## Getting Help

- **Documentation**: Run `just doc` to view API docs
- **Issues**: Check existing issues or create a new one
- **Discussions**: Use GitHub Discussions for questions
- **GitHub**: Open an issue for bug reports or feature requests

## Code of Conduct

We follow the Rust Code of Conduct: https://www.rust-lang.org/policies/code-of-conduct

Be respectful, inclusive, and constructive.

## License

By contributing, you agree that your contributions will be licensed under the same license as the project (MIT OR Apache-2.0).

---

Thank you for contributing to FastRender! 🎨
