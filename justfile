# FastRender justfile
#
# Just is a command runner (like make but better)
# Install: cargo install just
# Run: just <command>
# List all commands: just --list

# Default recipe - show available commands
default:
    @just --list

# ===========================================================================
# Building
# ===========================================================================

# Build the project in debug mode
build:
    cargo build

# Build the project in release mode
build-release:
    cargo build --release

# Clean build artifacts
clean:
    cargo clean

# ===========================================================================
# Testing
# ===========================================================================

# Run all tests
test:
    cargo test --all-features --verbose

# Run tests with output shown
test-output:
    cargo test --all-features --verbose -- --nocapture

# Run a specific test
test-one TEST:
    cargo test {{TEST}} -- --nocapture

# Run tests and show coverage (requires cargo-tarpaulin)
coverage:
    cargo tarpaulin --out Html --output-dir coverage --all-features

# ===========================================================================
# Code Quality
# ===========================================================================

# Format all code
fmt:
    cargo fmt --all

# Check if code is formatted
fmt-check:
    cargo fmt --all -- --check

# Run clippy linter
lint:
    cargo clippy --all-targets --all-features -- -D warnings

# Fix auto-fixable clippy issues
fix:
    cargo clippy --fix --allow-dirty --all-targets --all-features

# Run all checks (fmt, clippy, test)
check: fmt-check lint test

# ===========================================================================
# Documentation
# ===========================================================================

# Build and open documentation
doc:
    cargo doc --no-deps --all-features --open

# Build documentation without opening
doc-build:
    cargo doc --no-deps --all-features

# Check documentation builds without warnings
doc-check:
    RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --all-features

# ===========================================================================
# Benchmarking
# ===========================================================================

# Run benchmarks
bench:
    cargo bench

# ===========================================================================
# CI Simulation
# ===========================================================================

# Run all CI checks locally
ci: fmt-check lint test doc-check
    @echo "✅ All CI checks passed!"

# ===========================================================================
# Development
# ===========================================================================

# Watch for changes and run tests
watch:
    cargo watch -x test

# Watch for changes and run specific test
watch-one TEST:
    cargo watch -x "test {{TEST}}"

# ===========================================================================
# Release
# ===========================================================================

# Build optimized release binary
release:
    cargo build --release

# ===========================================================================
# Dependencies
# ===========================================================================

# Update dependencies
update:
    cargo update

# Check for outdated dependencies
outdated:
    cargo outdated

# Audit dependencies for security vulnerabilities
audit:
    cargo audit

# ===========================================================================
# Utilities
# ===========================================================================

# Count lines of code (requires tokei)
loc:
    tokei

# Show dependency tree
tree:
    cargo tree

# Install development dependencies
install-dev-tools:
    cargo install cargo-watch
    cargo install cargo-tarpaulin
    cargo install cargo-outdated
    cargo install cargo-audit
    cargo install tokei
    @echo "✅ Development tools installed!"
