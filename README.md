# FastRender - HTML/CSS Rendering Engine

A production-grade HTML/CSS rendering engine in Rust that renders web pages to pixel-perfect PNG images.

[![CI](https://github.com/anysphere/fastrender/actions/workflows/ci.yml/badge.svg)](https://github.com/anysphere/fastrender/actions/workflows/ci.yml)
[![License: MIT OR Apache-2.0](https://img.shields.io/badge/License-MIT%20OR%20Apache--2.0-blue.svg)](LICENSE)

## Features

- **Complete Rendering Pipeline**: HTML/CSS parsing → Style computation → Layout → Paint → PNG output
- **Modern CSS Support**:
  - Flexbox layout (via Taffy)
  - CSS Grid layout (via Taffy)
  - Table layout (custom implementation)
  - Block/Inline formatting contexts
  - CSS Variables
  - Media queries
  - Pseudo-elements (::before, ::after)
- **Professional Text Rendering**:
  - Unicode bidirectional text (via unicode-bidi)
  - Text shaping (via rustybuzz/HarfBuzz)
  - Font fallback chains
  - Hyphenation support
  - UAX #14 line breaking
- **Graphics Support**:
  - SVG rendering (via resvg)
  - Image loading and scaling
  - Data URL support
  - Gradient backgrounds
  - Border radius and shadows
- **Production Quality**:
  - Comprehensive error handling
  - Extensive test coverage
  - CI/CD pipeline
  - Benchmarks included

## Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/anysphere/fastrender.git
cd fastrender

# Build release binary
cargo build --release
```

### Basic Usage

```bash
# Render a webpage
./target/release/fetch_and_render https://example.com/

# Output: fetched_output.png
```

### API Usage

```rust
use fastrender::{FastRender, FastRenderConfig};

// Create renderer with default configuration
let renderer = FastRender::new(FastRenderConfig::default());

// Render HTML to PNG bytes
let html = "<html><body><h1>Hello World</h1></body></html>";
let png_data = renderer.render_to_png(html, 1200, 800)?;

// Or render to a file
renderer.render_to_file(html, "output.png", 1200, 800)?;
```

## Architecture

```
URL → HTML Fetch → CSS Extract → Parse → DOM Tree → Styled Nodes
                                                    ↓
PNG ← Paint ← Fragment Tree ← Layout ← Box Tree ← Style Resolution
```

### Key Components

| Component | Description |
|-----------|-------------|
| `dom` | HTML parsing with scraper |
| `css` | CSS parsing with cssparser |
| `style` | Style resolution and computation |
| `tree` | Box tree generation |
| `layout` | Layout algorithms (Block, Flex, Grid, Table, Inline) |
| `paint` | Display list generation and rasterization |
| `text` | Text shaping and rendering |

## Dependencies

| Crate | Purpose |
|-------|---------|
| Taffy | Flexbox and Grid layout (vendored v0.9.1) |
| tiny-skia | 2D graphics rasterization |
| rustybuzz | Text shaping (HarfBuzz port) |
| resvg | SVG rendering |
| scraper | HTML parsing |
| cssparser | CSS parsing |
| fontdb | Font database |
| image | PNG output |

### Why Taffy is Vendored

Taffy is vendored in `vendor/taffy/` because we made modifications to the grid auto-placement algorithm to properly respect source order. See [VENDOR_PATCHES.md](VENDOR_PATCHES.md) for details.

## Development

### Prerequisites

- Rust 1.70+ (MSRV)
- System fonts for text rendering tests

### Building

```bash
# Debug build
cargo build

# Release build (optimized)
cargo build --release

# Run tests
cargo test

# Run benchmarks
cargo bench
```

### Code Quality

```bash
# Run linter
cargo clippy --all-targets --all-features -- -D warnings

# Check formatting
cargo fmt --check

# Generate documentation
cargo doc --no-deps --open
```

### Project Structure

```
fastrender/
├── src/
│   ├── lib.rs              # Public API
│   ├── api.rs              # FastRender builder
│   ├── dom.rs              # HTML parsing
│   ├── css/                # CSS parsing
│   ├── style/              # Style computation
│   ├── tree/               # Box tree generation
│   ├── layout/             # Layout algorithms
│   │   ├── contexts/       # Formatting contexts
│   │   │   ├── block/      # Block layout
│   │   │   ├── flex.rs     # Flexbox (Taffy)
│   │   │   ├── grid.rs     # Grid (Taffy)
│   │   │   ├── inline/     # Inline layout
│   │   │   └── table/      # Table layout
│   │   └── ...
│   ├── paint/              # Painting and rasterization
│   ├── text/               # Text processing
│   └── bin/                # CLI tools
├── tests/                  # Integration tests
├── benches/                # Benchmarks
├── vendor/taffy/           # Vendored Taffy with patches
└── docs/                   # Documentation
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT license ([LICENSE-MIT](LICENSE-MIT))

at your option.

## Acknowledgments

- [Taffy](https://github.com/DioxusLabs/taffy) - Flexbox and Grid layout
- [resvg](https://github.com/RazrFalcon/resvg) - SVG rendering
- [tiny-skia](https://github.com/RazrFalcon/tiny-skia) - 2D graphics
- [rustybuzz](https://github.com/RazrFalcon/rustybuzz) - Text shaping
