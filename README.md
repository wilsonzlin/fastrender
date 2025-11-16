# FastRender - HTML/CSS Rendering Engine

A comprehensive HTML/CSS rendering engine built in Rust that renders web pages to pixel-perfect images.

## Features

- **Complete HTML/CSS Parser** - Handles complex DOM structures and CSS styling
- **Table-to-Flexbox Engine** - Custom table layout implementation for Taffy
- **SVG Support** - Integrated SVG rendering with resvg
- **Advanced Text Rendering** - Unicode support with professional typography
- **Image Processing** - Data URLs, external images, multiple formats
- **Pixel-Perfect Layout** - Professional-grade visual output

## Usage

### Basic Rendering
```bash
./target/release/fetch_and_render <URL>
```

### Example
```bash
./target/release/fetch_and_render https://example.com/
```

This will:
1. Fetch the HTML from the URL
2. Extract and load CSS stylesheets
3. Parse and render the complete page
4. Save the result as `fetched_output.png`

## Technical Architecture

- **Layout Engine**: Custom table-to-flexbox conversion for Taffy v0.5.2
- **CSS Processing**: Comprehensive parsing, inheritance, and application
- **Text Collection**: Advanced DOM text extraction and layout calculation
- **Rendering Pipeline**: HTML → CSS → Layout → Paint → PNG output

## Demo

The engine successfully renders complex web pages with complete fidelity:

- **Expected**: `expected.png` - Target design reference
- **Output**: `fetched_output.png` - Rendered result

Key achievements:
- ✅ Complex table layouts with proper cell alignment
- ✅ SVG logos and graphics rendering
- ✅ Navigation elements with styled text
- ✅ Interactive elements like vote arrows and buttons
- ✅ Professional typography and spacing

## Build

```bash
cargo build --release
```

The compiled binary will be available at `./target/release/fetch_and_render`.