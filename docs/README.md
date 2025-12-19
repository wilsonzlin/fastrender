# FastRender Documentation

This directory contains all technical documentation for FastRender.

## Directory Structure

```
docs/
├── architecture/      # High-level architecture and design decisions
├── core/              # Core types, box generation, formatting contexts
├── layout/            # Layout algorithms (block, inline, flex, grid, table, float, positioned)
├── text/              # Text system (fonts, shaping, line breaking)
├── paint/             # Paint system (display list, rasterization, stacking contexts)
├── css-features/      # CSS feature implementations (variables, media queries, etc.)
├── testing/           # Testing strategy, WPT integration, benchmarks
├── guides/            # Developer guides (migration, API design, code standards)
├── research/          # Research notes on specs and other implementations
├── tasks/             # Task-based implementation system
│   ├── wave-1/ to wave-6/  # Task definitions by wave
│   └── notes/              # Implementation notes from completed tasks
└── archive/           # Historical/completed documentation
    └── refactor/      # Completed refactoring phases
```

## Quick Links

### Architecture & Core
- [Architecture Decisions](architecture/decisions.md) - Key design choices
- [Foundation Research](architecture/foundation.md) - Background research
- [Type System](core/type-system.md) - Core types and primitives
- [Box Generation](core/box-generation.md) - DOM to box tree conversion
- [Formatting Contexts](core/formatting-contexts.md) - BFC, IFC, etc.

### Layout
- [Block Layout](layout/block.md) - Block formatting context
- [Inline Layout](layout/inline.md) - Inline formatting context
- [Flexbox](layout/flex.md) - CSS Flexbox via Taffy
- [Grid](layout/grid.md) - CSS Grid via Taffy
- [Table](layout/table.md) - CSS Tables
- [Float](layout/float.md) - Float positioning
- [Positioned](layout/positioned.md) - Absolute/relative/fixed/sticky

### Text
- [Font System](text/font-system.md) - Font loading and management
- [Text Shaping](text/shaping.md) - Unicode text shaping
- [Line Breaking](text/line-breaking.md) - UAX #14 line breaking

### Paint
- [Display List](paint/display-list.md) - Display list generation
- [Rasterization](paint/rasterization.md) - Pixel rendering
- [Stacking Contexts](paint/stacking-contexts.md) - Z-order and compositing

### CSS Features
- [CSS Variables](css-features/variables.md) - Custom properties
- [Media Queries](css-features/media-queries.md) - Responsive design
- [Pseudo-elements](css-features/pseudo-elements.md) - ::before, ::after
- [Margin Collapse](css-features/margin-collapse.md) - Vertical margin collapsing
- [Feature Roadmap](css-features/roadmap.md) - Planned CSS features

### Testing
- [Testing Strategy](testing/strategy.md) - Overall testing approach
- [WPT Integration](testing/wpt.md) - Web Platform Tests
- [Reference Tests](testing/reference-tests.md) - Visual regression tests
- [Benchmarking](testing/benchmarking.md) - Performance benchmarks
- Style regression harness: `cargo test --quiet --test style_tests`

### Debugging renders
- [Render/Paint Debug Flags](debugging.md) - Environment flags and usage for diagnosing blank/black renders

### Guides
- [Migration Guide](guides/migration.md) - Migrating from V1
- [API Design](guides/api-design.md) - Public API documentation
- [Code Standards](guides/code-standards.md) - Coding conventions
- [Reference Materials](guides/reference-materials.md) - External resources
- [Implementation Checklist](guides/implementation-checklist.md) - Progress tracking

### Research
- [Servo Layout](research/servo-layout-architecture.md) - Servo's layout architecture
- [Table Layout Spec](research/table-layout-spec.md) - CSS table specification
- [Unicode Text](research/unicode-text-algorithms.md) - Unicode algorithms
- [Stacking Context Spec](research/stacking-context-spec.md) - Stacking context rules

### Task System
- [Task System README](tasks/README.md) - How the task system works
- [Task Graph](tasks/TASK_GRAPH.md) - Full dependency graph
- [Task Template](tasks/TASK_TEMPLATE.md) - Template for new tasks

## For Contributors

1. **New to the project?** Start with [Architecture Decisions](architecture/decisions.md)
2. **Implementing a feature?** Find the relevant topic directory
3. **Working on a task?** See [Task System README](tasks/README.md)
4. **Writing tests?** See [Testing Strategy](testing/strategy.md)

## Document Conventions

- Each document is self-contained with context and references
- Code examples use Rust with syntax highlighting
- CSS spec references link to official W3C specifications
- Implementation notes reference specific source files
