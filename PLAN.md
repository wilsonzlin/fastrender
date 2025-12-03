# FastRender V2 - Complete Rebuild Plan

## Overview

This plan documents the complete architectural rebuild of FastRender from a hack-laden proof-of-concept into a production-grade HTML/CSS rendering engine.

**Current Status:** Proof-of-concept with fundamental architectural flaws
**Target Status:** Spec-compliant, maintainable, extensible rendering engine
**Estimated Timeline:** 6-12 months of focused development
**Approach:** Clean rebuild with proper abstractions

## Critical Findings

The current codebase has fatal architectural issues:
1. **No separation between Box Tree and DOM Tree** - fundamental CSS concept missing
2. **Table layout faked with Flexbox** - cannot work correctly
3. **Element-specific hacks throughout** - not generalizable
4. **Missing Inline Layout** - critical gap in layout modes
5. **No formatting context abstraction** - core CSS concept absent

**Verdict:** Current architecture cannot be incrementally fixed. Requires rebuild.

## Documentation Structure

This plan is broken into focused, detailed documents:

### Phase 0: Foundation & Research
- [`docs/architecture/foundation.md`](docs/architecture/foundation.md) - Specification study and architecture research
- [`docs/architecture/decisions.md`](docs/architecture/decisions.md) - Core architectural decisions and rationale

### Phase 1: Core Architecture
- [`docs/core/type-system.md`](docs/core/type-system.md) - Box tree, Fragment tree, core types
- [`docs/core/formatting-contexts.md`](docs/core/formatting-contexts.md) - BFC, IFC, FFC, GFC, TFC abstractions
- [`docs/core/box-generation.md`](docs/core/box-generation.md) - DOM to Box Tree transformation
- [`docs/core/integration.md`](docs/core/integration.md) - Engine integration and pipeline

### Phase 2: Layout Algorithms
- [`docs/layout/block.md`](docs/layout/block.md) - Block Formatting Context implementation
- [`docs/layout/inline.md`](docs/layout/inline.md) - Inline Formatting Context implementation
- [`docs/layout/flex.md`](docs/layout/flex.md) - Flexbox (Taffy integration)
- [`docs/layout/grid.md`](docs/layout/grid.md) - Grid (Taffy integration)
- [`docs/layout/table.md`](docs/layout/table.md) - Table layout from scratch
- [`docs/layout/float.md`](docs/layout/float.md) - Float positioning
- [`docs/layout/positioned.md`](docs/layout/positioned.md) - Absolute/relative/fixed positioning

### Phase 3: Text & Typography
- [`docs/text/font-system.md`](docs/text/font-system.md) - Font loading, caching, fallback
- [`docs/text/shaping.md`](docs/text/shaping.md) - Bidi, script itemization, shaping
- [`docs/text/line-breaking.md`](docs/text/line-breaking.md) - Line breaking algorithm

### Phase 4: Paint & Rendering
- [`docs/paint/display-list.md`](docs/paint/display-list.md) - Display list construction
- [`docs/paint/stacking-contexts.md`](docs/paint/stacking-contexts.md) - Z-index and paint order
- [`docs/paint/rasterization.md`](docs/paint/rasterization.md) - Pixel rendering

### Phase 5: CSS Features
- [`docs/css-features/roadmap.md`](docs/css-features/roadmap.md) - Complete CSS feature matrix
- [`docs/css-features/margin-collapse.md`](docs/css-features/margin-collapse.md) - Margin collapsing algorithm
- [`docs/css-features/pseudo-elements.md`](docs/css-features/pseudo-elements.md) - ::before, ::after
- [`docs/css-features/media-queries.md`](docs/css-features/media-queries.md) - @media support
- [`docs/css-features/variables.md`](docs/css-features/variables.md) - Custom properties (fix existing)

### Phase 6: Testing & Quality
- [`docs/testing/strategy.md`](docs/testing/strategy.md) - Comprehensive testing approach
- [`docs/testing/wpt.md`](docs/testing/wpt.md) - Web Platform Tests setup
- [`docs/testing/reference-tests.md`](docs/testing/reference-tests.md) - Pixel-comparison testing
- [`docs/testing/benchmarking.md`](docs/testing/benchmarking.md) - Performance measurement

### Supporting Documentation
- [`docs/guides/migration.md`](docs/guides/migration.md) - Migrating from V1 to V2
- [`docs/guides/reference-materials.md`](docs/guides/reference-materials.md) - Specs, browser code, resources
- [`docs/guides/implementation-checklist.md`](docs/guides/implementation-checklist.md) - Detailed task breakdown
- [`docs/guides/code-standards.md`](docs/guides/code-standards.md) - Coding conventions and patterns
- [`docs/guides/api-design.md`](docs/guides/api-design.md) - Public API surface

## Quick Start for AI Agents

### Understanding the Current State
1. Read the comprehensive analysis above (you've already done this)
2. Review [`docs/architecture/decisions.md`](docs/architecture/decisions.md)

### Starting Phase 0 (Foundation Research)
1. Follow [`docs/architecture/foundation.md`](docs/architecture/foundation.md)
2. Complete all specification reading
3. Study reference implementations
4. Document findings in `/docs/research/`

### Starting Phase 1 (Core Architecture)
1. Review all Phase 1 documents
2. Start with [`docs/core/type-system.md`](docs/core/type-system.md)
3. Follow the detailed implementation steps
4. Write tests before implementation
5. Verify against acceptance criteria

### Progressive Implementation
Each document contains:
- **Context**: Why this is needed
- **Specification References**: Exact spec sections
- **Implementation Steps**: Detailed, ordered tasks
- **Code Examples**: Complete, working examples
- **Tests**: What to test and how
- **Acceptance Criteria**: When it's done
- **Integration Points**: How it connects to other components

## Success Criteria

The rebuild is complete when:

1. **Architecture**
   - [ ] Clear separation: DOM Tree → Box Tree → Fragment Tree → Display List
   - [ ] Each layout mode is independent and spec-compliant
   - [ ] No element-specific hacks in layout/paint code
   - [ ] Formatting contexts properly implemented

2. **Testing**
   - [ ] >10,000 tests passing (including WPT)
   - [ ] >80% code coverage
   - [ ] All layout modes have pixel-comparison tests
   - [ ] Fuzzing runs without crashes

3. **Features**
   - [ ] Block, Inline, Flex, Grid, Table layouts working
   - [ ] Text rendering supports all scripts
   - [ ] CSS cascade fully implemented
   - [ ] No hardcoded workarounds

4. **Quality**
   - [ ] Code is simple, clear, self-documenting
   - [ ] Every module has single responsibility
   - [ ] Performance benchmarks show <100ms for typical pages
   - [ ] Memory usage is reasonable (<50MB for typical pages)

## Navigation

Start reading from Phase 0:
- Next: [`docs/architecture/foundation.md`](docs/architecture/foundation.md)
- Index: See "Documentation Structure" above
- Checklist: [`docs/guides/implementation-checklist.md`](docs/guides/implementation-checklist.md)

## Living Document

This plan will evolve as we:
- Learn from specification study
- Discover implementation challenges
- Add new features
- Refine approaches

Each phase document has a "Last Updated" date and version history.

---

**Note for AI Agents**: This is an exhaustive plan. Do not skip steps, take shortcuts, or assume knowledge. Follow the specifications exactly. When in doubt, consult the spec documents in `/docs/references/specs/`. Write tests first, implement second, verify third.
