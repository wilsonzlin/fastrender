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
- [`docs/plan/00-foundation-research.md`](docs/plan/00-foundation-research.md) - Specification study and architecture research
- [`docs/plan/00-architecture-decisions.md`](docs/plan/00-architecture-decisions.md) - Core architectural decisions and rationale

### Phase 1: Core Architecture
- [`docs/plan/01-type-system.md`](docs/plan/01-type-system.md) - Box tree, Fragment tree, core types
- [`docs/plan/01-formatting-contexts.md`](docs/plan/01-formatting-contexts.md) - BFC, IFC, FFC, GFC, TFC abstractions
- [`docs/plan/01-box-generation.md`](docs/plan/01-box-generation.md) - DOM to Box Tree transformation
- [`docs/plan/01-integration.md`](docs/plan/01-integration.md) - Engine integration and pipeline

### Phase 2: Layout Algorithms
- [`docs/plan/02-block-layout.md`](docs/plan/02-block-layout.md) - Block Formatting Context implementation
- [`docs/plan/02-inline-layout.md`](docs/plan/02-inline-layout.md) - Inline Formatting Context implementation
- [`docs/plan/02-flex-layout.md`](docs/plan/02-flex-layout.md) - Flexbox (Taffy integration)
- [`docs/plan/02-grid-layout.md`](docs/plan/02-grid-layout.md) - Grid (Taffy integration)
- [`docs/plan/02-table-layout.md`](docs/plan/02-table-layout.md) - Table layout from scratch
- [`docs/plan/02-float-layout.md`](docs/plan/02-float-layout.md) - Float positioning
- [`docs/plan/02-positioned-layout.md`](docs/plan/02-positioned-layout.md) - Absolute/relative/fixed positioning

### Phase 3: Text & Typography
- [`docs/plan/03-font-system.md`](docs/plan/03-font-system.md) - Font loading, caching, fallback
- [`docs/plan/03-text-shaping.md`](docs/plan/03-text-shaping.md) - Bidi, script itemization, shaping
- [`docs/plan/03-line-breaking.md`](docs/plan/03-line-breaking.md) - Line breaking algorithm

### Phase 4: Paint & Rendering
- [`docs/plan/04-display-list.md`](docs/plan/04-display-list.md) - Display list construction
- [`docs/plan/04-stacking-contexts.md`](docs/plan/04-stacking-contexts.md) - Z-index and paint order
- [`docs/plan/04-rasterization.md`](docs/plan/04-rasterization.md) - Pixel rendering

### Phase 5: CSS Features
- [`docs/plan/05-css-features-roadmap.md`](docs/plan/05-css-features-roadmap.md) - Complete CSS feature matrix
- [`docs/plan/05-margin-collapse.md`](docs/plan/05-margin-collapse.md) - Margin collapsing algorithm
- [`docs/plan/05-pseudo-elements.md`](docs/plan/05-pseudo-elements.md) - ::before, ::after
- [`docs/plan/05-media-queries.md`](docs/plan/05-media-queries.md) - @media support
- [`docs/plan/05-css-variables.md`](docs/plan/05-css-variables.md) - Custom properties (fix existing)

### Phase 6: Testing & Quality
- [`docs/plan/06-testing-strategy.md`](docs/plan/06-testing-strategy.md) - Comprehensive testing approach
- [`docs/plan/06-wpt-integration.md`](docs/plan/06-wpt-integration.md) - Web Platform Tests setup
- [`docs/plan/06-reference-tests.md`](docs/plan/06-reference-tests.md) - Pixel-comparison testing
- [`docs/plan/06-benchmarking.md`](docs/plan/06-benchmarking.md) - Performance measurement

### Supporting Documentation
- [`docs/plan/07-migration-guide.md`](docs/plan/07-migration-guide.md) - Migrating from V1 to V2
- [`docs/plan/08-reference-materials.md`](docs/plan/08-reference-materials.md) - Specs, browser code, resources
- [`docs/plan/09-implementation-checklist.md`](docs/plan/09-implementation-checklist.md) - Detailed task breakdown
- [`docs/plan/10-code-standards.md`](docs/plan/10-code-standards.md) - Coding conventions and patterns
- [`docs/plan/11-api-design.md`](docs/plan/11-api-design.md) - Public API surface

## Quick Start for AI Agents

### Understanding the Current State
1. Read the comprehensive analysis above (you've already done this)
2. Review [`docs/plan/00-architecture-decisions.md`](docs/plan/00-architecture-decisions.md)

### Starting Phase 0 (Foundation Research)
1. Follow [`docs/plan/00-foundation-research.md`](docs/plan/00-foundation-research.md)
2. Complete all specification reading
3. Study reference implementations
4. Document findings in `/docs/research/`

### Starting Phase 1 (Core Architecture)
1. Review all Phase 1 documents
2. Start with [`docs/plan/01-type-system.md`](docs/plan/01-type-system.md)
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
- Next: [`docs/plan/00-foundation-research.md`](docs/plan/00-foundation-research.md)
- Index: See "Documentation Structure" above
- Checklist: [`docs/plan/09-implementation-checklist.md`](docs/plan/09-implementation-checklist.md)

## Living Document

This plan will evolve as we:
- Learn from specification study
- Discover implementation challenges
- Add new features
- Refine approaches

Each phase document has a "Last Updated" date and version history.

---

**Note for AI Agents**: This is an exhaustive plan. Do not skip steps, take shortcuts, or assume knowledge. Follow the specifications exactly. When in doubt, consult the spec documents in `/docs/references/specs/`. Write tests first, implement second, verify third.
