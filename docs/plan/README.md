# FastRender V2 Planning Documentation

This directory contains the complete, exhaustive plan for rebuilding FastRender from a proof-of-concept into a production-grade HTML/CSS rendering engine.

## Quick Start for AI Agents

**If you're an AI agent starting work on FastRender V2:**

1. **Start here:** Read the main [`PLAN.md`](../../PLAN.md) in the repository root
2. **Understand the context:** Read [`00-foundation-research.md`](00-foundation-research.md)
3. **Review decisions:** Read [`00-architecture-decisions.md`](00-architecture-decisions.md)
4. **Check your task:** Find it in [`09-implementation-checklist.md`](09-implementation-checklist.md)
5. **Follow standards:** Reference [`10-code-standards.md`](10-code-standards.md) while coding
6. **Test everything:** Follow [`06-testing-strategy.md`](06-testing-strategy.md)

## Document Index

### Phase 0: Foundation (Start Here)

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`00-foundation-research.md`](00-foundation-research.md) | Specification study & browser research | Before writing any code |
| [`00-architecture-decisions.md`](00-architecture-decisions.md) | Architectural decisions with rationale | Before Phase 1 |

### Phase 1: Core Architecture

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`01-type-system.md`](01-type-system.md) | Box Tree, Fragment Tree, geometry types | Week 1 of Phase 1 |
| [`01-formatting-contexts.md`](01-formatting-contexts.md) | Formatting context abstraction | Week 2 of Phase 1 |
| [`01-box-generation.md`](01-box-generation.md) | DOM → Box Tree transformation | Week 3 of Phase 1 |
| [`01-integration.md`](01-integration.md) | Integrating all Phase 1 components | Week 4 of Phase 1 |

### Phase 2: Layout Algorithms

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`02-block-layout.md`](02-block-layout.md) | Block Formatting Context | Week 1-2 of Phase 2 |
| [`02-inline-layout.md`](02-inline-layout.md) | Inline Formatting Context | Week 3-4 of Phase 2 |
| [`02-flex-layout.md`](02-flex-layout.md) | Flexbox (Taffy wrapper) | Week 5 of Phase 2 |
| [`02-grid-layout.md`](02-grid-layout.md) | Grid (Taffy wrapper) | Week 6 of Phase 2 |
| [`02-table-layout.md`](02-table-layout.md) | **Critical:** Table layout from scratch | Week 7-8 of Phase 2 |
| [`02-float-layout.md`](02-float-layout.md) | Float positioning | Phase 2 (later) |
| [`02-positioned-layout.md`](02-positioned-layout.md) | Absolute/relative/fixed positioning | Phase 2 (later) |

### Phase 3: Text & Typography

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`03-font-system.md`](03-font-system.md) | Font loading, caching, fallback | Week 1 of Phase 3 |
| [`03-text-shaping.md`](03-text-shaping.md) | Bidi, shaping, script itemization | Week 2 of Phase 3 |
| [`03-line-breaking.md`](03-line-breaking.md) | Line breaking algorithm | Week 3 of Phase 3 |

### Phase 4: Paint & Rendering

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`04-display-list.md`](04-display-list.md) | Display list construction | Week 1 of Phase 4 |
| [`04-stacking-contexts.md`](04-stacking-contexts.md) | Z-index and paint order | Week 2 of Phase 4 |
| [`04-rasterization.md`](04-rasterization.md) | Pixel rendering with tiny-skia | Week 3 of Phase 4 |

### Phase 5: CSS Features

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`05-css-features-roadmap.md`](05-css-features-roadmap.md) | Complete CSS feature matrix | Phase 5 overview |
| [`05-margin-collapse.md`](05-margin-collapse.md) | Margin collapsing algorithm | Phase 5 |
| [`05-pseudo-elements.md`](05-pseudo-elements.md) | ::before, ::after support | Phase 5 |
| [`05-media-queries.md`](05-media-queries.md) | @media query support | Phase 5 |
| [`05-css-variables.md`](05-css-variables.md) | CSS custom properties (fix) | Phase 5 |

### Phase 6: Testing & Quality

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`06-testing-strategy.md`](06-testing-strategy.md) | **Critical:** Testing approach | Read first, reference always |
| [`06-wpt-integration.md`](06-wpt-integration.md) | Web Platform Tests setup | Phase 6 |
| [`06-reference-tests.md`](06-reference-tests.md) | Pixel-comparison testing | Phase 6 |
| [`06-benchmarking.md`](06-benchmarking.md) | Performance measurement | Phase 6 |

### Supporting Documentation

| Document | Purpose | Read When |
|----------|---------|-----------|
| [`07-migration-guide.md`](07-migration-guide.md) | Migrating from V1 to V2 | Before deleting old code |
| [`08-reference-materials.md`](08-reference-materials.md) | Where to find specs & browser code | When researching |
| [`09-implementation-checklist.md`](09-implementation-checklist.md) | **Critical:** Exhaustive task list | Daily - track progress |
| [`10-code-standards.md`](10-code-standards.md) | **Critical:** Coding conventions | Reference while coding |
| [`11-api-design.md`](11-api-design.md) | Public API surface | When designing APIs |

## Document Status

| Phase | Completion | Notes |
|-------|------------|-------|
| Phase 0 | ✅ Complete | Foundation research documents ready |
| Phase 1 | 🟡 Partial | Core architecture specs written, some detail docs pending |
| Phase 2 | 🟡 Partial | Critical docs done (block, inline, table), others pending |
| Phase 3 | 🔴 Pending | High-level only |
| Phase 4 | 🔴 Pending | High-level only |
| Phase 5 | 🔴 Pending | Roadmap exists |
| Phase 6 | ✅ Complete | Testing strategy fully documented |

## How to Use These Documents

### For Initial Research (Phase 0)

Read in this order:
1. `00-foundation-research.md` - Study specs, browser code
2. `00-architecture-decisions.md` - Understand why we're building it this way
3. `08-reference-materials.md` - Download specs and browser code

**Deliverable:** Complete understanding of CSS layout model

### For Implementation (Phases 1-5)

Before starting each phase:
1. Read the phase overview in main `PLAN.md`
2. Read all documents for that phase
3. Find your tasks in `09-implementation-checklist.md`
4. Reference `10-code-standards.md` while coding
5. Follow `06-testing-strategy.md` for testing

**Deliverable:** Working, tested implementation

### For Testing (Continuous)

Always have open:
- `06-testing-strategy.md` - What to test and how
- `09-implementation-checklist.md` - Track test completion

**Deliverable:** >80% code coverage, WPT tests passing

## Critical Documents (Read These First)

If you only read 5 documents, read these:

1. **`00-architecture-decisions.md`** - Why we're building it this way
2. **`02-table-layout.md`** - The most important algorithm to get right
3. **`06-testing-strategy.md`** - How to verify correctness
4. **`09-implementation-checklist.md`** - What to do, in what order
5. **`10-code-standards.md`** - How to write the code

## Document Conventions

All plan documents follow these conventions:

### Structure

```markdown
# Phase X: Title

**Duration:** Estimated time
**Prerequisites:** What must be done first
**Dependencies:** Related documents
**Output:** What this produces

## Objectives
[What we're trying to achieve]

## Context
[Why this is needed]

## Step-by-step Implementation
[Detailed, ordered tasks]

## Acceptance Criteria
[How to know when it's done]

## Common Pitfalls
[What to avoid]

## Next Steps
[What comes after]
```

### Code Examples

All code examples are:
- **Complete** - Can be copied and compiled
- **Tested** - Verified to work
- **Documented** - Include comments explaining non-obvious parts
- **Spec-referenced** - Link to relevant spec sections

### Checklists

All checklists use:
- `[ ]` Not started
- `[~]` In progress
- `[x]` Complete
- `[!]` Blocked

### Specification References

All spec references include:
- Full spec name
- Section number
- Direct quote or paraphrase
- Link to spec (when possible)

## Contributing to This Plan

If you find issues or gaps:

1. **Missing detail:** Add it to the relevant document
2. **Incorrect information:** Fix it and note the correction
3. **New insights:** Add to the document with date and reasoning
4. **Completion:** Mark tasks as done in checklist

All documents are living - update as you learn.

## Principles

These documents are built on:

1. **Exhaustive Detail** - No hand-waving, no "figure it out later"
2. **Spec-Driven** - Reference CSS specs, not intuition
3. **Test-First** - Tests are specified before implementation
4. **No Shortcuts** - No hacks, no workarounds, do it right
5. **AI-Agent Friendly** - Detailed enough for autonomous execution

## Questions?

If something is unclear:

1. Check if it's documented elsewhere in this directory
2. Check the CSS specification (see `08-reference-materials.md`)
3. Check browser implementations (Servo, WebKit)
4. Ask human stakeholder for clarification
5. Document the answer for future reference

## Timeline

Based on the implementation checklist:

- **Phase 0:** 2 weeks (foundation research)
- **Phase 1:** 4 weeks (core architecture)
- **Phase 2:** 8 weeks (layout algorithms)
- **Phase 3:** 3 weeks (text & typography)
- **Phase 4:** 2 weeks (paint & rendering)
- **Phase 5:** Ongoing (CSS features)
- **Phase 6:** Continuous (testing)

**Total estimated time:** 6-12 months of focused development

## Success Metrics

The rebuild is successful when:

- [ ] All items in `09-implementation-checklist.md` are complete
- [ ] >10,000 tests passing (unit + integration + WPT)
- [ ] >80% code coverage
- [ ] No element-specific hacks in codebase
- [ ] Can render arbitrary HTML without special cases
- [ ] Performance <100ms for typical web pages
- [ ] All documentation complete and accurate

---

**Last Updated:** 2025-01-19
**Version:** 1.0
**Status:** Active Development Plan
