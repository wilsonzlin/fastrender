# FastRender Refactoring Plan

**Purpose**: Transform the codebase from a working proof-of-concept into an elegant, globally coherent architecture.

**Philosophy**: Zero backwards compatibility concerns. Purge old code completely. Build the "perfect puzzle" where everything beautifully fits together.

---

## Quick Links

| Document | Purpose |
|----------|---------|
| [00-inventory.md](docs/archive/refactor/00-inventory.md) | Current state: metrics, file inventory, type duplicates, hack locations |
| [01-phase1-types.md](docs/archive/refactor/01-phase1-types.md) | Eliminate duplicate types (css::Color, ComputedStyles) |
| [02-phase2-modules.md](docs/archive/refactor/02-phase2-modules.md) | Split monster files (style/mod.rs, css.rs) |
| [03-phase3-hacks.md](docs/archive/refactor/03-phase3-hacks.md) | Remove all site-specific hardcoding |
| [04-phase4-css.md](docs/archive/refactor/04-phase4-css.md) | Proper @media and :is() support |
| [05-phase5-quality.md](docs/archive/refactor/05-phase5-quality.md) | Clippy, dead code, TODOs, vendored Taffy |
| [06-phase6-features.md](docs/archive/refactor/06-phase6-features.md) | ::before/::after, float, transform, RTL |

---

## Current State Summary

| Metric | Current | Target |
|--------|---------|--------|
| Tests Passing | 1512/1512 | 100% |
| HACK Comments | 6 | 0 |
| TODO/FIXME | 5 | 0 |
| Clippy Suppressions | 90+ | <20 |
| Duplicate Type Systems | 2 | 1 |
| style/mod.rs | 2186 lines | <100 lines |

**Key Problems**:
1. Two Color types (`css::Color` vs `style::Rgba`)
2. Two ComputedStyle types (`ComputedStyles` vs `ComputedStyle`)
3. Monster file `style/mod.rs` with 6 site-specific hacks
4. Hacky CSS preprocessing instead of proper parsing

---

## Target Architecture

```
src/
├── lib.rs              # Minimal re-exports
├── api.rs              # Public API
├── css/                # CSS Parsing (promoted from css.rs)
│   ├── parser.rs       # Stylesheet parsing
│   ├── selectors.rs    # Selector types
│   ├── properties.rs   # Value parsing
│   └── types.rs        # StyleSheet, Declaration, etc.
├── style/              # Style System (refactored)
│   ├── mod.rs          # ~50 lines, re-exports only
│   ├── computed.rs     # THE ComputedStyle
│   ├── cascade.rs      # apply_styles
│   ├── properties.rs   # apply_declaration
│   ├── types.rs        # Style enums
│   └── ...             # Existing focused modules
├── dom.rs              # Unchanged
├── tree/               # Unchanged
├── layout/             # Unchanged
├── paint/              # Unchanged
└── text/               # Unchanged
```

---

## Phase Overview

### Phase 1: Type System Unification
Eliminate duplicate types to establish single sources of truth.
- Delete `css::Color`, use `style::Rgba` everywhere
- Merge `ComputedStyles` into `ComputedStyle`
- Clean up re-exports and aliases

### Phase 2: Module Surgery
Split monster files into focused, single-responsibility modules.
- `style/mod.rs` (2186 lines) → 6+ focused modules
- `css.rs` (865 lines) → `css/` directory

### Phase 3: Remove Site-Specific Hacks
Delete all hardcoded class/tag checks from style application.
- `.toc` grid placement
- `.img-link` grid placement
- Image border-radius hacks
- `.subscribe-btn` styling
- `.toc::before` pseudo-element hack
- HN-specific element defaults

### Phase 4: CSS Parser Improvements
Replace string manipulation with proper parsing.
- Implement @media query support (enables Phase 3)
- Fix :is() selector handling
- Delete `preprocess_media_queries()` and `unwrap_is_pseudo()`

### Phase 5: Code Quality
Clean up technical debt.
- Reduce clippy suppressions from 90+ to <20
- Remove dead code
- Address all TODOs
- Document vendored Taffy

### Phase 6: Missing CSS Features
Implement features needed for proper CSS support.
- ::before/::after pseudo-elements
- Float styling
- Transform property
- Direction (RTL/LTR)

---

## Dependency Graph

```
Phase 1 ──────────────────────────────────────────────────┐
   │                                                      │
   v                                                      │
Phase 2 ──────────────────────────────────────────────────┤
   │                                                      │
   v                                                      │
Phase 4 (@media, :is()) ──┬── Phase 3 (remove hacks) ─────┤
                          │                               │
                          │   Phase 6.1 (::before) ───────┤
                          │                               │
                          └───────────────────────────────┤
                                                          │
Phase 5 (quality) ────────────────────────────────────────┘
```

**Critical Path**: 1 → 2 → 4 → 3 → 5

---

## Progress Tracking

### Phase 1: Type System Unification
- [ ] 1.1 Eliminate css::Color
- [ ] 1.2 Merge ComputedStyle types
- [ ] 1.3 Clean up re-exports

### Phase 2: Module Surgery
- [ ] 2.1 Split style/mod.rs
- [ ] 2.2 Promote css.rs to css/ directory

### Phase 3: Remove Site-Specific Hacks
- [ ] 3.1 Remove .toc grid hack
- [ ] 3.2 Remove .img-link hack
- [ ] 3.3 Remove image border-radius hack
- [ ] 3.4 Remove .subscribe-btn hack
- [ ] 3.5 Remove .toc::before hack
- [ ] 3.6 Move element defaults to user-agent stylesheet

### Phase 4: CSS Parser Improvements
- [ ] 4.1 Implement @media query support
- [ ] 4.2 Fix :is() selector support

### Phase 5: Code Quality
- [ ] 5.1 Reduce Clippy suppressions
- [ ] 5.2 Clean up dead code
- [ ] 5.3 Address all TODOs
- [ ] 5.4 Review vendored Taffy

### Phase 6: Missing CSS Features
- [ ] 6.1 ::before/::after pseudo-elements
- [ ] 6.2 Float styling
- [ ] 6.3 Transform property
- [ ] 6.4 Direction (RTL)

---

## Validation Commands

```bash
# After each change
cargo build && cargo test

# Check for issues
grep -rn "HACK\|TODO\|FIXME" src/
grep -rn "LegacyColor\|css::Color" src/
grep -rn "ComputedStyles" src/

# Line counts
wc -l src/style/mod.rs   # Target: <100
wc -l src/css.rs         # Target: 0 (deleted)

# Clippy
cargo clippy -- -D warnings
```

---

## Notes

- **No backwards compatibility**: This is pre-release code
- **Tests must pass**: Never break the 1512 passing tests
- **Incremental commits**: One logical change per commit
- **Run tests frequently**: After each file modification

---

*See individual phase documents for detailed implementation steps.*
