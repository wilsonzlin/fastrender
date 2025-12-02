# Phase 5: Code Quality & Housekeeping

**Goal**: Clean up technical debt, remove dead code, address warnings, improve maintainability.

**Prerequisites**: Phases 1-4 (major structural changes complete)

**Priority**: Medium - do after functional improvements

---

## Overview

This phase addresses accumulated technical debt:
1. Excessive clippy suppressions (90+)
2. Dead code hidden by `#![allow(dead_code)]`
3. Unresolved TODOs/FIXMEs
4. Vendored Taffy maintenance

---

## Task 5.1: Reduce Clippy Suppressions

### Current State

`src/lib.rs` lines 5-100 contain 90+ clippy allows:

```rust
// Many are appropriate for a rendering engine
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::too_many_arguments)]

// Some hide real issues
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(deprecated)]
```

### Categorization

#### Category A: Keep (Justified for Rendering)

These are legitimate for a graphics/layout engine:

```rust
// Floating point is core to rendering
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::float_cmp)]

// Complex functions are sometimes necessary
#![allow(clippy::too_many_arguments)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::cognitive_complexity)]

// Common in math code
#![allow(clippy::many_single_char_names)]
#![allow(clippy::similar_names)]
```

#### Category B: Keep (Style Preference)

These are style choices that should be consistent:

```rust
#![allow(clippy::needless_return)]  // Explicit returns can be clearer
#![allow(clippy::redundant_field_names)]  // x: x is clearer than x
#![allow(clippy::let_and_return)]  // Can aid debugging
#![allow(clippy::match_same_arms)]  // Sometimes clearer to be explicit
```

#### Category C: Remove (Temporary Hacks)

These hide real issues:

```rust
#![allow(dead_code)]           // REMOVE - clean up dead code
#![allow(unused_imports)]       // REMOVE - clean up imports
#![allow(unused_variables)]     // REMOVE - fix or prefix with _
#![allow(unused_assignments)]   // REMOVE - indicates logic issues
#![allow(unused_mut)]          // REMOVE - indicates unnecessary mut
#![allow(deprecated)]           // REMOVE - update deprecated usage
```

#### Category D: Evaluate (May Keep)

```rust
#![allow(missing_docs)]        // Consider adding docs
#![allow(clippy::module_inception)]  // Module naming preference
```

### Removal Process

#### Step 1: Remove dead_code Allow

```bash
# Remove from lib.rs
# Then build to see what's dead
cargo build 2>&1 | grep "warning.*dead_code"
```

For each dead code warning:
1. **Intentional future use**: Add `#[allow(dead_code)]` locally with comment
2. **Orphaned/obsolete**: Delete it
3. **Test-only**: Move to `#[cfg(test)]`

#### Step 2: Remove unused_imports Allow

```bash
cargo build 2>&1 | grep "warning.*unused_import"
```

For each:
1. **Actually unused**: Remove the import
2. **Used in cfg blocks**: Add `#[cfg(...)]` attribute
3. **Re-export**: Mark as `pub use` if intentional

#### Step 3: Remove Other Temporary Allows

One at a time:
1. Remove the allow
2. Build and see warnings
3. Fix each warning
4. Repeat

#### Step 4: Document Remaining Allows

After cleanup, document why each remaining allow exists:

```rust
// Justified allows for rendering engine

// Rendering uses extensive float math - precision loss is expected and managed
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]

// Layout functions genuinely need many parameters
#![allow(clippy::too_many_arguments)]
// etc.
```

### Target State

From 90+ allows to <20, all documented.

### Success Criteria

- [ ] `#![allow(dead_code)]` removed
- [ ] `#![allow(unused_imports)]` removed
- [ ] `#![allow(deprecated)]` removed
- [ ] All remaining allows documented
- [ ] Count < 20

---

## Task 5.2: Clean Up Dead Code

### After Removing dead_code Allow

Expected warnings in areas:
- Unused struct fields
- Unused functions
- Unused enum variants
- Unused type aliases

### Decision Framework

For each piece of dead code:

| Question | Action |
|----------|--------|
| Is this a public API intended for users? | Keep, document |
| Is this for a planned feature? | Add local #[allow] with issue/plan reference |
| Is this test infrastructure? | Move to #[cfg(test)] |
| Is this debug-only code? | Add #[cfg(debug_assertions)] |
| Is this truly orphaned? | Delete |

### Common Patterns

#### Pattern 1: Unused Error Variants

```rust
pub enum LayoutError {
    UnsupportedBoxType(String),
    CircularDependency,
    MissingContext(String),  // Maybe unused
}
```

**Action**: Keep error variants that could occur. They're part of API stability.

#### Pattern 2: Unused Struct Fields

```rust
pub struct ComputedStyle {
    // ...
    pub word_break: WordBreak,  // Maybe unused
}
```

**Action**: If the field represents a CSS property we intend to support, keep it.

#### Pattern 3: Unused Helper Functions

```rust
fn helper_nobody_calls() {
    // ...
}
```

**Action**: Delete unless it's clearly for future use.

### Cleanup Commands

```bash
# Find all dead code after removing allow
cargo build 2>&1 | grep "dead_code" > dead_code_report.txt

# Count occurrences
wc -l dead_code_report.txt

# Work through them
# Edit files, delete or annotate
# Repeat until cargo build shows no dead_code warnings
```

---

## Task 5.3: Address TODOs/FIXMEs

### Current Inventory

From `00-inventory.md`:

| Location | TODO | Priority |
|----------|------|----------|
| `src/style/mod.rs:2074` | Properly parse repeat() syntax | P2 |
| `src/paint/stacking.rs:538` | Check style.float | P1 |
| `src/tree/table_fixup.rs:450` | Get colspan from cell style | P3 |
| `src/text/pipeline.rs:699` | Set language from style | P3 |
| `src/text/pipeline.rs:838` | CSS direction property | P2 |
| `src/layout/contexts/positioned.rs:569` | transform, filter, isolation | P2 |

### For Each TODO

#### Option A: Implement Now

If it's small and blocking:
1. Implement the feature
2. Remove the TODO
3. Add tests

#### Option B: Create Issue and Update

If it's larger work:
1. Create a GitHub issue (if repo uses them)
2. Update TODO with reference:
   ```rust
   // TODO(#123): Implement transform property for stacking context creation
   ```

#### Option C: Remove if Obsolete

If the TODO no longer applies:
1. Verify it's truly obsolete
2. Delete the comment

### Specific Resolutions

#### TODO: style.float check (P1)

**Location**: `src/paint/stacking.rs:538`
```rust
// TODO: Check style.float when Float is added to ComputedStyles
```

**Resolution**: Float is already in style system. Implement the check:
```rust
// Check if element is floated
let is_floated = !matches!(style.float, Float::None);
if is_floated {
    // Floated elements create stacking contexts in some cases
}
```

#### TODO: repeat() syntax (P2)

**Location**: `src/style/mod.rs:2074` (after Phase 2: `src/style/grid.rs`)
```rust
// TODO: Properly parse repeat() syntax
```

**Resolution**: Implement `repeat()` in grid track parsing or create issue.

#### TODO: direction property (P2)

**Location**: `src/text/pipeline.rs:838`
```rust
// TODO: Implement CSS direction property support
```

**Resolution**: This is Phase 6 work. Update to reference it:
```rust
// TODO: Implement CSS direction property (see Phase 6 RTL/LTR support)
```

### Success Criteria

- [ ] All TODOs either implemented or updated with references
- [ ] No orphaned/stale TODOs
- [ ] P1 TODOs resolved

---

## Task 5.4: Review Vendored Taffy

### Current State

`vendor/taffy/` contains a forked version of the Taffy layout engine.

**From Cargo.toml**:
```toml
taffy = { path = "vendor/taffy" }
```

### Investigation Steps

#### Step 1: Document Changes

```bash
# Clone upstream taffy
cd /tmp
git clone https://github.com/DioxusLabs/taffy.git

# Diff against our vendor
diff -r /tmp/taffy/src vendor/taffy/src > taffy_changes.diff

# Analyze changes
wc -l taffy_changes.diff
```

#### Step 2: Understand Changes

For each change:
1. Is it a bug fix?
2. Is it a feature addition?
3. Is it specific to our use case?

#### Step 3: Decide on Strategy

**Option A: Upstream Changes**
If changes are general improvements:
1. Fork taffy on GitHub
2. Submit PR with our changes
3. If accepted, switch to crates.io version

**Option B: Maintain Fork**
If changes are specific to us:
1. Document why in `vendor/taffy/README.md`
2. Note the upstream version we forked from
3. Create process for syncing with upstream

**Option C: Remove Fork**
If changes are no longer needed:
1. Test with upstream taffy
2. Remove vendor directory
3. Use crates.io version

### Documentation Required

Create `vendor/taffy/FORK_NOTES.md`:
```markdown
# Taffy Fork Notes

## Upstream Version
Forked from taffy v0.X.Y (commit abc123)

## Changes Made
1. [describe change 1]
2. [describe change 2]

## Why Forked
[Explain why crates.io version doesn't work]

## Sync Process
To sync with upstream:
1. ...

## Exit Criteria
We can return to crates.io when:
- [condition 1]
- [condition 2]
```

---

## Task 5.5: Improve Test Coverage

### Current Coverage Areas

From `00-inventory.md`:
- 44 test files
- 1512 tests passing
- Golden image tests
- WPT integration

### Areas Likely Needing More Tests

After refactoring, ensure:

1. **New modules have tests**:
   - `style/cascade.rs`
   - `style/properties.rs`
   - `style/defaults.rs`
   - `style/grid.rs`
   - `css/parser.rs`
   - `css/properties.rs`

2. **Media query tests**:
   - Various viewport sizes
   - Feature combinations
   - Nested queries

3. **Selector tests**:
   - `:is()` pseudo-class
   - Complex combinators

### Test Organization

Ensure tests follow pattern:
- Unit tests in-file with `#[cfg(test)] mod tests`
- Integration tests in `tests/`
- Golden image tests in `tests/fixtures/`

---

## Validation

After completing Phase 5:

```bash
# Build with all warnings
RUSTFLAGS="-D warnings" cargo build

# Clippy with strict settings
cargo clippy -- -D warnings

# Count remaining allows
grep -c "allow(" src/lib.rs  # Target: <20

# No dead code
cargo build 2>&1 | grep -c "dead_code"  # Target: 0

# All tests pass
cargo test

# Check TODO count
grep -r "TODO" src/ | wc -l  # Should be documented ones only
```

---

## Commit Strategy

1. "chore: Remove dead_code clippy allow and clean up"
2. "chore: Remove unused_imports and clean up imports"
3. "chore: Address TODO in stacking.rs"
4. "chore: Document remaining clippy allows"
5. "docs: Add vendor/taffy fork documentation"

---

*Previous Phase: [04-phase4-css.md](./04-phase4-css.md)*
*Next Phase: [06-phase6-features.md](./06-phase6-features.md)*
