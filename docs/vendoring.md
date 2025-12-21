# Vendoring and local patches

FastRender vendors some dependencies to carry small, targeted patches.

## Taffy (vendored)

**Location:** `vendor/taffy/`

FastRender uses vendored Taffy for **flexbox and grid**. It is vendored so we can maintain local fixes without depending on upstream release cadence.

### Modifications

#### 1) Grid auto-placement: respect source order

**File:** `vendor/taffy/src/compute/grid/placement.rs`

**Change:** `place_grid_items` uses a single-pass placement that respects DOM source order.

**Marker:** `// MODIFIED: Single-pass placement respecting source order`

#### 2) Remove no-op clone calls

**File:** `vendor/taffy/src/compute/grid/placement.rs`

**Change:** Removed `.clone()` calls on references where the generic type doesn’t implement `Clone` (clippy cleanup).

### Updating the vendored copy

If you update `vendor/taffy/`, re-audit and re-apply local changes:

1. Replace `vendor/taffy/` with the new upstream version
2. Search for `// MODIFIED:` markers in the old copy and port them forward (or drop if upstream fixed them)
3. Run: `cargo test --quiet`
