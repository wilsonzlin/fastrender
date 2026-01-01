# Layout timeouts: CPU profiling (baseline)

This note captures baseline CPU profiles for representative **layout timeouts** plus one **slow-but-ok** layout hotspot.
The goal is to reduce uncertainty about what actually dominates CPU time on the hard-killed layout pages.

Pages profiled:
- **Timeouts (layout stage):** `nytimes.com`, `stackoverflow.com`
- **Slow OK (layout hotspot):** `cnet.com`

## Commands used

These runs use the same caches / bundled-font defaults as the pageset wrappers.

Warm caches (HTML + disk cache) for the chosen pages:

```bash
cargo build --release --features disk_cache --bin fetch_pages
target/release/fetch_pages --jobs 1 --timeout 120 --pages nytimes.com,stackoverflow.com,cnet.com

cargo build --release --features disk_cache --bin prefetch_assets
target/release/prefetch_assets --jobs 1 --timeout 120 --pages nytimes.com,stackoverflow.com,cnet.com
```

Build a symbolized `pageset_progress` binary suitable for profiling:

```bash
CARGO_PROFILE_RELEASE_DEBUG=1 CARGO_PROFILE_RELEASE_STRIP=none \
  RUSTFLAGS='-C force-frame-pointers=yes' \
  cargo build --release --features disk_cache --bin pageset_progress
```

Record profiles (writes `target/pageset/profiles/<stem>-<timestamp>.profile.json.gz`):

```bash
scripts/profile_samply.sh nytimes.com --timeout 5 --bundled-fonts --progress-dir target/pageset/profile-progress
scripts/profile_samply.sh stackoverflow.com --timeout 5 --bundled-fonts --progress-dir target/pageset/profile-progress
scripts/profile_samply.sh cnet.com --timeout 5 --bundled-fonts --progress-dir target/pageset/profile-progress
```

### Symbolicating frames (important)

Samply profiles often contain raw addresses (`0x...`) instead of function names.
To get useful terminal summaries, run the summary script with **the exact binary that was profiled**
(rebuilt binaries will not match the recorded addresses):

```bash
python3 scripts/samply_summary.py <profile.json.gz> --top 25 \
  --addr2line-binary target/release/pageset_progress
```

To map individual addresses to symbols / file+line by hand, use `llvm-addr2line` (include `-i` to
show inlined frames):

```bash
llvm-addr2line -f -C -i -a -e target/release/pageset_progress 0xDEADBEEF
```

### Interpreting terminal summaries (gotchas)

- The summary is **per-thread**; on pages where layout parallelism kicks in, CPU can be spread
  across many worker threads.
- The “inclusive” percentages are derived from the stack table and can be misleading when
  recursion/inlining repeats the same function multiple times in a stack. Treat them qualitatively
  and confirm in the full Samply UI when needed.

## Results

### `nytimes.com` (timeout, layout)

Dominant hotspots from the top inclusive stacks:

- ~76%: `BlockFormattingContext::compute_intrinsic_inline_size` (`src/layout/contexts/block/mod.rs`)
- Flex/grid intrinsic measurement also shows up prominently:
  - `FlexFormattingContext::compute_intrinsic_inline_size` (`src/layout/contexts/flex.rs`)
  - `GridFormattingContext::measure_grid_item` (`src/layout/contexts/grid.rs`)
  - Taffy grid sizing (`vendor/taffy/src/compute/grid/track_sizing.rs`, `.../grid/mod.rs`)
- Secondary but present: text shaping (`ShapingPipeline::{shape_with_context,shape_core}` in `src/text/pipeline.rs`)
- Notably present in hot stacks: `FormattingContextFactory::with_font_context_viewport_and_cb` (`src/layout/contexts/factory.rs`)

**Interpretation / categorization:**
- **Dominated by** `BlockFormattingContext::compute_intrinsic_inline_size` and **flex/grid measure** work (Taffy).
- **Not dominated by** tables.

### `stackoverflow.com` (timeout, layout)

Dominant hotspots from the top inclusive stacks:

- ~59%: `BlockFormattingContext::layout` / `BlockFormattingContext::layout_children` (`src/layout/contexts/block/mod.rs`)
- ~42%: float placement:
  - `FloatContext::compute_float_position`
  - `FloatContext::{edges_in_range_min_width_with_state,next_float_boundary_after_internal}`
  (`src/layout/float_context.rs`)
- ~33%: `BlockFormattingContext::compute_intrinsic_inline_size` (`src/layout/contexts/block/mod.rs`)

**Interpretation / categorization:**
- **Dominated by** block formatting + **float placement** scanning.
- **Not dominated by** Taffy (flex/grid) or tables.

### `cnet.com` (slow OK, layout hotspot)

Dominant hotspots from the top inclusive stacks:

- ~28%: inline text item construction:
  - `InlineFormattingContext::create_text_items_with_combine`
  - `InlineFormattingContext::create_text_item_from_normalized`
  (`src/layout/contexts/inline/mod.rs`)
- ~28%: shaping:
  - `ShapingPipeline::{shape_with_context,shape_core}` (`src/text/pipeline.rs`)
  - `rustybuzz::hb::ot_shape::shape_internal` + glyph metrics (`rustybuzz` crate)
- Inline layout work also shows up:
  - `InlineFormattingContext::layout_with_floats` (`src/layout/contexts/inline/mod.rs`)
  - `LineBuilder::reorder_lines_for_bidi` (`src/layout/contexts/inline/line_builder.rs`)
- Some flex/grid measure remains visible:
  - Taffy child layout (`vendor/taffy/src/tree/taffy_tree.rs`)
  - `FlexFormattingContext::compute_intrinsic_inline_size` (`src/layout/contexts/flex.rs`)
  - `BlockFormattingContext::compute_intrinsic_inline_size` (`src/layout/contexts/block/mod.rs`)

**Interpretation / categorization:**
- **Dominated by** text shaping and inline line layout, with non-trivial flex/Taffy measure.
- **Not dominated by** tables.

## Actionable conclusions

These three pages support the current hypotheses:

1. **Intrinsic sizing dominates layout timeouts** (especially `BlockFormattingContext::compute_intrinsic_inline_size`).
   - This aligns with existing work on intrinsic caching / reducing repeated intrinsic recomputation (tasks **25/26**).
2. **Flex/grid “measure amplification” is real** (nytimes + cnet both show Taffy grid/child layout in the hottest stacks).
   - This aligns with work on Taffy template/style caching + reducing measure fan-out (tasks **27/28/31**).
3. **Float layout can be a primary limiter** (stackoverflow: float placement routines are near the top).
   - This suggests targeted optimization in `FloatContext` scanning logic (task **26** if it covers floats, otherwise a new float-specific perf task).
4. **Text shaping is a first-class perf problem on “slow OK” pages** (cnet: shaping + inline line layout dominate CPU).
   - This aligns with shaping pipeline churn / fallback work (tasks **25/26**).
5. **These layout timeouts are far beyond the current wall-time budget**.
   - As a sanity check, both `nytimes.com` and `stackoverflow.com` still hard-timeout at `--timeout 20`,
     with <~2s spent before layout and the rest in layout.
6. **Layout parallelism is a major factor in wall-time**.
   - `cnet.com` completes around ~4s with default `--layout-parallel auto`, but becomes extremely slow
     in serial mode (`--layout-parallel off` took ~18s with `--timeout 20` in a follow-up run).
