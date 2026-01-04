# SVG `filterRes` mapping (offset + clipped filter regions)

FastRender implements SVG filter effects for CSS `filter: url(...)` in
`src/paint/svg_filter.rs`.

This note records how we interpret `filterRes`, especially when the resolved
filter region is *offset* and/or extends outside the current raster surface.

## Implementation semantics

When a filter defines `filterRes="<w> <h>"`, FastRender:

1. Resolves the filter region in device pixels (`filter_region`) using the
   element bbox passed to `apply_svg_filter*`.
2. Allocates an intermediate working pixmap of size `w×h` (clamped to
   `MAX_FILTER_RES`).
3. Maps the **entire resolved filter region** onto the working pixmap:

   - `scale_res_x = w / filter_region.width`
   - `scale_res_y = h / filter_region.height`
   - Source pixels are resampled into filter-region space using a translation
     of `-filter_region.x/y` (so the filter region origin maps to (0,0)).

4. Runs the filter graph in that scaled coordinate system.
5. Resamples the working pixmap back into the destination pixmap with the
   inverse transform, and clips the final output to the resolved filter region.

### Clipping + outside-the-pixmap behavior

`filterRes` scaling is based on the **unclipped filter region size** (the values
from the resolved SVG filter region), even when that region lies partially
outside the pixmap we are filtering.

Pixels that fall outside the available raster surface are treated as
transparent during the resample, so that enlarging the filter region does not
“smear” edge pixels into the missing area.

This is regression-tested by
`filter_res_region_outside_pixmap_is_transparent` in `src/paint/svg_filter.rs`.

## Fixture + tests

The main repro fixture is:

- `tests/fixtures/html/svg_filter_filterres_offset_clip.html`

It renders a high-frequency repeating gradient and applies a CSS
`filter:url(data:...#f)` where the SVG filter document contains:

- `filterUnits="userSpaceOnUse"`
- `x="-20" y="10" width="60" height="40"` (offset filter region)
- `filterRes="31 23"`
- `feOffset dy="5"` to make out-of-bbox painting and clipping visible

The fixture uses an *external* SVG filter document (embedded as a data URL)
instead of inline `<svg>` so we exercise the `load_svg_filter()` /
`parse_filter_node()` code path used by CSS `filter:url(...)` references.

There is an integration golden test:

- `tests/paint/svg_filter_filterres_offset_clip_golden.rs`
- Golden image: `tests/fixtures/golden/svg_filter_filterres_offset_clip.png`
- Update with:
  `UPDATE_SVG_FILTER_FILTERRES_OFFSET_CLIP_GOLDEN=1 cargo test -q svg_filter_filterres_offset_clip_matches_golden --test paint_tests`

## Chromium note (baseline)

During investigation we found that Chromium (tested with the Snap-packaged
Chromium 143) appears to ignore `filterRes` for CSS `filter:url(...)` SVG
filters: changing `filterRes` does not change the output. FastRender does honor
`filterRes`, so the golden is based on FastRender output rather than Chromium.

Headless screenshot command (Snap confinement requires copying the fixture into
`/root/snap/chromium/current/` first):

```bash
# Note: older Chrome/Chromium builds may not support `--headless=new`; use `--headless` instead.
/snap/bin/chromium --no-sandbox --headless=new --disable-gpu --hide-scrollbars \
  --run-all-compositor-stages-before-draw \
  --user-data-dir=/root/snap/chromium/current/tmp-profile \
  --window-size=200,200 --force-device-scale-factor=1 \
  --virtual-time-budget=1500 \
  --screenshot=/root/snap/chromium/current/out.png \
  file:///root/snap/chromium/current/svg_filter_filterres_offset_clip.html
```
