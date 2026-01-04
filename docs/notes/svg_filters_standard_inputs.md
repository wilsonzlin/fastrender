# SVG filter standard inputs (BackgroundImage/FillPaint/etc)

FastRender's SVG filter executor (`src/paint/svg_filter.rs`) natively supports the standard inputs
that are self-contained within the filtered element:

- `SourceGraphic`
- `SourceAlpha`

Some standard inputs depend on the surrounding paint pipeline and therefore require extra context
to be provided by the caller:

- `BackgroundImage` / `BackgroundAlpha`
  - The caller must snapshot the already-composited backdrop behind the filtered element and pass
    it into the SVG filter executor.
  - `BackgroundAlpha` is derived from the backdrop with **RGB = 0** and **A = backdrop alpha**
    (mirroring `SourceAlpha`'s "alpha-only" intent).
- `FillPaint` / `StrokePaint`
  - When the filtered element's computed fill/stroke paint is known at filter-application time, the
    caller can pass solid colors to the filter executor.
  - When unknown, FastRender uses a conservative fallback: if the filtered source pixmap is a
    single fully-opaque color, that color is used as a solid paint; otherwise the input is treated
    as transparent.

## Current pipeline support

The display-list paint backend snapshots backdrop pixels when (and only when) an SVG filter
references `BackgroundImage` or `BackgroundAlpha`.

At the time this was implemented, no pageset fixtures referenced these standard inputs, so coverage
is provided by the offline fixture `tests/fixtures/html/svg_filter_background_image.html` and unit
tests in `src/paint/svg_filter.rs`.

