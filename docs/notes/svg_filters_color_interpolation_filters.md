# SVG filters: `color-interpolation-filters` (color space + premultiplication)

FastRender’s SVG filter implementation lives in [`src/paint/svg_filter.rs`](../../src/paint/svg_filter.rs).
This note documents how we currently handle **color spaces** and **premultiplication** for filter
primitives so future edits don’t accidentally regress `color-interpolation-filters`.

Related: [SVG filter percentage resolution](svg_filters_percentages.md).

## Spec default: `linearRGB`

Per the SVG spec, the default value for `color-interpolation-filters` is **`linearRGB`**.

FastRender mirrors this during parsing:

- `parse_filter_node()` defaults `SvgFilter.color_interpolation_filters` to `LinearRGB` when the
  attribute is missing.
- Each primitive step (`FilterStep`) may override the filter-level value via its own
  `color-interpolation-filters` attribute.
- At runtime the effective value is resolved per step via:
  `step.color_interpolation_filters.unwrap_or(def.color_interpolation_filters)`
  (see `apply_svg_filter_scaled()`).

## Pixel storage: premultiplied RGBA8 (u8)

All filter surfaces (`SourceGraphic`, named intermediate results, and the “current” result) are
stored as `tiny_skia::Pixmap` with pixels of type `PremultipliedColorU8` (premultiplied RGBA8).

That implies:

- The pixmap always contains **premultiplied** components (`r/g/b <= a`).
- Intermediate results are **8-bit** per channel (no float surfaces).
- Most code in `svg_filter.rs` assumes premultiplied storage, especially when delegating to
  tiny-skia for compositing (`draw_pixmap`, `BlendMode`).

### Unpremultiplication policy

Many filter primitives are defined in terms of **unpremultiplied** per-channel math (operate on R,
G, B, A as independent channels, then repremultiply). In FastRender this is done via:

- `to_unpremultiplied()` / `to_premultiplied()` (single-pixel conversion)
- `unpremultiply_components()` / `premul_from_components()` (float helpers used by compositing code)

Primitives that do per-channel math must be careful to **unpremultiply first**. This is especially
important for min/max style operations like `feMorphology`, where operating on premultiplied values
can bias results toward transparent pixels.

Porter-Duff compositing, however, is naturally defined on premultiplied colors and should **stay
premultiplied** (e.g. SourceOver, In, Out, Atop, Xor).

## Color space conversions (sRGB <-> linearRGB)

FastRender’s `Pixmap` pixels do not carry an explicit colorspace tag; conversion is handled in
`svg_filter.rs` based on the resolved `ColorInterpolationFilters` for each primitive step.

Key helpers:

- `unpack_color(px, color_space)`:
  - converts `PremultipliedColorU8` -> **unpremultiplied** float RGBA in `[0, 1]`
  - if `color_space == LinearRGB`, converts RGB via `srgb_to_linear()`
- `pack_color(color, color_space)`:
  - if `color_space == LinearRGB`, converts RGB via `linear_to_srgb()`
  - converts back to premultiplied RGBA8
- `reencode_pixmap_to_linear_rgb(pixmap)` / `reencode_pixmap_to_srgb(pixmap)`:
  - in-place conversions for whole pixmaps (still stored as premultiplied RGBA8)
  - used when an algorithm operates directly on the pixmap’s channel bytes

Important: even when a primitive’s math runs in linearRGB, the primitive output is typically
**re-encoded back to sRGB RGBA8** before it is stored as an intermediate result. This keeps all
surfaces in a consistent storage format, but means linearRGB work is “pay per primitive” and can
incur quantization at primitive boundaries.

## Per-primitive behavior in FastRender

Below is a summary of how each supported primitive currently treats `color-interpolation-filters`.
“Runs in linearRGB” means we explicitly convert RGB to linear values for the math; “runs in sRGB”
means we operate on the stored (sRGB-encoded) bytes/floats.

- `feGaussianBlur`
  - Runs in **linearRGB** when `color-interpolation-filters: linearRGB`.
  - Implementation: `reencode_pixmap_to_linear_rgb()` -> `apply_gaussian_blur_cached()` ->
    `reencode_pixmap_to_srgb()`.
- `feDropShadow`
  - The blur portion follows the same policy as `feGaussianBlur` using the same re-encode helpers.
- `feColorMatrix`
  - Runs in **linearRGB** when requested.
  - Implementation uses `unpack_color()` / `pack_color()` per pixel.
- `feComponentTransfer`
  - Runs in **linearRGB** when requested.
  - Implementation uses `unpack_color()` / `pack_color()` per pixel (with LUTs per channel).
- `feDiffuseLighting` / `feSpecularLighting`
  - Lighting color is converted to the step’s color space (`lighting_color_in_space()`), and the
    output pixel is written via `pack_color()`.
  - Surface normals/heights are derived from **alpha** (color space does not affect alpha).
- `feDisplacementMap`
  - `in2` (the displacement map) is interpreted in the step’s color space for channel selection
    (`sample_to_color_space()`), since `xChannelSelector`/`yChannelSelector` are defined over those
    channel values.
  - The displaced `in1` pixels are copied as-is (no color-space math on the sampled colors).

Primitives that currently **do not explicitly apply sRGB <-> linearRGB conversion** (they operate
directly on stored premultiplied RGBA8, so behavior is effectively “sRGB” today):

- `feComposite` / `feMerge` / `feBlend` (tiny-skia Porter-Duff + blend modes operate on premultiplied
  bytes)
- `feOffset` / `feTile` / `feImage` (copy/resample and/or draw via tiny-skia without re-encoding)
- `feMorphology` (min/max is currently performed on premultiplied channel bytes)
- `feConvolveMatrix` (kernel math uses unpremultiplied channels, but does not convert sRGB <-> linearRGB)
- `feTurbulence` / `feFlood` (do not perform color-space-dependent math; outputs are written as RGBA8)

## Known limitations / implications

- **8-bit intermediate precision:** all intermediate results are RGBA8; linearRGB work quantizes
  back to 8-bit at primitive outputs.
- **Conversions at primitive boundaries:** linearRGB primitives convert in/out of linear for their
  computation instead of keeping a float/linear surface across multiple primitives.
- **Partial coverage of `color-interpolation-filters`:** some primitives do not yet apply explicit
  sRGB <-> linearRGB conversion (see list above), so `linearRGB` may not be fully honored for complex
  filter graphs.
