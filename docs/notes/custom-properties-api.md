# Custom properties API cleanup

FastRender historically exposed `style::variables::CssVariables`, a string-based
"CSS variables" helper that was not wired into style computation. The rendering
pipeline uses `ComputedStyle.custom_properties: CustomPropertyStore` plus
`style::var_resolution` for `var()` handling, so the extra helper drifted from
engine semantics and risked divergent behavior.

`CssVariables` has been removed. The canonical API is the same one the engine
uses: store custom properties in a `CustomPropertyStore` and
resolve `var()` references with `style::var_resolution::{resolve_var, resolve_var_for_property}`
against that map. This keeps a single source of truth for custom property
storage and resolution.
