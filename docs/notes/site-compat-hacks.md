# Site compatibility hacks

FastRender defaults to a spec-only pipeline. Page-specific heuristics are
disabled unless explicitly enabled via a compatibility profile.

Enable the hacks with either of the following:

- `FastRenderConfig::compat_mode(CompatProfile::SiteCompatibility)`
- `FastRenderConfig::with_site_compat_hacks()`
- `BoxGenerationOptions::with_compat_profile(CompatProfile::SiteCompatibility)` (for
  direct box-generation tests)

When enabled, the box-generation stage applies two targeted behaviors used by
internal page captures:

- Drop empty elements whose class attribute contains `ad-height-hold`, `ad__slot`,
  or `should-hold-space` (common placeholder ad slots).
- If a hidden `[data-testid="one-nav-overlay"]` is present, skip it and the
  subsequent `.FocusTrapContainer-*` drawer contents to mirror the closed state
  of that navigation widget.

These behaviors are intentionally off by default to keep the renderer free of
page-specific hacks.
