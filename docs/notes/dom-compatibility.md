# DOM compatibility mode

FastRender parses HTML without mutating author markup by default. Some pages expect
their bootstrap JavaScript to flip classes like `no-js` → `js-enabled` or to add
visibility gates such as `jsl10n-visible`; without scripting those class flips never
fire.

When a static render needs to mirror those initializations, enable compatibility mode:

- `FastRenderConfig::with_dom_compat_mode(DomCompatibilityMode::Compatibility)` for the
  high-level API
- `DomParseOptions::compatibility()` when calling `dom::parse_html_with_options`
- CLIs: pass `--dom-compat compat` (and optionally `--compat-profile site`) to
  `fetch_and_render`, `render_pages`, `pageset_progress` (run/worker),
  `bundle_page` (fetch/render), or `inspect_frag`.

Compatibility mode applies a small, generic set of mutations:

- If the `<html>` element has a `no-js` class, replace it with `js-enabled`
- Add `jsl10n-visible` to `<html>` and `<body>` when missing
- Lift common lazy-load URL stashes into real attributes (without executing JS):
  - `<img>`: if `src` is missing/empty **or** set to a recognized placeholder (`about:blank`, `#`,
    common 1×1 GIF `data:` URLs), copy from the first non-empty candidate among:
    - `data-gl-src`, `data-src`, `data-lazy-src`, `data-original`, `data-url`, `data-actualsrc`,
      `data-img-src`, `data-hires`, `data-src-retina`
  - `<img>`: if `srcset` is missing/empty, copy from the first non-empty candidate among:
    - `data-gl-srcset`, `data-srcset`, `data-lazy-srcset`, `data-original-srcset`,
      `data-original-set`, `data-actualsrcset`
  - `<img>` / `<source>`: if `sizes` is missing/empty, copy from `data-sizes`.
  - `<picture><source>`: if `srcset` is missing/empty, lift from the same `data-srcset` candidates
    as `<img>`.
  - `<iframe>`: if `src` is missing/empty (or placeholder), lift from `data-src`.

These lifts intentionally **do not overwrite** non-empty, non-placeholder author-provided
`src`/`srcset` values.

Leaving compatibility mode at `DomCompatibilityMode::Standard` (the default) keeps the
parsed DOM spec-faithful.
