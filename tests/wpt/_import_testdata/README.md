Synthetic WPT-like test data used by `import_wpt` tests.

The layout mirrors a tiny subset of upstream WPT with:
- a reftest under `css/simple/` that depends on `/resources` and a support stylesheet
- a mismatch reftest under `css/simple/` with `.html.ini` sidecars
- an absolute-origin resource URL under `css/simple/` (`http://web-platform.test:8000/...`)
- a visual test under `html/standalone/` that pulls in nested CSS with `@import`
- an external URL fixture under `html/network/` used to assert strict offline imports
