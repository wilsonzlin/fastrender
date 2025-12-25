Synthetic WPT-like test data used by `import_wpt` tests.

The layout mirrors a tiny subset of upstream WPT with:
- a reftest under `css/simple/` that depends on `/resources` and a support stylesheet
- a visual test under `html/standalone/` that pulls in nested CSS with `@import`
