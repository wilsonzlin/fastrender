Pending issues:
- CNN performance: cascade ~6–7s on inline-only CSS (~1.7MB, ~5.5k rules). Still timing out after cascade/box_tree (layout likely heavy). No fix yet; consider flex/layout hotspots (carousels/zones), skip heavy CSS/hidden content, or add layout perf profiling. SCRATCHPAD removed previously due to merge conflicts.
