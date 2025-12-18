No progress this iteration; still idle/available. Marker baseline/list-style-position/ellipsis regressions landed upstream.
cloudflare.com fetch/render timed out at 60s; no changes made
latimes.com fetch/render timed out at 60s; no changes made
latimes.com fetch/render timed out at 60s; no changes made (duplicate notes removed)
w3.org grid render fixed: grid layout now falls back to stacking children when Taffy reports zero-width grid items, forcing containers to fill the available width; added env logging FASTR_LOG_GRID_ROOT/FASTR_LOG_GRID_CHILDREN. Added w3.org to fetch_pages targets.
Added regression `grid_children_are_laid_out_even_when_taffy_reports_zero_width` to ensure grid items are laid out via their own formatting contexts (text visible) even when Taffy returns zero widths. Local patches were pushed; also cached at /tmp/w3-grid-patches.
booking.com fetch blocked: CloudFront WAF returns HTTP 202 with x-amzn-waf-action: challenge and empty body, so page cannot be cached/rendered via fetch_pages.
w3.org grid render fixed: grid layout now falls back to stacking children when Taffy reports zero-width grid items, forcing containers to fill the available width; added env logging FASTR_LOG_GRID_ROOT/FASTR_LOG_GRID_CHILDREN. Added w3.org to fetch_pages targets.
Added regression `grid_children_are_laid_out_even_when_taffy_reports_zero_width` to ensure grid items are laid out via their own formatting contexts (text visible) even when Taffy returns zero widths.
Local patches for the grid fix/regression: /tmp/w3-grid-patches/0001-Handle-zero-width-grid-grids.patch and 0002-Add-regression-for-grid-items-laid-out-when-Taffy-wi.patch (pushing blocked by GitHub SSH timeouts).
