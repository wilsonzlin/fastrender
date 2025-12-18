Grid/hero fix (Dec 18):
- Grid formatting context now lays out grid items via their own formatting contexts using the Taffy-resolved size, preserving child fragments (with box_ids) instead of producing empty grid items. GitHub hero headline now appears in the fragment tree/render.
- Debugging aids: FASTR_DISABLE_FLEX_CACHE; FASTR_TRACE_FLEX_TEXT; FASTR_TRACE_GRID_TEXT; FASTR_TRACE_POSITIONED; FASTR_TRACE_BOX_INFO; FASTR_DUMP_FRAGMENT; FASTR_FIND_BOX_TEXT/FASTR_FIND_TEXT. Box generation logs matching text when FASTR_FIND_BOX_TEXT is set.

Status: rebased onto latest main; grid fix + debug tooling on top. Workspace clean after this update. Older notes are in git history.
