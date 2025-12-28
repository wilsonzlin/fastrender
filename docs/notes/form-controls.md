# Form control rendering

FastRender treats native form controls as replaced elements so they participate in intrinsic sizing and paint their own UA appearance:

- `<input>`, `<select>`, `<textarea>`, and `<button>` generate `ReplacedType::FormControl` boxes unless `appearance: none` is set.
- Intrinsic widths respect HTML defaults (20 columns for text inputs/textarea unless overridden by `size`/`cols`/`rows`) and scale with the current font size, so number/date fields line up with plain text inputs when unstyled.
- Text-like inputs cover `text/search/url/tel/email` plus password masking, number inputs (with spinner affordance), and date-like inputs (`date`/`datetime-local`/`month`/`week`/`time`) with a simple drop-down glyph and default format placeholders. Unknown types fall back to an `Unknown` control and use placeholder/value text as the label.
- Color inputs render a swatch filled with the current value and a hex label, keeping the raw value visible and marking enabled controls invalid when parsing fails.
- Checked and indeterminate checkboxes/radios draw glyphs inside the control; selects render a text value plus a caret; ranges draw a track + thumb.
- Disabled, focus, focus-visible, required, and invalid states come from normal selector matching (`disabled`, `required`, constraint validity, `data-fastr-focus*` hints) and are reflected in the native painting (tinted overlays, accent changes). The `data-fastr-focus-visible` hint implies focus for native painting so standalone focus-visible markers are captured.
- Setting `appearance: none` skips the native replacement and leaves the element as a normal box for author styling.

The reference fixture at `tests/ref/fixtures/form_controls` exercises common control types and states (including size/rows/cols hints, invalid and disabled colors, unknown types, date/time variants, and focus-visible highlights). Regenerate the golden with:

```
UPDATE_GOLDEN=1 cargo test form_controls_reference_image_matches_golden
```
