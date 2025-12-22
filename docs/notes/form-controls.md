# Form control rendering

FastRender treats native form controls as replaced elements so they participate in intrinsic sizing and paint their own UA appearance:

- `<input>`, `<select>`, `<textarea>`, and `<button>` generate `ReplacedType::FormControl` boxes unless `appearance: none` is set.
- Intrinsic widths respect HTML defaults (20 columns for text inputs/textarea unless overridden) and scale with the current font size.
- Checked checkboxes/radios draw glyphs inside the control; selects render a text value plus a caret; ranges draw a track + thumb.
- Disabled and focus states come from normal selector matching (`disabled` attribute, `data-fastr-focus` for :focus), so UA and author CSS still apply around the native painting.
- Setting `appearance: none` skips the native replacement and leaves the element as a normal box for author styling.

The reference fixture at `tests/ref/fixtures/form_controls` exercises common control types and states. Regenerate the golden with:

```
UPDATE_GOLDEN=1 cargo test form_controls_reference_image_matches_golden
```
