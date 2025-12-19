// Aggregator for style regression tests under tests/style/.
// Cargo only runs test files at the root of `tests/`, so this harness
// pulls the nested modules into a single integration test crate.

#[path = "style/aria_label_noop_test.rs"]
mod aria_label_noop_test;

#[path = "style/background_position_logical_test.rs"]
mod background_position_logical_test;

#[path = "style/cascade_marker_test.rs"]
mod cascade_marker_test;

#[path = "style/flex_property_test.rs"]
mod flex_property_test;

#[path = "style/grid_property_test.rs"]
mod grid_property_test;

#[path = "style/logical_properties_test.rs"]
mod logical_properties_test;

#[path = "style/order_property_test.rs"]
mod order_property_test;

#[path = "style/supports_selector_test.rs"]
mod supports_selector_test;

#[path = "style/supports_writing_mode_test.rs"]
mod supports_writing_mode_test;

#[path = "style/tab_size_test.rs"]
mod tab_size_test;

#[path = "style/text_decoration_skip_ink_test.rs"]
mod text_decoration_skip_ink_test;
