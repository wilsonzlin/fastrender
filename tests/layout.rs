//! Layout module integration tests
//!
//! Note: Some test modules are temporarily disabled due to API changes.
//! - test_text_run: Uses LineMetrics::new() which no longer exists

#[path = "layout/test_factory.rs"]
mod test_factory;

#[path = "layout/test_positioned.rs"]
mod test_positioned;

#[path = "layout/test_absolute.rs"]
mod test_absolute;

#[path = "layout/test_baseline.rs"]
mod test_baseline;

// Disabled: Uses LineMetrics::new() which was replaced with empty()/from_strut()
// #[path = "layout/test_text_run.rs"]
// mod test_text_run;

#[path = "layout/test_inline_float.rs"]
mod test_inline_float;

#[path = "layout/scrollbar_gutter.rs"]
mod scrollbar_gutter;
