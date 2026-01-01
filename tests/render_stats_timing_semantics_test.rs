use fastrender::{LayoutDiagnostics, RenderStageTimings};

#[test]
fn render_stage_timings_accepts_legacy_text_keys_and_serializes_cpu_suffix() {
  let legacy = r#"{
    "text_fallback_ms": 1.25,
    "text_shape_ms": 2.5,
    "text_rasterize_ms": 3.75
  }"#;

  let timings: RenderStageTimings = serde_json::from_str(legacy).expect("deserialize legacy keys");
  assert_eq!(timings.text_fallback_cpu_ms, Some(1.25));
  assert_eq!(timings.text_shape_cpu_ms, Some(2.5));
  assert_eq!(timings.text_rasterize_cpu_ms, Some(3.75));

  let encoded = serde_json::to_value(&timings).expect("serialize timings");
  assert!(encoded.get("text_fallback_cpu_ms").is_some());
  assert!(encoded.get("text_shape_cpu_ms").is_some());
  assert!(encoded.get("text_rasterize_cpu_ms").is_some());
  assert!(
    encoded.get("text_fallback_ms").is_none(),
    "legacy key should not be emitted"
  );
  assert!(
    encoded.get("text_shape_ms").is_none(),
    "legacy key should not be emitted"
  );
  assert!(
    encoded.get("text_rasterize_ms").is_none(),
    "legacy key should not be emitted"
  );
}

#[test]
fn layout_diagnostics_accepts_legacy_taffy_keys_and_serializes_cpu_suffix() {
  let legacy = r#"{
    "taffy_flex_compute_ms": 123.0,
    "taffy_grid_compute_ms": 456.0
  }"#;

  let diag: LayoutDiagnostics = serde_json::from_str(legacy).expect("deserialize legacy keys");
  assert_eq!(diag.taffy_flex_compute_cpu_ms, Some(123.0));
  assert_eq!(diag.taffy_grid_compute_cpu_ms, Some(456.0));

  let encoded = serde_json::to_value(&diag).expect("serialize layout diagnostics");
  assert!(encoded.get("taffy_flex_compute_cpu_ms").is_some());
  assert!(encoded.get("taffy_grid_compute_cpu_ms").is_some());
  assert!(
    encoded.get("taffy_flex_compute_ms").is_none(),
    "legacy key should not be emitted"
  );
  assert!(
    encoded.get("taffy_grid_compute_ms").is_none(),
    "legacy key should not be emitted"
  );
}

