use fastrender::RenderStageTimings;
use serde_json::Value;

#[test]
fn render_stage_timings_deserializes_without_new_dom_fields() {
  // Simulate artifacts produced by older binaries before dom_* substage timings existed.
  let raw = r#"{
    "html_decode_ms": 1.0,
    "dom_parse_ms": 2.0
  }"#;

  let parsed: RenderStageTimings =
    serde_json::from_str(raw).expect("deserialize RenderStageTimings");
  assert_eq!(parsed.html_decode_ms, Some(1.0));
  assert_eq!(parsed.dom_parse_ms, Some(2.0));
  assert!(parsed.dom_html5ever_ms.is_none());
  assert!(parsed.dom_convert_ms.is_none());
  assert!(parsed.dom_shadow_attach_ms.is_none());
  assert!(parsed.dom_compat_ms.is_none());
  assert!(parsed.dom_meta_viewport_ms.is_none());
  assert!(parsed.dom_clone_ms.is_none());
  assert!(parsed.dom_top_layer_ms.is_none());
}

#[test]
fn render_stage_timings_roundtrips_with_dom_substage_fields() {
  let timings = RenderStageTimings {
    html_decode_ms: Some(1.0),
    dom_parse_ms: Some(2.0),
    dom_html5ever_ms: Some(0.75),
    dom_convert_ms: Some(0.5),
    dom_shadow_attach_ms: Some(0.25),
    dom_compat_ms: Some(0.0),
    dom_meta_viewport_ms: Some(0.1),
    dom_clone_ms: Some(0.2),
    dom_top_layer_ms: Some(0.3),
    ..RenderStageTimings::default()
  };

  let json = serde_json::to_string(&timings).expect("serialize RenderStageTimings");
  let roundtrip: RenderStageTimings =
    serde_json::from_str(&json).expect("deserialize RenderStageTimings");

  let a: Value = serde_json::to_value(&timings).expect("serialize to value");
  let b: Value = serde_json::to_value(&roundtrip).expect("serialize roundtrip");
  assert_eq!(a, b);
}
