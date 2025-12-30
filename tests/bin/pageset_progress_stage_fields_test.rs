#[path = "../../src/bin/pageset_progress.rs"]
mod pageset_progress;

use fastrender::api::RenderDiagnostics;
use fastrender::error::RenderStage;
use fastrender::render_control::StageHeartbeat;
use serde_json::json;
use std::time::Duration;

#[test]
fn timeout_records_stage_and_hotspot() {
  let mut progress = pageset_progress::PageProgress::new("https://timeout.test/".to_string());

  pageset_progress::populate_timeout_progress(
    &mut progress,
    RenderStage::Cascade,
    Duration::from_millis(1200),
  );

  let value = serde_json::to_value(&progress).expect("serialize progress");
  assert_eq!(value["status"], json!("timeout"));
  assert_eq!(
    value["hotspot"],
    json!(pageset_progress::hotspot_from_timeout_stage(RenderStage::Cascade))
  );
  assert_eq!(value["timeout_stage"], json!("cascade"));
}

#[test]
fn diagnostics_failure_stage_is_propagated() {
  let mut progress = pageset_progress::PageProgress::new("https://css.test/".to_string());
  let mut diagnostics = RenderDiagnostics::default();
  diagnostics.failure_stage = Some(RenderStage::Css);

  pageset_progress::apply_diagnostics_to_progress(&mut progress, &diagnostics);

  let value = serde_json::to_value(&progress).expect("serialize progress");
  assert_eq!(value["failure_stage"], json!("css"));
}

#[test]
fn heartbeat_stage_maps_to_timeout_stage() {
  #[derive(serde::Serialize)]
  struct TimeoutStageFields {
    timeout_stage: Option<pageset_progress::ProgressStage>,
  }

  let fields = TimeoutStageFields {
    timeout_stage: pageset_progress::progress_stage_from_heartbeat(StageHeartbeat::ReadCache),
  };

  let value = serde_json::to_value(&fields).expect("serialize timeout_stage");
  assert_eq!(value["timeout_stage"], json!("dom_parse"));
}
