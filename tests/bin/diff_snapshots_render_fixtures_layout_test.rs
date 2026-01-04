use serde_json::{json, Value};
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

fn minimal_style() -> Value {
  json!({
    "display": "block",
    "position": "static",
    "z_index": null,
    "visibility": "Visible",
    "opacity": 1.0,
    "overflow_x": "visible",
    "overflow_y": "visible",
    "top": "auto",
    "right": "auto",
    "bottom": "auto",
    "left": "auto",
    "margin": { "top": "0", "right": "0", "bottom": "0", "left": "0" },
    "margin_auto": { "top": false, "right": false, "bottom": false, "left": false },
    "padding": { "top": "0", "right": "0", "bottom": "0", "left": "0" },
    "border": { "top": "0", "right": "0", "bottom": "0", "left": "0" },
    "background_color": { "r": 0, "g": 0, "b": 0, "a": 0.0 },
    "color": { "r": 0, "g": 0, "b": 0, "a": 1.0 }
  })
}

fn rect(x: f32, y: f32, width: f32, height: f32) -> Value {
  json!({
    "x": x,
    "y": y,
    "width": width,
    "height": height,
  })
}

fn minimal_pipeline_snapshot_json() -> Value {
  json!({
    "schema_version": "v1",
    "dom": {
      "schema_version": "v1",
      "quirks_mode": "no_quirks",
      "root": {
        "node_id": 1,
        "kind": { "type": "document" },
        "children": []
      }
    },
    "styled": {
      "schema_version": "v1",
      "root": {
        "node_id": 1,
        "node": { "type": "document" },
        "style": minimal_style(),
        "children": []
      }
    },
    "box_tree": {
      "schema_version": "v1",
      "root": {
        "box_id": 1,
        "kind": { "type": "block", "formatting_context": "Block" },
        "style": minimal_style(),
        "children": []
      }
    },
    "fragment_tree": {
      "schema_version": "v1",
      "viewport": rect(0.0, 0.0, 100.0, 100.0),
      "roots": [
        {
          "fragment_id": 1,
          "bounds": rect(0.0, 0.0, 100.0, 100.0),
          "scroll_overflow": rect(0.0, 0.0, 100.0, 100.0),
          "fragment_index": 0,
          "fragment_count": 1,
          "fragmentainer_index": 0,
          "content": { "type": "block", "box_id": 1 },
          "children": []
        }
      ]
    },
    "display_list": {
      "schema_version": "v1",
      "items": []
    }
  })
}

fn write_snapshot(path: &Path) {
  fs::create_dir_all(path.parent().unwrap()).expect("create snapshot dir");
  let json = serde_json::to_string_pretty(&minimal_pipeline_snapshot_json()).expect("serialize");
  fs::write(path, format!("{json}\n")).expect("write snapshot");
}

#[test]
fn diff_snapshots_links_render_fixtures_layout_pngs() {
  let tmp = TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  let stem = "foo";
  write_snapshot(&before.join(stem).join("snapshot.json"));
  write_snapshot(&after.join(stem).join("snapshot.json"));

  fs::write(before.join(format!("{stem}.png")), b"").expect("write before png");
  fs::write(after.join(format!("{stem}.png")), b"").expect("write after png");

  let report_json = tmp.path().join("report.json");
  let report_html = tmp.path().join("report.html");
  let status = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"))
    .args([
      "--before",
      before.to_str().unwrap(),
      "--after",
      after.to_str().unwrap(),
      "--json",
      report_json.to_str().unwrap(),
      "--html",
      report_html.to_str().unwrap(),
    ])
    .status()
    .expect("run diff_snapshots");
  assert!(status.success(), "expected success, got {:?}", status.code());

  let report: Value =
    serde_json::from_str(&fs::read_to_string(&report_json).expect("read report"))
      .expect("parse report json");
  let entry = report["entries"]
    .as_array()
    .and_then(|entries| {
      entries
        .iter()
        .find(|e| e.get("name").and_then(|v| v.as_str()) == Some(stem))
    })
    .expect("missing entry for foo");

  assert_eq!(entry["status"], "matched");
  assert_eq!(
    entry.get("before_png").and_then(|v| v.as_str()),
    Some("before/foo.png"),
    "expected before_png link"
  );
  assert_eq!(
    entry.get("after_png").and_then(|v| v.as_str()),
    Some("after/foo.png"),
    "expected after_png link"
  );
}
