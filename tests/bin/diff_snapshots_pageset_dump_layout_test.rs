use serde_json::{json, Value};
use std::fs;
use std::path::Path;
use std::process::Command;

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

fn read_report(tmp: &tempfile::TempDir) -> Value {
  let report_path = tmp.path().join("diff_snapshots.json");
  assert!(report_path.exists(), "diff_snapshots.json missing");
  serde_json::from_str(&fs::read_to_string(report_path).expect("read report")).expect("parse json")
}

#[test]
fn diff_snapshots_supports_pageset_dump_layout() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_snapshot(&before.join("a").join("snapshot.json"));
  write_snapshot(&after.join("a").join("snapshot.json"));

  let status = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"))
    .current_dir(tmp.path())
    .args(["--before", before.to_str().unwrap(), "--after", after.to_str().unwrap()])
    .status()
    .expect("run diff_snapshots");
  assert!(status.success(), "expected success, got {:?}", status.code());

  assert!(
    tmp.path().join("diff_snapshots.html").exists(),
    "diff_snapshots.html missing"
  );

  let report = read_report(&tmp);
  let entry = report["entries"]
    .as_array()
    .and_then(|entries| {
      entries
        .iter()
        .find(|e| e.get("name").and_then(|v| v.as_str()) == Some("a"))
    })
    .expect("missing entry for a");
  assert_eq!(entry["status"], "matched");
}

#[test]
fn diff_snapshots_supports_pageset_dump_layout_missing_entries() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let before = tmp.path().join("before");
  let after = tmp.path().join("after");
  fs::create_dir_all(&before).unwrap();
  fs::create_dir_all(&after).unwrap();

  write_snapshot(&before.join("a").join("snapshot.json"));
  write_snapshot(&after.join("a").join("snapshot.json"));
  write_snapshot(&before.join("only_before").join("snapshot.json"));
  write_snapshot(&after.join("only_after").join("snapshot.json"));

  let status = Command::new(env!("CARGO_BIN_EXE_diff_snapshots"))
    .current_dir(tmp.path())
    .args(["--before", before.to_str().unwrap(), "--after", after.to_str().unwrap()])
    .status()
    .expect("run diff_snapshots");
  assert!(status.success(), "expected success, got {:?}", status.code());

  let report = read_report(&tmp);
  let entries = report["entries"].as_array().expect("entries array");

  let status_for = |name: &str| {
    entries
      .iter()
      .find(|e| e.get("name").and_then(|v| v.as_str()) == Some(name))
      .and_then(|e| e.get("status").and_then(|v| v.as_str()))
      .unwrap_or("<missing>")
      .to_string()
  };

  assert_eq!(status_for("a"), "matched");
  assert_eq!(status_for("only_before"), "missing_after");
  assert_eq!(status_for("only_after"), "missing_before");
}

