use serde_json::Value;
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn css_coverage_classifies_known_unknown_and_vendor_prefixed_properties() {
  let temp = TempDir::new().expect("tempdir");
  let fixtures = temp.path().join("fixtures");
  fs::create_dir_all(&fixtures).expect("create fixtures dir");

  fs::write(
    fixtures.join("fixture.css"),
    r#"
      .a {
        color: rgb(from red r g b);
        -webkit-transform: translateX(1px);
        rotate: 10deg;
        unknown-prop: 1;
      }
    "#,
  )
  .expect("write css fixture");

  let output = Command::new(env!("CARGO_BIN_EXE_css_coverage"))
    .args([
      "--fixtures",
      fixtures.to_str().expect("utf8 path"),
      "--json",
      "--sample-values",
      "3",
    ])
    .output()
    .expect("run css_coverage");

  assert!(
    output.status.success(),
    "expected success, got status {:?}, stderr:\n{}",
    output.status.code(),
    String::from_utf8_lossy(&output.stderr)
  );

  let report: Value = serde_json::from_slice(&output.stdout).expect("json parse");
  let properties = report["properties"]
    .as_array()
    .expect("properties array");

  let lookup = |name: &str| -> Value {
    properties
      .iter()
      .find(|p| p["name"].as_str() == Some(name))
      .cloned()
      .unwrap_or_else(|| panic!("missing property `{name}` in report: {properties:?}"))
  };

  let color = lookup("color");
  assert_eq!(
    color["known_style_property"].as_bool(),
    Some(true),
    "expected `color` to be classified as known: {color:?}"
  );
  let samples = color["sample_values"]
    .as_array()
    .expect("color.sample_values array");
  assert!(
    samples
      .iter()
      .any(|s| s["accepted"].as_bool() == Some(false)),
    "expected at least one sampled `color` value to be rejected (rgb(from ...)), got: {samples:?}"
  );

  let vendor = lookup("-webkit-transform");
  assert_eq!(
    vendor["known_style_property"].as_bool(),
    Some(false),
    "expected vendor-prefixed property to be unknown: {vendor:?}"
  );
  assert_eq!(
    vendor["vendor_prefixed"].as_bool(),
    Some(true),
    "expected vendor-prefixed flag: {vendor:?}"
  );
  assert_eq!(
    vendor["unprefixed"].as_str(),
    Some("transform"),
    "expected unprefixed to be transform: {vendor:?}"
  );
  assert_eq!(
    vendor["unprefixed_known_style_property"].as_bool(),
    Some(true),
    "expected unprefixed `transform` to be known: {vendor:?}"
  );

  let rotate = lookup("rotate");
  assert_eq!(
    rotate["known_style_property"].as_bool(),
    Some(false),
    "expected `rotate` to be unknown: {rotate:?}"
  );

  let unknown = lookup("unknown-prop");
  assert_eq!(
    unknown["known_style_property"].as_bool(),
    Some(false),
    "expected `unknown-prop` to be unknown: {unknown:?}"
  );
}
