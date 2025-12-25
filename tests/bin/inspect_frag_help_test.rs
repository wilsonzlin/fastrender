use std::fs;
use std::process::Command;

#[test]
fn inspect_frag_help_mentions_new_flags() {
  let output = Command::new(env!("CARGO_BIN_EXE_inspect_frag"))
    .arg("--help")
    .output()
    .expect("run inspect_frag --help");

  assert!(output.status.success());
  let help = if output.stderr.is_empty() {
    String::from_utf8_lossy(&output.stdout)
  } else {
    String::from_utf8_lossy(&output.stderr)
  };

  for needle in [
    "--dump-json",
    "--render-overlay",
    "--filter-selector",
    "--filter-id",
  ] {
    assert!(help.contains(needle), "help missing {needle}; got:\n{help}");
  }
}

#[test]
fn inspect_frag_dump_json_creates_files() {
  let tmp = tempfile::TempDir::new().expect("temp dir");
  let status = Command::new(env!("CARGO_BIN_EXE_inspect_frag"))
    .args([
      "--dump-json",
      tmp.path().to_str().unwrap(),
      "tests/fixtures/html/block_simple.html",
    ])
    .status()
    .expect("run inspect_frag --dump-json");

  assert!(status.success(), "inspect_frag should exit successfully");

  for name in [
    "dom.json",
    "styled.json",
    "box_tree.json",
    "fragment_tree.json",
    "display_list.json",
  ] {
    let path = tmp.path().join(name);
    assert!(path.exists(), "{name} should be written");
    let contents = fs::read_to_string(&path).expect("read json file");
    serde_json::from_str::<serde_json::Value>(&contents).expect("valid json");
  }
}
