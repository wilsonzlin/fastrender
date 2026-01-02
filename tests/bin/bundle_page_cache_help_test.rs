use std::process::Command;

#[test]
fn bundle_page_cache_help_mentions_cache_dir_alias() {
  let output = Command::new(env!("CARGO_BIN_EXE_bundle_page"))
    .args(["cache", "--help"])
    .output()
    .expect("run bundle_page cache --help");

  assert!(
    output.status.success(),
    "bundle_page cache --help should exit successfully"
  );

  // clap writes help to stdout; keep stderr for compatibility with older parsers.
  let help = if output.stderr.is_empty() {
    String::from_utf8_lossy(&output.stdout)
  } else {
    String::from_utf8_lossy(&output.stderr)
  };

  assert!(
    help.contains("--asset-cache-dir"),
    "help should mention --asset-cache-dir; got:\n{help}"
  );
  assert!(
    help.contains("aliases: --cache-dir"),
    "help should mention the --cache-dir alias; got:\n{help}"
  );
}
