use std::process::Command;

#[test]
fn help_mentions_default_output_and_dirs() {
    let output = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
        .arg("--help")
        .output()
        .expect("run fetch_and_render --help");

    assert!(output.status.success(), "--help should exit successfully");

    // fetch_and_render writes usage to stderr
    let help = String::from_utf8_lossy(&output.stderr);
    assert!(
        help.contains("<url>.png"),
        "help should mention default output naming (derived from URL); got:\n{}",
        help
    );
    assert!(
        help.contains("parent") && help.contains("dir"),
        "help should mention parent directory creation; got:\n{}",
        help
    );
}
