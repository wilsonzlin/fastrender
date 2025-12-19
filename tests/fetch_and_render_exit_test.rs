use std::process::Command;

#[test]
fn fetch_and_render_exits_non_zero_when_no_args() {
    let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
        .status()
        .expect("run fetch_and_render");

    assert!(!status.success(), "expected non-zero exit when no URL provided");
}

#[test]
fn fetch_and_render_exits_non_zero_for_missing_file_url() {
    let tmp = tempfile::TempDir::new().expect("tempdir");
    let missing = tmp.path().join("missing.html");
    let output = tmp.path().join("out.png");

    let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
        .arg(format!("file://{}", missing.display()))
        .arg(&output)
        .status()
        .expect("run fetch_and_render");

    assert!(
        !status.success(),
        "expected non-zero exit when input file is missing (got {:?})",
        status.code()
    );
}

#[test]
fn fetch_and_render_errors_on_unknown_option() {
    let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
        .args(["--definitely-not-a-flag"])
        .status()
        .expect("run fetch_and_render");

    assert!(
        !status.success(),
        "expected non-zero exit when an unknown option is provided (got {:?})",
        status.code()
    );
}
