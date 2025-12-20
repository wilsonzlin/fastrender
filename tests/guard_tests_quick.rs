// Convenience guard: ensure core guard tests are available for quick runs.
#[test]
fn guard_tests_quick_list() {
    // This test lists guard-related tests so `cargo test --quiet guard_tests_quick` remains fast.
    // It doesn’t assert on code; presence in the suite is the guard.
    let guards = ["readme_is_present", "fetch_and_render_exit_regression_is_present"];
    // Print to aid discovery when running with -- --nocapture.
    eprintln!("Guard tests available: {}", guards.join(", "));
}
