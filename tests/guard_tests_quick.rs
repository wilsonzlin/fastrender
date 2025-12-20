//! Quick guard placeholder to keep `just guard-tests-quick` working.

#[test]
fn guard_tests_quick() {
    // List guard-related tests so `cargo test --quiet guard_tests_quick` remains fast and discoverable.
    let guards = [
        "readme_guard",
        "readme_is_present",
        "fetch_and_render_example_is_present",
        "background_position_logical_regression_is_present",
        "fetch_and_render_exit_regression_is_present",
    ];
    // Print to aid discovery when running with -- --nocapture.
    eprintln!("Guard tests available: {}", guards.join(", "));
}
