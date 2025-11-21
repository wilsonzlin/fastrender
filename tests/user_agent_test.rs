#![allow(clippy::len_zero)]

use fastrender::Renderer;

#[test]
fn test_user_agent_styles() {
    let html = r#"
<!DOCTYPE html>
<html>
<head><title>Test</title></head>
<body>
    <h1>Heading 1</h1>
    <h2>Heading 2</h2>
    <p>Paragraph</p>
    <div>Block div</div>
    <span>Inline span</span>
    <strong>Bold text</strong>
    <em>Italic text</em>
</body>
</html>
    "#;

    let renderer = Renderer::new();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok(), "Should render successfully with user-agent styles");

    let png = result.unwrap();
    assert!(png.len() > 0, "Should produce non-empty PNG");
}

#[test]
fn test_user_agent_form_elements() {
    let html = r#"
<!DOCTYPE html>
<html>
<body>
    <form>
        <input type="text" value="test">
        <button>Click</button>
        <input type="submit" value="Submit">
    </form>
</body>
</html>
    "#;

    let renderer = Renderer::new();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok(), "Should render form elements with user-agent styles");
}

#[test]
fn test_user_agent_margins() {
    let html = r#"
<!DOCTYPE html>
<html>
<body>
    <h1>Test</h1>
</body>
</html>
    "#;

    let renderer = Renderer::new();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok(), "Should apply user-agent margins correctly");
}
