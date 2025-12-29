use fastrender::accessibility::{AccessibilityNode, CheckState, PressedState};
use fastrender::api::FastRender;

fn find_by_id<'a>(node: &'a AccessibilityNode, id: &str) -> Option<&'a AccessibilityNode> {
  if node.id.as_deref() == Some(id) {
    return Some(node);
  }
  for child in node.children.iter() {
    if let Some(found) = find_by_id(child, id) {
      return Some(found);
    }
  }
  None
}

#[test]
fn aria_state_flags_cover_common_controls() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <button id="pressed" aria-pressed="true">Pressed</button>
        <div id="checkbox" role="checkbox" aria-checked="mixed">Check me</div>
        <input id="native-checkbox" type="checkbox" checked />
        <div id="custom-option" role="option" aria-selected="true">Selected</div>
        <select id="list" multiple>
          <option id="list-opt1">One</option>
          <option id="list-opt2" selected>Two</option>
        </select>
        <button id="menu-button" aria-expanded="false" aria-haspopup="menu">Menu</button>
        <details id="details" open>
          <summary>Summary</summary>
          <div>Content</div>
        </details>
        <button id="aria-disabled" aria-disabled="true">Blocked</button>
        <button id="native-disabled" disabled>Native</button>
        <input id="required-invalid" aria-required="true" aria-invalid="true" />
        <a id="visited" href="#" data-fastr-visited="true">Visited</a>
      </body>
    </html>
  "##;

  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let pressed = find_by_id(&tree, "pressed").expect("pressed button");
  assert_eq!(pressed.states.pressed, Some(PressedState::True));

  let checkbox = find_by_id(&tree, "checkbox").expect("aria checkbox");
  assert_eq!(checkbox.states.checked, Some(CheckState::Mixed));

  let native_checkbox = find_by_id(&tree, "native-checkbox").expect("native checkbox");
  assert_eq!(native_checkbox.states.checked, Some(CheckState::True));

  let custom_option = find_by_id(&tree, "custom-option").expect("custom option");
  assert_eq!(custom_option.states.selected, Some(true));

  let list_opt1 = find_by_id(&tree, "list-opt1").expect("list option one");
  assert_eq!(list_opt1.states.selected, Some(false));
  let list_opt2 = find_by_id(&tree, "list-opt2").expect("list option two");
  assert_eq!(list_opt2.states.selected, Some(true));

  let menu_button = find_by_id(&tree, "menu-button").expect("menu button");
  assert_eq!(menu_button.states.expanded, Some(false));
  assert_eq!(menu_button.states.has_popup.as_deref(), Some("menu"));

  let details = find_by_id(&tree, "details").expect("details element");
  assert_eq!(details.states.expanded, Some(true));

  let aria_disabled = find_by_id(&tree, "aria-disabled").expect("aria-disabled button");
  assert!(aria_disabled.states.disabled);
  let native_disabled = find_by_id(&tree, "native-disabled").expect("native disabled");
  assert!(native_disabled.states.disabled);

  let required_invalid = find_by_id(&tree, "required-invalid").expect("required invalid");
  assert!(required_invalid.states.required);
  assert!(required_invalid.states.invalid);

  let visited = find_by_id(&tree, "visited").expect("visited link");
  assert!(visited.states.visited);
  assert!(visited.states.focusable);
}

#[test]
fn role_inference_and_heading_levels() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <button id="btn">Button text</button>
        <a id="link" href="#">Link text</a>
        <input id="checkbox" type="checkbox" />
        <input id="radio" type="radio" />
        <input id="textbox" type="text" value="Hello" />
        <select id="combo">
          <option id="combo-opt" selected>Combo option</option>
        </select>
        <select id="listbox" multiple>
          <option id="list-opt">List option</option>
        </select>
        <div id="custom-option" role="option" aria-selected="true" tabindex="0">Custom option</div>
        <div id="aria-heading" role="heading" aria-level="4">Aria heading</div>
        <nav id="nav">Nav area</nav>
        <aside id="aside" aria-label="Sidebar">Sidebar</aside>
        <main id="page-main">Main area</main>
        <article id="article">
          <header id="article-header">Article heading</header>
          <footer id="article-footer">Article footer</footer>
          <main id="nested-main">Nested main</main>
        </article>
        <header id="page-header">Page header</header>
        <footer id="page-footer">Page footer</footer>
      </body>
    </html>
  "##;

  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let button = find_by_id(&tree, "btn").expect("button");
  assert_eq!(button.role, "button");
  assert_eq!(button.name.as_deref(), Some("Button text"));

  let link = find_by_id(&tree, "link").expect("link");
  assert_eq!(link.role, "link");

  let checkbox = find_by_id(&tree, "checkbox").expect("checkbox");
  assert_eq!(checkbox.role, "checkbox");
  assert_eq!(checkbox.states.checked, Some(CheckState::False));

  let radio = find_by_id(&tree, "radio").expect("radio");
  assert_eq!(radio.role, "radio");
  assert_eq!(radio.states.checked, Some(CheckState::False));

  let textbox = find_by_id(&tree, "textbox").expect("textbox");
  assert_eq!(textbox.role, "textbox");
  assert_eq!(textbox.value.as_deref(), Some("Hello"));

  let combo = find_by_id(&tree, "combo").expect("combobox");
  assert_eq!(combo.role, "combobox");
  assert_eq!(combo.value.as_deref(), Some("Combo option"));
  let combo_opt = find_by_id(&tree, "combo-opt").expect("combo option");
  assert_eq!(combo_opt.role, "option");

  let listbox = find_by_id(&tree, "listbox").expect("listbox");
  assert_eq!(listbox.role, "listbox");
  let list_opt = find_by_id(&tree, "list-opt").expect("list option");
  assert_eq!(list_opt.role, "option");
  assert_eq!(list_opt.states.selected, Some(false));

  let custom_option = find_by_id(&tree, "custom-option").expect("custom option");
  assert_eq!(custom_option.role, "option");
  assert_eq!(custom_option.states.selected, Some(true));
  assert!(custom_option.states.focusable);

  let aria_heading = find_by_id(&tree, "aria-heading").expect("aria heading");
  assert_eq!(aria_heading.role, "heading");
  assert_eq!(aria_heading.level, Some(4));

  let nav = find_by_id(&tree, "nav").expect("nav");
  assert_eq!(nav.role, "navigation");

  let aside = find_by_id(&tree, "aside").expect("aside");
  assert_eq!(aside.role, "complementary");
  assert_eq!(aside.name.as_deref(), Some("Sidebar"));

  let page_main = find_by_id(&tree, "page-main").expect("page main");
  assert_eq!(page_main.role, "main");
  let nested_main = find_by_id(&tree, "nested-main").expect("nested main");
  assert_eq!(nested_main.role, "generic");

  let page_header = find_by_id(&tree, "page-header").expect("page header");
  assert_eq!(page_header.role, "banner");
  let article_header = find_by_id(&tree, "article-header").expect("article header");
  assert_eq!(article_header.role, "generic");

  let page_footer = find_by_id(&tree, "page-footer").expect("page footer");
  assert_eq!(page_footer.role, "contentinfo");
  let article_footer = find_by_id(&tree, "article-footer").expect("article footer");
  assert_eq!(article_footer.role, "generic");
}

#[test]
fn shadow_dom_nodes_keep_roles_and_names() {
  let mut renderer = FastRender::new().expect("renderer");
  let html = r##"
    <html>
      <body>
        <div id="host">
          <template shadowroot="open">
            <button id="shadow-button" aria-pressed="true">Shadow Action</button>
            <header id="shadow-header">Shadow Header</header>
          </template>
        </div>
      </body>
    </html>
  "##;

  let dom = renderer.parse_html(html).expect("parse");
  let tree = renderer
    .accessibility_tree(&dom, 800, 600)
    .expect("accessibility tree");

  let button = find_by_id(&tree, "shadow-button").expect("shadow button");
  assert_eq!(button.role, "button");
  assert_eq!(button.name.as_deref(), Some("Shadow Action"));
  assert_eq!(button.states.pressed, Some(PressedState::True));

  let header = find_by_id(&tree, "shadow-header").expect("shadow header");
  assert_eq!(header.name.as_deref(), Some("Shadow Header"));
  assert_eq!(header.role, "generic");
}
