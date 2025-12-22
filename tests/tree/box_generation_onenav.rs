use fastrender::compat::CompatProfile;
use fastrender::css::parser::parse_stylesheet;
use fastrender::dom;
use fastrender::dom::DomNode;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::tree::box_generation::generate_box_tree_with_anonymous_fixup;
use fastrender::tree::box_generation::generate_box_tree_with_anonymous_fixup_with_options;
use fastrender::tree::box_generation::BoxGenerationOptions;
use fastrender::tree::box_tree::BoxNode;

fn contains_class(node: &BoxNode, class: &str) -> bool {
  if let Some(info) = &node.debug_info {
    if info.classes.iter().any(|c| c == class) {
      return true;
    }
  }
  node
    .children
    .iter()
    .any(|child| contains_class(child, class))
}

#[test]
fn hidden_onenav_overlay_is_retained_by_default() {
  let html = r#"
        <div>
            <div data-testid="one-nav-overlay" class="Overlay-ljtLmi"></div>
            <div class="FocusTrapContainer-jqtblI"><span class="content">keep me</span></div>
            <div class="keep">keep me too</div>
        </div>
    "#;
  let css = r#"
        [data-testid="one-nav-overlay"] {
            visibility: hidden;
            opacity: 0;
        }
    "#;

  let dom: DomNode = dom::parse_html(html).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let box_tree = generate_box_tree_with_anonymous_fixup(&styled);

  assert!(contains_class(&box_tree.root, "keep"));
  assert!(contains_class(&box_tree.root, "content"));
  assert!(contains_class(&box_tree.root, "Overlay-ljtLmi"));
  assert!(contains_class(&box_tree.root, "FocusTrapContainer-jqtblI"));
}

#[test]
fn hidden_onenav_overlay_skips_drawer_with_site_compat() {
  let html = r#"
        <div>
            <div data-testid="one-nav-overlay" class="Overlay-ljtLmi"></div>
            <div class="FocusTrapContainer-jqtblI"><span class="content">keep me</span></div>
            <div class="keep">keep me too</div>
        </div>
    "#;
  let css = r#"
        [data-testid="one-nav-overlay"] {
            visibility: hidden;
            opacity: 0;
        }
    "#;

  let dom: DomNode = dom::parse_html(html).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let compat_options =
    BoxGenerationOptions::default().with_compat_profile(CompatProfile::SiteCompatibility);
  let box_tree = generate_box_tree_with_anonymous_fixup_with_options(&styled, &compat_options);

  assert!(contains_class(&box_tree.root, "keep"));
  assert!(contains_class(&box_tree.root, "content"));
  assert!(!contains_class(&box_tree.root, "Overlay-ljtLmi"));
  assert!(!contains_class(&box_tree.root, "FocusTrapContainer-jqtblI"));
}

#[test]
fn visible_onenav_overlay_retained_with_drawer_in_compat_mode() {
  let html = r#"
        <div>
            <div data-testid="one-nav-overlay" class="Overlay-ljtLmi"></div>
            <div class="FocusTrapContainer-jqtblI"><span class="content">keep me</span></div>
        </div>
    "#;
  let css = r#"
        [data-testid="one-nav-overlay"] {
            visibility: visible;
            opacity: 1;
        }
    "#;

  let dom: DomNode = dom::parse_html(html).unwrap();
  let stylesheet = parse_stylesheet(css).unwrap();
  let styled = apply_styles_with_media(&dom, &stylesheet, &MediaContext::screen(800.0, 600.0));
  let compat_options =
    BoxGenerationOptions::default().with_compat_profile(CompatProfile::SiteCompatibility);
  let box_tree = generate_box_tree_with_anonymous_fixup_with_options(&styled, &compat_options);

  assert!(contains_class(&box_tree.root, "Overlay-ljtLmi"));
  assert!(contains_class(&box_tree.root, "FocusTrapContainer-jqtblI"));
  assert!(contains_class(&box_tree.root, "content"));
}
