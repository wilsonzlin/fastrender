use base64::{engine::general_purpose::STANDARD, Engine as _};
use fastrender::paint::display_list::DisplayItem;
use fastrender::paint::display_list::ResolvedFilter;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::FastRender;

#[test]
fn filter_url_data_resolves_to_svg_filter() {
  let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><filter id='f'><feFlood flood-color='rgb(255,0,0)' flood-opacity='1' result='f'/><feComposite in='f' in2='SourceAlpha' operator='in'/></filter></svg>";
  let data_url = format!(
    "data:image/svg+xml;base64,{}#f",
    STANDARD.encode(svg.as_bytes())
  );
  let html = format!(
    "<style>body {{ margin: 0; }} #target {{ width: 4px; height: 4px; background: rgb(0, 0, 255); filter: url(\"{}\"); }}</style><div id=\"target\"></div>",
    data_url
  );

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let fragments = renderer
    .layout_document(&dom, 10, 10)
    .expect("layout document");

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragments.root);

  let has_svg_filter = list
    .items()
    .iter()
    .filter_map(|item| match item {
      DisplayItem::PushStackingContext(ctx) => Some(
        ctx
          .filters
          .iter()
          .any(|filter| matches!(filter, ResolvedFilter::SvgFilter(_))),
      ),
      _ => None,
    })
    .any(|v| v);

  assert!(
    has_svg_filter,
    "stacking context filters should include SVG filters resolved from url()"
  );
}

#[test]
fn filter_url_data_with_quotes_resolves_to_svg_filter() {
  let svg = "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><filter id='f'><feFlood flood-color='rgb(255,0,0)' flood-opacity='1' result='f'/><feComposite in='f' in2='SourceAlpha' operator='in'/></filter></svg>";
  let data_url = format!(
    "data:image/svg+xml;base64,{}#f",
    STANDARD.encode(svg.as_bytes())
  );
  let html = format!(
    "<style>body {{ margin: 0; }} #target {{ width: 4px; height: 4px; background: rgb(0, 0, 255); filter: url('{data_url}'); }}</style><div id=\"target\"></div>",
    data_url = data_url
  );

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let fragments = renderer
    .layout_document(&dom, 10, 10)
    .expect("layout document");

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragments.root);

  let has_svg_filter = list
    .items()
    .iter()
    .filter_map(|item| match item {
      DisplayItem::PushStackingContext(ctx) => Some(
        ctx.filters
          .iter()
          .any(|filter| matches!(filter, ResolvedFilter::SvgFilter(_))),
      ),
      _ => None,
    })
    .any(|v| v);

  assert!(
    has_svg_filter,
    "stacking context filters should include SVG filters resolved from url() with quotes"
  );
}
