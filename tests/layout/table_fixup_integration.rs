use fastrender::api::FastRender;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};

// These tests exercise the table fixup path inside table layout (not global tree fixup),
// ensuring invalid/mixed table content still renders rather than being silently dropped.
fn collect_text_fragments(node: &FragmentNode, out: &mut Vec<String>) {
  if let FragmentContent::Text { text, .. } = &node.content {
    let trimmed = text.trim();
    if !trimmed.is_empty() {
      out.push(trimmed.to_string());
    }
  }
  for child in node.children.iter() {
    collect_text_fragments(child, out);
  }
}

fn rendered_texts(html: &str) -> Vec<String> {
  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 800, 600).unwrap();
  let mut texts = Vec::new();
  collect_text_fragments(&tree.root, &mut texts);
  for fragment in &tree.additional_fragments {
    collect_text_fragments(fragment, &mut texts);
  }
  texts
}

#[test]
fn loose_cell_under_table_is_rendered() {
  let html = "<table><td>Loose</td></table>";
  let texts = rendered_texts(html);
  assert!(
    texts.iter().any(|t| t == "Loose"),
    "loose cell text should survive table layout, got {texts:?}"
  );
}

#[test]
fn text_inside_row_is_wrapped_into_cell() {
  let html = "<table><tr>oops<td>Cell</td></tr></table>";
  let texts = rendered_texts(html);
  assert!(
    texts.iter().any(|t| t == "oops"),
    "non-cell row content should be wrapped into a cell, got {texts:?}"
  );
  assert!(
    texts.iter().any(|t| t == "Cell"),
    "existing table cell should continue to render, got {texts:?}"
  );
}

#[test]
fn loose_cells_under_row_group_form_row() {
  let html = "<table><tbody><td>A</td><td>B</td></tbody></table>";
  let texts = rendered_texts(html);
  let pos_a = texts.iter().position(|t| t == "A");
  let pos_b = texts.iter().position(|t| t == "B");
  assert!(
    pos_a.is_some() && pos_b.is_some(),
    "expected both cells to render, got {texts:?}"
  );
  assert!(
    pos_a.unwrap() < pos_b.unwrap(),
    "row-group loose cells should appear in document order, got {texts:?}"
  );
}
