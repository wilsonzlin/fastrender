use fastrender::api::FastRender;
use fastrender::tree::fragment_tree::FragmentNode;
use std::fmt::Write;
use std::thread;

fn with_large_stack<F: FnOnce() + Send + 'static>(f: F) {
  const STACK_SIZE: usize = 8 * 1024 * 1024;
  let handle = thread::Builder::new()
    .stack_size(STACK_SIZE)
    .spawn(f)
    .expect("spawn test thread");
  handle.join().expect("join test thread");
}

fn count_fragments(node: &FragmentNode) -> usize {
  let mut total = 0;
  let mut stack = vec![node];
  while let Some(fragment) = stack.pop() {
    total += 1;
    for child in fragment.children.iter() {
      stack.push(child);
    }
  }
  total
}

#[test]
fn large_table_layout_finishes_with_reasonable_fragment_count() {
  with_large_stack(|| {
    let rows = 60usize;
    let cols = 40usize;
    let mut rows_html = String::with_capacity(rows * cols * 16);
    for r in 0..rows {
      rows_html.push_str("<tr>");
      for c in 0..cols {
        let _ = write!(&mut rows_html, "<td>{r}-{c}</td>");
      }
      rows_html.push_str("</tr>");
    }

    let html = format!(
      r#"
      <html>
        <head>
          <style>
            table {{ border-collapse: collapse; width: 100%; }}
            td {{ border: 1px solid black; padding: 2px; }}
          </style>
        </head>
        <body>
          <table>
            {rows_html}
          </table>
        </body>
      </html>
    "#
    );

    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html(&html).unwrap();
    let tree = renderer.layout_document(&dom, 1600, 1200).unwrap();

    let mut fragment_count = count_fragments(&tree.root);
    for extra in &tree.additional_fragments {
      fragment_count += count_fragments(extra);
    }
    let cell_count = rows.saturating_mul(cols);
    assert!(
      fragment_count >= cell_count,
      "expected at least one fragment per cell, got {fragment_count} for {cell_count} cells"
    );
    assert!(
      fragment_count <= cell_count.saturating_mul(50),
      "large table produced {fragment_count} fragments for {cell_count} cells"
    );
  });
}
