use fastrender::api::FastRender;
use fastrender::style::display::Display;
use fastrender::tree::fragment_tree::FragmentNode;
use std::fmt::Write;
use std::thread;
use std::time::{Duration, Instant};

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

fn collect_cell_tops(node: &FragmentNode, tops: &mut Vec<f32>) {
  if node
    .style
    .as_ref()
    .map(|s| matches!(s.display, Display::TableCell))
    .unwrap_or(false)
  {
    tops.push(node.bounds.y());
  }
  for child in &node.children {
    collect_cell_tops(child, tops);
  }
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

#[test]
fn very_large_spanning_table_stays_fast_and_stable() {
  with_large_stack(|| {
    let rows = 160usize;
    let cols = 40usize;
    let mut rows_html = String::with_capacity(rows * cols * 18);
    for r in 0..rows {
      rows_html.push_str("<tr>");
      let mut c = 0usize;
      while c < cols {
        if r % 30 == 1 && c == 0 && r + 3 < rows {
          let _ = write!(
            &mut rows_html,
            r#"<td rowspan="3" colspan="2">span-{r}</td>"#
          );
          c += 2;
          continue;
        }
        if r % 25 == 0 && c + 3 <= cols {
          let _ = write!(&mut rows_html, r#"<td colspan="3">wide-{r}-{c}</td>"#);
          c += 3;
          continue;
        }
        let _ = write!(&mut rows_html, "<td>{r}-{c}</td>");
        c += 1;
      }
      rows_html.push_str("</tr>");
    }

    let html = format!(
      r#"
      <html>
        <head>
          <style>
            table {{ border-collapse: collapse; width: 100%; }}
            td {{ border: 1px solid #333; padding: 2px 4px; }}
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
    let start = Instant::now();
    let tree = renderer.layout_document(&dom, 1440, 1200).unwrap();
    let elapsed = start.elapsed();

    assert!(
      elapsed < Duration::from_secs(12),
      "large spanning table layout took {:?}",
      elapsed
    );

    let mut tops = Vec::new();
    collect_cell_tops(&tree.root, &mut tops);
    for extra in &tree.additional_fragments {
      collect_cell_tops(extra, &mut tops);
    }
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());
    tops.dedup_by(|a, b| (*a - *b).abs() < 0.25);
    assert!(
      tops.len() >= rows,
      "expected at least {rows} distinct row offsets, saw {}",
      tops.len()
    );
  });
}

#[test]
fn gigantic_spanning_table_finishes_quickly_and_stays_ordered() {
  with_large_stack(|| {
    let rows = 200usize;
    let cols = 50usize;
    let mut rows_html = String::with_capacity(rows * cols * 20);
    for r in 0..rows {
      rows_html.push_str("<tr>");
      let mut c = 0usize;
      while c < cols {
        if r % 25 == 0 && r + 4 < rows && c == 0 {
          let _ = write!(
            &mut rows_html,
            r#"<td rowspan="4" colspan="5">block-{r}</td>"#
          );
          c += 5;
          continue;
        }
        if r % 12 == 0 && c + 3 <= cols {
          let _ = write!(&mut rows_html, r#"<td colspan="3">wide-{r}-{c}</td>"#);
          c += 3;
          continue;
        }
        if c % 15 == 0 && r + 2 < rows {
          let _ = write!(&mut rows_html, r#"<td rowspan="2">tall-{r}-{c}</td>"#);
          c += 1;
          continue;
        }
        let _ = write!(&mut rows_html, "<td>{r}-{c}</td>");
        c += 1;
      }
      rows_html.push_str("</tr>");
    }

    let html = format!(
      r#"
      <html>
        <head>
          <style>
            table {{ border-collapse: collapse; width: 1200px; }}
            td {{ border: 1px solid #444; padding: 2px 3px; }}
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
    let start = Instant::now();
    let tree = renderer.layout_document(&dom, 1600, 1400).unwrap();
    let elapsed = start.elapsed();
    assert!(
      elapsed < Duration::from_secs(10),
      "giant spanning table layout took {:?}",
      elapsed
    );

    let mut tops = Vec::new();
    collect_cell_tops(&tree.root, &mut tops);
    for extra in &tree.additional_fragments {
      collect_cell_tops(extra, &mut tops);
    }
    tops.sort_by(|a, b| a.partial_cmp(b).unwrap());
    tops.dedup_by(|a, b| (*a - *b).abs() < 0.25);
    assert!(
      tops.len() >= rows,
      "expected at least {rows} distinct row offsets, saw {}",
      tops.len()
    );
    assert!(
      tops.windows(2).all(|w| w[1] >= w[0]),
      "row offsets were not monotonically increasing"
    );
  });
}
