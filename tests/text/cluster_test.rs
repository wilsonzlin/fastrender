use fastrender::text::cluster::find_atomic_clusters;

#[test]
fn combining_mark_forms_single_cluster() {
  let text = "a\u{0301}";
  let spans = find_atomic_clusters(text);
  assert_eq!(spans, vec![0..text.len()]);
}

#[test]
fn flag_sequence_is_single_cluster() {
  let text = "🇺🇸";
  let spans = find_atomic_clusters(text);
  assert_eq!(spans, vec![0..text.len()]);
}

#[test]
fn zwj_sequence_is_single_cluster() {
  let text = "👨\u{200D}👩";
  let spans = find_atomic_clusters(text);
  assert_eq!(spans, vec![0..text.len()]);
}

#[test]
fn mixed_text_clusters_correctly() {
  let text = "a👨\u{200D}👩b";
  let spans = find_atomic_clusters(text);
  let start_middle = 'a'.len_utf8();
  let end_middle = text.len() - 'b'.len_utf8();
  assert_eq!(
    spans,
    vec![
      0..start_middle,
      start_middle..end_middle,
      end_middle..text.len()
    ]
  );
}
