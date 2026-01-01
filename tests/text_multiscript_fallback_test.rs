use fastrender::text::pipeline::ShapedRun;
use fastrender::{ComputedStyle, FontConfig, FontContext, FontDatabase, ShapingPipeline};

const MULTISCRIPT_SAMPLE: &str = "Hello العربية עברית हिन्दी বাংলা ไทย 中文 日本語 かな カナ 한국어";

/// Representative characters for the scripts we expect bundled fonts to cover.
///
/// These are used by both the fontdb-level and shaping-level tests. They should stay in sync with
/// `MULTISCRIPT_SAMPLE`.
const REPRESENTATIVE_CHARS: &[(&str, char)] = &[
  ("Latin", 'H'),
  ("Arabic", 'ع'),
  ("Hebrew", 'ע'),
  ("Devanagari", 'ह'),
  ("Bengali", 'ব'),
  ("Thai", 'ท'),
  ("Han", '中'),
  ("Hiragana", 'か'),
  ("Katakana", 'カ'),
  ("Hangul", '한'),
];

fn is_bidi_control_char(ch: char) -> bool {
  matches!(
    ch,
    '\u{061C}' // ARABIC LETTER MARK
      | '\u{200E}' // LEFT-TO-RIGHT MARK
      | '\u{200F}' // RIGHT-TO-LEFT MARK
      | '\u{202A}'..='\u{202E}' // embedding/override marks
      | '\u{2066}'..='\u{2069}' // isolate marks
  )
}

fn is_ignorable_cluster_char(ch: char) -> bool {
  ch.is_whitespace() || ch.is_control() || is_bidi_control_char(ch)
}

fn cluster_start_for_char(run: &ShapedRun, local_byte_index: usize) -> Option<u32> {
  if run.glyphs.is_empty() {
    return None;
  }

  // HarfBuzz cluster values refer to byte indices in the run text. For RTL runs, glyphs are in
  // visual order, so clusters can appear in descending order; sort so we can binary-search by
  // position.
  let mut clusters: Vec<u32> = run.glyphs.iter().map(|glyph| glyph.cluster).collect();
  clusters.sort_unstable();
  clusters.dedup();

  clusters
    .iter()
    .copied()
    .filter(|cluster| (*cluster as usize) <= local_byte_index)
    .last()
    .or_else(|| clusters.first().copied())
}

fn cluster_char(run: &ShapedRun, cluster: u32) -> Option<char> {
  run
    .text
    .get(cluster as usize..)
    .and_then(|s| s.chars().next())
}

fn assert_cluster_at_byte_index_has_nonzero_glyph(runs: &[ShapedRun], byte_index: usize, ch: char) {
  let run = runs
    .iter()
    .find(|run| byte_index >= run.start && byte_index < run.end)
    .unwrap_or_else(|| {
      panic!(
        "no shaped run covered character {ch:?} at byte {byte_index}; shaped {} runs",
        runs.len()
      )
    });

  let local_index = byte_index - run.start;
  let cluster = cluster_start_for_char(run, local_index).unwrap_or_else(|| {
    panic!(
      "run covering character {ch:?} produced no glyph clusters (run text {:?})",
      run.text
    )
  });

  let cluster_glyphs: Vec<_> = run
    .glyphs
    .iter()
    .filter(|glyph| glyph.cluster == cluster)
    .collect();

  assert!(
    !cluster_glyphs.is_empty(),
    "no glyphs were emitted for cluster {cluster} (character {ch:?}, run text {:?})",
    run.text
  );

  let cluster_ch = cluster_char(run, cluster).unwrap_or(ch);
  let notdef_glyphs: Vec<_> = cluster_glyphs
    .iter()
    .filter(|glyph| glyph.glyph_id == 0)
    .collect();

  if !notdef_glyphs.is_empty() && !is_ignorable_cluster_char(cluster_ch) {
    panic!(
      "bundled shaping emitted .notdef glyphs for character {ch:?} (cluster char {cluster_ch:?} U+{:04X}, cluster {cluster}) using font family {:?}; glyph ids in cluster: {:?}; run text: {:?}",
      cluster_ch as u32,
      run.font.family,
      cluster_glyphs.iter().map(|g| g.glyph_id).collect::<Vec<_>>(),
      run.text
    );
  }
}

#[test]
fn bundled_multiscript_string_has_no_notdef_glyphs() {
  let pipeline = ShapingPipeline::new();
  let font_ctx = FontContext::with_config(FontConfig::bundled_only());

  let mut style = ComputedStyle::default();
  style.font_family = vec!["sans-serif".to_string()].into();
  style.font_size = 32.0;

  let runs = pipeline
    .shape(MULTISCRIPT_SAMPLE, &style, &font_ctx)
    .expect("shaping multiscript sample with bundled fonts");
  assert!(
    !runs.is_empty(),
    "multiscript sample should produce shaped runs"
  );

  for (byte_index, ch) in MULTISCRIPT_SAMPLE.char_indices() {
    if is_ignorable_cluster_char(ch) {
      continue;
    }
    assert_cluster_at_byte_index_has_nonzero_glyph(&runs, byte_index, ch);
  }
}

#[test]
fn bundled_font_database_has_representative_glyphs() {
  let db = FontDatabase::with_config(&FontConfig::bundled_only());
  assert!(
    !db.is_empty(),
    "bundled_only font database should not be empty"
  );

  for (script, ch) in REPRESENTATIVE_CHARS {
    let has_glyph = db.faces().any(|face| db.has_glyph_cached(face.id, *ch));
    assert!(
      has_glyph,
      "bundled font database is missing a glyph for {script} character {ch:?} (U+{:04X})",
      *ch as u32
    );
  }
}
