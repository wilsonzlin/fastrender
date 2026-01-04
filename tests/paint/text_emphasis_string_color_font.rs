use fastrender::paint::display_list::DisplayItem;
use fastrender::text::color_fonts::parse_cpal_palette;
use fastrender::text::font_db::FontConfig;
use fastrender::{FastRender, RenderArtifactRequest, RenderArtifacts, RenderOptions};
use tiny_skia::Pixmap;

fn render_html_with_display_list(
  html: &str,
  width: u32,
  height: u32,
) -> (Pixmap, fastrender::paint::display_list::DisplayList) {
  let font_config = FontConfig::default()
    .with_system_fonts(false)
    .with_bundled_fonts(true);
  let mut renderer = FastRender::builder()
    .font_sources(font_config)
    .build()
    .expect("renderer");
  let options = RenderOptions::new().with_viewport(width, height);
  let mut artifacts = RenderArtifacts::new(RenderArtifactRequest {
    display_list: true,
    ..Default::default()
  });
  let pixmap = renderer
    .render_html_with_options_and_artifacts(html, options, &mut artifacts)
    .expect("render html");
  let display_list = artifacts
    .display_list
    .take()
    .expect("display list captured");
  (pixmap, display_list)
}

fn unpremultiply(channel: u8, alpha: u8) -> u8 {
  if alpha == 0 {
    return 0;
  }
  (((channel as u16) * 255 + (alpha as u16 / 2)) / alpha as u16).min(255) as u8
}

fn has_pixel_matching<F>(pixmap: &Pixmap, predicate: F) -> bool
where
  F: Fn(u8, u8, u8, u8) -> bool,
{
  for px in pixmap.data().chunks_exact(4) {
    let a = px[3];
    if a == 0 {
      continue;
    }
    let r = unpremultiply(px[2], a);
    let g = unpremultiply(px[1], a);
    let b = unpremultiply(px[0], a);
    if predicate(r, g, b, a) {
      return true;
    }
  }
  false
}

fn read_u16_be(data: &[u8], offset: usize) -> Option<u16> {
  let bytes = data.get(offset..offset + 2)?;
  Some(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn read_u32_be(data: &[u8], offset: usize) -> Option<u32> {
  let bytes = data.get(offset..offset + 4)?;
  Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

fn colr_v0_layer_palette_indices(colr_data: &[u8], glyph_id: u16) -> Vec<u16> {
  let version = read_u16_be(colr_data, 0).unwrap_or(1);
  if version != 0 {
    return Vec::new();
  }
  let num_base = read_u16_be(colr_data, 2).unwrap_or(0) as usize;
  let base_offset = read_u32_be(colr_data, 4).unwrap_or(0) as usize;
  let layer_offset = read_u32_be(colr_data, 8).unwrap_or(0) as usize;
  let num_layers = read_u16_be(colr_data, 12).unwrap_or(0) as usize;
  if num_base == 0 || num_layers == 0 {
    return Vec::new();
  }

  let mut first_layer = None;
  let mut layer_count = 0usize;
  for i in 0..num_base {
    let off = base_offset + i * 6;
    let Some(gid) = read_u16_be(colr_data, off) else {
      break;
    };
    if gid == glyph_id {
      first_layer = Some(read_u16_be(colr_data, off + 2).unwrap_or(0) as usize);
      layer_count = read_u16_be(colr_data, off + 4).unwrap_or(0) as usize;
      break;
    }
  }
  let Some(first_layer) = first_layer else {
    return Vec::new();
  };
  if layer_count == 0 || first_layer + layer_count > num_layers {
    return Vec::new();
  }

  let mut indices = Vec::with_capacity(layer_count);
  for i in 0..layer_count {
    let off = layer_offset + (first_layer + i) * 4 + 2;
    let Some(idx) = read_u16_be(colr_data, off) else {
      break;
    };
    indices.push(idx);
  }
  indices
}

fn contains_color_approx(pixmap: &Pixmap, target: (u8, u8, u8), tolerance: u8) -> bool {
  has_pixel_matching(pixmap, |r, g, b, _a| {
    let dr = r.abs_diff(target.0);
    let dg = g.abs_diff(target.1);
    let db = b.abs_diff(target.2);
    dr <= tolerance && dg <= tolerance && db <= tolerance
  })
}

#[test]
fn text_emphasis_string_respects_font_palette_overrides() {
  let font_path =
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");
  let font_bytes = std::fs::read(&font_path).expect("read ColorTestCOLR font");
  let face = ttf_parser::Face::parse(&font_bytes, 0).expect("parse ColorTestCOLR font");
  let glyph_id = face.glyph_index('A').expect("glyph A exists").0;
  let colr_data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"COLR"))
    .expect("COLR table present");
  let palette_indices = colr_v0_layer_palette_indices(colr_data, glyph_id);
  let mut used_entries: Vec<u16> = palette_indices
    .into_iter()
    .filter(|idx| *idx != 0xFFFF)
    .collect();
  used_entries.sort_unstable();
  used_entries.dedup();
  let override_entry = used_entries
    .first()
    .copied()
    .expect("glyph should reference a concrete CPAL palette entry");

  let cpal_data = face
    .raw_face()
    .table(ttf_parser::Tag::from_bytes(b"CPAL"))
    .expect("CPAL table present");
  let palette = parse_cpal_palette(cpal_data, 0).expect("parse palette 0");
  let base_color = palette
    .colors
    .get(override_entry as usize)
    .copied()
    .expect("palette entry exists");
  let base_rgb = (base_color.r, base_color.g, base_color.b);
  let override_rgb = (0_u8, 255_u8, 0_u8);
  assert_ne!(
    base_rgb, override_rgb,
    "fixture palette entry unexpectedly already green"
  );
  let base_rgbs: Vec<(u8, u8, u8)> = used_entries
    .iter()
    .filter_map(|idx| palette.colors.get(*idx as usize).map(|c| (c.r, c.g, c.b)))
    .filter(|rgb| *rgb != override_rgb)
    .collect();
  assert!(
    !base_rgbs.is_empty(),
    "fixture should use at least one non-green base palette color"
  );

  let html = r#"
    <!doctype html>
    <html>
      <head>
        <style>
          @font-face {
            font-family: "ColorTestCOLR";
            src: url("tests/fonts/ColorTestCOLR.ttf") format("truetype");
          }
          @font-face {
            font-family: "NotoSymbols";
            src: url("tests/fixtures/fonts/NotoSansSymbols-subset.ttf") format("truetype");
          }
          @font-palette-values --green {
            font-family: "ColorTestCOLR";
            base-palette: 0;
            override-colors: OVERRIDE_COLORS;
          }
          body {
            margin: 0;
            padding: 120px;
            background: white;
          }
          .sample {
            font-family: "ColorTestCOLR", "NotoSymbols";
            font-palette: --green;
            font-size: 96px;
            line-height: 1;
            /* Render the base glyph as white-on-white so only emphasis marks affect pixels. */
            color: white;
            text-emphasis-style: "A";
            text-emphasis-color: black;
            text-emphasis-position: under;
          }
        </style>
      </head>
      <body>
        <div class="sample">★</div>
      </body>
    </html>
  "#;
  let override_colors_css = used_entries
    .iter()
    .map(|idx| format!("{idx} rgb(0 255 0)"))
    .collect::<Vec<_>>()
    .join(", ");
  let html = html.replace("OVERRIDE_COLORS", &override_colors_css);

  let (pixmap, display_list) = render_html_with_display_list(&html, 320, 320);

  let mut saw_emphasis = false;
  for item in display_list.items() {
    let DisplayItem::Text(text) = item else {
      continue;
    };
    let Some(emphasis) = &text.emphasis else {
      continue;
    };
    let Some(emphasis_text) = &emphasis.text else {
      continue;
    };
    saw_emphasis = true;
    assert!(
      emphasis_text
        .runs
        .iter()
        .any(|run| {
          used_entries.iter().all(|idx| {
            run
              .palette_overrides
              .iter()
              .any(|(entry, c)| *entry == *idx && c.g > 200 && c.r < 80 && c.b < 80)
          })
        }),
      "expected shaped emphasis text runs to carry palette override colors"
    );
  }
  assert!(saw_emphasis, "fixture did not generate emphasis marks for inspection");

  assert!(
    contains_color_approx(&pixmap, override_rgb, 30),
    "expected recolored (override) green pixels in emphasis mark"
  );
  for base_rgb in base_rgbs {
    assert!(
      !contains_color_approx(&pixmap, base_rgb, 30),
      "expected base palette color {:?} to be absent in emphasis mark",
      base_rgb
    );
  }
}

#[test]
fn text_emphasis_string_handles_font_fallback_runs() {
  let html = r#"
    <!doctype html>
    <html>
      <head>
        <style>
          @font-face {
            font-family: "ColorTestCOLR";
            src: url("tests/fonts/ColorTestCOLR.ttf") format("truetype");
          }
          @font-face {
            font-family: "NotoSymbols";
            src: url("tests/fixtures/fonts/NotoSansSymbols-subset.ttf") format("truetype");
          }
          body {
            margin: 0;
            padding: 120px;
            background: white;
          }
          .sample {
            font-family: "ColorTestCOLR", "NotoSymbols";
            font-size: 96px;
            line-height: 1;
            color: white;
            text-emphasis-style: "A★";
            text-emphasis-color: black;
            text-emphasis-position: under;
          }
        </style>
      </head>
      <body>
        <div class="sample">★</div>
      </body>
    </html>
  "#;

  let (pixmap, display_list) = render_html_with_display_list(html, 320, 320);

  let mut saw_multi_run = false;
  for item in display_list.items() {
    let DisplayItem::Text(text) = item else {
      continue;
    };
    let Some(emphasis) = &text.emphasis else {
      continue;
    };
    let Some(emphasis_text) = &emphasis.text else {
      continue;
    };
    if emphasis_text.runs.len() > 1 {
      saw_multi_run = true;
      break;
    }
  }
  assert!(
    saw_multi_run,
    "expected emphasis string shaping to produce multiple runs (font fallback)"
  );

  let has_black = has_pixel_matching(&pixmap, |r, g, b, _a| r < 25 && g < 25 && b < 25);
  let has_colored = has_pixel_matching(&pixmap, |r, g, b, _a| {
    // Identify non-grayscale pixels (color glyph output) without assuming a specific palette.
    r != g || g != b
  });

  assert!(
    has_black,
    "expected monochrome fallback glyph in emphasis string to render with emphasis color"
  );
  assert!(
    has_colored,
    "expected COLR glyph in emphasis string to render alongside fallback glyph"
  );
}
