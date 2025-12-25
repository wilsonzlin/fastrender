use fastrender::api::{FastRender, RenderDiagnostics, RenderOptions};
use fastrender::css::loader::infer_base_url;
use fastrender::resource::{FetchedResource, ResourceFetcher};
use fastrender::style::color::Rgba;
use fastrender::style::media::MediaType;
use fastrender::Pixmap;
use image::codecs::png::PngEncoder;
use image::ColorType;
use image::ImageEncoder;
use image::RgbaImage;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::sync::{Arc, Mutex};

struct RecordingFetcher {
  map: HashMap<String, (Vec<u8>, Option<String>)>,
  requests: Mutex<Vec<String>>,
}

impl RecordingFetcher {
  fn new() -> Self {
    Self {
      map: HashMap::new(),
      requests: Mutex::new(Vec::new()),
    }
  }

  fn with(mut self, url: &str, body: impl Into<Vec<u8>>, content_type: &str) -> Self {
    self.map.insert(
      url.to_string(),
      (body.into(), Some(content_type.to_string())),
    );
    self
  }

  fn requests(&self) -> Vec<String> {
    self.requests.lock().unwrap().clone()
  }
}

impl ResourceFetcher for RecordingFetcher {
  fn fetch(&self, url: &str) -> fastrender::error::Result<FetchedResource> {
    self.requests.lock().unwrap().push(url.to_string());
    if let Some((bytes, content_type)) = self.map.get(url) {
      return Ok(FetchedResource::new(bytes.clone(), content_type.clone()));
    }
    Err(fastrender::error::Error::Io(io::Error::new(
      io::ErrorKind::NotFound,
      format!("missing resource: {url}"),
    )))
  }
}

fn png_bytes(color: Rgba) -> Vec<u8> {
  let alpha = (color.a.clamp(0.0, 1.0) * 255.0).round() as u8;
  let img = RgbaImage::from_pixel(1, 1, image::Rgba([color.r, color.g, color.b, alpha]));
  let mut out = Vec::new();
  PngEncoder::new(&mut out)
    .write_image(img.as_raw(), 1, 1, ColorType::Rgba8.into())
    .expect("encode png");
  out
}

fn sample(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let width = pixmap.width();
  let data = pixmap.data();
  let idx = ((y * width + x) * 4) as usize;
  (data[idx], data[idx + 1], data[idx + 2], data[idx + 3])
}

#[test]
fn nested_imports_resolve_against_base_and_stylesheet_urls() {
  let html = fs::read_to_string("tests/fixtures/html/import_base.html").unwrap();
  let base_hint = "https://example.com/site/index.html";

  let entry_url = "https://example.com/site/static/css/entry.css";
  let colors_url = "https://example.com/site/static/imports/colors.css";
  let theme_url = "https://example.com/site/static/imports/deep/theme.css";
  let entry_image = "https://example.com/site/static/images/ok.png";
  let deep_image = "https://example.com/site/static/imports/images/deep.png";

  let entry_css = r#"
    @import url("../imports/colors.css");
    html, body { margin: 0; padding: 0; }
    #target {
      width: 40px;
      height: 40px;
      margin: 16px;
      background: url("../images/ok.png") no-repeat rgb(0, 0, 0);
    }
  "#;
  let colors_css = r#"
    @import "deep/theme.css";
    #target { color: rgb(200, 0, 0); }
  "#;
  let theme_css = r#"
    body { background: rgb(5, 10, 15); }
    #target { background-image: url("../images/deep.png"); }
  "#;

  let fetcher = RecordingFetcher::new()
    .with(entry_url, entry_css.as_bytes(), "text/css")
    .with(colors_url, colors_css.as_bytes(), "text/css")
    .with(theme_url, theme_css.as_bytes(), "text/css")
    .with(entry_image, png_bytes(Rgba::rgb(10, 0, 0)), "image/png")
    .with(deep_image, png_bytes(Rgba::rgb(20, 0, 0)), "image/png");
  let fetcher: Arc<RecordingFetcher> = Arc::new(fetcher);
  let fetcher_for_renderer: Arc<dyn ResourceFetcher> = fetcher.clone();

  let mut renderer = FastRender::builder()
    .fetcher(fetcher_for_renderer)
    .build()
    .unwrap();

  let base_url = infer_base_url(&html, base_hint).into_owned();
  renderer.set_base_url(base_url.clone());

  let mut diagnostics = RenderDiagnostics::default();
  let html_with_css = renderer.inline_stylesheets_for_document(
    &html,
    &base_url,
    MediaType::Screen,
    None,
    &mut diagnostics,
  );
  assert!(
    diagnostics.fetch_errors.is_empty(),
    "expected stylesheet fetches to succeed: {:?}",
    diagnostics.fetch_errors
  );

  let pixmap = renderer
    .render_html_with_options(
      &html_with_css,
      RenderOptions::default().with_viewport(80, 80),
    )
    .expect("rendered document");

  assert_eq!(sample(&pixmap, 1, 1), (5, 10, 15, 255));

  let requests = fetcher.requests();
  assert!(requests.contains(&entry_url.to_string()));
  assert!(requests.contains(&colors_url.to_string()));
  assert!(requests.contains(&theme_url.to_string()));
  assert!(
    requests.contains(&deep_image.to_string()),
    "url() inside deepest import should resolve relative to that sheet"
  );
  assert!(
    requests.contains(&entry_image.to_string()),
    "background image from entry sheet should resolve against base URL and be fetched"
  );
}
