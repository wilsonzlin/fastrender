use fastrender::api::{FastRender, FastRenderConfig, RenderOptions};
use fastrender::debug::runtime::RuntimeToggles;
use fastrender::error::{Error, Result};
use fastrender::resource::{FetchDestination, FetchRequest, FetchedResource, ResourceFetcher};
use std::collections::HashMap;
use std::io;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, PartialEq, Eq)]
struct RecordedRequest {
  url: String,
  destination: FetchDestination,
  referrer: Option<String>,
}

#[derive(Default)]
struct RecordingRequestFetcher {
  responses: HashMap<String, (Vec<u8>, Option<String>)>,
  requests: Arc<Mutex<Vec<RecordedRequest>>>,
}

impl RecordingRequestFetcher {
  fn with_entry(mut self, url: &str, body: &str, content_type: &str) -> Self {
    self.responses.insert(
      url.to_string(),
      (body.as_bytes().to_vec(), Some(content_type.to_string())),
    );
    self
  }

  fn requests(&self) -> Vec<RecordedRequest> {
    self
      .requests
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner())
      .clone()
  }
}

impl ResourceFetcher for RecordingRequestFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    let Some((bytes, content_type)) = self.responses.get(url) else {
      return Err(Error::Io(io::Error::new(
        io::ErrorKind::NotFound,
        format!("missing resource: {url}"),
      )));
    };
    Ok(FetchedResource::new(bytes.clone(), content_type.clone()))
  }

  fn fetch_with_request(&self, req: FetchRequest<'_>) -> Result<FetchedResource> {
    self
      .requests
      .lock()
      .unwrap_or_else(|poisoned| poisoned.into_inner())
      .push(RecordedRequest {
        url: req.url.to_string(),
        destination: req.destination,
        referrer: req.referrer.map(|r| r.to_string()),
      });
    self.fetch(req.url)
  }
}

#[test]
fn file_document_base_like_text_in_script_does_not_override_inferred_origin() {
  let html = r#"<!doctype html><html><head>
    <script>var s='<base href="https://bad.example/">'</script>
    <link rel="stylesheet" href="style.css">
  </head><body></body></html>"#;

  let document_url = "file:///tmp/cache/good.example.html";
  let stylesheet_url = "https://good.example/style.css";

  let fetcher = Arc::new(
    RecordingRequestFetcher::default().with_entry(
      stylesheet_url,
      "body { color: rgb(1, 2, 3); }",
      "text/css",
    ),
  );
  let toggles = RuntimeToggles::from_map(HashMap::from([(
    "FASTR_FETCH_LINK_CSS".to_string(),
    "1".to_string(),
  )]));
  let config = FastRenderConfig::default().with_runtime_toggles(toggles);
  let mut renderer =
    FastRender::with_config_and_fetcher(config, Some(fetcher.clone() as Arc<dyn ResourceFetcher>))
      .unwrap();

  renderer
    .render_html_with_stylesheets(
      html,
      document_url,
      RenderOptions::new().with_viewport(64, 64),
    )
    .unwrap();

  let requests = fetcher.requests();
  let stylesheet_request = requests
    .iter()
    .find(|request| request.destination == FetchDestination::Style)
    .expect("stylesheet request");
  assert_eq!(stylesheet_request.url, stylesheet_url);
  assert_eq!(stylesheet_request.referrer.as_deref(), Some(document_url));
}

#[test]
fn file_document_relative_base_href_resolves_against_inferred_origin() {
  let html = r#"<!doctype html><html><head>
    <base href="assets/">
    <link rel="stylesheet" href="style.css">
  </head><body></body></html>"#;

  let document_url = "file:///tmp/cache/good.example.html";
  let stylesheet_url = "https://good.example/assets/style.css";

  let fetcher = Arc::new(
    RecordingRequestFetcher::default().with_entry(
      stylesheet_url,
      "body { color: rgb(1, 2, 3); }",
      "text/css",
    ),
  );
  let toggles = RuntimeToggles::from_map(HashMap::from([(
    "FASTR_FETCH_LINK_CSS".to_string(),
    "1".to_string(),
  )]));
  let config = FastRenderConfig::default().with_runtime_toggles(toggles);
  let mut renderer =
    FastRender::with_config_and_fetcher(config, Some(fetcher.clone() as Arc<dyn ResourceFetcher>))
      .unwrap();

  renderer
    .render_html_with_stylesheets(
      html,
      document_url,
      RenderOptions::new().with_viewport(64, 64),
    )
    .unwrap();

  let requests = fetcher.requests();
  let stylesheet_request = requests
    .iter()
    .find(|request| request.destination == FetchDestination::Style)
    .expect("stylesheet request");
  assert_eq!(stylesheet_request.url, stylesheet_url);
  assert_eq!(stylesheet_request.referrer.as_deref(), Some(document_url));
}

