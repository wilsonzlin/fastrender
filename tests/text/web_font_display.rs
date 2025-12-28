use std::fs;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use fastrender::css::types::{FontDisplay, FontFaceRule, FontFaceSource};
use fastrender::resource::FetchedResource;
use fastrender::text::font_db::{FontDatabase, FontStretch, FontStyle};
use fastrender::text::font_loader::{
  FontContext, FontFetcher, FontLoadStatus, WebFontLoadOptions, WebFontPolicy,
};

fn fixture_font_bytes() -> Option<Vec<u8>> {
  let path = Path::new("tests/fixtures/fonts/DejaVuSans-subset.ttf");
  if !path.exists() {
    return None;
  }
  fs::read(path).ok()
}

fn context_with_fetcher(fetcher: Arc<dyn FontFetcher>) -> Option<(FontContext, String)> {
  let data = fixture_font_bytes()?;
  let mut db = FontDatabase::empty();
  db.load_font_data(data).ok()?;
  let ctx = FontContext::with_database_and_fetcher(Arc::new(db), fetcher);
  let fallback_family = ctx
    .database()
    .first_font()
    .map(|font| font.family.clone())?;
  Some((ctx, fallback_family))
}

#[derive(Clone)]
struct DelayedFetcher {
  data: Vec<u8>,
  delay: Duration,
}

impl FontFetcher for DelayedFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    if self.delay > Duration::ZERO {
      std::thread::sleep(self.delay);
    }
    Ok(FetchedResource::with_final_url(
      self.data.clone(),
      Some("font/ttf".to_string()),
      Some(url.to_string()),
    ))
  }
}

#[derive(Clone)]
struct RecordingFetcher {
  data: Vec<u8>,
  calls: Arc<Mutex<Vec<String>>>,
}

impl RecordingFetcher {
  fn new(data: Vec<u8>) -> Self {
    Self {
      data,
      calls: Arc::new(Mutex::new(Vec::new())),
    }
  }

  fn calls(&self) -> Vec<String> {
    self.calls.lock().map(|c| c.clone()).unwrap_or_default()
  }
}

impl FontFetcher for RecordingFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    if let Ok(mut calls) = self.calls.lock() {
      calls.push(url.to_string());
    }
    Ok(FetchedResource::with_final_url(
      self.data.clone(),
      Some("font/ttf".to_string()),
      Some(url.to_string()),
    ))
  }
}

#[test]
fn font_display_block_waits_for_policy_timeout() {
  let data = match fixture_font_bytes() {
    Some(bytes) => bytes,
    None => return,
  };
  let fetcher = Arc::new(DelayedFetcher {
    data: data.clone(),
    delay: Duration::from_millis(200),
  });
  let (ctx, fallback_family) = match context_with_fetcher(fetcher) {
    Some(pair) => pair,
    None => return,
  };

  let face = FontFaceRule {
    family: Some("SlowBlock".to_string()),
    sources: vec![FontFaceSource::url(
      "http://example.com/slow.ttf".to_string(),
    )],
    display: FontDisplay::Block,
    ..Default::default()
  };

  let options = WebFontLoadOptions {
    policy: WebFontPolicy::BlockUntilLoaded {
      timeout: Duration::from_millis(150),
    },
  };
  let start = Instant::now();
  let report = ctx
    .load_web_fonts_with_options(&[face], None, None, options)
    .expect("apply blocking policy");
  let elapsed = start.elapsed();

  assert!(
    elapsed >= Duration::from_millis(120),
    "blocking policy should wait through the configured window (elapsed {:?})",
    elapsed
  );
  assert!(
    elapsed < Duration::from_millis(400),
    "blocking should stop once the timeout elapses (elapsed {:?})",
    elapsed
  );

  let resolved = ctx
    .get_font_full(
      &["SlowBlock".to_string(), fallback_family],
      400,
      FontStyle::Normal,
      FontStretch::Normal,
    )
    .expect("resolve fallback font");
  assert_ne!(resolved.family, "SlowBlock");
  assert!(
    report
      .events
      .iter()
      .any(|e| matches!(e.status, FontLoadStatus::Skipped { .. })),
    "load report should capture the skipped slow font"
  );
}

#[test]
fn font_display_swap_returns_immediately() {
  let data = match fixture_font_bytes() {
    Some(bytes) => bytes,
    None => return,
  };
  let fetcher = Arc::new(DelayedFetcher {
    data: data.clone(),
    delay: Duration::from_millis(250),
  });
  let (ctx, fallback_family) = match context_with_fetcher(fetcher) {
    Some(pair) => pair,
    None => return,
  };

  let face = FontFaceRule {
    family: Some("SwapFace".to_string()),
    sources: vec![FontFaceSource::url(
      "http://example.com/swap.ttf".to_string(),
    )],
    display: FontDisplay::Swap,
    ..Default::default()
  };

  let start = Instant::now();
  ctx
    .load_web_fonts_with_options(
      &[face],
      None,
      None,
      WebFontLoadOptions {
        policy: WebFontPolicy::BlockUntilLoaded {
          timeout: Duration::from_millis(500),
        },
      },
    )
    .expect("load swap face");
  let elapsed = start.elapsed();
  assert!(
    elapsed < Duration::from_millis(200),
    "swap display should not block layout (elapsed {:?})",
    elapsed
  );

  let resolved = ctx
    .get_font_full(
      &["SwapFace".to_string(), fallback_family],
      400,
      FontStyle::Normal,
      FontStretch::Normal,
    )
    .expect("resolve font with fallback");
  assert_ne!(resolved.family, "SwapFace");
}

#[test]
fn unicode_range_filters_fetches() {
  let data = match fixture_font_bytes() {
    Some(bytes) => bytes,
    None => return,
  };
  let fetcher = RecordingFetcher::new(data.clone());
  let fetcher_arc: Arc<dyn FontFetcher> = Arc::new(fetcher.clone());
  let (ctx, _fallback_family) = match context_with_fetcher(fetcher_arc) {
    Some(pair) => pair,
    None => return,
  };

  let matching = FontFaceRule {
    family: Some("Match".to_string()),
    sources: vec![FontFaceSource::url(
      "https://example.com/match.woff2".to_string(),
    )],
    unicode_ranges: vec![(0x0041, 0x005a)],
    ..Default::default()
  };
  let skipped = FontFaceRule {
    family: Some("Skip".to_string()),
    sources: vec![FontFaceSource::url(
      "https://example.com/skip.woff2".to_string(),
    )],
    unicode_ranges: vec![(0x4e00, 0x4e10)],
    ..Default::default()
  };

  let used = vec![0x0041u32];
  let report = ctx
    .load_web_fonts_with_options(
      &[matching, skipped],
      None,
      Some(&used),
      WebFontLoadOptions::default(),
    )
    .expect("filter web fonts by unicode-range");

  let calls = fetcher.calls();
  assert_eq!(calls.len(), 1);
  assert_eq!(calls[0], "https://example.com/match.woff2");
  assert!(
    report
      .events
      .iter()
      .any(|e| e.family == "Skip" && matches!(e.status, FontLoadStatus::Skipped { .. })),
    "skipped face should be recorded as filtered"
  );
}
