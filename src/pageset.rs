//! Shared pageset metadata and helpers for canonical stems.
//!
//! The canonical stem for a page matches [`crate::resource::normalize_page_name`]: strip the
//! scheme, drop a leading `www.`, lowercase the host, and sanitize for filesystem usage. All
//! pageset tools should use this module to keep cache/progress filenames and `--pages` filters
//! consistent.

use crate::resource::normalize_page_name;
use std::collections::{BTreeMap, HashSet};
use std::path::PathBuf;

/// Cached HTML directory used by `fetch_pages` and consumers.
pub const CACHE_HTML_DIR: &str = "fetches/html";

/// Target pages for testing and regressions.
pub const PAGESET_URLS: &[&str] = &[
  // Tier 1: Simple
  "https://example.com",
  "https://example.org",
  "https://example.net",
  // Tier 2: Text-heavy
  "https://news.ycombinator.com",
  "https://lobste.rs",
  "https://lite.cnn.com",
  "https://text.npr.org",
  "https://iana.org",
  // Tier 3: Modern
  "https://google.com",
  "https://duckduckgo.com",
  "https://wikipedia.org",
  "https://www.w3.org",
  // Tier 4: Complex
  "https://github.com",
  "https://gitlab.com",
  "https://stackoverflow.com",
  "https://reddit.com",
  "https://twitter.com",
  "https://weibo.cn",
  "https://alibaba.com",
  "https://arxiv.org",
  "https://amazon.com",
  "https://youtube.com",
  "https://facebook.com",
  "https://linkedin.com",
  "https://microsoft.com",
  "https://apple.com",
  "https://developer.apple.com",
  "https://openai.com",
  "https://icloud.com",
  "https://nytimes.com",
  "http://neverssl.com",
  "https://htmldog.com",
  "https://ietf.org",
  "https://bbc.com",
  "https://nhk.or.jp",
  "https://si.edu",
  "https://cdc.gov",
  "https://britannica.com",
  "https://zillow.com",
  "https://apache.org",
  "https://python.org",
  "https://cnn.com",
  "https://fast.com",
  "https://theguardian.com",
  "https://nyu.edu",
  "https://openbsd.org",
  "https://debian.org",
  "https://gentoo.org",
  "https://archlinux.org",
  "https://manjaro.org",
  "https://sina.com.cn",
  "https://espn.com",
  "https://imdb.com",
  "https://craigslist.org",
  "https://pinterest.com",
  "https://medium.com",
  "https://quora.com",
  "https://twitch.tv",
  "https://dropbox.com",
  "https://gitlab.io",
  "https://stripe.com",
  "https://shopify.com",
  "https://weebly.com",
  "https://etsy.com",
  "https://ebay.com",
  "https://walmart.com",
  "https://usatoday.com",
  "https://newsweek.com",
  "https://techmeme.com",
  "https://fortune.com",
  "https://cbsnews.com",
  "https://vox.com",
  "https://msnbc.com",
  "https://buzzfeed.com",
  "https://msn.com",
  "https://huffpost.com",
  "https://airbnb.com",
  "https://booking.com",
  "https://yelp.com",
  "https://spotify.com",
  "https://reuters.com",
  "https://netflix.com",
  "https://wsj.com",
  "https://bloomberg.com",
  "https://time.com",
  "https://forbes.com",
  "https://foxnews.com",
  "https://techcrunch.com",
  "https://theverge.com",
  "https://wired.com",
  "https://arstechnica.com",
  "https://engadget.com",
  "https://nbcnews.com",
  "https://dailymail.co.uk",
  "https://figma.com",
  "https://ft.com",
  "https://phoronix.com",
  "https://nationalgeographic.com",
  "https://cnet.com",
  "https://developer.mozilla.org",
  "https://developer.mozilla.org/en-US/docs/Web/CSS/writing-mode",
  "https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientation",
  "https://developer.mozilla.org/en-US/docs/Web/CSS/text-combine-upright",
  "https://developer.mozilla.org/en-US/docs/Web/CSS/transform",
  "https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_multicol_layout/Using_multi-column_layouts",
  "https://developer.mozilla.org/en-US/docs/Learn/Forms/Your_first_form",
  "https://howtogeek.com",
  "https://macrumors.com",
  "https://washingtonpost.com",
  "https://abcnews.go.com",
  "https://washington.edu",
  "https://berkeley.edu",
  "https://xkcd.com",
  "https://fandom.com",
  "https://ikea.com",
  "https://elpais.com",
  "https://ndtv.com",
  "https://yahoo.com",
  "https://nasa.gov",
  "https://stackexchange.com",
  "https://rust-lang.org",
  "https://blog.rust-lang.org",
  "https://rfc-editor.org",
  "https://tesco.com",
  "https://bing.com",
  "https://discord.com",
  "https://weather.com",
  "https://bbc.co.uk",
  "https://npmjs.com",
  "https://latimes.com",
  "https://cloudflare.com",
  "https://aliexpress.com",
  "https://apnews.com",
  "https://aljazeera.com",
  "https://tripadvisor.com",
  "https://vogue.com",
  "https://theatlantic.com",
  "https://economist.com",
  "https://newyorker.com",
  "https://hbr.org",
  "https://sqlite.org",
  "https://nginx.org",
  "https://go.dev",
  "https://docs.rs",
  "https://doc.rust-lang.org",
  "https://www.openstreetmap.org",
  "https://docs.python.org",
  "https://kotlinlang.org",
  "https://slashdot.org",
];

fn strip_collision_suffix(raw: &str) -> &str {
  if let Some((candidate, suffix)) = raw.rsplit_once("--") {
    if suffix.len() == 8 && suffix.chars().all(|c| c.is_ascii_hexdigit()) {
      return candidate;
    }
  }
  raw
}

fn parse_collision_suffix(raw: &str) -> Option<(&str, &str)> {
  raw
    .rsplit_once("--")
    .filter(|(_, suffix)| suffix.len() == 8 && suffix.chars().all(|c| c.is_ascii_hexdigit()))
}

/// Canonical pageset stem for filtering and reporting.
pub fn pageset_stem(url_or_stem: &str) -> Option<String> {
  let trimmed = strip_collision_suffix(url_or_stem.trim());
  normalize_page_name(trimmed)
}

/// Deterministic short hash used to disambiguate cache/progress stems for collisions.
pub fn pageset_short_hash(input: &str) -> String {
  let mut hash: u32 = 0x811c9dc5;
  for byte in input.as_bytes() {
    hash ^= u32::from(*byte);
    hash = hash.wrapping_mul(0x01000193);
  }
  format!("{hash:08x}")
}

/// Path to the cached HTML for a given cache stem.
pub fn cache_html_path(stem: &str) -> PathBuf {
  PathBuf::from(CACHE_HTML_DIR).join(format!("{stem}.html"))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PagesetEntry {
  pub url: String,
  pub stem: String,
  pub cache_stem: String,
}

fn pageset_urls() -> Vec<String> {
  if let Ok(raw) = std::env::var("FASTR_PAGESET_URLS") {
    let urls: Vec<String> = raw
      .split(',')
      .map(str::trim)
      .filter(|u| !u.is_empty())
      .map(str::to_string)
      .collect();
    if !urls.is_empty() {
      return urls;
    }
  }
  PAGESET_URLS.iter().map(|u| u.to_string()).collect()
}

fn build_pageset_entries(urls: &[String]) -> (Vec<PagesetEntry>, Vec<(String, Vec<String>)>) {
  let mut normalized = Vec::new();
  for url in urls {
    let Some(stem) = pageset_stem(url) else {
      continue;
    };
    normalized.push((url.clone(), stem));
  }

  let mut stem_to_urls: BTreeMap<String, Vec<String>> = BTreeMap::new();
  for (url, stem) in &normalized {
    stem_to_urls
      .entry(stem.clone())
      .or_default()
      .push(url.clone());
  }

  let collisions: Vec<_> = stem_to_urls
    .iter()
    .filter(|(_, urls)| urls.len() > 1)
    .map(|(stem, urls)| (stem.clone(), urls.clone()))
    .collect();

  let mut deduped: BTreeMap<String, PagesetEntry> = BTreeMap::new();
  for (url, stem) in normalized {
    let cache_stem = match stem_to_urls.get(&stem) {
      Some(urls) if urls.len() > 1 => format!("{stem}--{}", pageset_short_hash(&url)),
      _ => stem.clone(),
    };
    deduped.insert(
      cache_stem.clone(),
      PagesetEntry {
        url,
        stem,
        cache_stem,
      },
    );
  }

  (deduped.into_values().collect(), collisions)
}

/// Canonical, collision-aware pageset entries sorted by cache stem.
pub fn pageset_entries_with_collisions() -> (Vec<PagesetEntry>, Vec<(String, Vec<String>)>) {
  build_pageset_entries(&pageset_urls())
}

/// Canonical, collision-aware pageset entries sorted by cache stem.
pub fn pageset_entries() -> Vec<PagesetEntry> {
  pageset_entries_with_collisions().0
}

#[derive(Debug, Clone, Default)]
pub struct PagesetFilter {
  stems: HashSet<String>,
  cache_stems: HashSet<String>,
}

impl PagesetFilter {
  pub fn from_inputs(inputs: &[String]) -> Option<Self> {
    let mut filter = PagesetFilter::default();
    for input in inputs {
      filter.add(input);
    }
    if filter.stems.is_empty() && filter.cache_stems.is_empty() {
      None
    } else {
      Some(filter)
    }
  }

  fn add(&mut self, raw: &str) {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
      return;
    }

    if let Some((base, suffix)) = parse_collision_suffix(trimmed) {
      if let Some(stem) = pageset_stem(base) {
        self
          .cache_stems
          .insert(format!("{stem}--{}", suffix.to_ascii_lowercase()));
        return;
      }
    }

    if let Some(stem) = pageset_stem(trimmed) {
      self.stems.insert(stem);
    }
  }

  pub fn matches_entry(&self, entry: &PagesetEntry) -> bool {
    self.matches_cache_stem(&entry.cache_stem, Some(&entry.stem))
  }

  pub fn matches_cache_stem(&self, cache_stem: &str, canonical: Option<&str>) -> bool {
    if self.cache_stems.contains(cache_stem) {
      return true;
    }
    if let Some(stem) = canonical {
      if self.stems.contains(stem) {
        return true;
      }
    } else if let Some(stem) = pageset_stem(cache_stem) {
      if self.stems.contains(&stem) {
        return true;
      }
    }
    false
  }

  pub fn unmatched(&self, selected: &[PagesetEntry]) -> Vec<String> {
    let mut remaining_cache = self.cache_stems.clone();
    let mut remaining_stems = self.stems.clone();
    for entry in selected {
      remaining_cache.remove(&entry.cache_stem);
      remaining_stems.remove(&entry.stem);
    }
    let mut missing: Vec<String> = remaining_cache.into_iter().collect();
    missing.extend(remaining_stems.into_iter());
    missing.sort();
    missing
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn pageset_stem_normalizes_www_and_scheme() {
    assert_eq!(
      pageset_stem("https://www.example.com").as_deref(),
      Some("example.com")
    );
    assert_eq!(pageset_stem("example.com").as_deref(), Some("example.com"));
  }

  #[test]
  fn pageset_stem_strips_trailing_punctuation() {
    assert_eq!(
      pageset_stem(" https://Example.com./path/ ").as_deref(),
      Some("example.com_path")
    );
  }

  #[test]
  fn pageset_stem_drops_collision_suffix() {
    let base = pageset_stem("https://rust-lang.org").unwrap();
    let hashed = format!("{base}--deadbeef");
    assert_eq!(pageset_stem(&hashed).as_deref(), Some(base.as_str()));
  }

  #[test]
  fn collision_cache_stems_are_unique() {
    let urls = vec!["https://example.com", "http://example.com/"]
      .into_iter()
      .map(str::to_string)
      .collect::<Vec<_>>();
    let (entries, collisions) = build_pageset_entries(&urls);
    assert_eq!(collisions.len(), 1);
    assert_eq!(collisions[0].0, "example.com");
    assert_eq!(entries.len(), 2);
    assert_ne!(entries[0].cache_stem, entries[1].cache_stem);
    assert_eq!(entries[0].stem, "example.com");
    assert_eq!(entries[1].stem, "example.com");
  }

  #[test]
  fn cache_html_path_uses_cache_dir() {
    let path = cache_html_path("example.com");
    assert!(path.ends_with("fetches/html/example.com.html"));
  }
}
