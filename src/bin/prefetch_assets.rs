//! Prefetch CSS (and optional HTML/CSS subresources) into the disk-backed cache.
//!
//! This is a best-effort helper intended for the pageset workflow:
//! `fetch_pages` caches HTML, then this tool warms `fetches/assets/` so the
//! render step doesn't spend its 5s budget waiting on first-run network fetches.

mod common;

#[cfg(not(feature = "disk_cache"))]
fn main() {
  eprintln!(
    "prefetch_assets requires the `disk_cache` feature. Re-run with `--features disk_cache`."
  );
  std::process::exit(2);
}

#[cfg(feature = "disk_cache")]
mod disk_cache_main {
  use clap::{ArgAction, Parser};
  use fastrender::css::encoding::decode_css_bytes;
  use fastrender::css::loader::{
    absolutize_css_urls_cow, extract_css_links, extract_embedded_css_urls,
    link_rel_is_stylesheet_candidate, resolve_href, resolve_href_with_base,
  };
  use fastrender::css::parser::{extract_scoped_css_sources, parse_stylesheet, StylesheetSource};
  use fastrender::css::types::CssImportLoader;
  use fastrender::css::types::{FontFaceSource, StyleSheet};
  use fastrender::debug::runtime;
  use fastrender::dom::parse_html;
  use fastrender::html::asset_discovery::discover_html_asset_urls;
  use fastrender::pageset::{cache_html_path, pageset_entries, PagesetEntry, PagesetFilter};
  use fastrender::resource::{
    CachingFetcherConfig, DiskCachingFetcher, FetchedResource, ResourceFetcher,
    DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
  };
  use fastrender::style::media::{MediaContext, MediaQuery, MediaQueryCache};
  use rayon::prelude::*;
  use rayon::ThreadPoolBuilder;
  use std::cell::RefCell;
  use std::collections::{BTreeSet, HashMap, HashSet};
  use std::path::{Path, PathBuf};
  use std::sync::Arc;
  use std::time::Duration;

  use crate::common::args::{parse_shard, DiskCacheArgs, TimeoutArgs, ViewportArgs};
  use crate::common::asset_discovery::{discover_css_urls, extract_inline_css_chunks};
  use crate::common::disk_cache_stats::scan_disk_cache_dir;
  use crate::common::render_pipeline::{
    build_http_fetcher, decode_html_resource, disk_cache_namespace, read_cached_document,
  };

  const DEFAULT_ASSET_DIR: &str = "fetches/assets";

  #[derive(Parser, Debug)]
  #[command(name = "prefetch_assets", version, about)]
  struct Args {
    /// Number of parallel pages to prefetch
    #[arg(long, short, default_value_t = num_cpus::get())]
    jobs: usize,

    #[command(flatten)]
    timeout: TimeoutArgs,

    /// Override the User-Agent header
    #[arg(long, default_value = DEFAULT_USER_AGENT)]
    user_agent: String,

    /// Override the Accept-Language header
    #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
    accept_language: String,

    #[command(flatten)]
    viewport: ViewportArgs,

    #[command(flatten)]
    disk_cache: DiskCacheArgs,

    /// Prefetch font URLs referenced by fetched CSS (true/false)
    #[arg(
      long,
      default_value_t = true,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_fonts: bool,

    /// Prefetch image-like URLs referenced directly from HTML (true/false)
    #[arg(
      long,
      default_value_t = false,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_images: bool,

    /// Prefetch iframe/object/embed documents referenced directly from HTML (true/false)
    ///
    /// This also best-effort warms the discovered document's linked stylesheets (and their
    /// `@import` chains/fonts), plus HTML images when `--prefetch-images` is enabled.
    ///
    /// This can explode on pages that embed many third-party iframes, so it defaults to off.
    #[arg(
      long,
      alias = "prefetch-documents",
      default_value_t = false,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_iframes: bool,

    /// Prefetch non-CSS assets referenced via `url(...)` in CSS (true/false)
    #[arg(
      long,
      default_value_t = false,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_css_url_assets: bool,

    /// Safety valve: cap the number of unique discovered assets per page (0 disables the cap)
    #[arg(long, default_value_t = 2000)]
    max_discovered_assets_per_page: usize,

    /// Override disk cache directory (defaults to fetches/assets)
    #[arg(long, default_value = DEFAULT_ASSET_DIR)]
    cache_dir: PathBuf,

    /// Prefetch only listed pages (comma-separated URLs or stems)
    #[arg(long, value_delimiter = ',')]
    pages: Option<Vec<String>>,

    /// Process only a deterministic shard of the page set (index/total, 0-based)
    #[arg(long, value_parser = parse_shard)]
    shard: Option<(usize, usize)>,
  }

  #[derive(Debug, Default, Clone)]
  struct PageSummary {
    stem: String,
    discovered_css: usize,
    fetched_css: usize,
    failed_css: usize,
    fetched_imports: usize,
    failed_imports: usize,
    fetched_fonts: usize,
    failed_fonts: usize,
    discovered_images: usize,
    fetched_images: usize,
    failed_images: usize,
    discovered_documents: usize,
    fetched_documents: usize,
    failed_documents: usize,
    discovered_css_assets: usize,
    fetched_css_assets: usize,
    failed_css_assets: usize,
    skipped: bool,
  }

  #[derive(Debug, Clone, Copy)]
  struct PrefetchOptions {
    prefetch_fonts: bool,
    prefetch_images: bool,
    prefetch_iframes: bool,
    prefetch_css_url_assets: bool,
    max_discovered_assets_per_page: usize,
  }

  fn merge_page_summary(into: &mut PageSummary, other: PageSummary) {
    into.discovered_css += other.discovered_css;
    into.fetched_css += other.fetched_css;
    into.failed_css += other.failed_css;
    into.fetched_imports += other.fetched_imports;
    into.failed_imports += other.failed_imports;
    into.fetched_fonts += other.fetched_fonts;
    into.failed_fonts += other.failed_fonts;
    into.discovered_images += other.discovered_images;
    into.fetched_images += other.fetched_images;
    into.failed_images += other.failed_images;
    into.discovered_documents += other.discovered_documents;
    into.fetched_documents += other.fetched_documents;
    into.failed_documents += other.failed_documents;
    into.discovered_css_assets += other.discovered_css_assets;
    into.fetched_css_assets += other.fetched_css_assets;
    into.failed_css_assets += other.failed_css_assets;
  }

  fn selected_pages(
    entries: &[PagesetEntry],
    filter: Option<&PagesetFilter>,
    shard: Option<(usize, usize)>,
  ) -> Vec<PagesetEntry> {
    let filtered: Vec<PagesetEntry> = entries
      .iter()
      .cloned()
      .filter(|entry| match filter {
        Some(filter) => filter.matches_entry(entry),
        None => true,
      })
      .collect();

    if let Some((index, total)) = shard {
      filtered
        .into_iter()
        .enumerate()
        .filter(|(idx, _)| idx % total == index)
        .map(|(_, entry)| entry)
        .collect()
    } else {
      filtered
    }
  }

  fn normalize_prefetch_url(url: &str) -> Option<String> {
    let trimmed = url.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
      return None;
    }
    if trimmed.starts_with("data:") {
      return None;
    }

    // Fetchers treat URLs with different fragments as distinct; strip fragments so we only cache
    // the underlying resource once (e.g. `sprite.svg#icon`).
    let trimmed = trimmed.split('#').next().unwrap_or("").trim();
    if trimmed.is_empty() {
      return None;
    }

    let parsed = url::Url::parse(trimmed).ok()?;
    match parsed.scheme() {
      "http" | "https" | "file" => Some(parsed.to_string()),
      _ => None,
    }
  }

  fn looks_like_css_url(url: &str) -> bool {
    url::Url::parse(url)
      .ok()
      .map(|parsed| parsed.path().to_ascii_lowercase().ends_with(".css"))
      .unwrap_or(false)
  }

  fn looks_like_font_url(url: &str) -> bool {
    let Ok(parsed) = url::Url::parse(url) else {
      return false;
    };
    let ext = Path::new(parsed.path())
      .extension()
      .and_then(|e| e.to_str())
      .unwrap_or("")
      .to_ascii_lowercase();
    matches!(
      ext.as_str(),
      "woff2" | "woff" | "ttf" | "otf" | "eot" | "ttc"
    )
  }

  fn looks_like_html_document(res: &FetchedResource, requested_url: &str) -> bool {
    let is_html = res
      .content_type
      .as_deref()
      .map(|ct| {
        let ct = ct.to_ascii_lowercase();
        ct.starts_with("text/html")
          || ct.starts_with("application/xhtml+xml")
          || ct.starts_with("application/html")
          || ct.contains("+html")
      })
      .unwrap_or(false);
    if is_html {
      return true;
    }

    let candidate = res.final_url.as_deref().unwrap_or(requested_url);
    let Ok(parsed) = url::Url::parse(candidate) else {
      return false;
    };
    let lower = parsed.path().to_ascii_lowercase();
    lower.ends_with(".html") || lower.ends_with(".htm") || lower.ends_with(".xhtml")
  }

  fn insert_unique_with_cap(set: &mut BTreeSet<String>, url: String, max: usize) -> bool {
    if set.contains(&url) {
      return true;
    }
    if max != 0 && set.len() >= max {
      return false;
    }
    set.insert(url);
    true
  }

  fn record_image_candidate(set: &mut BTreeSet<String>, url: &str, max: usize) {
    let Some(normalized) = normalize_prefetch_url(url) else {
      return;
    };
    let _ = insert_unique_with_cap(set, normalized, max);
  }

  fn record_document_candidate(set: &mut BTreeSet<String>, url: &str, max: usize) {
    let Some(normalized) = normalize_prefetch_url(url) else {
      return;
    };
    let _ = insert_unique_with_cap(set, normalized, max);
  }

  fn record_css_url_asset_candidate(set: &mut BTreeSet<String>, url: &str, max: usize) {
    let Some(normalized) = normalize_prefetch_url(url) else {
      return;
    };
    if looks_like_css_url(&normalized) || looks_like_font_url(&normalized) {
      return;
    }
    let _ = insert_unique_with_cap(set, normalized, max);
  }

  struct PrefetchImportLoader<'a> {
    fetcher: &'a dyn ResourceFetcher,
    css_cache: RefCell<HashMap<String, String>>,
    summary: &'a RefCell<PageSummary>,
    css_asset_urls: Option<&'a RefCell<BTreeSet<String>>>,
    max_discovered_assets_per_page: usize,
  }

  impl<'a> PrefetchImportLoader<'a> {
    fn new(
      fetcher: &'a dyn ResourceFetcher,
      summary: &'a RefCell<PageSummary>,
      css_asset_urls: Option<&'a RefCell<BTreeSet<String>>>,
      max_discovered_assets_per_page: usize,
    ) -> Self {
      Self {
        fetcher,
        css_cache: RefCell::new(HashMap::new()),
        summary,
        css_asset_urls,
        max_discovered_assets_per_page,
      }
    }
  }

  impl CssImportLoader for PrefetchImportLoader<'_> {
    fn load(&self, url: &str) -> fastrender::Result<String> {
      if let Some(cached) = self.css_cache.borrow().get(url).cloned() {
        return Ok(cached);
      }

      match self.fetcher.fetch(url) {
        Ok(res) => {
          self.summary.borrow_mut().fetched_imports += 1;
          let base = res.final_url.as_deref().unwrap_or(url);
          let decoded = decode_css_bytes(&res.bytes, res.content_type.as_deref());
          let rewritten = match absolutize_css_urls_cow(&decoded, base) {
            Ok(std::borrow::Cow::Owned(css)) => css,
            Ok(std::borrow::Cow::Borrowed(_)) | Err(_) => decoded,
          };

          if let Some(css_asset_urls) = self.css_asset_urls {
            let discovered = discover_css_urls(&rewritten, base);
            let mut set = css_asset_urls.borrow_mut();
            for url in discovered {
              record_css_url_asset_candidate(&mut set, &url, self.max_discovered_assets_per_page);
            }
          }

          self
            .css_cache
            .borrow_mut()
            .insert(url.to_string(), rewritten.clone());

          Ok(rewritten)
        }
        Err(err) => {
          self.summary.borrow_mut().failed_imports += 1;
          Err(err)
        }
      }
    }
  }

  #[derive(Debug, Clone)]
  enum StylesheetTask {
    Inline(String),
    External(String),
  }

  fn stylesheet_type_is_css(type_attr: Option<&str>) -> bool {
    match type_attr {
      None => true,
      Some(value) => {
        let mime = value.split(';').next().map(str::trim).unwrap_or("");
        mime.is_empty() || mime.eq_ignore_ascii_case("text/css")
      }
    }
  }

  fn media_attr_allows(
    media_attr: Option<&str>,
    media_ctx: &MediaContext,
    cache: &mut MediaQueryCache,
  ) -> bool {
    match media_attr {
      None => true,
      Some(media) => {
        let trimmed = media.trim();
        if trimmed.is_empty() {
          return true;
        }

        match MediaQuery::parse_list(trimmed) {
          Ok(list) => media_ctx.evaluate_list_with_cache(&list, Some(cache)),
          Err(_) => false,
        }
      }
    }
  }

  fn discover_dom_stylesheet_tasks(
    html: &str,
    base_url: &str,
    media_ctx: &MediaContext,
    media_query_cache: &mut MediaQueryCache,
  ) -> Option<Vec<StylesheetTask>> {
    let dom = parse_html(html).ok()?;
    let scoped_sources = extract_scoped_css_sources(&dom);
    let toggles = runtime::runtime_toggles();
    let fetch_link_css = toggles.truthy_with_default("FASTR_FETCH_LINK_CSS", true);
    let preload_stylesheets_enabled =
      toggles.truthy_with_default("FASTR_FETCH_PRELOAD_STYLESHEETS", true);
    let modulepreload_stylesheets_enabled =
      toggles.truthy_with_default("FASTR_FETCH_MODULEPRELOAD_STYLESHEETS", false);
    let alternate_stylesheets_enabled =
      toggles.truthy_with_default("FASTR_FETCH_ALTERNATE_STYLESHEETS", true);

    let mut tasks = Vec::new();
    let mut seen_external: HashSet<String> = HashSet::new();

    let mut consider_source = |source: &StylesheetSource| match source {
      StylesheetSource::Inline(inline) => {
        if inline.disabled || !stylesheet_type_is_css(inline.type_attr.as_deref()) {
          return;
        }
        if !media_attr_allows(inline.media.as_deref(), media_ctx, media_query_cache) {
          return;
        }
        if inline.css.trim().is_empty() {
          return;
        }
        tasks.push(StylesheetTask::Inline(inline.css.clone()));
      }
      StylesheetSource::External(link) => {
        if !fetch_link_css {
          return;
        }
        if link.disabled
          || !link_rel_is_stylesheet_candidate(
            &link.rel,
            link.as_attr.as_deref(),
            preload_stylesheets_enabled,
            modulepreload_stylesheets_enabled,
            alternate_stylesheets_enabled,
          )
          || !stylesheet_type_is_css(link.type_attr.as_deref())
        {
          return;
        }
        if !media_attr_allows(link.media.as_deref(), media_ctx, media_query_cache) {
          return;
        }
        if link.href.trim().is_empty() {
          return;
        }

        let Some(stylesheet_url) = resolve_href_with_base(Some(base_url), &link.href) else {
          return;
        };
        if seen_external.insert(stylesheet_url.clone()) {
          tasks.push(StylesheetTask::External(stylesheet_url));
        }
      }
    };

    for source in scoped_sources.document.iter() {
      consider_source(source);
    }

    let mut shadow_hosts: Vec<usize> = scoped_sources.shadows.keys().copied().collect();
    shadow_hosts.sort_unstable();
    for host in shadow_hosts {
      if let Some(sources) = scoped_sources.shadows.get(&host) {
        for source in sources {
          consider_source(source);
        }
      }
    }

    Some(tasks)
  }

  fn prefetch_fonts_from_stylesheet(
    fetcher: &dyn ResourceFetcher,
    css_base_url: &str,
    sheet: &StyleSheet,
    media_ctx: &MediaContext,
    media_query_cache: &mut MediaQueryCache,
    seen_fonts: &mut HashSet<String>,
    summary: &mut PageSummary,
  ) {
    for face in sheet.collect_font_face_rules_with_cache(media_ctx, Some(media_query_cache)) {
      for source in face.sources {
        let FontFaceSource::Url(url_source) = source else {
          continue;
        };
        let Some(resolved) = resolve_href(css_base_url, &url_source.url) else {
          continue;
        };
        if resolved.starts_with("data:") {
          continue;
        }
        if !seen_fonts.insert(resolved.clone()) {
          continue;
        }
        match fetcher.fetch(&resolved) {
          Ok(_) => summary.fetched_fonts += 1,
          Err(_) => summary.failed_fonts += 1,
        }
      }
    }
  }

  fn prefetch_assets_for_html(
    stem: &str,
    html: &str,
    base_url: &str,
    fetcher: &dyn ResourceFetcher,
    media_ctx: &MediaContext,
    opts: PrefetchOptions,
  ) -> PageSummary {
    let summary = RefCell::new(PageSummary {
      stem: stem.to_string(),
      ..PageSummary::default()
    });

    let mut media_query_cache = MediaQueryCache::default();

    let mut tasks: Vec<StylesheetTask> =
      match discover_dom_stylesheet_tasks(html, base_url, media_ctx, &mut media_query_cache) {
        Some(tasks) => {
          if tasks.is_empty() {
            // Pages that load their primary stylesheet dynamically (without emitting `<link rel="stylesheet">`
            // or `<style>`) are still best-effort handled by scanning the raw HTML for `.css`-looking
            // substrings.
            let mut out = Vec::new();
            let mut seen = HashSet::new();
            if let Ok(urls) = extract_embedded_css_urls(html, base_url) {
              for url in urls {
                if seen.insert(url.clone()) {
                  out.push(StylesheetTask::External(url));
                }
              }
            }
            out
          } else {
            tasks
          }
        }
        None => {
          // DOM parse failed; fall back to the string-based extraction used by older versions.
          let mut out = Vec::new();
          let mut seen = HashSet::new();
          if let Ok(urls) = extract_css_links(html, base_url, media_ctx.media_type) {
            for url in urls {
              if seen.insert(url.clone()) {
                out.push(StylesheetTask::External(url));
              }
            }
          }
          if let Ok(urls) = extract_embedded_css_urls(html, base_url) {
            for url in urls {
              if seen.insert(url.clone()) {
                out.push(StylesheetTask::External(url));
              }
            }
          }
          out
        }
      };

    summary.borrow_mut().discovered_css = tasks.len();
    if tasks.is_empty()
      && !(opts.prefetch_images || opts.prefetch_iframes || opts.prefetch_css_url_assets)
    {
      return summary.into_inner();
    }

    let mut image_urls: BTreeSet<String> = BTreeSet::new();
    let mut document_urls: BTreeSet<String> = BTreeSet::new();
    if opts.prefetch_images || opts.prefetch_iframes {
      let html_assets = discover_html_asset_urls(html, base_url);
      if opts.prefetch_images {
        for url in html_assets.images {
          record_image_candidate(&mut image_urls, &url, opts.max_discovered_assets_per_page);
        }
      }
      if opts.prefetch_iframes {
        for url in html_assets.documents {
          record_document_candidate(
            &mut document_urls,
            &url,
            opts.max_discovered_assets_per_page,
          );
        }
      }
    }

    let css_asset_urls = RefCell::new(BTreeSet::<String>::new());
    if opts.prefetch_css_url_assets {
      let mut set = css_asset_urls.borrow_mut();
      for chunk in extract_inline_css_chunks(html) {
        for url in discover_css_urls(&chunk, base_url) {
          record_css_url_asset_candidate(&mut set, &url, opts.max_discovered_assets_per_page);
        }
      }
    }

    if !tasks.is_empty() {
      // Process external stylesheets in sorted order for more reproducible network/cache behavior.
      tasks.sort_by(|a, b| match (a, b) {
        (StylesheetTask::Inline(_), StylesheetTask::External(_)) => std::cmp::Ordering::Less,
        (StylesheetTask::External(_), StylesheetTask::Inline(_)) => std::cmp::Ordering::Greater,
        (StylesheetTask::Inline(_), StylesheetTask::Inline(_)) => std::cmp::Ordering::Equal,
        (StylesheetTask::External(a), StylesheetTask::External(b)) => a.cmp(b),
      });

      let css_asset_urls_ref = if opts.prefetch_css_url_assets {
        Some(&css_asset_urls)
      } else {
        None
      };
      let import_loader = PrefetchImportLoader::new(
        fetcher,
        &summary,
        css_asset_urls_ref,
        opts.max_discovered_assets_per_page,
      );
      let mut seen_fonts: HashSet<String> = HashSet::new();

      for task in tasks {
        match task {
          StylesheetTask::Inline(css) => {
            if opts.prefetch_css_url_assets {
              let scan_css = match absolutize_css_urls_cow(&css, base_url) {
                Ok(css) => css,
                Err(_) => std::borrow::Cow::Borrowed(css.as_str()),
              };
              let discovered = discover_css_urls(scan_css.as_ref(), base_url);
              {
                let mut set = css_asset_urls.borrow_mut();
                for url in discovered {
                  record_css_url_asset_candidate(
                    &mut set,
                    &url,
                    opts.max_discovered_assets_per_page,
                  );
                }
              }
            }

            let sheet: StyleSheet = match parse_stylesheet(&css) {
              Ok(sheet) => sheet,
              Err(_) => continue,
            };

            let resolved = match sheet.resolve_imports_with_cache(
              &import_loader,
              Some(base_url),
              media_ctx,
              Some(&mut media_query_cache),
            ) {
              Ok(sheet) => sheet,
              Err(_) => sheet,
            };

            if opts.prefetch_fonts {
              let mut summary = summary.borrow_mut();
              prefetch_fonts_from_stylesheet(
                fetcher,
                base_url,
                &resolved,
                media_ctx,
                &mut media_query_cache,
                &mut seen_fonts,
                &mut summary,
              );
            }
          }
          StylesheetTask::External(css_url) => match fetcher.fetch(&css_url) {
            Ok(res) => {
              summary.borrow_mut().fetched_css += 1;
              let sheet_base = res.final_url.as_deref().unwrap_or(&css_url);
              let mut css_text = decode_css_bytes(&res.bytes, res.content_type.as_deref());
              if let Ok(std::borrow::Cow::Owned(rewritten)) =
                absolutize_css_urls_cow(&css_text, sheet_base)
              {
                css_text = rewritten;
              }

              if opts.prefetch_css_url_assets {
                let discovered = discover_css_urls(&css_text, sheet_base);
                {
                  let mut set = css_asset_urls.borrow_mut();
                  for url in discovered {
                    record_css_url_asset_candidate(
                      &mut set,
                      &url,
                      opts.max_discovered_assets_per_page,
                    );
                  }
                }
              }

              let sheet: StyleSheet = match parse_stylesheet(&css_text) {
                Ok(sheet) => sheet,
                Err(_) => continue,
              };

              let resolved = match sheet.resolve_imports_with_cache(
                &import_loader,
                Some(sheet_base),
                media_ctx,
                Some(&mut media_query_cache),
              ) {
                Ok(sheet) => sheet,
                Err(_) => sheet,
              };

              if opts.prefetch_fonts {
                let mut summary = summary.borrow_mut();
                prefetch_fonts_from_stylesheet(
                  fetcher,
                  sheet_base,
                  &resolved,
                  media_ctx,
                  &mut media_query_cache,
                  &mut seen_fonts,
                  &mut summary,
                );
              }
            }
            Err(_) => summary.borrow_mut().failed_css += 1,
          },
        }
      }
    }

    let css_asset_urls = css_asset_urls.into_inner();

    {
      let mut summary = summary.borrow_mut();
      summary.discovered_images = image_urls.len();
      summary.discovered_documents = document_urls.len();
      summary.discovered_css_assets = css_asset_urls.len();
    }

    if opts.prefetch_images {
      let mut summary = summary.borrow_mut();
      for url in &image_urls {
        match fetcher.fetch(url) {
          Ok(_) => summary.fetched_images += 1,
          Err(_) => summary.failed_images += 1,
        }
      }
    }

    if opts.prefetch_iframes {
      let mut fetched_docs: Vec<(String, FetchedResource)> = Vec::new();
      {
        let mut summary = summary.borrow_mut();
        for url in &document_urls {
          match fetcher.fetch(url) {
            Ok(res) => {
              summary.fetched_documents += 1;
              fetched_docs.push((url.clone(), res));
            }
            Err(_) => summary.failed_documents += 1,
          }
        }
      }

      // Best-effort: when iframe/object/embed prefetching is enabled, also warm their subresource
      // dependencies (stylesheets/@imports/fonts, and HTML images when enabled). This reduces
      // render-deadline network fetches because iframes are rendered as nested documents.
      let nested_opts = PrefetchOptions {
        prefetch_iframes: false,
        ..opts
      };
      for (url, res) in fetched_docs {
        if !looks_like_html_document(&res, &url) {
          continue;
        }
        let base_hint = res.final_url.as_deref().unwrap_or(&url);
        let doc = decode_html_resource(&res, base_hint);
        let nested_summary = prefetch_assets_for_html(
          &format!("{stem}::document:{url}"),
          &doc.html,
          &doc.base_url,
          fetcher,
          media_ctx,
          nested_opts,
        );
        merge_page_summary(&mut summary.borrow_mut(), nested_summary);
      }
    }

    if opts.prefetch_css_url_assets {
      let mut summary = summary.borrow_mut();
      for url in &css_asset_urls {
        match fetcher.fetch(url) {
          Ok(_) => summary.fetched_css_assets += 1,
          Err(_) => summary.failed_css_assets += 1,
        }
      }
    }

    summary.into_inner()
  }

  fn prefetch_page(
    entry: &PagesetEntry,
    fetcher: &dyn ResourceFetcher,
    media_ctx: &MediaContext,
    opts: PrefetchOptions,
  ) -> PageSummary {
    let cache_path = cache_html_path(&entry.cache_stem);
    if !cache_path.exists() {
      return PageSummary {
        stem: entry.cache_stem.clone(),
        skipped: true,
        ..PageSummary::default()
      };
    }

    let cached = match read_cached_document(&cache_path) {
      Ok(doc) => doc,
      Err(_) => {
        return PageSummary {
          stem: entry.cache_stem.clone(),
          skipped: true,
          ..PageSummary::default()
        };
      }
    };

    prefetch_assets_for_html(
      &entry.cache_stem,
      &cached.document.html,
      &cached.document.base_url,
      fetcher,
      media_ctx,
      opts,
    )
  }

  #[cfg(test)]
  mod tests {
    use super::*;
    use fastrender::resource::{CachingFetcherConfig, DiskCacheConfig, FetchedResource};
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;
    use std::time::Duration;

    #[derive(Clone)]
    struct PanicFetcher;

    impl ResourceFetcher for PanicFetcher {
      fn fetch(&self, _url: &str) -> fastrender::Result<FetchedResource> {
        panic!("network fetch should not be called for disk cache hits");
      }
    }

    #[test]
    fn prefetch_warms_disk_cache_for_html_images_and_css_url_assets() {
      let listener = TcpListener::bind("127.0.0.1:0").expect("bind");
      listener.set_nonblocking(true).expect("set_nonblocking");
      let addr = listener.local_addr().expect("addr");

      const CSS: &str =
        "@import \"/imported.css\";\nbody { background-image: url(\"/bg.png#hash\"); }\n";
      const IMPORTED_CSS: &str = "div { background-image: url(/import.png); }\n";
      const IMG_BYTES: &[u8] = b"img";
      const BG_BYTES: &[u8] = b"bg";
      const INLINE_BYTES: &[u8] = b"inline";
      const IMPORT_BYTES: &[u8] = b"import";

      let done = Arc::new(AtomicBool::new(false));
      let server_done = Arc::clone(&done);
      let handle = std::thread::spawn(move || {
        while !server_done.load(Ordering::SeqCst) {
          match listener.accept() {
            Ok((mut stream, _)) => {
              let mut buf = [0u8; 4096];
              let n = stream.read(&mut buf).unwrap_or(0);
              let req = String::from_utf8_lossy(&buf[..n]);
              let path = req
                .lines()
                .next()
                .and_then(|line| line.split_whitespace().nth(1))
                .unwrap_or("/");
              let path = path.split('?').next().unwrap_or(path);

              let (status, content_type, body): (&str, &str, &[u8]) = match path {
                "/style.css" => ("200 OK", "text/css", CSS.as_bytes()),
                "/imported.css" => ("200 OK", "text/css", IMPORTED_CSS.as_bytes()),
                "/img.png" => ("200 OK", "image/png", IMG_BYTES),
                "/bg.png" => ("200 OK", "image/png", BG_BYTES),
                "/inline.png" => ("200 OK", "image/png", INLINE_BYTES),
                "/import.png" => ("200 OK", "image/png", IMPORT_BYTES),
                _ => ("404 Not Found", "text/plain", b"not found"),
              };

              let response = format!(
                "HTTP/1.1 {status}\r\nContent-Length: {}\r\nContent-Type: {content_type}\r\nCache-Control: max-age=86400\r\nConnection: close\r\n\r\n",
                body.len()
              );
              let _ = stream.write_all(response.as_bytes());
              let _ = stream.write_all(body);
            }
            Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
              std::thread::sleep(Duration::from_millis(5));
            }
            Err(_) => break,
          }
        }
      });

      let tmp = tempfile::tempdir().expect("tempdir");
      let cache_dir = tmp.path().join("cache");

      let base = format!("http://{addr}");
      let document_url = format!("{base}/index.html");
      let html = format!(
        r#"<!doctype html>
<html>
  <head>
    <link rel="stylesheet" href="/style.css">
  </head>
  <body style="background-image: url(/inline.png#frag)">
    <img src="/img.png#fragment">
  </body>
</html>"#
      );

      let html_path = tmp.path().join("cached.html");
      std::fs::write(&html_path, html).expect("write html");
      let mut meta_path = html_path.clone();
      meta_path.set_extension("html.meta");
      std::fs::write(
        &meta_path,
        format!("content-type: text/html\nurl: {document_url}\n"),
      )
      .expect("write meta");

      let cached = read_cached_document(&html_path).expect("read cached document");

      let http = build_http_fetcher(
        DEFAULT_USER_AGENT,
        DEFAULT_ACCEPT_LANGUAGE,
        Some(Duration::from_secs(2)),
      );
      let mut disk_config = DiskCacheConfig {
        max_bytes: 0,
        ..DiskCacheConfig::default()
      };
      disk_config.namespace = Some(disk_cache_namespace(
        DEFAULT_USER_AGENT,
        DEFAULT_ACCEPT_LANGUAGE,
      ));

      let fetcher = DiskCachingFetcher::with_configs(
        http,
        &cache_dir,
        CachingFetcherConfig {
          honor_http_cache_freshness: true,
          ..CachingFetcherConfig::default()
        },
        disk_config.clone(),
      );

      let media_ctx = MediaContext::screen(800.0, 600.0);
      let opts = PrefetchOptions {
        prefetch_fonts: false,
        prefetch_images: true,
        prefetch_iframes: false,
        prefetch_css_url_assets: true,
        max_discovered_assets_per_page: 2000,
      };

      let summary = prefetch_assets_for_html(
        "test",
        &cached.document.html,
        &cached.document.base_url,
        &fetcher,
        &media_ctx,
        opts,
      );

      done.store(true, Ordering::SeqCst);
      handle.join().expect("server thread");

      assert_eq!(summary.fetched_css, 1);
      assert_eq!(summary.fetched_imports, 1);
      assert_eq!(summary.discovered_images, 1);
      assert_eq!(summary.fetched_images, 1);
      assert_eq!(summary.failed_images, 0);
      assert_eq!(summary.discovered_css_assets, 3);
      assert_eq!(summary.fetched_css_assets, 3);
      assert_eq!(summary.failed_css_assets, 0);

      let entries: Vec<_> = std::fs::read_dir(&cache_dir)
        .expect("read cache dir")
        .map(|e| e.expect("dir entry").path())
        .collect();
      let bin_count = entries
        .iter()
        .filter(|p| p.to_string_lossy().ends_with(".bin"))
        .count();
      let meta_count = entries
        .iter()
        .filter(|p| p.to_string_lossy().ends_with(".bin.meta"))
        .count();
      assert!(
        bin_count >= 4,
        "expected at least 4 cached .bin entries, got {bin_count} (entries={entries:?})"
      );
      assert!(
        meta_count >= 4,
        "expected at least 4 cached .bin.meta entries, got {meta_count} (entries={entries:?})"
      );

      // Ensure the persisted resources are actually usable from disk without network access.
      let offline = DiskCachingFetcher::with_configs(
        PanicFetcher,
        &cache_dir,
        CachingFetcherConfig {
          honor_http_cache_freshness: true,
          ..CachingFetcherConfig::default()
        },
        disk_config,
      );
      let res = offline.fetch(&format!("{base}/img.png")).expect("disk hit");
      assert_eq!(res.bytes, IMG_BYTES);
    }
  }

  fn log_disk_cache_stats(
    phase: &str,
    cache_dir: &Path,
    lock_stale_after: Duration,
    max_bytes: u64,
    lock_stale_secs: u64,
  ) {
    let stats = match scan_disk_cache_dir(cache_dir, lock_stale_after) {
      Ok(stats) => stats,
      Err(err) => {
        println!("Disk cache stats ({phase}): unavailable ({err})");
        return;
      }
    };

    println!(
      "Disk cache stats ({phase}): bin_count={} meta_count={} alias_count={} bin_bytes={} locks={} stale_locks={} tmp={} journal={}",
      stats.bin_count,
      stats.meta_count,
      stats.alias_count,
      stats.bin_bytes,
      stats.lock_count,
      stats.stale_lock_count,
      stats.tmp_count,
      stats.journal_bytes
    );
    println!("{}", stats.usage_summary(max_bytes));

    if max_bytes != 0 && stats.bin_bytes > max_bytes {
      println!(
        "Warning: disk cache usage exceeds max_bytes (bin_bytes={} > max_bytes={}). Consider increasing --disk-cache-max-bytes or setting FASTR_DISK_CACHE_MAX_BYTES=0 to disable eviction.",
        stats.bin_bytes, max_bytes
      );
    }
    if stats.stale_lock_count > 0 {
      println!(
        "Warning: disk cache contains {} stale .lock file(s). Consider tuning FASTR_DISK_CACHE_LOCK_STALE_SECS (currently {}).",
        stats.stale_lock_count, lock_stale_secs
      );
    }
    println!();
  }

  pub fn main() {
    let args = Args::parse();

    if args.jobs == 0 {
      eprintln!("jobs must be > 0");
      std::process::exit(2);
    }

    let timeout_secs = args.timeout.seconds(Some(30));
    let per_request_timeout_label = timeout_secs.unwrap_or(0);

    let page_filter = args
      .pages
      .as_ref()
      .and_then(|pages| PagesetFilter::from_inputs(pages));

    let entries = pageset_entries();
    let selected = selected_pages(&entries, page_filter.as_ref(), args.shard);
    if selected.is_empty() {
      if page_filter.is_some() {
        println!("No pages matched the provided filter");
      } else {
        println!("No pages to prefetch");
      }
      std::process::exit(1);
    }

    if let Some(filter) = &page_filter {
      let missing = filter.unmatched(&selected);
      if !missing.is_empty() {
        println!("Warning: unknown pages in filter: {}", missing.join(", "));
      }
    }

    let media_ctx = MediaContext::screen(
      args.viewport.viewport.0 as f32,
      args.viewport.viewport.1 as f32,
    )
    .with_device_pixel_ratio(args.viewport.dpr)
    .with_env_overrides();
    let http = build_http_fetcher(
      &args.user_agent,
      &args.accept_language,
      timeout_secs.map(Duration::from_secs),
    );
    let mut disk_config = args.disk_cache.to_config();
    let lock_stale_after = disk_config.lock_stale_after;
    disk_config.namespace = Some(disk_cache_namespace(
      &args.user_agent,
      &args.accept_language,
    ));
    let fetcher: Arc<dyn ResourceFetcher> = Arc::new(DiskCachingFetcher::with_configs(
      http,
      args.cache_dir.clone(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      disk_config,
    ));

    let opts = PrefetchOptions {
      prefetch_fonts: args.prefetch_fonts,
      prefetch_images: args.prefetch_images,
      prefetch_iframes: args.prefetch_iframes,
      prefetch_css_url_assets: args.prefetch_css_url_assets,
      max_discovered_assets_per_page: args.max_discovered_assets_per_page,
    };

    println!(
      "Prefetching assets for {} page(s) ({} parallel, {}s timeout, fonts={} images={} iframes={} css_url_assets={} max_assets_per_page={})...",
      selected.len(),
      args.jobs,
      per_request_timeout_label,
      args.prefetch_fonts,
      args.prefetch_images,
      args.prefetch_iframes,
      args.prefetch_css_url_assets,
      args.max_discovered_assets_per_page
    );
    if args.prefetch_images || args.prefetch_iframes {
      println!(
        "HTML assets: images={} iframes={}",
        args.prefetch_images, args.prefetch_iframes
      );
    }
    if let Some((index, total)) = args.shard {
      println!("Shard: {}/{}", index, total);
    }
    println!("Cache dir: {}", args.cache_dir.display());
    let max_age = if args.disk_cache.max_age_secs == 0 {
      "none".to_string()
    } else {
      format!("{}s", args.disk_cache.max_age_secs)
    };
    println!(
      "Disk cache: max_bytes={} max_age={}",
      args.disk_cache.max_bytes, max_age
    );
    log_disk_cache_stats(
      "start",
      &args.cache_dir,
      lock_stale_after,
      args.disk_cache.max_bytes,
      args.disk_cache.lock_stale_secs,
    );
    println!();

    let pool = ThreadPoolBuilder::new()
      .num_threads(args.jobs)
      .build()
      .expect("create thread pool");

    let mut results: Vec<PageSummary> = pool.install(|| {
      selected
        .par_iter()
        .map(|entry| prefetch_page(entry, fetcher.as_ref(), &media_ctx, opts))
        .collect()
    });

    results.sort_by(|a, b| a.stem.cmp(&b.stem));

    let mut total_discovered = 0usize;
    let mut total_fetched = 0usize;
    let mut total_failed = 0usize;
    let mut total_skipped = 0usize;
    let mut total_imports_fetched = 0usize;
    let mut total_imports_failed = 0usize;
    let mut total_fonts_fetched = 0usize;
    let mut total_fonts_failed = 0usize;
    let mut total_images_discovered = 0usize;
    let mut total_images_fetched = 0usize;
    let mut total_images_failed = 0usize;
    let mut total_documents_discovered = 0usize;
    let mut total_documents_fetched = 0usize;
    let mut total_documents_failed = 0usize;
    let mut total_css_assets_discovered = 0usize;
    let mut total_css_assets_fetched = 0usize;
    let mut total_css_assets_failed = 0usize;

    for r in &results {
      if r.skipped {
        total_skipped += 1;
        println!("• {} (no cached HTML, skipped)", r.stem);
        continue;
      }
      total_discovered += r.discovered_css;
      total_fetched += r.fetched_css;
      total_failed += r.failed_css;
      total_imports_fetched += r.fetched_imports;
      total_imports_failed += r.failed_imports;
      total_fonts_fetched += r.fetched_fonts;
      total_fonts_failed += r.failed_fonts;
      total_images_discovered += r.discovered_images;
      total_images_fetched += r.fetched_images;
      total_images_failed += r.failed_images;
      total_documents_discovered += r.discovered_documents;
      total_documents_fetched += r.fetched_documents;
      total_documents_failed += r.failed_documents;
      total_css_assets_discovered += r.discovered_css_assets;
      total_css_assets_fetched += r.fetched_css_assets;
      total_css_assets_failed += r.failed_css_assets;

      let mut line = format!(
        "• {} css={} fetched={} failed={} imports_fetched={} imports_failed={} fonts_fetched={} fonts_failed={}",
        r.stem,
        r.discovered_css,
        r.fetched_css,
        r.failed_css,
        r.fetched_imports,
        r.failed_imports,
        r.fetched_fonts,
        r.failed_fonts
      );
      if args.prefetch_images {
        line.push_str(&format!(
          " images={} img_fetched={} img_failed={}",
          r.discovered_images, r.fetched_images, r.failed_images
        ));
      }
      if args.prefetch_iframes {
        line.push_str(&format!(
          " docs={} docs_fetched={} docs_failed={}",
          r.discovered_documents, r.fetched_documents, r.failed_documents
        ));
      }
      if args.prefetch_css_url_assets {
        line.push_str(&format!(
          " css_assets={} css_assets_fetched={} css_assets_failed={}",
          r.discovered_css_assets, r.fetched_css_assets, r.failed_css_assets
        ));
      }
      println!("{line}");
    }

    println!();
    let mut done = format!(
      "Done: css_discovered={} css_fetched={} css_failed={} pages_skipped={} imports_fetched={} imports_failed={} fonts_fetched={} fonts_failed={}",
      total_discovered,
      total_fetched,
      total_failed,
      total_skipped,
      total_imports_fetched,
      total_imports_failed,
      total_fonts_fetched,
      total_fonts_failed
    );
    if args.prefetch_images {
      done.push_str(&format!(
        " images_discovered={} images_fetched={} images_failed={}",
        total_images_discovered, total_images_fetched, total_images_failed
      ));
    }
    if args.prefetch_iframes {
      done.push_str(&format!(
        " docs_discovered={} docs_fetched={} docs_failed={}",
        total_documents_discovered, total_documents_fetched, total_documents_failed
      ));
    }
    if args.prefetch_css_url_assets {
      done.push_str(&format!(
        " css_assets_discovered={} css_assets_fetched={} css_assets_failed={}",
        total_css_assets_discovered, total_css_assets_fetched, total_css_assets_failed
      ));
    }
    println!("{done}");
    println!();
    log_disk_cache_stats(
      "end",
      &args.cache_dir,
      lock_stale_after,
      args.disk_cache.max_bytes,
      args.disk_cache.lock_stale_secs,
    );

    // Best-effort tool: do not fail the process on fetch errors.
  }
}

#[cfg(feature = "disk_cache")]
fn main() {
  disk_cache_main::main();
}
