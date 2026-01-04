use anyhow::{bail, Context, Result};
use clap::Args;
use fastrender::css::loader::resolve_href;
use fastrender::resource::bundle::{Bundle, BundledFetcher, BundledResourceInfo};
use fastrender::resource::FetchedResource;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::is_data_url;
use regex::Regex;
use sha2::{Digest, Sha256};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use url::Url;

const DEFAULT_FIXTURE_ROOT: &str = "tests/pages/fixtures";
const ASSETS_DIR: &str = "assets";
const HASH_PREFIX_BYTES: usize = 16;

#[derive(Args, Debug)]
pub struct ImportPageFixtureArgs {
  /// Bundle directory or .tar produced by `bundle_page fetch`
  pub bundle: PathBuf,

  /// Name of the fixture directory to create under `--output-root`
  pub fixture_name: String,

  /// Root directory for fixtures (defaults to tests/pages/fixtures)
  #[arg(long, default_value = DEFAULT_FIXTURE_ROOT)]
  pub output_root: PathBuf,

  /// Allow replacing an existing fixture directory
  #[arg(long)]
  pub overwrite: bool,

  /// Replace missing resources with empty placeholder assets instead of failing the import
  #[arg(long)]
  pub allow_missing: bool,

  /// Allow leaving http(s) references in rewritten HTML/CSS (skips validation)
  #[arg(long)]
  pub allow_http_references: bool,

  /// Use the legacy HTML rewrite behavior (rewrites every src/href/poster/data attribute).
  ///
  /// The default behavior only rewrites URLs in places FastRender actually fetches as
  /// subresources (stylesheets, images, etc.).
  #[arg(long)]
  pub legacy_rewrite: bool,

  /// Validate without writing any files
  #[arg(long)]
  pub dry_run: bool,
}

pub fn run_import_page_fixture(args: ImportPageFixtureArgs) -> Result<()> {
  let fixture_dir = args.output_root.join(&args.fixture_name);
  if fixture_dir.exists() {
    if !args.overwrite {
      bail!(
        "Fixture directory {} already exists; pass --overwrite to replace it",
        fixture_dir.display()
      );
    }
    if !args.dry_run {
      fs::remove_dir_all(&fixture_dir).with_context(|| {
        format!(
          "failed to clear existing fixture at {}",
          fixture_dir.display()
        )
      })?;
    }
  }

  let bundle = Bundle::load(&args.bundle)
    .with_context(|| format!("failed to load bundle at {}", args.bundle.display()))?;
  let manifest = bundle.manifest().clone();
  let (doc_meta, doc_bytes) = bundle.document();
  let document_html = String::from_utf8_lossy(&doc_bytes).to_string();
  let document_base = if doc_meta.final_url.is_empty() {
    manifest.original_url.clone()
  } else {
    doc_meta.final_url.clone()
  };
  let document_base_url =
    Url::parse(&document_base).context("failed to parse document base URL from bundle")?;
  let effective_base = find_base_url(&document_html, &document_base_url);

  let fetcher = BundledFetcher::new(bundle);
  let mut catalog = AssetCatalog::new(args.allow_missing);
  for (url, info) in &manifest.resources {
    let resource = fetcher
      .fetch(url)
      .with_context(|| format!("failed to read bundled resource for {}", url))?;
    catalog.add_resource(url, info, &resource)?;
  }

  catalog.rewrite_stylesheets()?;
  catalog.rewrite_html_assets(args.legacy_rewrite)?;

  let rewritten_html = rewrite_html(
    &document_html,
    &effective_base,
    ReferenceContext::Html,
    &mut catalog,
    args.legacy_rewrite,
  )?;

  catalog.fail_if_missing()?;

  if !args.allow_http_references {
    validate_no_remote_fetchable_subresources_in_html("index.html", &rewritten_html)?;
    catalog.validate_no_remote_fetchable_subresources_in_css()?;
    catalog.validate_no_remote_fetchable_subresources_in_html_assets()?;
  }

  if args.dry_run {
    println!(
      "✓ Dry run: {} assets would be written to {}",
      catalog.assets.len(),
      fixture_dir.display()
    );
    return Ok(());
  }

  let assets_dir = fixture_dir.join(ASSETS_DIR);
  fs::create_dir_all(&assets_dir)
    .with_context(|| format!("failed to create assets directory {}", assets_dir.display()))?;
  fs::write(fixture_dir.join("index.html"), rewritten_html.as_bytes()).with_context(|| {
    format!(
      "failed to write {}",
      fixture_dir.join("index.html").display()
    )
  })?;

  for asset in catalog.assets.values() {
    fs::write(assets_dir.join(&asset.filename), &asset.bytes).with_context(|| {
      format!(
        "failed to write asset {}",
        assets_dir.join(&asset.filename).display()
      )
    })?;
  }

  println!(
    "✓ Imported bundle {} into {} ({} assets)",
    args.bundle.display(),
    fixture_dir.display(),
    catalog.assets.len()
  );

  Ok(())
}

#[derive(Clone, Copy, Debug)]
enum ReferenceContext {
  Html,
  Css,
}

#[derive(Clone, Debug)]
struct AssetData {
  filename: String,
  bytes: Vec<u8>,
  content_type: Option<String>,
  source_url: String,
}

#[derive(Default, Debug)]
struct AssetCatalog {
  assets: BTreeMap<String, AssetData>,
  url_to_filename: HashMap<String, String>,
  allow_missing: bool,
  missing_urls: BTreeSet<String>,
}

impl AssetCatalog {
  fn new(allow_missing: bool) -> Self {
    Self {
      assets: BTreeMap::new(),
      url_to_filename: HashMap::new(),
      allow_missing,
      missing_urls: BTreeSet::new(),
    }
  }

  fn add_resource(
    &mut self,
    url: &str,
    info: &BundledResourceInfo,
    res: &FetchedResource,
  ) -> Result<()> {
    let source_url = info.final_url.clone().unwrap_or_else(|| url.to_string());
    let ext = extension_from_resource(&info.path, res.content_type.as_deref());
    // HTML/CSS rewriting depends on the document's base URL, so byte-based deduplication can
    // incorrectly collapse multiple distinct source URLs into one rewritten output.
    let base_sensitive = is_html_extension(&ext)
      || ext == "css"
      || res
        .content_type
        .as_deref()
        .map(|ct| {
          let lower = ct.to_ascii_lowercase();
          lower.contains("html") || lower.contains("css")
        })
        .unwrap_or(false);
    let filename_hash = if base_sensitive {
      hash_bytes(source_url.as_bytes())
    } else {
      hash_bytes(&res.bytes)
    };
    let filename = format!("{filename_hash}.{ext}");
    if let Some(existing) = self.assets.get(&filename) {
      if existing.bytes != res.bytes {
        bail!(
          "hash collision while importing {} ({}); existing asset has different contents",
          url,
          filename
        );
      }
    }

    let data = AssetData {
      filename: filename.clone(),
      bytes: res.bytes.clone(),
      content_type: res.content_type.clone(),
      source_url,
    };

    self.assets.insert(filename.clone(), data);
    self
      .url_to_filename
      .insert(url.to_string(), filename.clone());
    let decoded_url = decode_html_entities_if_needed(url);
    if decoded_url.as_ref() != url {
      self
        .url_to_filename
        .entry(decoded_url.to_string())
        .or_insert_with(|| filename.clone());
    }
    if let Some(final_url) = &info.final_url {
      self
        .url_to_filename
        .entry(final_url.clone())
        .or_insert_with(|| filename.clone());
      let decoded_final = decode_html_entities_if_needed(final_url);
      if decoded_final.as_ref() != final_url {
        self
          .url_to_filename
          .entry(decoded_final.to_string())
          .or_insert_with(|| filename.clone());
      }
    }
    Ok(())
  }

  fn rewrite_stylesheets(&mut self) -> Result<()> {
    let names: Vec<String> = self.assets.keys().cloned().collect();
    for name in names {
      let Some(asset) = self.assets.get(&name).cloned() else {
        continue;
      };
      let is_css = asset
        .content_type
        .as_deref()
        .map(|ct| ct.to_ascii_lowercase().contains("css"))
        .unwrap_or_else(|| name.to_ascii_lowercase().ends_with(".css"));

      if !is_css {
        continue;
      }

      let base = Url::parse(&asset.source_url).with_context(|| {
        format!(
          "stylesheet {} has unparsable URL {}",
          name, asset.source_url
        )
      })?;
      let css = String::from_utf8_lossy(&asset.bytes).to_string();
      let rewritten = rewrite_css(&css, &base, self, ReferenceContext::Css)
        .with_context(|| format!("failed to rewrite stylesheet {}", name))?;
      self.assets.insert(
        name.clone(),
        AssetData {
          filename: asset.filename.clone(),
          bytes: rewritten.into_bytes(),
          content_type: asset.content_type.clone(),
          source_url: asset.source_url.clone(),
        },
      );
    }

    Ok(())
  }

  fn rewrite_html_assets(&mut self, legacy_rewrite: bool) -> Result<()> {
    let names: Vec<String> = self.assets.keys().cloned().collect();
    for name in names {
      let Some(asset) = self.assets.get(&name).cloned() else {
        continue;
      };
      if !is_html_asset(&asset) {
        continue;
      }

      let base = Url::parse(&asset.source_url).with_context(|| {
        format!(
          "HTML asset {} has unparsable URL {}",
          name, asset.source_url
        )
      })?;
      let html = String::from_utf8_lossy(&asset.bytes).to_string();
      let effective_base = find_base_url(&html, &base);
      // HTML assets are written under `assets/`, so their rewritten subresource URLs must be
      // relative to the assets directory (same as CSS).
      let rewritten = rewrite_html(
        &html,
        &effective_base,
        ReferenceContext::Css,
        self,
        legacy_rewrite,
      )
      .with_context(|| format!("failed to rewrite HTML asset {}", name))?;

      self.assets.insert(
        name.clone(),
        AssetData {
          filename: asset.filename.clone(),
          bytes: rewritten.into_bytes(),
          content_type: asset.content_type.clone(),
          source_url: asset.source_url.clone(),
        },
      );
    }
    Ok(())
  }

  fn path_for(&mut self, url: &str, ctx: ReferenceContext) -> Option<String> {
    if let Some(filename) = self.url_to_filename.get(url) {
      return match ctx {
        ReferenceContext::Html => Some(format!("{ASSETS_DIR}/{filename}")),
        ReferenceContext::Css => Some(filename.clone()),
      };
    }

    if !self.allow_missing {
      self.missing_urls.insert(url.to_string());
      return None;
    }

    let ext = extension_from_path(url);
    let filename = format!("missing_{}.{}", hash_bytes(url.as_bytes()), ext);
    if !self.assets.contains_key(&filename) {
      self.assets.insert(
        filename.clone(),
        AssetData {
          filename: filename.clone(),
          bytes: Vec::new(),
          content_type: None,
          source_url: url.to_string(),
        },
      );
    }
    self
      .url_to_filename
      .entry(url.to_string())
      .or_insert_with(|| filename.clone());

    match ctx {
      ReferenceContext::Html => Some(format!("{ASSETS_DIR}/{filename}")),
      ReferenceContext::Css => Some(filename),
    }
  }

  /// Like `path_for`, but always returns a deterministic placeholder path for missing resources.
  ///
  /// This is intended for URLs that are fetchable in browsers (e.g. preloads, media sources) but
  /// are not required for FastRender output correctness.
  fn path_for_optional(&mut self, url: &str, ctx: ReferenceContext) -> String {
    if let Some(filename) = self.url_to_filename.get(url) {
      return match ctx {
        ReferenceContext::Html => format!("{ASSETS_DIR}/{filename}"),
        ReferenceContext::Css => filename.clone(),
      };
    }

    let ext = extension_from_path(url);
    let filename = format!("missing_{}.{}", hash_bytes(url.as_bytes()), ext);
    if !self.assets.contains_key(&filename) {
      self.assets.insert(
        filename.clone(),
        AssetData {
          filename: filename.clone(),
          bytes: Vec::new(),
          content_type: None,
          source_url: url.to_string(),
        },
      );
    }
    self
      .url_to_filename
      .entry(url.to_string())
      .or_insert_with(|| filename.clone());

    match ctx {
      ReferenceContext::Html => format!("{ASSETS_DIR}/{filename}"),
      ReferenceContext::Css => filename,
    }
  }

  fn fail_if_missing(&self) -> Result<()> {
    if self.allow_missing || self.missing_urls.is_empty() {
      return Ok(());
    }

    let mut msg = String::from("bundle is missing required subresources:\n");
    for url in &self.missing_urls {
      msg.push_str("  - ");
      msg.push_str(url);
      msg.push('\n');
    }
    msg.push_str(
      "\nRe-run with --allow-missing to create deterministic empty placeholder assets.\n",
    );
    bail!(msg)
  }

  fn validate_no_remote_fetchable_subresources_in_css(&self) -> Result<()> {
    let mut remote: BTreeSet<String> = BTreeSet::new();

    for asset in self.assets.values() {
      let is_css = asset
        .content_type
        .as_deref()
        .map(|ct| ct.to_ascii_lowercase().contains("css"))
        .unwrap_or_else(|| asset.filename.to_ascii_lowercase().ends_with(".css"));
      if !is_css {
        continue;
      }

      let css = String::from_utf8_lossy(&asset.bytes);
      for url in extract_fetchable_css_urls(&css) {
        if is_remote_fetch_url(&url) {
          remote.insert(url);
        }
      }
    }

    if remote.is_empty() {
      return Ok(());
    }

    let mut msg = String::from("rewritten CSS still contains remote fetchable subresources:\n");
    for url in &remote {
      msg.push_str("  - ");
      msg.push_str(url);
      msg.push('\n');
    }
    bail!(msg)
  }

  fn validate_no_remote_fetchable_subresources_in_html_assets(&self) -> Result<()> {
    for asset in self.assets.values() {
      if !is_html_asset(asset) {
        continue;
      }
      let html = String::from_utf8_lossy(&asset.bytes);
      validate_no_remote_fetchable_subresources_in_html(&asset.filename, &html)?;
    }
    Ok(())
  }
}

fn rewrite_html(
  input: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
  legacy_rewrite: bool,
) -> Result<String> {
  let mut rewritten = input.to_string();

  // Normalize <base href> to keep the output local.
  let base_tag_regex = Regex::new(
    "(?is)(?P<prefix><base[^>]*href\\s*=\\s*[\"'])(?P<url>[^\"'>]+)(?P<suffix>[\"'][^>]*>)",
  )
  .expect("base regex must compile");
  rewritten = apply_rewrite(
    &base_tag_regex,
    &rewritten,
    base_url,
    ctx,
    catalog,
    Some("."),
  )?;

  if legacy_rewrite {
    let attr_regex = Regex::new(
      "(?is)(?P<prefix>(?:src|href|poster|data)\\s*=\\s*[\"'])(?P<url>[^\"'>]+)(?P<suffix>[\"'])",
    )
    .expect("attr regex must compile");
    rewritten = apply_rewrite(
      &attr_regex,
      &rewritten,
      base_url,
      ctx,
      catalog,
      None,
    )?;

    let content_url_regex = Regex::new(
      "(?is)(?P<prefix>content\\s*=\\s*[\"'])(?P<url>https?://[^\"'>]+)(?P<suffix>[\"'])",
    )
    .expect("content url regex must compile");
    rewritten = apply_rewrite(
      &content_url_regex,
      &rewritten,
      base_url,
      ctx,
      catalog,
      Some("."),
    )?;

    let srcset_regex =
      Regex::new("(?is)(?P<prefix>srcset\\s*=\\s*[\"'])(?P<value>[^\"']+)(?P<suffix>[\"'])")
        .expect("srcset regex must compile");
    let mut srcset_error: Option<anyhow::Error> = None;
    rewritten = srcset_regex
      .replace_all(
        &rewritten,
        |caps: &regex::Captures<'_>| match rewrite_srcset(&caps["value"], base_url, ctx, catalog) {
          Ok(value) => format!("{}{}{}", &caps["prefix"], value, &caps["suffix"]),
          Err(err) => {
            srcset_error = Some(err);
            caps[0].to_string()
          }
        },
      )
      .to_string();
    if let Some(err) = srcset_error {
      return Err(err);
    }
  } else {
    rewritten = rewrite_html_resource_attrs(&rewritten, base_url, ctx, catalog)?;
  }

  let mut style_attr_error: Option<anyhow::Error> = None;
  let style_attr_double =
    Regex::new("(?is)(?P<prefix>\\sstyle\\s*=\\s*\")(?P<css>[^\"]*)(?P<suffix>\")")
      .expect("style attr regex must compile");
  rewritten = style_attr_double
    .replace_all(&rewritten, |caps: &regex::Captures<'_>| {
      let css = &caps["css"];
      match rewrite_css(css, base_url, catalog, ctx) {
        Ok(css) => format!("{}{}{}", &caps["prefix"], css, &caps["suffix"]),
        Err(err) => {
          style_attr_error = Some(err);
          caps[0].to_string()
        }
      }
    })
    .to_string();

  let style_attr_single =
    Regex::new("(?is)(?P<prefix>\\sstyle\\s*=\\s*')(?P<css>[^']*)(?P<suffix>')")
      .expect("style attr regex must compile");
  rewritten = style_attr_single
    .replace_all(&rewritten, |caps: &regex::Captures<'_>| {
      let css = &caps["css"];
      match rewrite_css(css, base_url, catalog, ctx) {
        Ok(css) => format!("{}{}{}", &caps["prefix"], css, &caps["suffix"]),
        Err(err) => {
          style_attr_error = Some(err);
          caps[0].to_string()
        }
      }
    })
    .to_string();

  if let Some(err) = style_attr_error {
    return Err(err);
  }

  let style_tag_regex =
    Regex::new("(?is)(?P<prefix><style[^>]*>)(?P<body>.*?)(?P<suffix></style>)")
      .expect("style tag regex must compile");
  let mut style_tag_error: Option<anyhow::Error> = None;
  rewritten = style_tag_regex
    .replace_all(&rewritten, |caps: &regex::Captures<'_>| {
      match rewrite_css(&caps["body"], base_url, catalog, ctx) {
        Ok(css) => format!("{}{}{}", &caps["prefix"], css, &caps["suffix"]),
        Err(err) => {
          style_tag_error = Some(err);
          caps[0].to_string()
        }
      }
    })
    .to_string();
  if let Some(err) = style_tag_error {
    return Err(err);
  }

  Ok(rewritten)
}

fn apply_rewrite(
  regex: &Regex,
  input: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
  force_value: Option<&str>,
) -> Result<String> {
  let mut error: Option<anyhow::Error> = None;
  let rewritten = regex
    .replace_all(input, |caps: &regex::Captures<'_>| {
      let new_value = if let Some(forced) = force_value {
        Some(forced.to_string())
      } else {
        match rewrite_reference(&caps["url"], base_url, ctx, catalog) {
          Ok(value) => value,
          Err(err) => {
            error = Some(err);
            None
          }
        }
      };

      match new_value {
        Some(value) => format!("{}{}{}", &caps["prefix"], value, &caps["suffix"]),
        None => caps[0].to_string(),
      }
    })
    .to_string();

  if let Some(err) = error {
    return Err(err);
  }

  Ok(rewritten)
}

fn rewrite_srcset(
  input: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
) -> Result<String> {
  rewrite_srcset_with_limit(input, base_url, ctx, catalog, usize::MAX)
}

fn rewrite_css(
  input: &str,
  base_url: &Url,
  catalog: &mut AssetCatalog,
  ctx: ReferenceContext,
) -> Result<String> {
  let url_regex =
    // Some real pages ship malformed url() values (e.g. missing the closing ')'). We still want to
    // rewrite the URL so the imported fixture is fully offline/deterministic.
    Regex::new("(?i)(?P<prefix>url\\(\\s*[\"']?)(?P<url>[^\"')]+)(?P<suffix>[\"']?\\s*\\)?)")
      .expect("url regex must compile");
  let import_regex =
    Regex::new("(?i)(?P<prefix>@import\\s*['\"])(?P<url>[^\"']+)(?P<suffix>['\"])")
      .expect("import regex must compile");

  let mut rewritten = apply_rewrite(&url_regex, input, base_url, ctx, catalog, None)?;
  rewritten = apply_rewrite(&import_regex, &rewritten, base_url, ctx, catalog, None)?;
  Ok(rewritten)
}

fn rewrite_reference(
  raw: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
) -> Result<Option<String>> {
  let decoded = decode_html_entities_if_needed(raw.trim());
  let trimmed = decoded.trim();
  if trimmed.is_empty()
    || trimmed.starts_with('#')
    || trimmed.to_ascii_lowercase().starts_with("javascript:")
    || is_data_url(trimmed)
    || trimmed.to_ascii_lowercase().starts_with("about:")
    || trimmed.to_ascii_lowercase().starts_with("mailto:")
    || trimmed.to_ascii_lowercase().starts_with("tel:")
    || trimmed.contains("data:")
  {
    return Ok(None);
  }

  let Some(resolved) = resolve_href(base_url.as_str(), trimmed) else {
    if catalog.allow_missing {
      return Ok(None);
    }
    bail!("could not resolve URL '{trimmed}' against base {base_url}");
  };

  if is_data_url(&resolved) {
    return Ok(None);
  }

  let (without_fragment, fragment) = split_fragment(&resolved);
  let mut path = match catalog.path_for(&without_fragment, ctx) {
    Some(path) => path,
    None => return Ok(None),
  };

  if let Some(fragment) = fragment {
    path.push('#');
    path.push_str(&fragment);
  }

  Ok(Some(path))
}

fn rewrite_reference_optional(
  raw: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
) -> Result<Option<String>> {
  let decoded = decode_html_entities_if_needed(raw.trim());
  let trimmed = decoded.trim();
  if trimmed.is_empty()
    || trimmed.starts_with('#')
    || trimmed.to_ascii_lowercase().starts_with("javascript:")
    || is_data_url(trimmed)
    || trimmed.to_ascii_lowercase().starts_with("about:")
    || trimmed.to_ascii_lowercase().starts_with("mailto:")
    || trimmed.to_ascii_lowercase().starts_with("tel:")
    || trimmed.contains("data:")
  {
    return Ok(None);
  }

  let Some(resolved) = resolve_href(base_url.as_str(), trimmed) else {
    return Ok(None);
  };

  if is_data_url(&resolved) {
    return Ok(None);
  }

  let (without_fragment, fragment) = split_fragment(&resolved);
  let mut path = catalog.path_for_optional(&without_fragment, ctx);

  if let Some(fragment) = fragment {
    path.push('#');
    path.push_str(&fragment);
  }

  Ok(Some(path))
}

fn split_fragment(url: &str) -> (String, Option<String>) {
  match url.find('#') {
    Some(idx) => (url[..idx].to_string(), Some(url[idx + 1..].to_string())),
    None => (url.to_string(), None),
  }
}

fn find_base_url(html: &str, document_base: &Url) -> Url {
  let regex = Regex::new("(?is)<base[^>]*href\\s*=\\s*[\"']([^\"'>]+)[\"']")
    .expect("base regex must compile");
  if let Some(caps) = regex.captures(html) {
    if let Some(href) = caps.get(1) {
      if let Some(resolved) = resolve_href(document_base.as_str(), href.as_str()) {
        if let Ok(parsed) = Url::parse(&resolved) {
          return parsed;
        }
      }
    }
  }
  document_base.clone()
}

fn extension_from_path(path: &str) -> String {
  if let Ok(url) = Url::parse(path) {
    if let Some(ext) = Path::new(url.path()).extension().and_then(|e| e.to_str()) {
      let ext = ext.to_ascii_lowercase();
      if ext.is_empty() {
        return "bin".to_string();
      }
      return ext;
    }
    return "bin".to_string();
  }

  Path::new(path)
    .extension()
    .and_then(|e| e.to_str())
    .unwrap_or("bin")
    .to_ascii_lowercase()
}

fn extension_from_resource(path: &str, content_type: Option<&str>) -> String {
  let ext = extension_from_path(path);
  if ext != "bin" {
    return ext;
  }

  let Some(ct) = content_type else {
    return ext;
  };
  let lower = ct.to_ascii_lowercase();
  if lower.contains("html") {
    return "html".to_string();
  }
  if lower.contains("css") {
    return "css".to_string();
  }
  ext
}

fn is_html_extension(ext: &str) -> bool {
  matches!(ext, "html" | "htm" | "xhtml")
}

fn is_html_asset(asset: &AssetData) -> bool {
  if asset
    .content_type
    .as_deref()
    .map(|ct| ct.to_ascii_lowercase().contains("html"))
    .unwrap_or(false)
  {
    return true;
  }
  let lower = asset.filename.to_ascii_lowercase();
  lower.ends_with(".html") || lower.ends_with(".htm") || lower.ends_with(".xhtml")
}

fn hash_bytes(bytes: &[u8]) -> String {
  let digest = Sha256::digest(bytes);
  digest
    .iter()
    .take(HASH_PREFIX_BYTES)
    .map(|b| format!("{b:02x}"))
    .collect::<String>()
}

fn rewrite_and_strip_link_tags(
  input: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
  screen_stylesheets: &HashSet<String>,
) -> Result<String> {
  fn capture_first_match<'t>(
    caps: &'t regex::Captures<'t>,
    groups: &[usize],
  ) -> Option<regex::Match<'t>> {
    groups.iter().find_map(|idx| caps.get(*idx))
  }

  let link_tag = Regex::new("(?is)<link\\b[^>]*>").expect("link tag regex must compile");
  let attr_rel = Regex::new("(?is)(?:^|\\s)rel\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("link rel regex must compile");
  let attr_href =
    Regex::new("(?is)(?:^|\\s)href\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("link href regex must compile");

  let mut out = String::with_capacity(input.len());
  let mut last = 0usize;
  for tag_match in link_tag.find_iter(input) {
    let tag = &input[tag_match.start()..tag_match.end()];

    let rel_value = attr_rel
      .captures(tag)
      .and_then(|c| capture_first_match(&c, &[1, 2, 3]).map(|m| m.as_str().to_string()))
      .unwrap_or_default();

    let mut has_stylesheet = false;
    let mut is_strippable_fetchable = false;
    for token in rel_value.split_ascii_whitespace() {
      if token.eq_ignore_ascii_case("stylesheet") {
        has_stylesheet = true;
      } else if token.eq_ignore_ascii_case("preload")
        || token.eq_ignore_ascii_case("prefetch")
        || token.eq_ignore_ascii_case("icon")
        || token.eq_ignore_ascii_case("apple-touch-icon")
        || token.eq_ignore_ascii_case("apple-touch-icon-precomposed")
        || token.eq_ignore_ascii_case("manifest")
        || token.eq_ignore_ascii_case("mask-icon")
        || token.eq_ignore_ascii_case("preconnect")
        || token.eq_ignore_ascii_case("dns-prefetch")
      {
        is_strippable_fetchable = true;
      }
    }

    // Strip link tags that would trigger network loads in browsers but are not needed for
    // FastRender output correctness (preloads, icons, preconnect/dns-prefetch, etc).
    if !has_stylesheet && is_strippable_fetchable {
      out.push_str(&input[last..tag_match.start()]);
      last = tag_match.end();
      continue;
    }

    let mut rewritten_tag = tag.to_string();
    if has_stylesheet {
      if let Some(href_caps) = attr_href.captures(tag) {
        if let Some(href_match) = capture_first_match(&href_caps, &[1, 2, 3]) {
          let decoded = decode_html_entities_if_needed(href_match.as_str());
          let resolved =
            resolve_href(base_url.as_str(), decoded.trim()).unwrap_or_else(|| "".to_string());
          let required = !resolved.is_empty() && screen_stylesheets.contains(&resolved);
          let rewritten_href = if required {
            rewrite_reference(href_match.as_str(), base_url, ctx, catalog)?
          } else {
            rewrite_reference_optional(href_match.as_str(), base_url, ctx, catalog)?
          };
          if let Some(new_value) = rewritten_href {
            let start = href_match.start();
            let end = href_match.end();
            rewritten_tag = format!(
              "{}{}{}",
              &rewritten_tag[..start],
              new_value,
              &rewritten_tag[end..]
            );
          }
        }
      }
    }

    out.push_str(&input[last..tag_match.start()]);
    out.push_str(&rewritten_tag);
    last = tag_match.end();
  }
  out.push_str(&input[last..]);
  Ok(out)
}

fn rewrite_html_resource_attrs(
  input: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
) -> Result<String> {
  let stylesheet_urls: HashSet<String> = fastrender::css::loader::extract_css_links(
    input,
    base_url.as_str(),
    fastrender::style::media::MediaType::Screen,
  )
  .unwrap_or_default()
  .into_iter()
  .collect();

  let mut rewritten =
    rewrite_and_strip_link_tags(input, base_url, ctx, catalog, &stylesheet_urls)?;

  let img_src = Regex::new("(?is)<img[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("img src regex must compile");
  rewritten = replace_attr_values_with(&img_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference(raw, base_url, ctx, catalog)
  })?;

  let iframe_src =
    Regex::new("(?is)<iframe[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("iframe src regex must compile");
  rewritten = replace_attr_values_with(&iframe_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference(raw, base_url, ctx, catalog)
  })?;

  let embed_src =
    Regex::new("(?is)<embed[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("embed src regex must compile");
  rewritten = replace_attr_values_with(&embed_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference(raw, base_url, ctx, catalog)
  })?;

  let object_data =
    Regex::new("(?is)<object[^>]*\\sdata\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("object data regex must compile");
  rewritten = replace_attr_values_with(&object_data, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference(raw, base_url, ctx, catalog)
  })?;

  let video_poster =
    Regex::new("(?is)<video[^>]*\\sposter\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("video poster regex must compile");
  rewritten = replace_attr_values_with(&video_poster, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference(raw, base_url, ctx, catalog)
  })?;

  // Media sources are fetchable in browsers but generally not required for FastRender's output. Use
  // deterministic placeholders so imported fixtures remain offline even if the bundle didn't
  // capture the media.
  let video_src =
    Regex::new("(?is)<video[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("video src regex must compile");
  rewritten = replace_attr_values_with(&video_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference_optional(raw, base_url, ctx, catalog)
  })?;

  let audio_src =
    Regex::new("(?is)<audio[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("audio src regex must compile");
  rewritten = replace_attr_values_with(&audio_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference_optional(raw, base_url, ctx, catalog)
  })?;

  let track_src =
    Regex::new("(?is)<track[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("track src regex must compile");
  rewritten = replace_attr_values_with(&track_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference_optional(raw, base_url, ctx, catalog)
  })?;

  let source_src =
    Regex::new("(?is)<source[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("source src regex must compile");
  rewritten = replace_attr_values_with(&source_src, &rewritten, &[1, 2, 3], |raw| {
    rewrite_reference_optional(raw, base_url, ctx, catalog)
  })?;

  // The bundler only captures a limited number of srcset candidates (to avoid pathological pages
  // exploding bundle size). Keep the rewritten fixture aligned with that by truncating srcsets so
  // FastRender won't pick a missing candidate at render time.
  const IMG_SRCSET_MAX_CANDIDATES: usize = 1;
  const SRCSET_MAX_CANDIDATES: usize = 32;
  let img_srcset = Regex::new("(?is)<img[^>]*\\ssrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
    .expect("img srcset regex must compile");
  rewritten = replace_attr_values_with(&img_srcset, &rewritten, &[1, 2], |raw| {
    rewrite_srcset_with_limit(raw, base_url, ctx, catalog, IMG_SRCSET_MAX_CANDIDATES)
      .map(Some)
      .or_else(|err| Err(err))
  })?;

  let source_srcset = Regex::new("(?is)<source[^>]*\\ssrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
    .expect("source srcset regex must compile");
  rewritten = replace_attr_values_with(&source_srcset, &rewritten, &[1, 2], |raw| {
    // Unlike the legacy srcset rewrite, keep the candidate cap low to avoid pathological inputs.
    rewrite_srcset_with_limit(raw, base_url, ctx, catalog, SRCSET_MAX_CANDIDATES)
      .map(Some)
      .or_else(|err| Err(err))
  })?;

  Ok(rewritten)
}

fn replace_attr_values_with<F>(
  regex: &Regex,
  input: &str,
  groups: &[usize],
  mut rewrite: F,
) -> Result<String>
where
  F: FnMut(&str) -> Result<Option<String>>,
{
  if !regex.is_match(input) {
    return Ok(input.to_string());
  }

  let mut out = String::with_capacity(input.len());
  let mut last = 0;
  for caps in regex.captures_iter(input) {
    let Some(m) = groups.iter().find_map(|&idx| caps.get(idx)) else {
      continue;
    };
    out.push_str(&input[last..m.start()]);
    match rewrite(m.as_str())? {
      Some(new_value) => out.push_str(&new_value),
      None => out.push_str(m.as_str()),
    }
    last = m.end();
  }
  out.push_str(&input[last..]);
  Ok(out)
}

fn validate_no_remote_fetchable_subresources_in_html(label: &str, html: &str) -> Result<()> {
  let mut remote: BTreeSet<String> = BTreeSet::new();

  fn capture_first_match<'t>(
    caps: &'t regex::Captures<'t>,
    groups: &[usize],
  ) -> Option<regex::Match<'t>> {
    groups.iter().find_map(|idx| caps.get(*idx))
  }

  // Fetchable <link> resources (stylesheets, icons, preloads, prefetch, preconnect, etc.).
  // We intentionally validate the authored href/srcset strings to catch scheme-relative URLs.
  let link_tag = Regex::new("(?is)<link\\b[^>]*>").expect("link tag validation regex");
  let attr_rel = Regex::new("(?is)(?:^|\\s)rel\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("link rel validation regex");
  let attr_href = Regex::new("(?is)(?:^|\\s)href\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("link href validation regex");
  let attr_imagesrcset =
    Regex::new("(?is)(?:^|\\s)imagesrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
      .expect("link imagesrcset validation regex");

  for tag_match in link_tag.find_iter(html) {
    let tag = tag_match.as_str();
    let rel_value = attr_rel
      .captures(tag)
      .and_then(|c| capture_first_match(&c, &[1, 2, 3]).map(|m| m.as_str().to_string()))
      .unwrap_or_default();

    let mut is_fetchable = false;
    for token in rel_value.split_ascii_whitespace() {
      if token.eq_ignore_ascii_case("stylesheet")
        || token.eq_ignore_ascii_case("preload")
        || token.eq_ignore_ascii_case("prefetch")
        || token.eq_ignore_ascii_case("icon")
        || token.eq_ignore_ascii_case("apple-touch-icon")
        || token.eq_ignore_ascii_case("apple-touch-icon-precomposed")
        || token.eq_ignore_ascii_case("manifest")
        || token.eq_ignore_ascii_case("mask-icon")
        || token.eq_ignore_ascii_case("preconnect")
        || token.eq_ignore_ascii_case("dns-prefetch")
      {
        is_fetchable = true;
        break;
      }
    }
    if !is_fetchable {
      continue;
    }

    if let Some(href_caps) = attr_href.captures(tag) {
      if let Some(m) = capture_first_match(&href_caps, &[1, 2, 3]) {
        let decoded = decode_html_entities_if_needed(m.as_str());
        let trimmed = decoded.trim();
        if is_remote_fetch_url(trimmed) {
          remote.insert(trimmed.to_string());
        }
      }
    }

    if let Some(srcset_caps) = attr_imagesrcset.captures(tag) {
      if let Some(m) = capture_first_match(&srcset_caps, &[1, 2]) {
        for candidate in parse_srcset_urls(m.as_str(), 32) {
          let decoded = decode_html_entities_if_needed(candidate.trim());
          let trimmed = decoded.trim();
          if is_remote_fetch_url(trimmed) {
            remote.insert(trimmed.to_string());
          }
        }
      }
    }
  }

  let img_src = Regex::new("(?is)<img[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("img src validation regex");
  collect_remote_attr_values(&img_src, html, &[1, 2, 3], &mut remote);

  let iframe_src =
    Regex::new("(?is)<iframe[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("iframe src validation regex");
  collect_remote_attr_values(&iframe_src, html, &[1, 2, 3], &mut remote);

  let embed_src =
    Regex::new("(?is)<embed[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("embed src validation regex");
  collect_remote_attr_values(&embed_src, html, &[1, 2, 3], &mut remote);

  let object_data =
    Regex::new("(?is)<object[^>]*\\sdata\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("object data validation regex");
  collect_remote_attr_values(&object_data, html, &[1, 2, 3], &mut remote);

  let video_poster =
    Regex::new("(?is)<video[^>]*\\sposter\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
      .expect("video poster validation regex");
  collect_remote_attr_values(&video_poster, html, &[1, 2, 3], &mut remote);

  let video_src = Regex::new("(?is)<video[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("video src validation regex");
  collect_remote_attr_values(&video_src, html, &[1, 2, 3], &mut remote);

  let audio_src = Regex::new("(?is)<audio[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("audio src validation regex");
  collect_remote_attr_values(&audio_src, html, &[1, 2, 3], &mut remote);

  let track_src = Regex::new("(?is)<track[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("track src validation regex");
  collect_remote_attr_values(&track_src, html, &[1, 2, 3], &mut remote);

  let source_src = Regex::new("(?is)<source[^>]*\\ssrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("source src validation regex");
  collect_remote_attr_values(&source_src, html, &[1, 2, 3], &mut remote);

  let img_srcset = Regex::new("(?is)<img[^>]*\\ssrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
    .expect("img srcset validation regex");
  collect_remote_srcset_candidates(&img_srcset, html, &[1, 2], &mut remote);

  let source_srcset = Regex::new("(?is)<source[^>]*\\ssrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
    .expect("source srcset validation regex");
  collect_remote_srcset_candidates(&source_srcset, html, &[1, 2], &mut remote);

  // Inline CSS inside <style> and style="".
  let style_tag = Regex::new("(?is)<style[^>]*>(.*?)</style>").expect("style tag validation regex");
  for caps in style_tag.captures_iter(html) {
    if let Some(css) = caps.get(1).map(|m| m.as_str()) {
      for url in extract_fetchable_css_urls(css) {
        if is_remote_fetch_url(&url) {
          remote.insert(url);
        }
      }
    }
  }

  let style_attr_double =
    Regex::new("(?is)\\sstyle\\s*=\\s*\"([^\"]*)\"").expect("style attr validation regex");
  for caps in style_attr_double.captures_iter(html) {
    if let Some(css) = caps.get(1).map(|m| m.as_str()) {
      for url in extract_fetchable_css_urls(css) {
        if is_remote_fetch_url(&url) {
          remote.insert(url);
        }
      }
    }
  }
  let style_attr_single =
    Regex::new("(?is)\\sstyle\\s*=\\s*'([^']*)'").expect("style attr validation regex");
  for caps in style_attr_single.captures_iter(html) {
    if let Some(css) = caps.get(1).map(|m| m.as_str()) {
      for url in extract_fetchable_css_urls(css) {
        if is_remote_fetch_url(&url) {
          remote.insert(url);
        }
      }
    }
  }

  if remote.is_empty() {
    return Ok(());
  }

  let mut msg = format!("{label} still contains remote fetchable subresources:\n");
  for url in &remote {
    msg.push_str("  - ");
    msg.push_str(url);
    msg.push('\n');
  }
  bail!(msg)
}

fn collect_remote_attr_values(
  regex: &Regex,
  input: &str,
  groups: &[usize],
  out: &mut BTreeSet<String>,
) {
  for caps in regex.captures_iter(input) {
    let Some(m) = groups.iter().find_map(|&idx| caps.get(idx)) else {
      continue;
    };
    let decoded = decode_html_entities_if_needed(m.as_str());
    let trimmed = decoded.trim();
    if is_remote_fetch_url(trimmed) {
      out.insert(trimmed.to_string());
    }
  }
}

fn collect_remote_srcset_candidates(
  regex: &Regex,
  input: &str,
  groups: &[usize],
  out: &mut BTreeSet<String>,
) {
  for caps in regex.captures_iter(input) {
    let Some(m) = groups.iter().find_map(|&idx| caps.get(idx)) else {
      continue;
    };
    for candidate in parse_srcset_urls(m.as_str(), 32) {
      let decoded = decode_html_entities_if_needed(candidate.trim());
      let trimmed = decoded.trim();
      if is_remote_fetch_url(trimmed) {
        out.insert(trimmed.to_string());
      }
    }
  }
}

fn parse_srcset_urls(srcset: &str, max_candidates: usize) -> Vec<String> {
  fastrender::html::image_attrs::parse_srcset_with_limit(srcset, max_candidates)
    .into_iter()
    .map(|candidate| candidate.url)
    .collect()
}

fn rewrite_srcset_with_limit(
  input: &str,
  base_url: &Url,
  ctx: ReferenceContext,
  catalog: &mut AssetCatalog,
  max_candidates: usize,
) -> Result<String> {
  use fastrender::tree::box_tree::SrcsetDescriptor;

  let mut rewritten = Vec::new();
  for candidate in fastrender::html::image_attrs::parse_srcset_with_limit(input, max_candidates) {
    let rewritten_url = rewrite_reference(&candidate.url, base_url, ctx, catalog)?
      .unwrap_or_else(|| candidate.url);
    let entry = match candidate.descriptor {
      SrcsetDescriptor::Density(d) if d == 1.0 => rewritten_url,
      descriptor => format!("{rewritten_url} {descriptor}"),
    };
    rewritten.push(entry);
  }
  Ok(rewritten.join(", "))
}

fn is_remote_fetch_url(url: &str) -> bool {
  let lower = url.trim_start().to_ascii_lowercase();
  lower.starts_with("http://") || lower.starts_with("https://") || lower.starts_with("//")
}

fn decode_html_entities_if_needed(input: &str) -> Cow<'_, str> {
  if !input.contains('&') {
    return Cow::Borrowed(input);
  }
  Cow::Owned(decode_html_entities(input))
}

fn decode_html_entities(input: &str) -> String {
  let mut out = String::with_capacity(input.len());
  let mut chars = input.chars().peekable();
  while let Some(c) = chars.next() {
    if c != '&' {
      out.push(c);
      continue;
    }

    let mut entity = String::new();
    while let Some(&next) = chars.peek() {
      entity.push(next);
      chars.next();
      if next == ';' {
        break;
      }
    }

    if entity.is_empty() {
      out.push('&');
      continue;
    }

    let mut ent = entity.as_str();
    if let Some(stripped) = ent.strip_prefix('/') {
      ent = stripped;
    }

    let decoded = match ent {
      "amp;" => Some('&'),
      "quot;" => Some('"'),
      "apos;" => Some('\''),
      "lt;" => Some('<'),
      "gt;" => Some('>'),
      _ => {
        if let Some(num) = ent.strip_prefix('#') {
          let trimmed = num.trim_end_matches(';');
          if let Some(hex) = trimmed.strip_prefix(['x', 'X']) {
            u32::from_str_radix(hex, 16).ok().and_then(char::from_u32)
          } else {
            trimmed.parse::<u32>().ok().and_then(char::from_u32)
          }
        } else {
          None
        }
      }
    };

    if let Some(ch) = decoded {
      out.push(ch);
    } else {
      out.push('&');
      out.push_str(&entity);
    }
  }
  out
}

fn extract_fetchable_css_urls(css: &str) -> Vec<String> {
  use cssparser::{Parser, ParserInput, Token};

  fn record(out: &mut Vec<String>, raw: &str) {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
      return;
    }
    out.push(trimmed.to_string());
  }

  fn scan<'i, 't>(parser: &mut Parser<'i, 't>, out: &mut Vec<String>) {
    while !parser.is_exhausted() {
      let token = match parser.next_including_whitespace_and_comments() {
        Ok(t) => t,
        Err(_) => break,
      };

      match token {
        Token::UnquotedUrl(url) => record(out, url.as_ref()),
        Token::Function(name) if name.eq_ignore_ascii_case("url") => {
          let parse_result = parser.parse_nested_block(|nested| {
            let mut arg: Option<String> = None;
            while !nested.is_exhausted() {
              match nested.next_including_whitespace_and_comments() {
                Ok(Token::WhiteSpace(_)) | Ok(Token::Comment(_)) => {}
                Ok(Token::QuotedString(s)) | Ok(Token::UnquotedUrl(s)) => {
                  arg = Some(s.as_ref().to_string());
                  break;
                }
                Ok(Token::Ident(s)) => {
                  arg = Some(s.as_ref().to_string());
                  break;
                }
                Ok(Token::BadUrl(_)) | Err(_) => break,
                Ok(_) => {}
              }
            }
            Ok::<_, cssparser::ParseError<'i, ()>>(arg)
          });

          if let Ok(Some(arg)) = parse_result {
            record(out, &arg);
          }
        }
        Token::AtKeyword(name) if name.eq_ignore_ascii_case("import") => {
          let mut target: Option<String> = None;
          while !parser.is_exhausted() {
            let next = match parser.next_including_whitespace_and_comments() {
              Ok(t) => t,
              Err(_) => break,
            };
            match next {
              Token::WhiteSpace(_) | Token::Comment(_) => continue,
              Token::QuotedString(s) | Token::UnquotedUrl(s) => {
                target = Some(s.as_ref().to_string());
                break;
              }
              Token::Function(fname) if fname.eq_ignore_ascii_case("url") => {
                let parse_result = parser.parse_nested_block(|nested| {
                  let mut arg: Option<String> = None;
                  while !nested.is_exhausted() {
                    match nested.next_including_whitespace_and_comments() {
                      Ok(Token::WhiteSpace(_)) | Ok(Token::Comment(_)) => {}
                      Ok(Token::QuotedString(s)) | Ok(Token::UnquotedUrl(s)) => {
                        arg = Some(s.as_ref().to_string());
                        break;
                      }
                      Ok(Token::Ident(s)) => {
                        arg = Some(s.as_ref().to_string());
                        break;
                      }
                      Ok(Token::BadUrl(_)) | Err(_) => break,
                      Ok(_) => {}
                    }
                  }
                  Ok::<_, cssparser::ParseError<'i, ()>>(arg)
                });
                target = parse_result.ok().flatten();
                break;
              }
              Token::Ident(s) => {
                target = Some(s.as_ref().to_string());
                break;
              }
              Token::Semicolon => break,
              _ => break,
            }
          }
          if let Some(target) = target {
            record(out, &target);
          }
        }
        Token::Function(_)
        | Token::ParenthesisBlock
        | Token::SquareBracketBlock
        | Token::CurlyBracketBlock => {
          let _ = parser.parse_nested_block(|nested| {
            scan(nested, out);
            Ok::<_, cssparser::ParseError<'i, ()>>(())
          });
        }
        _ => {}
      }
    }
  }

  let mut out = Vec::new();
  let mut input = ParserInput::new(css);
  let mut parser = Parser::new(&mut input);
  scan(&mut parser, &mut out);
  out
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json::json;
  use std::fs;
  use tempfile::tempdir;

  fn write_synthetic_bundle(dir: &Path, include_font: bool) -> Result<()> {
    let resources_dir = dir.join("resources");
    fs::create_dir_all(&resources_dir)?;

    let document_html = r#"<!doctype html>
<html>
  <head>
    <link rel="preconnect" href="https://cdn.example.test">
    <link rel="preload" href="https://example.test/preload.png" as="image">
    <link rel="icon" href="https://example.test/favicon.ico">
    <link rel="stylesheet" href="https://example.test/print.css" media="print">
  </head>
  <body>
    <iframe src="https://example.test/frame.html"></iframe>
  </body>
</html>
"#;
    fs::write(dir.join("document.html"), document_html)?;

    let frame_html = r#"<!doctype html>
<html>
  <head>
    <style>@font-face{src:url(//cdn.example.test/font.woff2)}</style>
  </head>
  <body>
    <img src="https://example.test/img.png">
    <video controls><source src="https://example.test/movie.mp4" type="video/mp4"></video>
  </body>
</html>
"#;
    fs::write(resources_dir.join("00000_frame.html"), frame_html)?;

    fs::write(resources_dir.join("00001_img.png"), b"dummy png")?;
    if include_font {
      fs::write(resources_dir.join("00002_font.woff2"), b"dummy font")?;
    }

    let mut resources = serde_json::Map::new();
    resources.insert(
      "https://example.test/frame.html".to_string(),
      json!({
        "path": "resources/00000_frame.html",
        "content_type": "text/html; charset=utf-8",
        "status": 200,
        "final_url": "https://example.test/frame.html",
        "etag": null,
        "last_modified": null
      }),
    );
    resources.insert(
      "https://example.test/img.png".to_string(),
      json!({
        "path": "resources/00001_img.png",
        "content_type": "image/png",
        "status": 200,
        "final_url": "https://example.test/img.png",
        "etag": null,
        "last_modified": null
      }),
    );
    if include_font {
      resources.insert(
        "https://cdn.example.test/font.woff2".to_string(),
        json!({
          "path": "resources/00002_font.woff2",
          "content_type": "font/woff2",
          "status": 200,
          "final_url": "https://cdn.example.test/font.woff2",
          "etag": null,
          "last_modified": null
        }),
      );
    }

    let manifest = json!({
      "version": 1,
      "original_url": "https://example.test/",
      "document": {
        "path": "document.html",
        "content_type": "text/html; charset=utf-8",
        "final_url": "https://example.test/",
        "status": 200,
        "etag": null,
        "last_modified": null
      },
      "render": {
        "viewport": [800, 600],
        "device_pixel_ratio": 1.0,
        "scroll_x": 0.0,
        "scroll_y": 0.0,
        "full_page": false
      },
      "resources": resources
    });
    fs::write(
      dir.join("bundle.json"),
      serde_json::to_vec_pretty(&manifest).expect("manifest json"),
    )?;

    Ok(())
  }

  fn assert_no_remote_url_strings(content: &str) {
    let lower = content.to_ascii_lowercase();
    assert!(
      !lower.contains("http://"),
      "unexpected remote http:// reference: {content}"
    );
    assert!(
      !lower.contains("https://"),
      "unexpected remote https:// reference: {content}"
    );
    assert!(
      !lower.contains("url(//"),
      "unexpected scheme-relative url(): {content}"
    );
    assert!(
      !lower.contains("src=\"//") && !lower.contains("src='//"),
      "unexpected scheme-relative src= reference: {content}"
    );
    assert!(
      !lower.contains("href=\"//") && !lower.contains("href='//"),
      "unexpected scheme-relative href= reference: {content}"
    );
    assert!(
      !lower.contains("srcset=\"//") && !lower.contains("srcset='//"),
      "unexpected scheme-relative srcset= reference: {content}"
    );
  }

  #[test]
  fn imports_and_rewrites_nested_html_assets() -> Result<()> {
    let bundle_dir = tempdir()?;
    write_synthetic_bundle(bundle_dir.path(), true)?;

    let output = tempdir()?;
    let output_root = output.path().join("fixtures");
    let fixture_name = "example_test";

    run_import_page_fixture(ImportPageFixtureArgs {
      bundle: bundle_dir.path().to_path_buf(),
      fixture_name: fixture_name.to_string(),
      output_root: output_root.clone(),
      overwrite: true,
      allow_missing: false,
      allow_http_references: false,
      legacy_rewrite: false,
      dry_run: false,
    })?;

    let fixture_dir = output_root.join(fixture_name);
    let index_html = fs::read_to_string(fixture_dir.join("index.html"))?;
    assert_no_remote_url_strings(&index_html);
    assert!(
      !index_html.contains("rel=\"preconnect\"")
        && !index_html.contains("rel=\"preload\"")
        && !index_html.contains("rel=\"icon\""),
      "expected preconnect/preload/icon link tags to be stripped from output HTML"
    );

    let assets_dir = fixture_dir.join(ASSETS_DIR);
    let mut html_assets = Vec::new();
    let mut png_asset = None;
    let mut woff2_asset = None;
    for entry in fs::read_dir(&assets_dir)? {
      let entry = entry?;
      if !entry.file_type()?.is_file() {
        continue;
      }
      let filename = entry.file_name().to_string_lossy().to_string();
      if filename.ends_with(".html") {
        html_assets.push(filename);
      } else if filename.ends_with(".png") {
        png_asset = Some(filename);
      } else if filename.ends_with(".woff2") {
        woff2_asset = Some(filename);
      }
    }
    assert_eq!(html_assets.len(), 1, "expected exactly one HTML asset");

    let frame_asset = &html_assets[0];
    assert!(
      index_html.contains(&format!("{ASSETS_DIR}/{frame_asset}")),
      "index.html should rewrite iframe src to point at the local HTML asset"
    );

    let frame_html = fs::read_to_string(assets_dir.join(frame_asset))?;
    assert_no_remote_url_strings(&frame_html);

    let optional_css_url = "https://example.test/print.css";
    let optional_css_asset = format!("missing_{}.css", hash_bytes(optional_css_url.as_bytes()));
    assert!(
      index_html.contains(&format!("{ASSETS_DIR}/{optional_css_asset}")),
      "expected optional stylesheet href to be rewritten"
    );
    assert!(
      assets_dir.join(&optional_css_asset).exists(),
      "expected optional stylesheet placeholder asset to be created"
    );

    let optional_video_url = "https://example.test/movie.mp4";
    let optional_video_asset = format!("missing_{}.mp4", hash_bytes(optional_video_url.as_bytes()));
    assert!(
      frame_html.contains(&optional_video_asset),
      "expected nested HTML asset to reference optional media placeholder {optional_video_asset}"
    );
    assert!(
      assets_dir.join(&optional_video_asset).exists(),
      "expected optional media placeholder asset to be created"
    );

    let png_asset = png_asset.expect("missing png asset");
    let woff2_asset = woff2_asset.expect("missing woff2 asset");
    assert!(
      frame_html.contains(&png_asset),
      "iframe HTML should reference local image asset {png_asset}"
    );
    assert!(
      frame_html.contains(&woff2_asset),
      "iframe HTML should reference local font asset {woff2_asset}"
    );

    Ok(())
  }

  #[test]
  fn import_fails_when_nested_html_references_missing_asset() -> Result<()> {
    let bundle_dir = tempdir()?;
    write_synthetic_bundle(bundle_dir.path(), false)?;

    let output = tempdir()?;
    let output_root = output.path().join("fixtures");

    let res = run_import_page_fixture(ImportPageFixtureArgs {
      bundle: bundle_dir.path().to_path_buf(),
      fixture_name: "example_test".to_string(),
      output_root,
      overwrite: true,
      allow_missing: false,
      allow_http_references: false,
      legacy_rewrite: false,
      dry_run: false,
    });
    assert!(
      res.is_err(),
      "import should fail when a fetchable asset is missing"
    );
    Ok(())
  }

  #[test]
  fn rewrite_css_rewrites_unclosed_url_functions() -> Result<()> {
    let base = Url::parse("https://example.test/")?;
    let mut catalog = AssetCatalog::new(true);
    let css = "background-image:url(https://example.test/img";
    let rewritten = rewrite_css(css, &base, &mut catalog, ReferenceContext::Html)?;
    assert!(
      !rewritten.contains("https://example.test/img"),
      "expected url() to be rewritten, got: {rewritten}"
    );
    assert!(
      rewritten.contains("assets/missing_"),
      "expected placeholder asset to be inserted, got: {rewritten}"
    );
    Ok(())
  }

  #[test]
  fn rewrite_css_rewrites_import_without_whitespace() -> Result<()> {
    let base = Url::parse("https://example.test/")?;
    let mut catalog = AssetCatalog::new(true);
    let css = "@import\"https://example.test/style.css\";";
    let rewritten = rewrite_css(css, &base, &mut catalog, ReferenceContext::Html)?;
    assert!(
      !rewritten.contains("https://example.test/style.css"),
      "expected @import to be rewritten, got: {rewritten}"
    );
    assert!(
      rewritten.contains("assets/missing_"),
      "expected placeholder asset to be inserted, got: {rewritten}"
    );
    Ok(())
  }
} 
