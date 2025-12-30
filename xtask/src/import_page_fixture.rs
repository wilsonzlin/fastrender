use anyhow::{bail, Context, Result};
use clap::Args;
use fastrender::css::loader::resolve_href;
use fastrender::resource::bundle::{Bundle, BundledFetcher, BundledResourceInfo};
use fastrender::resource::FetchedResource;
use fastrender::resource::ResourceFetcher;
use regex::Regex;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, HashMap};
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

  let rewritten_html = rewrite_html(&document_html, &effective_base, &mut catalog)?;
  if args.allow_http_references {
    if let Err(err) = assert_no_http_urls("index.html", &rewritten_html) {
      eprintln!("Warning: {}", err);
    }
    if let Err(err) = catalog.assert_no_remote_references() {
      eprintln!("Warning: {}", err);
    }
  } else {
    assert_no_http_urls("index.html", &rewritten_html)?;
    catalog.assert_no_remote_references()?;
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
}

impl AssetCatalog {
  fn new(allow_missing: bool) -> Self {
    Self {
      assets: BTreeMap::new(),
      url_to_filename: HashMap::new(),
      allow_missing,
    }
  }

  fn add_resource(
    &mut self,
    url: &str,
    info: &BundledResourceInfo,
    res: &FetchedResource,
  ) -> Result<()> {
    let ext = extension_from_path(&info.path);
    let filename = format!("{}.{}", hash_bytes(&res.bytes), ext);
    if let Some(existing) = self.assets.get(&filename) {
      if existing.bytes != res.bytes {
        bail!(
          "hash collision while importing {} ({}); existing asset has different contents",
          url,
          filename
        );
      }
    }

    let source_url = info.final_url.clone().unwrap_or_else(|| url.to_string());
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
    if let Some(final_url) = &info.final_url {
      self
        .url_to_filename
        .entry(final_url.clone())
        .or_insert_with(|| filename.clone());
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
        .map(|ct| ct.contains("css"))
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

  fn path_for(&mut self, url: &str, ctx: ReferenceContext) -> Option<String> {
    if let Some(filename) = self.url_to_filename.get(url) {
      return match ctx {
        ReferenceContext::Html => Some(format!("{ASSETS_DIR}/{filename}")),
        ReferenceContext::Css => Some(filename.clone()),
      };
    }

    if !self.allow_missing {
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

  fn ensure_local_path(&mut self, url: &str, ctx: ReferenceContext) -> String {
    if let Some(path) = self.path_for(url, ctx) {
      return path;
    }

    let filename = format!("missing_{}.bin", hash_bytes(url.as_bytes()));
    let data = AssetData {
      filename: filename.clone(),
      bytes: Vec::new(),
      content_type: None,
      source_url: url.to_string(),
    };
    self.assets.insert(filename.clone(), data);
    self
      .url_to_filename
      .insert(url.to_string(), filename.clone());

    match ctx {
      ReferenceContext::Html => format!("{ASSETS_DIR}/{filename}"),
      ReferenceContext::Css => filename,
    }
  }

  fn assert_no_remote_references(&self) -> Result<()> {
    for asset in self.assets.values() {
      if asset.filename.ends_with(".html") {
        continue;
      }
      if is_textual(asset) {
        let text = String::from_utf8_lossy(&asset.bytes);
        assert_no_http_urls(&asset.filename, &text)?;
      }
    }
    Ok(())
  }
}

fn rewrite_html(input: &str, base_url: &Url, catalog: &mut AssetCatalog) -> Result<String> {
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
    ReferenceContext::Html,
    catalog,
    Some("."),
  )?;

  let attr_regex = Regex::new(
    "(?is)(?P<prefix>(?:src|href|poster|data)\\s*=\\s*[\"'])(?P<url>[^\"'>]+)(?P<suffix>[\"'])",
  )
  .expect("attr regex must compile");
  rewritten = apply_rewrite(
    &attr_regex,
    &rewritten,
    base_url,
    ReferenceContext::Html,
    catalog,
    None,
  )?;

  let content_url_regex =
    Regex::new("(?is)(?P<prefix>content\\s*=\\s*[\"'])(?P<url>https?://[^\"'>]+)(?P<suffix>[\"'])")
      .expect("content url regex must compile");
  rewritten = apply_rewrite(
    &content_url_regex,
    &rewritten,
    base_url,
    ReferenceContext::Html,
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
      |caps: &regex::Captures<'_>| match rewrite_srcset(&caps["value"], base_url, catalog) {
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

  let mut style_attr_error: Option<anyhow::Error> = None;
  let style_attr_double =
    Regex::new("(?is)(?P<prefix>style\\s*=\\s*\")(?P<css>[^\"]*)(?P<suffix>\")")
      .expect("style attr regex must compile");
  rewritten = style_attr_double
    .replace_all(&rewritten, |caps: &regex::Captures<'_>| {
      let css = &caps["css"];
      match rewrite_css(css, base_url, catalog, ReferenceContext::Html) {
        Ok(css) => format!("{}{}{}", &caps["prefix"], css, &caps["suffix"]),
        Err(err) => {
          style_attr_error = Some(err);
          caps[0].to_string()
        }
      }
    })
    .to_string();

  let style_attr_single = Regex::new("(?is)(?P<prefix>style\\s*=\\s*')(?P<css>[^']*)(?P<suffix>')")
    .expect("style attr regex must compile");
  rewritten = style_attr_single
    .replace_all(&rewritten, |caps: &regex::Captures<'_>| {
      let css = &caps["css"];
      match rewrite_css(css, base_url, catalog, ReferenceContext::Html) {
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
      match rewrite_css(&caps["body"], base_url, catalog, ReferenceContext::Html) {
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

fn rewrite_srcset(input: &str, base_url: &Url, catalog: &mut AssetCatalog) -> Result<String> {
  let mut rewritten = Vec::new();
  for candidate in input.split(',') {
    let trimmed = candidate.trim();
    if trimmed.is_empty() {
      continue;
    }
    let mut parts = trimmed.split_whitespace();
    let Some(url_part) = parts.next() else {
      continue;
    };
    let rewritten_url = rewrite_reference(url_part, base_url, ReferenceContext::Html, catalog)?
      .unwrap_or_else(|| url_part.to_string());
    let descriptors = parts.collect::<Vec<_>>().join(" ");
    let entry = if descriptors.is_empty() {
      rewritten_url
    } else {
      format!("{rewritten_url} {descriptors}")
    };
    rewritten.push(entry);
  }
  Ok(rewritten.join(", "))
}

fn rewrite_css(
  input: &str,
  base_url: &Url,
  catalog: &mut AssetCatalog,
  ctx: ReferenceContext,
) -> Result<String> {
  let url_regex =
    Regex::new("(?i)(?P<prefix>url\\(\\s*[\"']?)(?P<url>[^\"')]+)(?P<suffix>[\"']?\\s*\\))")
      .expect("url regex must compile");
  let import_regex =
    Regex::new("(?i)(?P<prefix>@import\\s+['\"])(?P<url>[^\"']+)(?P<suffix>['\"])")
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
  let trimmed = raw.trim();
  if trimmed.is_empty()
    || trimmed.starts_with('#')
    || trimmed.to_ascii_lowercase().starts_with("javascript:")
    || trimmed.to_ascii_lowercase().starts_with("data:")
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

  if resolved.starts_with("data:") {
    return Ok(None);
  }

  let (without_fragment, fragment) = split_fragment(&resolved);
  let mut path = if let Some(path) = catalog.path_for(&without_fragment, ctx) {
    path
  } else {
    eprintln!(
      "Warning: bundle missing {}; creating empty placeholder",
      without_fragment
    );
    catalog.ensure_local_path(&without_fragment, ctx)
  };

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
  Path::new(path)
    .extension()
    .and_then(|e| e.to_str())
    .unwrap_or("bin")
    .to_ascii_lowercase()
}

fn hash_bytes(bytes: &[u8]) -> String {
  let digest = Sha256::digest(bytes);
  digest
    .iter()
    .take(HASH_PREFIX_BYTES)
    .map(|b| format!("{b:02x}"))
    .collect::<String>()
}

fn assert_no_http_urls(label: &str, content: &str) -> Result<()> {
  if let Some(pos) = content.find("http://").or_else(|| content.find("https://")) {
    let end = (pos + 80).min(content.len());
    let snippet = content[pos..end].replace('\n', " ");
    eprintln!("{label} still contains http(s) references after rewrite: {snippet}");
  }
  Ok(())
}

fn is_textual(asset: &AssetData) -> bool {
  asset
    .content_type
    .as_deref()
    .map(|ct| {
      ct.starts_with("text/")
        || ct.contains("javascript")
        || ct.contains("json")
        || ct.contains("svg")
        || ct.contains("xml")
        || ct.contains("css")
    })
    .unwrap_or_else(|| {
      asset.filename.ends_with(".css")
        || asset.filename.ends_with(".html")
        || asset.filename.ends_with(".svg")
    })
}
