use crate::compat::CompatProfile;
use crate::dom::DomCompatibilityMode;
use crate::error::{Error, Result};
use crate::resource::{FetchedResource, ResourceFetcher};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

/// File name of the bundle manifest inside directories and archives.
pub const BUNDLE_MANIFEST: &str = "bundle.json";

/// Schema version for bundle manifests.
pub const BUNDLE_VERSION: u32 = 1;

/// Render settings captured with the bundle.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleRenderConfig {
  pub viewport: (u32, u32),
  pub device_pixel_ratio: f32,
  pub scroll_x: f32,
  pub scroll_y: f32,
  pub full_page: bool,
  #[serde(default)]
  pub same_origin_subresources: bool,
  #[serde(default)]
  pub allowed_subresource_origins: Vec<String>,
  #[serde(default)]
  pub compat_profile: CompatProfile,
  #[serde(default)]
  pub dom_compat_mode: DomCompatibilityMode,
}

/// Metadata describing the bundled document.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundledDocument {
  pub path: String,
  pub content_type: Option<String>,
  pub final_url: String,
  pub status: Option<u16>,
  pub etag: Option<String>,
  pub last_modified: Option<String>,
}

/// Metadata describing a bundled resource.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundledResourceInfo {
  pub path: String,
  pub content_type: Option<String>,
  pub status: Option<u16>,
  pub final_url: Option<String>,
  pub etag: Option<String>,
  pub last_modified: Option<String>,
}

/// Manifest describing all resources contained in a bundle.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleManifest {
  pub version: u32,
  pub original_url: String,
  pub document: BundledDocument,
  pub render: BundleRenderConfig,
  pub resources: BTreeMap<String, BundledResourceInfo>,
}

#[derive(Clone)]
struct BundledResource {
  info: BundledResourceInfo,
  bytes: Arc<Vec<u8>>,
}

impl BundledResource {
  fn from_parts(info: BundledResourceInfo, bytes: Vec<u8>) -> Self {
    Self {
      info,
      bytes: Arc::new(bytes),
    }
  }

  fn as_fetched(&self) -> FetchedResource {
    let mut res = FetchedResource::with_final_url(
      (*self.bytes).clone(),
      self.info.content_type.clone(),
      self.info.final_url.clone(),
    );
    res.status = self.info.status;
    res.etag = self.info.etag.clone();
    res.last_modified = self.info.last_modified.clone();
    res
  }
}

/// In-memory representation of a bundle.
pub struct Bundle {
  manifest: BundleManifest,
  document_bytes: Arc<Vec<u8>>,
  resources: HashMap<String, BundledResource>,
}

impl Bundle {
  /// Load a bundle from a directory or `.tar` archive path.
  pub fn load(path: impl AsRef<Path>) -> Result<Self> {
    let path = path.as_ref();
    if path.is_dir() {
      Self::load_directory(path)
    } else {
      Self::load_archive(path)
    }
  }

  /// Returns the parsed manifest.
  pub fn manifest(&self) -> &BundleManifest {
    &self.manifest
  }

  /// Returns the bundled document metadata and bytes.
  pub fn document(&self) -> (&BundledDocument, Arc<Vec<u8>>) {
    (&self.manifest.document, Arc::clone(&self.document_bytes))
  }

  fn load_directory(dir: &Path) -> Result<Self> {
    let manifest_path = dir.join(BUNDLE_MANIFEST);
    let manifest_bytes = fs::read(&manifest_path).map_err(|e| {
      Error::Io(std::io::Error::new(
        e.kind(),
        format!(
          "Failed to read manifest {path:?}: {e}",
          path = manifest_path
        ),
      ))
    })?;
    let manifest: BundleManifest = serde_json::from_slice(&manifest_bytes)
      .map_err(|e| Error::Other(format!("Invalid bundle manifest: {e}")))?;
    Self::build_from_manifest(dir, manifest, None)
  }

  fn load_archive(path: &Path) -> Result<Self> {
    let file = fs::File::open(path).map_err(|e| {
      Error::Io(std::io::Error::new(
        e.kind(),
        format!("Failed to open bundle archive {path:?}: {e}"),
      ))
    })?;
    let mut archive = tar::Archive::new(file);
    let mut files: HashMap<String, Vec<u8>> = HashMap::new();
    for entry in archive.entries().map_err(Error::Io)? {
      let mut entry = entry.map_err(Error::Io)?;
      if !entry.header().entry_type().is_file() {
        continue;
      }
      let path = entry
        .path()
        .map_err(Error::Io)?
        .to_string_lossy()
        .trim_start_matches("./")
        .to_string();
      let mut data = Vec::new();
      entry.read_to_end(&mut data).map_err(Error::Io)?;
      files.insert(path, data);
    }

    let manifest_bytes = files.get(BUNDLE_MANIFEST).ok_or_else(|| {
      Error::Other(format!(
        "Bundle archive {} missing manifest",
        path.display()
      ))
    })?;
    let manifest: BundleManifest = serde_json::from_slice(manifest_bytes)
      .map_err(|e| Error::Other(format!("Invalid bundle manifest: {e}")))?;
    Self::build_from_manifest(path, manifest, Some(files))
  }

  fn build_from_manifest(
    base_path: &Path,
    manifest: BundleManifest,
    archive_files: Option<HashMap<String, Vec<u8>>>,
  ) -> Result<Self> {
    if manifest.version != BUNDLE_VERSION {
      return Err(Error::Other(format!(
        "Unsupported bundle version {} (expected {})",
        manifest.version, BUNDLE_VERSION
      )));
    }

    let fetch_file = |relative: &str| -> Result<Vec<u8>> {
      let relative_path = validate_relative_path(relative)?;
      let relative_str = relative_path.to_string_lossy().to_string();
      if let Some(files) = archive_files.as_ref() {
        files.get(&relative_str).cloned().ok_or_else(|| {
          Error::Other(format!(
            "Bundle missing resource file referenced in manifest: {}",
            relative
          ))
        })
      } else {
        let target = base_path.join(&relative_path);
        if target.is_dir() {
          return Err(Error::Other(format!(
            "Bundle entry {} resolves to directory",
            relative
          )));
        }
        fs::read(&target).map_err(Error::Io)
      }
    };

    let document_bytes = fetch_file(&manifest.document.path)?;
    let document_bytes = Arc::new(document_bytes);

    let mut resources: HashMap<String, BundledResource> = HashMap::new();
    for (original_url, info) in &manifest.resources {
      let data = fetch_file(&info.path)?;
      let resource = BundledResource::from_parts(info.clone(), data);
      resources.insert(original_url.clone(), resource.clone());
      if let Some(final_url) = &info.final_url {
        resources
          .entry(final_url.clone())
          .or_insert_with(|| resource.clone());
      }
    }

    Ok(Self {
      manifest,
      document_bytes,
      resources,
    })
  }

  fn resource_for_url(&self, url: &str) -> Option<&BundledResource> {
    self.resources.get(url)
  }
}

fn validate_relative_path(path: &str) -> Result<PathBuf> {
  let candidate = Path::new(path);
  if candidate.is_absolute()
    || candidate
      .components()
      .any(|c| matches!(c, Component::ParentDir | Component::RootDir))
  {
    return Err(Error::Other(format!(
      "Bundle entry path must be relative: {}",
      path
    )));
  }
  Ok(candidate.to_path_buf())
}

/// [`ResourceFetcher`] implementation that serves resources from a bundle without network access.
#[derive(Clone)]
pub struct BundledFetcher {
  bundle: Arc<Bundle>,
}

impl BundledFetcher {
  /// Construct a bundled fetcher from a loaded [`Bundle`].
  pub fn new(bundle: Bundle) -> Self {
    Self {
      bundle: Arc::new(bundle),
    }
  }
}

impl ResourceFetcher for BundledFetcher {
  fn fetch(&self, url: &str) -> Result<FetchedResource> {
    let doc_matches =
      url == self.bundle.manifest.original_url || url == self.bundle.manifest.document.final_url;

    if doc_matches {
      let (doc_meta, bytes) = self.bundle.document();
      let mut res = FetchedResource::with_final_url(
        (*bytes).clone(),
        doc_meta.content_type.clone(),
        Some(doc_meta.final_url.clone()),
      );
      res.status = doc_meta.status;
      res.etag = doc_meta.etag.clone();
      res.last_modified = doc_meta.last_modified.clone();
      return Ok(res);
    }

    if let Some(resource) = self.bundle.resource_for_url(url) {
      return Ok(resource.as_fetched());
    }

    Err(Error::Other(format!(
      "Resource not found in bundle: {}",
      url
    )))
  }

  fn fetch_with_validation(
    &self,
    url: &str,
    _etag: Option<&str>,
    _last_modified: Option<&str>,
  ) -> Result<FetchedResource> {
    self.fetch(url)
  }
}
