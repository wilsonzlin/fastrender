use crate::debug::runtime;
use url::Url;

use super::DocumentOrigin;
use super::FetchedResource;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CorsMode {
  Anonymous,
  UseCredentials,
}

/// Returns true when subresource CORS enforcement is enabled.
///
/// Controlled by `FASTR_FETCH_ENFORCE_CORS` (truthy/falsey). Defaults to `false`.
pub fn cors_enforcement_enabled() -> bool {
  runtime::runtime_toggles().truthy_with_default("FASTR_FETCH_ENFORCE_CORS", false)
}

/// Validate the `Access-Control-Allow-Origin` response header for a fetched resource.
///
/// This is a best-effort approximation of Chromium's CORS checks for resources like web fonts.
/// It is intentionally strict about invalid header values (notably comma-separated origins).
pub fn validate_cors_allow_origin(
  resource: &FetchedResource,
  requested_url: &str,
  document_origin: &DocumentOrigin,
  mode: CorsMode,
) -> std::result::Result<(), String> {
  let effective_url = resource.final_url.as_deref().unwrap_or(requested_url);
  let parsed = match Url::parse(effective_url) {
    Ok(parsed) => parsed,
    Err(_) => return Ok(()),
  };

  if !matches!(parsed.scheme(), "http" | "https") {
    return Ok(());
  }

  let target_origin = DocumentOrigin::from_parsed_url(&parsed);
  if target_origin.same_origin(document_origin) {
    return Ok(());
  }

  let raw = resource
    .access_control_allow_origin
    .as_deref()
    .map(str::trim)
    .filter(|v| !v.is_empty())
    .ok_or_else(|| "blocked by CORS: missing Access-Control-Allow-Origin".to_string())?;

  if raw == "*" {
    return match mode {
      CorsMode::Anonymous => Ok(()),
      CorsMode::UseCredentials => Err(
        "blocked by CORS: Access-Control-Allow-Origin * is not allowed for credentialed requests"
          .to_string(),
      ),
    };
  }

  // Chromium treats multiple origins as invalid even if one matches.
  if raw.contains(',') {
    return Err(format!(
      "blocked by CORS: invalid Access-Control-Allow-Origin (multiple values): {raw}"
    ));
  }

  if raw.eq_ignore_ascii_case("null") {
    if !document_origin.is_http_like() {
      return Ok(());
    }
    return Err(format!(
      "blocked by CORS: Access-Control-Allow-Origin null does not match document origin {document_origin}"
    ));
  }

  let parsed_origin =
    Url::parse(raw).map_err(|_| format!("blocked by CORS: invalid Access-Control-Allow-Origin: {raw}"))?;
  let allowed_origin = DocumentOrigin::from_parsed_url(&parsed_origin);
  if allowed_origin.same_origin(document_origin) {
    return Ok(());
  }

  Err(format!(
    "blocked by CORS: Access-Control-Allow-Origin {allowed_origin} does not match document origin {document_origin}"
  ))
}

