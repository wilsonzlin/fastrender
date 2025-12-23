use super::FetchedResource;
use crate::error::{Error, ImageError, Result};
use base64::Engine;

const DATA_URL_PREFIX: &str = "data:";
const DEFAULT_MEDIA_TYPE: &str = "text/plain";
const DEFAULT_CHARSET: &str = "charset=US-ASCII";

/// Decode a data: URL into bytes and content type following RFC 2397 semantics.
pub(crate) fn decode_data_url(url: &str) -> Result<FetchedResource> {
  if !url.starts_with(DATA_URL_PREFIX) {
    return Err(Error::Image(ImageError::InvalidDataUrl {
      reason: "URL does not start with 'data:'".to_string(),
    }));
  }

  let rest = &url[DATA_URL_PREFIX.len()..];
  let (metadata, data) = rest.split_once(',').ok_or_else(|| {
    Error::Image(ImageError::InvalidDataUrl {
      reason: "Missing comma in data URL".to_string(),
    })
  })?;

  let parsed = parse_metadata(metadata);

  let bytes = if parsed.is_base64 {
    decode_base64_data(data)?
  } else {
    percent_decode(data)?
  };

  Ok(FetchedResource::new(bytes, Some(parsed.content_type)))
}

struct DataUrlMetadata {
  content_type: String,
  is_base64: bool,
}

fn parse_metadata(metadata: &str) -> DataUrlMetadata {
  let mut parts = metadata.split(';');
  let mediatype = parts.next().unwrap_or("").trim();

  let mut is_base64 = false;
  let mut params: Vec<String> = Vec::new();

  for param in parts {
    let trimmed = param.trim();
    if trimmed.is_empty() {
      continue;
    }
    if trimmed.eq_ignore_ascii_case("base64") {
      is_base64 = true;
      continue;
    }
    params.push(trimmed.to_string());
  }

  let mut content_type = if mediatype.is_empty() {
    DEFAULT_MEDIA_TYPE.to_string()
  } else {
    mediatype.to_string()
  };

  if mediatype.is_empty() && !has_charset(&params) {
    params.insert(0, DEFAULT_CHARSET.to_string());
  }

  if !params.is_empty() {
    content_type.push(';');
    content_type.push_str(&params.join(";"));
  }

  DataUrlMetadata {
    content_type,
    is_base64,
  }
}

fn has_charset(params: &[String]) -> bool {
  params.iter().any(|param| match param.split_once('=') {
    Some((name, _)) => name.trim().eq_ignore_ascii_case("charset"),
    None => param.trim().eq_ignore_ascii_case("charset"),
  })
}

/// Decode base64 payloads, tolerating ASCII whitespace for robustness.
fn decode_base64_data(data: &str) -> Result<Vec<u8>> {
  let mut cleaned = Vec::with_capacity(data.len());
  let mut saw_whitespace = false;

  for byte in data.bytes() {
    if byte.is_ascii_whitespace() {
      saw_whitespace = true;
      continue;
    }
    cleaned.push(byte);
  }

  let input = if saw_whitespace {
    cleaned.as_slice()
  } else {
    data.as_bytes()
  };

  base64::engine::general_purpose::STANDARD
    .decode(input)
    .map_err(|e| {
      Error::Image(ImageError::InvalidDataUrl {
        reason: format!("Invalid base64: {e}"),
      })
    })
}

/// Percent-decode a URL payload without treating '+' specially.
fn percent_decode(input: &str) -> Result<Vec<u8>> {
  let mut out = Vec::with_capacity(input.len());
  let bytes = input.as_bytes();
  let mut i = 0;

  while i < bytes.len() {
    match bytes[i] {
      b'%' => {
        if i + 2 >= bytes.len() {
          return Err(Error::Image(ImageError::InvalidDataUrl {
            reason: "Incomplete percent-escape".to_string(),
          }));
        }
        let hi = (bytes[i + 1] as char).to_digit(16);
        let lo = (bytes[i + 2] as char).to_digit(16);
        match (hi, lo) {
          (Some(hi), Some(lo)) => {
            out.push(((hi << 4) | lo) as u8);
            i += 3;
          }
          _ => {
            return Err(Error::Image(ImageError::InvalidDataUrl {
              reason: "Invalid percent-escape".to_string(),
            }));
          }
        }
      }
      byte => {
        out.push(byte);
        i += 1;
      }
    }
  }

  Ok(out)
}
