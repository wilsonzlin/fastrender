use super::FetchedResource;
use crate::error::{Error, ImageError, Result};
use base64::Engine;
use std::io::{self, Read};

const DATA_URL_PREFIX: &str = "data:";
const DEFAULT_MEDIA_TYPE: &str = "text/plain";
const DEFAULT_CHARSET: &str = "charset=US-ASCII";

/// Decode a data: URL into bytes and content type following RFC 2397 semantics.
pub(crate) fn decode_data_url(url: &str) -> Result<FetchedResource> {
  if !url
    .get(..DATA_URL_PREFIX.len())
    .map(|prefix| prefix.eq_ignore_ascii_case(DATA_URL_PREFIX))
    .unwrap_or(false)
  {
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

/// Decode up to the first `max_bytes` of a data: URL payload.
///
/// This is intended for probing image metadata without decoding the entire inline payload.
/// The returned bytes are always a prefix of the fully-decoded data.
pub(crate) fn decode_data_url_prefix(url: &str, max_bytes: usize) -> Result<FetchedResource> {
  if !url
    .get(..DATA_URL_PREFIX.len())
    .map(|prefix| prefix.eq_ignore_ascii_case(DATA_URL_PREFIX))
    .unwrap_or(false)
  {
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

  let bytes = if max_bytes == 0 {
    Vec::new()
  } else if parsed.is_base64 {
    decode_base64_prefix(data, max_bytes)?
  } else {
    percent_decode_prefix(data, max_bytes)?
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

struct WhitespaceStrippingReader<'a> {
  bytes: &'a [u8],
  pos: usize,
}

impl<'a> WhitespaceStrippingReader<'a> {
  fn new(bytes: &'a [u8]) -> Self {
    Self { bytes, pos: 0 }
  }
}

impl<'a> Read for WhitespaceStrippingReader<'a> {
  fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
    if buf.is_empty() {
      return Ok(0);
    }

    let mut written = 0usize;
    while written < buf.len() && self.pos < self.bytes.len() {
      let byte = self.bytes[self.pos];
      self.pos += 1;
      if byte.is_ascii_whitespace() {
        continue;
      }
      buf[written] = byte;
      written += 1;
    }
    Ok(written)
  }
}

fn decode_base64_prefix(data: &str, max_bytes: usize) -> Result<Vec<u8>> {
  if max_bytes == 0 {
    return Ok(Vec::new());
  }

  let mut stripped = WhitespaceStrippingReader::new(data.as_bytes());
  let mut decoder =
    base64::read::DecoderReader::new(&mut stripped, &base64::engine::general_purpose::STANDARD);

  let mut out = Vec::with_capacity(max_bytes.min(64 * 1024));
  let mut buf = [0u8; 8 * 1024];
  while out.len() < max_bytes {
    let remaining = max_bytes - out.len();
    let to_read = remaining.min(buf.len());
    let read = decoder.read(&mut buf[..to_read]).map_err(|e| {
      Error::Image(ImageError::InvalidDataUrl {
        reason: format!("Invalid base64: {e}"),
      })
    })?;
    if read == 0 {
      break;
    }
    out.extend_from_slice(&buf[..read]);
  }
  Ok(out)
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

fn percent_decode_prefix(input: &str, max_bytes: usize) -> Result<Vec<u8>> {
  if max_bytes == 0 {
    return Ok(Vec::new());
  }

  let mut out = Vec::with_capacity(input.len().min(max_bytes));
  let bytes = input.as_bytes();
  let mut i = 0;

  while i < bytes.len() && out.len() < max_bytes {
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

pub(crate) fn encode_base64_data_url(media_type: &str, data: &[u8]) -> String {
  let mut url = String::from("data:");
  url.push_str(media_type);
  url.push_str(";base64,");
  url.push_str(&base64::engine::general_purpose::STANDARD.encode(data));
  url
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn decodes_data_url_case_insensitive_scheme() {
    let res = decode_data_url("DATA:text/plain;base64,aGk=").expect("decode data url");
    assert_eq!(res.bytes, b"hi");
    assert_eq!(res.content_type.as_deref(), Some("text/plain"));
  }

  #[test]
  fn data_url_prefix_decodes_base64_prefix() {
    let bytes: Vec<u8> = (0..128u8).collect();
    let url = encode_base64_data_url("application/octet-stream", &bytes);

    let decoded = decode_data_url_prefix(&url, 13).expect("decode prefix");
    assert_eq!(
      decoded.content_type.as_deref(),
      Some("application/octet-stream")
    );
    assert_eq!(decoded.bytes, bytes[..13]);
  }

  #[test]
  fn data_url_prefix_decodes_base64_with_whitespace() {
    let bytes: Vec<u8> = (0..64u8).collect();
    let mut url = encode_base64_data_url("application/octet-stream", &bytes);
    let (_, payload) = url.split_once(',').expect("comma");
    let injected = payload
      .as_bytes()
      .chunks(16)
      .map(|chunk| std::str::from_utf8(chunk).expect("utf8"))
      .collect::<Vec<_>>()
      .join("\n");
    url.truncate(url.find(',').expect("comma") + 1);
    url.push_str(&injected);

    let decoded = decode_data_url_prefix(&url, 17).expect("decode prefix");
    assert_eq!(decoded.bytes, bytes[..17]);
  }

  #[test]
  fn data_url_prefix_decodes_percent_prefix() {
    let url = "data:text/plain,hello%20world";
    let decoded = decode_data_url_prefix(url, 5).expect("decode prefix");
    assert_eq!(decoded.content_type.as_deref(), Some("text/plain"));
    assert_eq!(decoded.bytes, b"hello");
  }
}
