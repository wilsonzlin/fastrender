//! cURL-based HTTP backend for [`HttpFetcher`].
//!
//! This backend exists as a last-resort fallback for sites that fail with the Rust HTTP/TLS stack
//! (e.g. due to TLS fingerprinting or HTTP/2 quirks). It shells out to the system `curl` binary
//! via [`std::process::Command`].
//!
//! Notes:
//! - This module is intentionally dependency-light (no libcurl bindings).
//! - A per-`HttpFetcher` cookie jar is maintained as a temp file and shared across clones.
//! - Redirects are handled in Rust (not `--location`) so [`ResourcePolicy`] checks apply to every
//!   hop, matching the primary backend semantics.
#![allow(clippy::too_many_lines)]

use super::HttpFetcher;
use crate::error::{Error, RenderError, ResourceError, Result};
use crate::render_control;
use http::HeaderMap;
use std::io::{self, BufRead, BufReader, Read};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::MutexGuard;
use std::thread;
use std::time::{Duration, Instant};
use tempfile::{Builder, TempPath};
use url::Url;

#[derive(Debug, Default)]
pub(super) struct CookieJarState {
  jar: Option<TempPath>,
}

impl CookieJarState {
  fn ensure_path(&mut self) -> io::Result<&Path> {
    if self.jar.is_none() {
      let file = Builder::new().prefix("fastr-curl-cookies").tempfile()?;
      self.jar = Some(file.into_temp_path());
    }
    Ok(self.jar.as_ref().expect("jar should be initialized").as_ref())
  }
}

#[derive(Debug)]
struct CurlResponse {
  status: u16,
  headers: HeaderMap,
  body: Vec<u8>,
}

#[derive(Debug)]
enum CurlError {
  Failure(CurlFailure),
  BodyTooLarge { status: u16, observed: usize, limit: usize },
}

#[derive(Debug)]
struct CurlFailure {
  exit_status: Option<ExitStatus>,
  stderr: String,
  spawn_error: Option<io::Error>,
}

impl CurlFailure {
  fn message(&self) -> String {
    if let Some(err) = &self.spawn_error {
      return err.to_string();
    }
    let code = self
      .exit_status
      .as_ref()
      .and_then(|s| s.code())
      .map(|c| c.to_string())
      .unwrap_or_else(|| "<signal>".to_string());
    if self.stderr.trim().is_empty() {
      format!("curl failed (exit code {code})")
    } else {
      format!("curl failed (exit code {code}): {}", self.stderr.trim())
    }
  }

  fn retryable(&self) -> bool {
    let Some(status) = self.exit_status.as_ref().and_then(|s| s.code()) else {
      // Signaled/killed: don't retry.
      return false;
    };
    // Common transient curl exit codes. We err on the side of retrying since higher-level budgets
    // and deadlines cap total time.
    matches!(status, 5 | 6 | 7 | 18 | 28 | 35 | 52 | 56 | 92)
      || self.stderr.to_ascii_lowercase().contains("connection reset")
      || self.stderr.to_ascii_lowercase().contains("timed out")
      || self.stderr.to_ascii_lowercase().contains("timeout")
      || self.stderr.to_ascii_lowercase().contains("http2")
  }
}

fn sanitize_header_value(value: &str) -> String {
  value
    .chars()
    .map(|c| match c {
      '\r' | '\n' | '\0' => ' ',
      other => other,
    })
    .collect::<String>()
    .trim()
    .to_string()
}

pub(super) fn build_curl_args(
  url: &str,
  cookie_jar: &Path,
  timeout: Option<Duration>,
  headers: &[(String, String)],
) -> Vec<String> {
  let mut args = Vec::new();
  args.push("-q".to_string());
  args.push("--globoff".to_string());
  args.push("--silent".to_string());
  args.push("--show-error".to_string());
  args.push("--dump-header".to_string());
  args.push("-".to_string());
  args.push("-b".to_string());
  args.push(cookie_jar.display().to_string());
  args.push("-c".to_string());
  args.push(cookie_jar.display().to_string());
  if let Some(timeout) = timeout.filter(|t| !t.is_zero()) {
    args.push("--max-time".to_string());
    args.push(format!("{:.3}", timeout.as_secs_f64()));
  }
  for (name, value) in headers {
    args.push("--header".to_string());
    args.push(format!("{}: {}", name, sanitize_header_value(value)));
  }
  args.push("--".to_string());
  args.push(url.to_string());
  args
}

fn parse_status_line(line: &str) -> Option<u16> {
  let trimmed = line.trim();
  let mut parts = trimmed.split_whitespace();
  let proto = parts.next()?;
  if !proto.to_ascii_lowercase().starts_with("http/") {
    return None;
  }
  let code = parts.next()?;
  code.parse::<u16>().ok()
}

fn parse_header_line(line: &str, headers: &mut HeaderMap) {
  let trimmed = line.trim_end_matches(['\r', '\n']);
  if let Some((name, value)) = trimmed.split_once(':') {
    let name = name.trim();
    let value = value.trim();
    if name.is_empty() {
      return;
    }
    let Ok(name) = http::header::HeaderName::from_bytes(name.as_bytes()) else {
      return;
    };
    let Ok(value) = http::HeaderValue::from_bytes(value.as_bytes()) else {
      return;
    };
    headers.append(name, value);
  }
}

fn read_curl_headers<R: BufRead>(reader: &mut R) -> io::Result<(String, u16, HeaderMap)> {
  // curl may emit multiple header blocks (e.g. 100 Continue, proxy CONNECT). We read blocks until
  // we hit the final response head.
  loop {
    let mut status_line = String::new();
    let read = reader.read_line(&mut status_line)?;
    if read == 0 {
      return Err(io::Error::new(
        io::ErrorKind::UnexpectedEof,
        "curl produced no HTTP headers",
      ));
    }

    let status = parse_status_line(&status_line).ok_or_else(|| {
      io::Error::new(
        io::ErrorKind::InvalidData,
        format!("invalid HTTP status line from curl: {}", status_line.trim()),
      )
    })?;

    let mut headers = HeaderMap::new();
    loop {
      let mut line = String::new();
      let read = reader.read_line(&mut line)?;
      if read == 0 {
        break;
      }
      if line.trim_end_matches(['\r', '\n']).is_empty() {
        break;
      }
      parse_header_line(&line, &mut headers);
    }

    let status_lower = status_line.to_ascii_lowercase();
    let provisional = (100..200).contains(&status) || status_lower.contains("connection established");
    if provisional {
      continue;
    }

    return Ok((status_line, status, headers));
  }
}

fn run_curl(
  url: &str,
  cookie_jar: &Path,
  timeout: Option<Duration>,
  headers: &[(String, String)],
  body_limit: usize,
) -> std::result::Result<CurlResponse, CurlError> {
  let args = build_curl_args(url, cookie_jar, timeout, headers);
  let mut command = Command::new("curl");
  command.args(&args);
  command.stdout(Stdio::piped());
  command.stderr(Stdio::piped());

  let mut child = match command.spawn() {
    Ok(child) => child,
    Err(err) => {
      return Err(CurlError::Failure(CurlFailure {
        exit_status: None,
        stderr: String::new(),
        spawn_error: Some(err),
      }));
    }
  };

  let mut stderr = child.stderr.take().expect("stderr piped");
  let stderr_handle = thread::spawn(move || {
    let mut buf = Vec::new();
    let _ = stderr.read_to_end(&mut buf);
    String::from_utf8_lossy(&buf).to_string()
  });

  let stdout = child.stdout.take().expect("stdout piped");
  let mut reader = BufReader::new(stdout);

  let (status_line, status, headers) = match read_curl_headers(&mut reader) {
    Ok(parsed) => parsed,
    Err(err) => {
      let _ = child.kill();
      let exit_status = child.wait().ok();
      let stderr = stderr_handle.join().unwrap_or_default();
      return Err(CurlError::Failure(CurlFailure {
        exit_status,
        stderr: format!("{stderr}\n{err}").trim().to_string(),
        spawn_error: None,
      }));
    }
  };

  let has_location = headers.get("location").is_some();
  let should_discard_body = (300..400).contains(&status) && has_location;

  let mut body = Vec::new();
  let mut buf = [0u8; 8192];
  loop {
    let read = match reader.read(&mut buf) {
      Ok(read) => read,
      Err(err) => {
        let _ = child.kill();
        let exit_status = child.wait().ok();
        let stderr = stderr_handle.join().unwrap_or_default();
        return Err(CurlError::Failure(CurlFailure {
          exit_status,
          stderr: format!("{stderr}\n{err}").trim().to_string(),
          spawn_error: None,
        }));
      }
    };
    if read == 0 {
      break;
    }

    if should_discard_body {
      // Drain without buffering to match the primary backend (redirect bodies are ignored).
      continue;
    }

    let next_len = body.len().saturating_add(read);
    if next_len > body_limit {
      let _ = child.kill();
      let _ = child.wait();
      let _ = stderr_handle.join();
      return Err(CurlError::BodyTooLarge {
        status,
        observed: next_len,
        limit: body_limit,
      });
    }

    body.extend_from_slice(&buf[..read]);
  }

  let exit_status = child.wait().ok();
  let stderr = stderr_handle.join().unwrap_or_default();

  let Some(exit_status) = exit_status else {
    return Err(CurlError::Failure(CurlFailure {
      exit_status: None,
      stderr,
      spawn_error: None,
    }));
  };

  if !exit_status.success() {
    return Err(CurlError::Failure(CurlFailure {
      exit_status: Some(exit_status),
      stderr: format!("{stderr}\n{status_line}").trim().to_string(),
      spawn_error: None,
    }));
  }

  Ok(CurlResponse {
    status,
    headers,
    body,
  })
}

fn lock_cookie_jar(
  fetcher: &HttpFetcher,
) -> MutexGuard<'_, CookieJarState> {
  fetcher
    .curl_cookie_jar
    .lock()
    .unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub(super) fn fetch_http_with_accept_inner<'a>(
  fetcher: &HttpFetcher,
  url: &str,
  accept_encoding: Option<&str>,
  validators: Option<super::HttpCacheValidators<'a>>,
  deadline: &Option<render_control::RenderDeadline>,
  started: Instant,
) -> Result<super::FetchedResource> {
  let mut current = url.to_string();
  let mut validators = validators;

  let max_attempts = if deadline
    .as_ref()
    .and_then(render_control::RenderDeadline::timeout_limit)
    .is_some()
  {
    1
  } else {
    fetcher.retry_policy.max_attempts.max(1)
  };

  let timeout_budget = if deadline.is_none()
    && fetcher.policy.request_timeout_is_total_budget
    && !fetcher.policy.request_timeout.is_zero()
  {
    Some(fetcher.policy.request_timeout)
  } else {
    None
  };

  let budget_exhausted_error = |current_url: &str, attempt: usize| -> Error {
    let budget = timeout_budget.expect("budget mode should be active");
    let elapsed = started.elapsed();
    Error::Resource(
      ResourceError::new(
        current_url.to_string(),
        format!(
          "overall HTTP timeout budget exceeded (budget={budget:?}, elapsed={elapsed:?}){}",
          super::format_attempt_suffix(attempt, max_attempts)
        ),
      )
      .with_final_url(current_url.to_string()),
    )
  };

  let mut jar_guard = lock_cookie_jar(fetcher);
  let cookie_path = jar_guard
    .ensure_path()
    .map_err(|e| Error::Resource(ResourceError::new(url.to_string(), e.to_string()).with_source(e)))?
    .to_path_buf();

  'redirects: for _ in 0..fetcher.policy.max_redirects {
    fetcher.policy.ensure_url_allowed(&current)?;

    for attempt in 1..=max_attempts {
      fetcher.policy.ensure_url_allowed(&current)?;

      let stage_hint = super::render_stage_hint_from_url(&current);
      if let Some(deadline) = deadline.as_ref().filter(|d| d.is_enabled()) {
        deadline.check(stage_hint).map_err(Error::Render)?;
      }

      let allowed_limit = fetcher.policy.allowed_response_limit()?;
      let per_request_timeout = fetcher.deadline_aware_timeout(deadline.as_ref(), &current)?;
      let mut effective_timeout = per_request_timeout.unwrap_or(fetcher.policy.request_timeout);

      if let Some(budget) = timeout_budget {
        match budget.checked_sub(started.elapsed()) {
          Some(remaining) if remaining > super::HTTP_DEADLINE_BUFFER => {
            let budget_timeout = remaining.saturating_sub(super::HTTP_DEADLINE_BUFFER);
            effective_timeout = effective_timeout.min(budget_timeout);
          }
          _ => return Err(budget_exhausted_error(&current, attempt)),
        }
      }

      let accept_encoding_value = accept_encoding.unwrap_or(super::SUPPORTED_ACCEPT_ENCODING);
      let headers = super::build_http_header_pairs(
        &current,
        &fetcher.user_agent,
        &fetcher.accept_language,
        accept_encoding_value,
        validators,
      );

      let network_timer = super::start_network_fetch_diagnostics();
      let response = run_curl(
        &current,
        &cookie_path,
        (!effective_timeout.is_zero()).then_some(effective_timeout),
        &headers,
        allowed_limit,
      );
      super::finish_network_fetch_diagnostics(network_timer);

      let response = match response {
        Ok(res) => res,
        Err(CurlError::BodyTooLarge {
          status,
          observed,
          limit,
        }) => {
          if let Some(remaining) = fetcher.policy.remaining_budget() {
            if observed > remaining {
              let err = ResourceError::new(
                current.clone(),
                format!(
                  "total bytes budget exceeded ({} > {} bytes remaining)",
                  observed, remaining
                ),
              )
              .with_status(status)
              .with_final_url(current.clone());
              return Err(Error::Resource(err));
            }
          }

          let err = ResourceError::new(
            current.clone(),
            format!("response too large ({} > {} bytes)", observed, limit),
          )
          .with_status(status)
          .with_final_url(current.clone());
          return Err(Error::Resource(err));
        }
        Err(CurlError::Failure(failure)) => {
          if attempt < max_attempts && failure.retryable() {
            let mut backoff = super::compute_backoff(&fetcher.retry_policy, attempt, &current);
            let mut can_retry = true;
            if let Some(deadline) = deadline.as_ref() {
              if deadline.timeout_limit().is_some() {
                match deadline.remaining_timeout() {
                  Some(remaining) if !remaining.is_zero() => {
                    let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                    backoff = backoff.min(max_sleep);
                  }
                  _ => can_retry = false,
                }
              }
            }
            if let Some(budget) = timeout_budget {
              match budget.checked_sub(started.elapsed()) {
                Some(remaining) if remaining > super::HTTP_DEADLINE_BUFFER => {
                  let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                  backoff = backoff.min(max_sleep);
                }
                _ => can_retry = false,
              }
            }
            if can_retry {
              super::log_http_retry(&failure.message(), attempt, max_attempts, &current, backoff);
              if !backoff.is_zero() {
                super::sleep_with_deadline(deadline.as_ref(), stage_hint, backoff)
                  .map_err(Error::Render)?;
              }
              continue;
            }
            if timeout_budget.is_some() {
              return Err(budget_exhausted_error(&current, attempt));
            }
          }

          let overall_timeout = deadline.as_ref().and_then(|d| d.timeout_limit());
          let mut message = failure.message();
          let lower = message.to_ascii_lowercase();
          if lower.contains("timeout") || lower.contains("timed out") {
            if let Some(overall) = overall_timeout {
              message.push_str(&format!(
                " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?}, overall_timeout={overall:?})"
              ));
            } else if let Some(budget) = timeout_budget {
              message.push_str(&format!(
                " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?}, overall_timeout_budget={budget:?})"
              ));
            } else {
              message.push_str(&format!(
                " (attempt {attempt}/{max_attempts}, per_attempt_timeout={effective_timeout:?})"
              ));
            }
          } else {
            message.push_str(&super::format_attempt_suffix(attempt, max_attempts));
          }

          let mut err = ResourceError::new(current.clone(), message).with_final_url(current.clone());
          if let Some(source) = failure.spawn_error {
            err = err.with_source(source);
          }
          return Err(Error::Resource(err));
        }
      };

      let status_code = response.status;
      if (300..400).contains(&status_code) {
        if let Some(loc) = response
          .headers
          .get("location")
          .and_then(|h| h.to_str().ok())
        {
          let next = Url::parse(&current)
            .ok()
            .and_then(|base| base.join(loc).ok())
            .map(|u| u.to_string())
            .unwrap_or_else(|| loc.to_string());
          fetcher.policy.ensure_url_allowed(&next)?;
          current = next;
          validators = None;
          continue 'redirects;
        }
      }

      let retry_after = if fetcher.retry_policy.respect_retry_after
        && super::retryable_http_status(status_code)
      {
        super::parse_retry_after(&response.headers)
      } else {
        None
      };

      let encodings = super::parse_content_encodings(&response.headers);
      let content_type = response
        .headers
        .get("content-type")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());
      let decode_stage = super::decode_stage_for_content_type(content_type.as_deref());
      let etag = response
        .headers
        .get("etag")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());
      let last_modified = response
        .headers
        .get("last-modified")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());
      let cache_policy = super::parse_http_cache_policy(&response.headers);

      let bytes = match super::decode_content_encodings(response.body, &encodings, allowed_limit, decode_stage)
      {
        Ok(decoded) => decoded,
        Err(super::ContentDecodeError::DeadlineExceeded { stage, elapsed, .. }) => {
          return Err(Error::Render(RenderError::Timeout { stage, elapsed }));
        }
        Err(super::ContentDecodeError::DecompressionFailed { .. }) if accept_encoding.is_none() => {
          drop(jar_guard);
          return fetch_http_with_accept_inner(
            fetcher,
            &current,
            Some("identity"),
            validators,
            deadline,
            started,
          );
        }
        Err(err) => {
          let err = err.into_resource_error(current.clone(), status_code, current.clone());
          return Err(Error::Resource(err));
        }
      };

      super::record_network_fetch_bytes(bytes.len());
      let is_retryable_status = super::retryable_http_status(status_code);

      if bytes.is_empty() && !super::http_status_allows_empty_body(status_code) {
        let mut can_retry = attempt < max_attempts;
        if can_retry {
          let mut backoff = super::compute_backoff(&fetcher.retry_policy, attempt, &current);
          if let Some(retry_after) = retry_after {
            backoff = backoff.max(retry_after);
          }

          if let Some(deadline) = deadline.as_ref() {
            if deadline.timeout_limit().is_some() {
              match deadline.remaining_timeout() {
                Some(remaining) if !remaining.is_zero() => {
                  let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                  backoff = backoff.min(max_sleep);
                }
                _ => can_retry = false,
              }
            }
          }
          if let Some(budget) = timeout_budget {
            match budget.checked_sub(started.elapsed()) {
              Some(remaining) if remaining > super::HTTP_DEADLINE_BUFFER => {
                let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                backoff = backoff.min(max_sleep);
              }
              _ => can_retry = false,
            }
          }

          if can_retry {
            super::log_http_retry(
              &format!("empty body (status {status_code})"),
              attempt,
              max_attempts,
              &current,
              backoff,
            );
            if !backoff.is_zero() {
              super::sleep_with_deadline(deadline.as_ref(), stage_hint, backoff)
                .map_err(Error::Render)?;
            }
            continue;
          }
          if timeout_budget.is_some() {
            return Err(budget_exhausted_error(&current, attempt));
          }
        }

        let mut message = "empty HTTP response body".to_string();
        if attempt < max_attempts {
          message.push_str(" (retry aborted: render deadline exceeded)");
        }
        message.push_str(&super::format_attempt_suffix(attempt, max_attempts));
        let err = ResourceError::new(current.clone(), message)
          .with_status(status_code)
          .with_final_url(current.clone());
        return Err(Error::Resource(err));
      }

      if is_retryable_status {
        let mut can_retry = attempt < max_attempts;
        if can_retry {
          let mut backoff = super::compute_backoff(&fetcher.retry_policy, attempt, &current);
          if let Some(retry_after) = retry_after {
            backoff = backoff.max(retry_after);
          }
          if let Some(deadline) = deadline.as_ref() {
            if deadline.timeout_limit().is_some() {
              match deadline.remaining_timeout() {
                Some(remaining) if !remaining.is_zero() => {
                  let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                  backoff = backoff.min(max_sleep);
                }
                _ => can_retry = false,
              }
            }
          }
          if let Some(budget) = timeout_budget {
            match budget.checked_sub(started.elapsed()) {
              Some(remaining) if remaining > super::HTTP_DEADLINE_BUFFER => {
                let max_sleep = remaining.saturating_sub(Duration::from_millis(1));
                backoff = backoff.min(max_sleep);
              }
              _ => can_retry = false,
            }
          }
          if can_retry {
            super::log_http_retry(
              &format!("status {status_code}"),
              attempt,
              max_attempts,
              &current,
              backoff,
            );
            if !backoff.is_zero() {
              super::sleep_with_deadline(deadline.as_ref(), stage_hint, backoff)
                .map_err(Error::Render)?;
            }
            continue;
          }
          if timeout_budget.is_some() {
            return Err(budget_exhausted_error(&current, attempt));
          }
        }

        if status_code != 202 {
          let mut message = if attempt < max_attempts {
            "retryable HTTP status (retry aborted: render deadline exceeded)".to_string()
          } else {
            "retryable HTTP status (retries exhausted)".to_string()
          };
          message.push_str(&super::format_attempt_suffix(attempt, max_attempts));
          let err = ResourceError::new(current.clone(), message)
            .with_status(status_code)
            .with_final_url(current.clone());
          return Err(Error::Resource(err));
        }
      }

      if bytes.len() > allowed_limit {
        if let Some(remaining) = fetcher.policy.remaining_budget() {
          if bytes.len() > remaining {
            let err = ResourceError::new(
              current.clone(),
              format!(
                "total bytes budget exceeded ({} > {} bytes remaining)",
                bytes.len(),
                remaining
              ),
            )
            .with_status(status_code)
            .with_final_url(current.clone());
            return Err(Error::Resource(err));
          }
        }
        let err = ResourceError::new(
          current.clone(),
          format!("response too large ({} > {} bytes)", bytes.len(), allowed_limit),
        )
        .with_status(status_code)
        .with_final_url(current.clone());
        return Err(Error::Resource(err));
      }

      fetcher.policy.reserve_budget(bytes.len())?;
      let mut resource = super::FetchedResource::with_final_url(
        bytes,
        content_type,
        Some(current.clone()),
      );
      resource.status = Some(status_code);
      resource.etag = etag;
      resource.last_modified = last_modified;
      resource.cache_policy = cache_policy;
      render_control::check_active(decode_stage).map_err(Error::Render)?;
      return Ok(resource);
    }
  }

  Err(Error::Resource(
    ResourceError::new(url, format!("too many redirects (limit {})", fetcher.policy.max_redirects))
      .with_final_url(current),
  ))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::resource::DEFAULT_ACCEPT_LANGUAGE;
  use crate::resource::DEFAULT_USER_AGENT;
  use std::path::PathBuf;

  #[test]
  fn header_parsing_skips_provisional_blocks() {
    let input = b"HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-Type: text/html\r\nETag: \"abc\"\r\n\r\n";
    let mut reader = BufReader::new(&input[..]);
    let (_status_line, status, headers) = read_curl_headers(&mut reader).expect("parse");
    assert_eq!(status, 200);
    assert_eq!(
      headers
        .get("content-type")
        .and_then(|h| h.to_str().ok())
        .unwrap(),
      "text/html"
    );
    assert_eq!(
      headers.get("etag").and_then(|h| h.to_str().ok()).unwrap(),
      "\"abc\""
    );
  }

  #[test]
  fn build_args_are_separate_and_include_headers() {
    let cookie = PathBuf::from("/tmp/cookies.txt");
    let headers = super::super::build_http_header_pairs(
      "https://example.com/",
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
      "gzip, deflate, br",
      None,
    );
    let args = build_curl_args(
      "https://example.com/",
      &cookie,
      Some(Duration::from_secs(3)),
      &headers,
    );
    assert!(args.contains(&"--silent".to_string()));
    assert!(args.contains(&"--show-error".to_string()));
    assert!(args.contains(&"--dump-header".to_string()));
    assert!(args.contains(&"--globoff".to_string()));
    assert!(args.contains(&"--max-time".to_string()));
    assert!(args.iter().any(|a| a.contains("User-Agent:")));
    assert!(args.iter().any(|a| a == "--"));
    assert_eq!(args.last().unwrap(), "https://example.com/");
  }

  #[test]
  fn build_args_sanitizes_header_values() {
    let cookie = PathBuf::from("/tmp/cookies.txt");
    let headers = vec![("X-Test".to_string(), "a\r\nb\0c".to_string())];
    let args = build_curl_args("https://example.com/", &cookie, None, &headers);
    let header_value = args
      .iter()
      .skip_while(|v| *v != "--header")
      .nth(1)
      .expect("expected header value");
    assert!(!header_value.contains('\r'));
    assert!(!header_value.contains('\n'));
    assert!(!header_value.contains('\0'));
  }

  #[test]
  fn status_line_parses_http2() {
    assert_eq!(parse_status_line("HTTP/2 204\r\n"), Some(204));
  }
}
