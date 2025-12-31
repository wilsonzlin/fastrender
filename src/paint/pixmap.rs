use crate::error::RenderError;
use std::collections::TryReserveError;
use tiny_skia::{IntSize, Pixmap};

const BYTES_PER_PIXEL: u64 = 4;
/// Upper bound on a single pixmap allocation to avoid process aborts on OOM.
pub(crate) const MAX_PIXMAP_BYTES: u64 = 512 * 1024 * 1024;

#[cfg(test)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct NewPixmapAllocRecord {
  pub width: u32,
  pub height: u32,
  pub file: &'static str,
  pub line: u32,
}

#[cfg(test)]
thread_local! {
  static RECORD_NEW_PIXMAP: std::cell::Cell<bool> = std::cell::Cell::new(false);
  static NEW_PIXMAP_RECORDS: std::cell::RefCell<Vec<NewPixmapAllocRecord>> =
    std::cell::RefCell::new(Vec::new());
}

/// Records `new_pixmap` allocations for the current thread.
///
/// This is intended for unit tests that want to assert on temporary allocation patterns without
/// relying on global allocators or OS-level accounting.
#[cfg(test)]
pub(crate) struct NewPixmapAllocRecorder;

#[cfg(test)]
impl NewPixmapAllocRecorder {
  pub(crate) fn start() -> Self {
    RECORD_NEW_PIXMAP.with(|flag| flag.set(true));
    NEW_PIXMAP_RECORDS.with(|records| records.borrow_mut().clear());
    Self
  }

  pub(crate) fn take(&self) -> Vec<NewPixmapAllocRecord> {
    NEW_PIXMAP_RECORDS.with(|records| std::mem::take(&mut *records.borrow_mut()))
  }
}

#[cfg(test)]
impl Drop for NewPixmapAllocRecorder {
  fn drop(&mut self) {
    RECORD_NEW_PIXMAP.with(|flag| flag.set(false));
    NEW_PIXMAP_RECORDS.with(|records| records.borrow_mut().clear());
  }
}

pub(crate) fn guard_allocation_bytes(bytes: u64, context: &str) -> Result<usize, RenderError> {
  if bytes > MAX_PIXMAP_BYTES {
    return Err(RenderError::InvalidParameters {
      message: format!(
        "{context}: allocation would require {bytes} bytes (limit {MAX_PIXMAP_BYTES})"
      ),
    });
  }
  usize::try_from(bytes).map_err(|_| RenderError::InvalidParameters {
    message: format!("{context}: allocation size {bytes} does not fit in usize"),
  })
}

pub(crate) fn reserve_buffer(bytes: u64, context: &str) -> Result<Vec<u8>, RenderError> {
  let capacity = guard_allocation_bytes(bytes, context)?;
  let mut buffer = Vec::new();
  buffer
    .try_reserve_exact(capacity)
    .map_err(|err: TryReserveError| RenderError::InvalidParameters {
      message: format!("{context}: buffer allocation failed for {bytes} bytes: {err}"),
    })?;
  Ok(buffer)
}

fn guard_dimensions(width: u32, height: u32, context: &str) -> Result<usize, RenderError> {
  if width == 0 || height == 0 {
    return Err(RenderError::InvalidParameters {
      message: format!("{context}: pixmap size is zero ({width}x{height})"),
    });
  }

  let pixels = (width as u64)
    .checked_mul(height as u64)
    .ok_or(RenderError::InvalidParameters {
      message: format!("{context}: pixmap dimensions overflow ({width}x{height})"),
    })?;
  let bytes = pixels
    .checked_mul(BYTES_PER_PIXEL)
    .ok_or(RenderError::InvalidParameters {
      message: format!("{context}: pixmap byte size overflow ({width}x{height})"),
    })?;
  if bytes > MAX_PIXMAP_BYTES {
    return Err(RenderError::InvalidParameters {
      message: format!(
        "{context}: pixmap {}x{} would allocate {} bytes (limit {})",
        width, height, bytes, MAX_PIXMAP_BYTES
      ),
    });
  }

  Ok(bytes as usize)
}

fn allocate_pixmap_bytes(bytes: usize) -> Result<Vec<u8>, RenderError> {
  let mut buffer = Vec::new();
  if let Err(err) = buffer.try_reserve_exact(bytes) {
    return Err(RenderError::InvalidParameters {
      message: format!("pixmap allocation failed: {err}"),
    });
  }
  buffer.resize(bytes, 0);
  Ok(buffer)
}

#[track_caller]
pub(crate) fn new_pixmap_with_context(
  width: u32,
  height: u32,
  context: &str,
) -> Result<Pixmap, RenderError> {
  let caller = std::panic::Location::caller();
  let context = format!("{context} (at {}:{})", caller.file(), caller.line());
  let bytes = guard_dimensions(width, height, &context)?;
  let buffer = allocate_pixmap_bytes(bytes)?;
  let size = IntSize::from_wh(width, height).ok_or(RenderError::InvalidParameters {
    message: format!(
      "{context}: pixmap dimensions out of range ({}x{})",
      width, height
    ),
  })?;
  Pixmap::from_vec(buffer, size).ok_or(RenderError::InvalidParameters {
    message: format!(
      "{context}: pixmap creation failed for {}x{} ({} bytes)",
      width, height, bytes
    ),
  })
}

#[track_caller]
pub(crate) fn new_pixmap(width: u32, height: u32) -> Option<Pixmap> {
  #[cfg(test)]
  {
    if RECORD_NEW_PIXMAP.with(|flag| flag.get()) {
      let caller = std::panic::Location::caller();
      NEW_PIXMAP_RECORDS.with(|records| {
        records.borrow_mut().push(NewPixmapAllocRecord {
          width,
          height,
          file: caller.file(),
          line: caller.line(),
        });
      });
    }
  }
  new_pixmap_with_context(width, height, "pixmap").ok()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn rejects_zero_dimensions() {
    assert!(matches!(
      new_pixmap_with_context(0, 10, "zero"),
      Err(RenderError::InvalidParameters { .. })
    ));
    assert!(matches!(
      new_pixmap_with_context(10, 0, "zero"),
      Err(RenderError::InvalidParameters { .. })
    ));
  }

  #[test]
  fn rejects_overflow_and_limit() {
    assert!(matches!(
      new_pixmap_with_context(u32::MAX, 2, "overflow"),
      Err(RenderError::InvalidParameters { .. })
    ));

    let bytes_per_row = MAX_PIXMAP_BYTES / BYTES_PER_PIXEL + 1;
    let width = bytes_per_row as u32;
    assert!(matches!(
      new_pixmap_with_context(width, 1, "too_big"),
      Err(RenderError::InvalidParameters { .. })
    ));
  }

  #[test]
  fn allocates_small_pixmaps() {
    let pixmap = new_pixmap_with_context(4, 4, "ok").expect("small pixmap");
    assert_eq!(pixmap.width(), 4);
    assert_eq!(pixmap.height(), 4);
  }

  #[test]
  fn rejects_oversized_buffer_allocation() {
    assert!(reserve_buffer(MAX_PIXMAP_BYTES + 1, "buffer").is_err());
  }
}
