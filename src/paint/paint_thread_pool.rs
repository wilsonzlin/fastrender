use rayon::{ThreadPool, ThreadPoolBuilder};
use std::borrow::Cow;
use std::sync::{LazyLock, OnceLock};

const PAINT_THREADS_ENV: &str = "FASTR_PAINT_THREADS";

#[derive(Debug)]
struct PaintThreadPool {
  pool: ThreadPool,
  threads: usize,
}

#[derive(Debug)]
enum PaintThreadPoolState {
  Ready(PaintThreadPool),
  Error(String),
}

static PAINT_THREAD_POOL: LazyLock<OnceLock<PaintThreadPoolState>> = LazyLock::new(OnceLock::new);

#[derive(Debug)]
pub(crate) struct PaintPoolSelection<'a> {
  /// Thread pool to install before running paint-related Rayon work.
  ///
  /// `None` means we should run Rayon work in the current/global pool.
  pub(crate) pool: Option<&'a ThreadPool>,
  /// Thread count available for parallel paint work.
  pub(crate) threads: usize,
  /// If no dedicated pool is selected, describes why.
  pub(crate) dedicated_fallback: Option<Cow<'static, str>>,
}

fn parse_paint_threads_env() -> Result<Option<usize>, String> {
  match std::env::var(PAINT_THREADS_ENV) {
    Ok(raw) => {
      let raw = raw.trim();
      if raw.is_empty() {
        return Err(format!("{PAINT_THREADS_ENV} is set but empty"));
      }
      raw.parse::<usize>()
        .map(Some)
        .map_err(|_| format!("{PAINT_THREADS_ENV}={raw:?} is not a valid positive integer"))
    }
    Err(std::env::VarError::NotPresent) => Ok(None),
    Err(err) => Err(format!("failed to read {PAINT_THREADS_ENV}: {err}")),
  }
}

/// Select the Rayon thread pool that should be used for paint work.
///
/// When `FASTR_PAINT_THREADS` is set to a value greater than 1, a lazily-initialised dedicated
/// thread pool is returned. Otherwise, callers should use the current/global Rayon pool.
pub(crate) fn paint_pool() -> PaintPoolSelection<'static> {
  let current_threads = rayon::current_num_threads().max(1);

  match parse_paint_threads_env() {
    Ok(None) => PaintPoolSelection {
      pool: None,
      threads: current_threads,
      dedicated_fallback: Some(Cow::Borrowed(
        "dedicated paint pool disabled (set FASTR_PAINT_THREADS>1 to enable)",
      )),
    },
    Ok(Some(threads)) if threads <= 1 => PaintPoolSelection {
      pool: None,
      threads: current_threads,
      dedicated_fallback: Some(Cow::Owned(format!(
        "dedicated paint pool disabled ({PAINT_THREADS_ENV} must be >1, got {threads})"
      ))),
    },
    Ok(Some(threads)) => {
      let state = PAINT_THREAD_POOL.get_or_init(|| match ThreadPoolBuilder::new()
        .num_threads(threads)
        .build()
      {
        Ok(pool) => PaintThreadPoolState::Ready(PaintThreadPool { pool, threads }),
        Err(err) => PaintThreadPoolState::Error(err.to_string()),
      });

      match state {
        PaintThreadPoolState::Ready(pool) => PaintPoolSelection {
          pool: Some(&pool.pool),
          threads: pool.threads.max(1),
          dedicated_fallback: None,
        },
        PaintThreadPoolState::Error(err) => PaintPoolSelection {
          pool: None,
          threads: current_threads,
          dedicated_fallback: Some(Cow::Owned(format!(
            "dedicated paint pool unavailable: {err}"
          ))),
        },
      }
    }
    Err(reason) => PaintPoolSelection {
      pool: None,
      threads: current_threads,
      dedicated_fallback: Some(Cow::Owned(format!(
        "dedicated paint pool disabled ({reason})"
      ))),
    },
  }
}
