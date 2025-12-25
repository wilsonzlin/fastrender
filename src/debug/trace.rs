use serde::Serialize;
use std::borrow::Cow;
use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Instant;

#[derive(Clone, Default)]
pub(crate) struct TraceHandle {
  inner: Option<Arc<TraceState>>,
}

impl TraceHandle {
  pub(crate) fn enabled() -> Self {
    Self {
      inner: Some(Arc::new(TraceState::new())),
    }
  }

  pub(crate) fn disabled() -> Self {
    Self { inner: None }
  }

  pub(crate) fn is_enabled(&self) -> bool {
    self.inner.is_some()
  }

  pub(crate) fn span(&self, name: &'static str, cat: &'static str) -> TraceSpan<'_> {
    match &self.inner {
      Some(state) => TraceSpan::new(state.clone(), Cow::Borrowed(name), cat),
      None => TraceSpan::noop(),
    }
  }

  pub(crate) fn span_owned(&self, name: String, cat: &'static str) -> TraceSpan<'_> {
    match &self.inner {
      Some(state) => TraceSpan::new(state.clone(), Cow::Owned(name), cat),
      None => TraceSpan::noop(),
    }
  }

  pub(crate) fn write_chrome_trace(&self, path: &Path) -> std::io::Result<()> {
    let Some(state) = &self.inner else {
      return Ok(());
    };

    if let Some(parent) = path.parent() {
      if !parent.as_os_str().is_empty() {
        std::fs::create_dir_all(parent)?;
      }
    }

    let events = match state.events.lock() {
      Ok(events) => events.clone(),
      Err(err) => err.into_inner().clone(),
    };
    let mut file = std::fs::File::create(path)?;
    let trace_file = TraceFile {
      trace_events: events,
    };
    serde_json::to_writer(&mut file, &trace_file)?;
    file.write_all(b"\n")
  }
}

struct TraceState {
  start: Instant,
  events: Mutex<Vec<TraceEvent>>,
}

impl TraceState {
  fn new() -> Self {
    Self {
      start: Instant::now(),
      events: Mutex::new(Vec::new()),
    }
  }

  fn push_event(&self, name: Cow<'static, str>, cat: &'static str, start: Instant, end: Instant) {
    let ts = start.duration_since(self.start).as_micros() as u64;
    let dur = end.duration_since(start).as_micros() as u64;
    let tid = std::thread::current().id().as_u64();
    if let Ok(mut events) = self.events.lock() {
      events.push(TraceEvent {
        name: name.into_owned(),
        cat: cat.to_string(),
        ph: "X",
        ts,
        dur,
        pid: std::process::id(),
        tid,
      });
    }
  }
}

pub(crate) struct TraceSpan<'a> {
  state: Option<Arc<TraceState>>,
  name: Cow<'static, str>,
  cat: &'static str,
  start: Option<Instant>,
  _phantom: std::marker::PhantomData<&'a ()>,
}

impl<'a> TraceSpan<'a> {
  fn new(state: Arc<TraceState>, name: Cow<'static, str>, cat: &'static str) -> Self {
    Self {
      state: Some(state),
      name,
      cat,
      start: Some(Instant::now()),
      _phantom: std::marker::PhantomData,
    }
  }

  fn noop() -> Self {
    Self {
      state: None,
      name: Cow::Borrowed(""),
      cat: "",
      start: None,
      _phantom: std::marker::PhantomData,
    }
  }
}

impl Drop for TraceSpan<'_> {
  fn drop(&mut self) {
    if let (Some(state), Some(start)) = (&self.state, self.start) {
      state.push_event(self.name.clone(), self.cat, start, Instant::now());
    }
  }
}

#[derive(Serialize, Clone)]
struct TraceEvent {
  name: String,
  cat: String,
  ph: &'static str,
  ts: u64,
  dur: u64,
  pid: u32,
  tid: u64,
}

#[derive(Serialize)]
struct TraceFile {
  #[serde(rename = "traceEvents")]
  trace_events: Vec<TraceEvent>,
}
