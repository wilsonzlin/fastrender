use std::alloc::{GlobalAlloc, Layout, System};
use std::fmt::Write;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::css::parser::{
  parse_stylesheet, parse_stylesheet_with_media, parse_stylesheet_with_media_cached_arc,
  parse_stylesheet_with_media_cached_by_url_arc,
};
use fastrender::css::types::CssImportLoader;
use fastrender::style::media::{MediaContext, MediaQueryCache};

const APPLE_STYLESHEET: &str =
  include_str!("../tests/pages/fixtures/apple.com/assets/bc56bc2a7a8f28c1f7d1f49327547b3b.css");

const DEBIAN_STYLESHEETS: [&str; 5] = [
  include_str!("../tests/pages/fixtures/debian.org/assets/3ae02e0afd2181eb0ff37bf94673c666.css"),
  include_str!("../tests/pages/fixtures/debian.org/assets/5dd64848164e9158a29823613e675af9.css"),
  include_str!("../tests/pages/fixtures/debian.org/assets/752495a8a164e0f64d717ad6a392518c.css"),
  include_str!("../tests/pages/fixtures/debian.org/assets/c032e7a896b8475b1aa6d7d7ba429760.css"),
  include_str!("../tests/pages/fixtures/debian.org/assets/c63ccc983ab22b239286b58b0fc8838d.css"),
];

const DISCORD_STYLESHEET: &str =
  include_str!("../tests/pages/fixtures/discord.com/assets/2164002ff1c149956ba135bfcc553b74.css");
struct NoImportLoader;

impl CssImportLoader for NoImportLoader {
  fn load(&self, url: &str) -> fastrender::error::Result<String> {
    unreachable!("unexpected import load in benchmark: {url}");
  }
}

struct StaticImportLoader;

impl CssImportLoader for StaticImportLoader {
  fn load(&self, _url: &str) -> fastrender::error::Result<String> {
    Ok(".imported { color: red; }\n".to_string())
  }
}

struct CountingAllocator;

static COUNT_ALLOCATIONS: AtomicBool = AtomicBool::new(false);
static ALLOC_CALLS: AtomicUsize = AtomicUsize::new(0);
static ALLOC_BYTES: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAllocator {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    if COUNT_ALLOCATIONS.load(Ordering::Relaxed) {
      ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
      ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
    }
    System.alloc(layout)
  }

  unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
    if COUNT_ALLOCATIONS.load(Ordering::Relaxed) {
      ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
      ALLOC_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
    }
    System.alloc_zeroed(layout)
  }

  unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
    if COUNT_ALLOCATIONS.load(Ordering::Relaxed) {
      ALLOC_CALLS.fetch_add(1, Ordering::Relaxed);
      ALLOC_BYTES.fetch_add(new_size, Ordering::Relaxed);
    }
    System.realloc(ptr, layout, new_size)
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    System.dealloc(ptr, layout)
  }
}

#[global_allocator]
static GLOBAL: CountingAllocator = CountingAllocator;

fn allocation_counts() -> (usize, usize) {
  (
    ALLOC_CALLS.load(Ordering::Relaxed),
    ALLOC_BYTES.load(Ordering::Relaxed),
  )
}

struct AllocationCountGuard;

impl AllocationCountGuard {
  fn new() -> Self {
    COUNT_ALLOCATIONS.store(true, Ordering::Relaxed);
    Self
  }
}

impl Drop for AllocationCountGuard {
  fn drop(&mut self) {
    COUNT_ALLOCATIONS.store(false, Ordering::Relaxed);
  }
}

fn allocation_delta<R>(f: impl FnOnce() -> R) -> (usize, usize, R) {
  let (calls_start, bytes_start) = allocation_counts();
  let _guard = AllocationCountGuard::new();
  let result = f();
  drop(_guard);
  let (calls_end, bytes_end) = allocation_counts();
  (
    calls_end.saturating_sub(calls_start),
    bytes_end.saturating_sub(bytes_start),
    result,
  )
}

fn bench_css_parse_pageset(c: &mut Criterion) {
  let mut group = c.benchmark_group("css_parse_pageset");
  group
    .sample_size(10)
    .warm_up_time(Duration::from_secs(1))
    .measurement_time(Duration::from_secs(4));

  group.bench_function("apple.com/full", |b| {
    b.iter(|| {
      let sheet = parse_stylesheet(black_box(APPLE_STYLESHEET)).expect("parse stylesheet");
      black_box(sheet);
    });
  });

  // Print allocation stats once for the full stylesheet parse so changes that reduce allocator
  // pressure (like interned property names) are easy to validate locally.
  static PRINTED_APPLE: AtomicBool = AtomicBool::new(false);
  if !PRINTED_APPLE.swap(true, Ordering::Relaxed) {
    // Warm caches (e.g. known-property tables) so the allocation delta is closer to steady state.
    let _ = parse_stylesheet(APPLE_STYLESHEET).expect("warm parse stylesheet");
    let (calls, bytes, result) = allocation_delta(|| parse_stylesheet(APPLE_STYLESHEET));
    let _ = result.expect("allocation-count parse stylesheet");
    eprintln!("apple.com/full allocations/parse: calls={calls} bytes={bytes}");
  }

  let media_ctx = MediaContext::screen(1200.0, 800.0);
  group.bench_function("apple.com/pruned_media_1200x800", |b| {
    b.iter(|| {
      let mut cache = MediaQueryCache::default();
      let sheet =
        parse_stylesheet_with_media(black_box(APPLE_STYLESHEET), &media_ctx, Some(&mut cache))
          .expect("parse stylesheet with media pruning");
      black_box(sheet);
    });
  });

  let loader = NoImportLoader;
  group.bench_function(
    "apple.com/pruned_media_1200x800/resolve_imports_owned",
    |b| {
      b.iter(|| {
        let mut cache = MediaQueryCache::default();
        let sheet =
          parse_stylesheet_with_media(black_box(APPLE_STYLESHEET), &media_ctx, Some(&mut cache))
            .expect("parse stylesheet with media pruning");
        let resolved = sheet
          .resolve_imports_owned_with_cache(
            &loader,
            Some("https://example.com/apple.css"),
            &media_ctx,
            Some(&mut cache),
          )
          .expect("resolve imports");
        black_box(resolved);
      });
    },
  );

  group.bench_function(
    "apple.com/pruned_media_1200x800/resolve_imports_borrowed",
    |b| {
      b.iter(|| {
        let mut cache = MediaQueryCache::default();
        let sheet =
          parse_stylesheet_with_media(black_box(APPLE_STYLESHEET), &media_ctx, Some(&mut cache))
            .expect("parse stylesheet with media pruning");
        let resolved = sheet
          .resolve_imports_with_cache(
            &loader,
            Some("https://example.com/apple.css"),
            &media_ctx,
            Some(&mut cache),
          )
          .expect("resolve imports");
        black_box(resolved);
      });
    },
  );

  group.bench_function("debian.org/multi_full", |b| {
    b.iter(|| {
      for css in DEBIAN_STYLESHEETS {
        let sheet = parse_stylesheet(black_box(css)).expect("parse stylesheet");
        black_box(sheet);
      }
    });
  });

  group.bench_function("debian.org/multi_pruned_media_1200x800", |b| {
    b.iter(|| {
      let mut cache = MediaQueryCache::default();
      for css in DEBIAN_STYLESHEETS {
        let sheet = parse_stylesheet_with_media(black_box(css), &media_ctx, Some(&mut cache))
          .expect("parse stylesheet with media pruning");
        black_box(sheet);
      }
    });
  });

  group.bench_function("discord.com/full", |b| {
    b.iter(|| {
      let sheet = parse_stylesheet(black_box(DISCORD_STYLESHEET)).expect("parse stylesheet");
      black_box(sheet);
    });
  });

  group.bench_function("discord.com/pruned_media_1200x800", |b| {
    b.iter(|| {
      let mut cache = MediaQueryCache::default();
      let sheet =
        parse_stylesheet_with_media(black_box(DISCORD_STYLESHEET), &media_ctx, Some(&mut cache))
          .expect("parse stylesheet with media pruning");
      black_box(sheet);
    });
  });

  let cached_base_url = "https://example.test/assets/apple-content-hash.css";
  let cached_url = "https://example.test/assets/apple-url-keyed.css";

  // Prime the process-wide parsed stylesheet caches so the benchmark measures cache-hit overhead.
  let _ = parse_stylesheet_with_media_cached_arc(
    APPLE_STYLESHEET,
    Some(cached_base_url),
    &media_ctx,
    None,
  )
  .expect("cached parse should succeed");
  let _ =
    parse_stylesheet_with_media_cached_by_url_arc(APPLE_STYLESHEET, cached_url, &media_ctx, None)
      .expect("cached parse should succeed");

  group.bench_function("apple.com/pruned_media_cached_hit_content_hash", |b| {
    b.iter(|| {
      let sheet = parse_stylesheet_with_media_cached_arc(
        black_box(APPLE_STYLESHEET),
        Some(cached_base_url),
        &media_ctx,
        None,
      )
      .expect("cached parse hit");
      black_box(sheet);
    });
  });

  group.bench_function("apple.com/pruned_media_cached_hit_url_keyed", |b| {
    b.iter(|| {
      let sheet = parse_stylesheet_with_media_cached_by_url_arc(
        black_box(APPLE_STYLESHEET),
        cached_url,
        &media_ctx,
        None,
      )
      .expect("cached parse hit");
      black_box(sheet);
    });
  });

  // Micro-benchmark: large stylesheet with many declarations that do not reference `var()`.
  fn synthetic_no_var_stylesheet(declarations: usize) -> String {
    let mut css = String::with_capacity(declarations.saturating_mul(32));
    css.push_str(".synthetic{");
    for idx in 0..declarations {
      match idx % 6 {
        0 => {
          let _ = write!(css, "margin-left:{}px;", (idx % 16) + 1);
        }
        1 => {
          let _ = write!(css, "padding-top:{}px;", (idx % 12) + 1);
        }
        2 => css.push_str("opacity:0.5;"),
        3 => {
          let _ = write!(css, "z-index:{};", (idx % 10) as i32);
        }
        4 => css.push_str("display:block;"),
        _ => css.push_str("position:relative;"),
      }
    }
    css.push_str("}\n");
    css
  }

  let synthetic_css = synthetic_no_var_stylesheet(25_000);
  group.bench_function("synthetic_no_var_25k_decls", |b| {
    b.iter(|| {
      let sheet = parse_stylesheet(black_box(&synthetic_css)).expect("parse stylesheet");
      black_box(sheet);
    });
  });

  // Micro-benchmark: large stylesheet that repeats the same small set of property/value literals.
  //
  // This stresses the parsed-value memoization cache: the parser should avoid re-tokenizing and
  // re-parsing identical values, while also keeping per-declaration cache lookup overhead low.
  fn synthetic_repeat_values_stylesheet(repeats: usize) -> String {
    let mut css = String::with_capacity(repeats.saturating_mul(64));
    css.push_str(".synthetic_repeat{");
    for _ in 0..repeats {
      css.push_str("width:16px;height:16px;color:#fff;display:none;");
    }
    css.push_str("}\n");
    css
  }

  // 4 declarations per repeat.
  let repeat_values_css = synthetic_repeat_values_stylesheet(25_000 / 4);
  group.bench_function("synthetic_repeat_values_25k_decls", |b| {
    b.iter(|| {
      let sheet = parse_stylesheet(black_box(&repeat_values_css)).expect("parse stylesheet");
      black_box(sheet);
    });
  });

  let synthetic_import_css = format!("@import \"imported.css\";\n{synthetic_css}");
  let import_loader = StaticImportLoader;
  group.bench_function(
    "synthetic_import_25k_decls/resolve_imports_owned",
    |b| {
      b.iter(|| {
        let mut cache = MediaQueryCache::default();
        let sheet = parse_stylesheet_with_media(
          black_box(&synthetic_import_css),
          &media_ctx,
          Some(&mut cache),
        )
        .expect("parse stylesheet with media pruning");
        let resolved = sheet
          .resolve_imports_owned_with_cache(
            &import_loader,
            Some("https://example.com/synthetic.css"),
            &media_ctx,
            Some(&mut cache),
          )
          .expect("resolve imports");
        black_box(resolved);
      });
    },
  );
  group.bench_function(
    "synthetic_import_25k_decls/resolve_imports_borrowed",
    |b| {
      b.iter(|| {
        let mut cache = MediaQueryCache::default();
        let sheet = parse_stylesheet_with_media(
          black_box(&synthetic_import_css),
          &media_ctx,
          Some(&mut cache),
        )
        .expect("parse stylesheet with media pruning");
        let resolved = sheet
          .resolve_imports_with_cache(
            &import_loader,
            Some("https://example.com/synthetic.css"),
            &media_ctx,
            Some(&mut cache),
          )
          .expect("resolve imports");
        black_box(resolved);
      });
    },
  );

  static PRINTED_SYNTHETIC: AtomicBool = AtomicBool::new(false);
  if !PRINTED_SYNTHETIC.swap(true, Ordering::Relaxed) {
    let _ = parse_stylesheet(&synthetic_css).expect("warm parse synthetic stylesheet");
    let (calls, bytes, result) = allocation_delta(|| parse_stylesheet(&synthetic_css));
    let _ = result.expect("allocation-count parse synthetic stylesheet");
    eprintln!("synthetic_no_var_25k_decls allocations/parse: calls={calls} bytes={bytes}");
  }

  group.finish();
}

criterion_group!(benches, bench_css_parse_pageset);
criterion_main!(benches);
