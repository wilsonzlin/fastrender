use std::time::Duration;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::css::parser::{parse_stylesheet, parse_stylesheet_with_media};
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

  group.finish();
}

criterion_group!(benches, bench_css_parse_pageset);
criterion_main!(benches);
