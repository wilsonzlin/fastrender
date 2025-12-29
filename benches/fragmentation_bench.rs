use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::layout::axis::FragmentAxes;
use fastrender::layout::fragmentation::{FragmentationAnalyzer, FragmentationContext};
use fastrender::style::types::{Direction, WritingMode};
use fastrender::{FragmentNode, Rect};

fn build_flow_root(lines: usize, line_height: f32) -> FragmentNode {
  let mut children = Vec::with_capacity(lines);
  for i in 0..lines {
    let y = i as f32 * line_height;
    children.push(FragmentNode::new_line(
      Rect::from_xywh(0.0, y, 600.0, line_height),
      line_height * 0.8,
      Vec::new(),
    ));
  }
  let total_height = lines as f32 * line_height;
  FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 600.0, total_height), children)
}

fn bench_fragmentation(c: &mut Criterion) {
  let root = build_flow_root(5_000, 1.0);
  let axes =
    FragmentAxes::from_writing_mode_and_direction(WritingMode::HorizontalTb, Direction::Ltr);

  c.bench_function("fragmentation_reuse_analyzer", |b| {
    b.iter(|| {
      let mut analyzer =
        FragmentationAnalyzer::new(&root, FragmentationContext::Column, axes, None);
      let mut total = 0usize;
      for height in [140.0, 120.0, 160.0, 110.0, 150.0] {
        let boundaries = analyzer
          .boundaries(height, analyzer.content_extent().max(height))
          .unwrap();
        total += boundaries.len();
      }
      black_box(total)
    });
  });

  c.bench_function("fragmentation_collect_each_time", |b| {
    b.iter(|| {
      let mut total = 0usize;
      for height in [140.0, 120.0, 160.0, 110.0, 150.0] {
        let mut analyzer =
          FragmentationAnalyzer::new(&root, FragmentationContext::Column, axes, None);
        let boundaries = analyzer
          .boundaries(height, analyzer.content_extent().max(height))
          .unwrap();
        total += boundaries.len();
      }
      black_box(total)
    });
  });
}

criterion_group!(fragmentation_benches, bench_fragmentation);
criterion_main!(fragmentation_benches);
