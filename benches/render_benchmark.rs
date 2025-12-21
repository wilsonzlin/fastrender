use criterion::black_box;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use fastrender::FastRender;

fn simple_html_benchmark(c: &mut Criterion) {
  let html = r#"
        <html>
            <head>
                <style>
                    body { font-family: sans-serif; margin: 20px; }
                    .box { width: 100px; height: 100px; background-color: blue; }
                </style>
            </head>
            <body>
                <h1>Simple Page</h1>
                <div class="box"></div>
            </body>
        </html>
    "#;

  c.bench_function("simple html render", |b| {
    let mut renderer = FastRender::new().unwrap();
    b.iter(|| renderer.render_to_png(black_box(html), 800, 600).unwrap());
  });
}

fn complex_html_benchmark(c: &mut Criterion) {
  let html = r#"
        <html>
            <head>
                <style>
                    body { font-family: sans-serif; }
                    .container { display: flex; flex-wrap: wrap; }
                    .card {
                        width: 200px;
                        height: 150px;
                        margin: 10px;
                        padding: 15px;
                        background: linear-gradient(135deg, #667eea, #764ba2);
                        border-radius: 10px;
                        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
                    }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="card">Card 1</div>
                    <div class="card">Card 2</div>
                    <div class="card">Card 3</div>
                    <div class="card">Card 4</div>
                    <div class="card">Card 5</div>
                    <div class="card">Card 6</div>
                </div>
            </body>
        </html>
    "#;

  c.bench_function("complex html render", |b| {
    let mut renderer = FastRender::new().unwrap();
    b.iter(|| renderer.render_to_png(black_box(html), 1920, 1080).unwrap());
  });
}

fn grid_layout_benchmark(c: &mut Criterion) {
  let html = r#"
        <html>
            <head>
                <style>
                    .grid {
                        display: grid;
                        grid-template-columns: repeat(4, 1fr);
                        grid-gap: 20px;
                    }
                    .item {
                        height: 100px;
                        background-color: #3498db;
                        border-radius: 5px;
                    }
                </style>
            </head>
            <body>
                <div class="grid">
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                </div>
            </body>
        </html>
    "#;

  c.bench_function("grid layout render", |b| {
    let mut renderer = FastRender::new().unwrap();
    b.iter(|| renderer.render_to_png(black_box(html), 1200, 800).unwrap());
  });
}

criterion_group!(
  benches,
  simple_html_benchmark,
  complex_html_benchmark,
  grid_layout_benchmark
);
criterion_main!(benches);
