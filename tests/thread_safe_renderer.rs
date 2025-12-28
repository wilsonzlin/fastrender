use fastrender::api::{FastRenderPool, FastRenderPoolConfig, RenderOptions};

fn assert_send_sync<T: Send + Sync>() {}

#[test]
fn pooled_renderer_renders_consistently_across_threads() {
  assert_send_sync::<FastRenderPool>();

  let pool = FastRenderPool::new().expect("pool");
  let html = r#"
    <html>
      <head>
        <style>
          body { margin: 0; background: white; }
          .box { width: 32px; height: 32px; background: rgb(10, 200, 30); }
        </style>
      </head>
      <body>
        <div class="box"></div>
      </body>
    </html>
  "#;
  let width = 64;
  let height = 64;

  let expected = pool
    .render_html(html, width, height)
    .expect("baseline render")
    .data()
    .to_vec();

  let threads = 4;
  let handles: Vec<_> = (0..threads)
    .map(|_| {
      let pool = pool.clone();
      let html = html.to_string();
      std::thread::spawn(move || {
        pool
          .render_html(&html, width, height)
          .expect("thread render")
          .data()
          .to_vec()
      })
    })
    .collect();

  for handle in handles {
    let data = handle.join().expect("thread join");
    assert_eq!(data, expected);
  }
}

#[test]
fn pooled_renderer_handles_multiple_documents_in_parallel() {
  let pool =
    FastRenderPool::with_config(FastRenderPoolConfig::new().with_pool_size(2)).expect("pool");
  let pages: Vec<(String, RenderOptions)> = vec![
    (
      r#"
      <html>
        <body style="margin:0;background:rgb(200,10,10)">
          <div style="width:8px;height:8px;background:rgb(0,0,0)"></div>
        </body>
      </html>
    "#
      .to_string(),
      RenderOptions::new().with_viewport(32, 32),
    ),
    (
      r#"
      <html>
        <body style="margin:0;background:rgb(10,200,10)">
          <div style="width:16px;height:4px;background:rgb(50,50,50)"></div>
        </body>
      </html>
    "#
      .to_string(),
      RenderOptions::new()
        .with_viewport(40, 24)
        .with_scroll(2.0, 0.0),
    ),
    (
      r#"
      <html>
        <body style="margin:0;background:rgb(10,10,200)">
          <div style="width:4px;height:16px;background:rgb(255,255,0)"></div>
        </body>
      </html>
    "#
      .to_string(),
      RenderOptions::new()
        .with_viewport(24, 40)
        .with_fit_canvas_to_content(true),
    ),
  ];

  let expected: Vec<Vec<u8>> = pages
    .iter()
    .map(|(html, opts)| {
      pool
        .render_html_with_options(html, opts.clone())
        .expect("baseline render")
        .data()
        .to_vec()
    })
    .collect();

  let handles: Vec<_> = (0..pages.len() * 3)
    .map(|i| {
      let pool = pool.clone();
      let idx = i % pages.len();
      let html = pages[idx].0.clone();
      let opts = pages[idx].1.clone();
      let expected = expected[idx].clone();
      std::thread::spawn(move || {
        let data = pool
          .render_html_with_options(&html, opts)
          .expect("parallel render")
          .data()
          .to_vec();
        assert_eq!(data, expected);
      })
    })
    .collect();

  for handle in handles {
    handle.join().expect("thread join");
  }
}
