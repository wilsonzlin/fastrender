use fastrender::api::FastRenderPool;

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
