use std::fs;
use std::process::Command;

#[test]
fn dom_compat_flag_flips_class_dependent_rendering() {
  let tmp = tempfile::TempDir::new().expect("tempdir");
  let html_path = tmp.path().join("page.html");
  fs::write(
    &html_path,
    r#"<!doctype html><html class="no-js"><head><style>
html, body { margin: 0; width: 100%; height: 100%; }
html.no-js body { background: rgb(255, 0, 0); }
html.js-enabled body { background: rgb(0, 255, 0); }
</style></head><body></body></html>"#,
  )
  .expect("write html fixture");

  let url = format!("file://{}", html_path.display());
  let standard_png = tmp.path().join("standard.png");
  let compat_png = tmp.path().join("compat.png");

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .args([&url, standard_png.to_str().unwrap()])
    .args(["--viewport", "64x64"])
    .status()
    .expect("run fetch_and_render (standard)");
  assert!(
    status.success(),
    "standard render should exit successfully without compat flag"
  );

  let status = Command::new(env!("CARGO_BIN_EXE_fetch_and_render"))
    .args(["--dom-compat", "compat", &url, compat_png.to_str().unwrap()])
    .args(["--viewport", "64x64"])
    .status()
    .expect("run fetch_and_render (compat)");
  assert!(
    status.success(),
    "compat render should exit successfully with dom-compat flag"
  );

  let standard_image = image::open(&standard_png)
    .expect("open standard render")
    .into_rgba8();
  let compat_image = image::open(&compat_png)
    .expect("open compat render")
    .into_rgba8();

  let standard_pixel = standard_image.get_pixel(0, 0);
  let compat_pixel = compat_image.get_pixel(0, 0);

  assert!(
    standard_pixel.0[0] > 200 && standard_pixel.0[1] < 80,
    "standard run should keep the red background from html.no-js"
  );
  assert!(
    compat_pixel.0[1] > 200 && compat_pixel.0[0] < 80,
    "dom compat run should flip to the green background from html.js-enabled"
  );
}
