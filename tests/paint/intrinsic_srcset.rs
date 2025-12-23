use base64::prelude::BASE64_STANDARD;
use fastrender::api::FastRender;
use fastrender::tree::box_tree::ReplacedType;
use fastrender::tree::fragment_tree::FragmentContent;
use image::codecs::png::PngEncoder;
use image::ExtendedColorType;

fn solid_color_png(width: u32, height: u32, r: u8, g: u8, b: u8, a: u8) -> String {
  let mut buf = Vec::new();
  let pixels: Vec<u8> = std::iter::repeat([r, g, b, a])
    .take((width * height) as usize)
    .flatten()
    .collect();

  PngEncoder::new(&mut buf)
    .write_image(&pixels, width, height, ExtendedColorType::Rgba8)
    .expect("encode png");

  format!("data:image/png;base64,{}", BASE64_STANDARD.encode(&buf))
}

#[test]
fn img_srcset_without_src_uses_intrinsic_size() {
  let red = solid_color_png(4, 2, 255, 0, 0, 255);
  let html = format!(
    r#"
    <style>
      body {{ margin: 0; background: rgb(255 255 255); }}
      img {{ display: block; }}
    </style>
    <img srcset="{red} 1x">
    "#
  );

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let fragments = renderer
    .layout_document(&dom, 10, 10)
    .expect("layout document");
  let image_fragment = fragments
    .iter_fragments()
    .find(|f| {
      matches!(
        f.content,
        FragmentContent::Replaced {
          replaced_type: ReplacedType::Image { .. },
          ..
        }
      )
    })
    .expect("image fragment");

  assert!(
    (image_fragment.bounds.width() - 4.0).abs() < 1e-3,
    "expected intrinsic width from srcset"
  );
  assert!(
    (image_fragment.bounds.height() - 2.0).abs() < 1e-3,
    "expected intrinsic height from srcset"
  );

  let pixmap = renderer.render_html(&html, 10, 10).expect("render html");
  let inside = pixmap.pixel(1, 0).expect("inside pixel");
  assert_eq!((inside.red(), inside.green(), inside.blue()), (255, 0, 0));

  let outside_right = pixmap.pixel(5, 0).expect("right of image");
  assert_eq!(
    (
      outside_right.red(),
      outside_right.green(),
      outside_right.blue()
    ),
    (255, 255, 255)
  );
  let outside_bottom = pixmap.pixel(0, 3).expect("below image");
  assert_eq!(
    (
      outside_bottom.red(),
      outside_bottom.green(),
      outside_bottom.blue()
    ),
    (255, 255, 255)
  );
}

#[test]
fn picture_empty_img_src_uses_source_and_dpr() {
  let low_dpi = solid_color_png(2, 2, 255, 0, 0, 255);
  let high_dpi = solid_color_png(4, 4, 0, 255, 0, 255);
  let html = format!(
    r#"
    <style>
      body {{ margin: 0; background: rgb(255 255 255); }}
      img {{ display: block; }}
    </style>
    <picture>
      <source srcset="{high_dpi} 2x, {low_dpi} 1x" type="image/png">
      <img src="" alt="">
    </picture>
    "#
  );

  let mut renderer = FastRender::builder()
    .device_pixel_ratio(2.0)
    .build()
    .expect("renderer");

  let dom = renderer.parse_html(&html).expect("parse html");
  let fragments = renderer
    .layout_document(&dom, 10, 10)
    .expect("layout document");
  let image_fragment = fragments
    .iter_fragments()
    .find(|f| {
      matches!(
        f.content,
        FragmentContent::Replaced {
          replaced_type: ReplacedType::Image { .. },
          ..
        }
      )
    })
    .expect("image fragment");

  assert!(
    (image_fragment.bounds.width() - 2.0).abs() < 1e-3,
    "intrinsic width should follow selected source"
  );
  assert!(
    (image_fragment.bounds.height() - 2.0).abs() < 1e-3,
    "intrinsic height should follow selected source"
  );

  let pixmap = renderer.render_html(&html, 10, 10).expect("render html");
  let inside = pixmap.pixel(1, 1).expect("inside pixel");
  assert_eq!((inside.red(), inside.green(), inside.blue()), (0, 255, 0));

  let outside = pixmap.pixel(6, 0).expect("outside pixel");
  assert_eq!(
    (outside.red(), outside.green(), outside.blue()),
    (255, 255, 255)
  );
}
