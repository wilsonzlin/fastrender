use base64::prelude::BASE64_STANDARD;
use base64::Engine as _;
use fastrender::api::FastRender;
use fastrender::tree::box_tree::ReplacedType;
use fastrender::tree::fragment_tree::FragmentContent;
use fastrender::RenderOptions;
use image::codecs::png::PngEncoder;
use image::ExtendedColorType;
use image::ImageEncoder;

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

#[test]
fn width_descriptor_uses_sizes_media_conditions() {
  let small = solid_color_png(20, 20, 255, 0, 0, 255);
  let large = solid_color_png(40, 40, 0, 0, 255, 255);
  let html = format!(
    r#"
    <style>
      body {{ margin: 0; background: rgb(255 255 255); }}
      img {{ display: block; }}
    </style>
    <img srcset="{small} 20w, {large} 40w" sizes="(max-width: 150px) 20px, 40px">
    "#
  );

  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(&html).expect("parse html");
  let narrow = renderer
    .layout_document(&dom, 120, 100)
    .expect("narrow layout");
  let narrow_fragment = narrow
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
    .expect("narrow image fragment");
  assert!(
    (narrow_fragment.bounds.width() - 20.0).abs() < 1e-3
      && (narrow_fragment.bounds.height() - 20.0).abs() < 1e-3,
    "expected 20px candidate for narrow viewport"
  );

  let narrow_options = RenderOptions::new().with_viewport(120, 100);
  let narrow_pixmap = renderer
    .render_html_with_options(&html, narrow_options)
    .expect("render narrow html");
  let narrow_px = narrow_pixmap.pixel(5, 5).expect("narrow pixel");
  assert_eq!(
    (narrow_px.red(), narrow_px.green(), narrow_px.blue()),
    (255, 0, 0),
    "narrow viewport should use small red candidate"
  );

  let mut wide_renderer = FastRender::new().expect("wide renderer");
  let wide_dom = wide_renderer.parse_html(&html).expect("parse html");
  let wide = wide_renderer
    .layout_document(&wide_dom, 400, 100)
    .expect("wide layout");
  let wide_fragment = wide
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
    .expect("wide image fragment");
  assert!(
    (wide_fragment.bounds.width() - 40.0).abs() < 1e-3
      && (wide_fragment.bounds.height() - 40.0).abs() < 1e-3,
    "expected 40px candidate for wide viewport"
  );

  let wide_options = RenderOptions::new().with_viewport(400, 100);
  let wide_pixmap = wide_renderer
    .render_html_with_options(&html, wide_options)
    .expect("render wide html");
  let wide_px = wide_pixmap.pixel(10, 10).expect("wide pixel");
  assert_eq!(
    (wide_px.red(), wide_px.green(), wide_px.blue()),
    (0, 0, 255),
    "wide viewport should use large blue candidate"
  );
}

#[test]
fn width_descriptor_tracks_device_pixel_ratio() {
  let base = solid_color_png(20, 20, 0, 255, 0, 255);
  let low = solid_color_png(20, 20, 255, 0, 0, 255);
  let high = solid_color_png(40, 40, 0, 0, 255, 255);
  let html = format!(
    r#"
    <style>
      body {{ margin: 0; }}
      img {{ display: block; width: 20px; height: 20px; }}
    </style>
    <img src="{base}" srcset="{low} 20w, {high} 40w">
    "#
  );

  let mut renderer_1x = FastRender::builder()
    .device_pixel_ratio(1.0)
    .build()
    .expect("renderer 1x");
  let pixmap_1x = renderer_1x.render_html(&html, 20, 20).expect("render 1x");
  let px_1x = pixmap_1x.pixel(10, 10).expect("center pixel 1x");
  assert_eq!(
    (px_1x.red(), px_1x.green(), px_1x.blue()),
    (255, 0, 0),
    "1x should select 20w red candidate"
  );

  let mut renderer_2x = FastRender::builder()
    .device_pixel_ratio(2.0)
    .build()
    .expect("renderer 2x");
  let pixmap_2x = renderer_2x.render_html(&html, 20, 20).expect("render 2x");
  let px_2x = pixmap_2x.pixel(10, 10).expect("center pixel 2x");
  assert_eq!(
    (px_2x.red(), px_2x.green(), px_2x.blue()),
    (0, 0, 255),
    "2x should select 40w blue candidate"
  );
}
