use fastrender::image_loader::ImageCache;
use fastrender::resource::HttpFetcher;
use std::sync::Arc;

#[test]
fn data_url_svg_with_percent_encoded_hash_renders() {
  let url = "data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"6\" height=\"6\" fill=\"none\"><path stroke=\"%23000\" d=\"M.5 0v2M2 .5H0M.5 4v2M2 5.5H0M5.5 4v2M4 5.5h2M4 .5h2M5.5 2V0\"/></svg>";

  let cache =
    ImageCache::with_base_url_and_fetcher("file:///".to_string(), Arc::new(HttpFetcher::new()));
  let image = cache.load(url).expect("SVG data URL should decode");
  assert!(image.is_vector, "expected SVG to be treated as vector");
}
