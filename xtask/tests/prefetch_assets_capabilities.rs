use xtask::{
  extract_prefetch_assets_args, parse_prefetch_assets_capabilities, PrefetchAssetsSupport,
};

#[test]
fn parses_prefetch_assets_capabilities_and_extracts_supported_flags() {
  let json = r#"
{
  "name": "prefetch_assets",
  "disk_cache_feature": true,
  "flags": {
    "prefetch_fonts": true,
    "prefetch_images": false,
    "prefetch_iframes": true,
    "prefetch_embeds": false,
    "prefetch_icons": true,
    "prefetch_video_posters": false,
    "prefetch_css_url_assets": true,
    "max_discovered_assets_per_page": true,
    "max_images_per_page": false,
    "max_image_urls_per_element": true
  }
}
"#;

  let capabilities = parse_prefetch_assets_capabilities(json).expect("parse capabilities JSON");
  assert_eq!(capabilities.name, "prefetch_assets");
  assert!(capabilities.disk_cache_feature);

  let support = capabilities.flags;
  assert_eq!(
    support,
    PrefetchAssetsSupport {
      prefetch_fonts: true,
      prefetch_images: false,
      prefetch_iframes: true,
      prefetch_embeds: false,
      prefetch_icons: true,
      prefetch_video_posters: false,
      prefetch_css_url_assets: true,
      max_discovered_assets_per_page: true,
      max_images_per_page: false,
      max_image_urls_per_element: true,
    }
  );

  let extra = vec![
    "--prefetch-fonts".to_string(),
    "false".to_string(),
    "--prefetch-images".to_string(),
    "--prefetch-iframes=true".to_string(),
    "--prefetch-documents".to_string(),
    "true".to_string(),
    "--prefetch-embeds".to_string(),
    "--prefetch-icons".to_string(),
    "--prefetch-video-posters=1".to_string(),
    "--prefetch-css-url-assets".to_string(),
    "--max-discovered-assets-per-page".to_string(),
    "10".to_string(),
    "--max-images-per-page=5".to_string(),
    "--max-image-urls-per-element".to_string(),
    "2".to_string(),
    "--foo".to_string(),
    "bar".to_string(),
  ];

  let (prefetch_args, pageset_args) = extract_prefetch_assets_args(&extra, support);
  assert_eq!(
    prefetch_args,
    vec![
      "--prefetch-fonts".to_string(),
      "false".to_string(),
      "--prefetch-iframes=true".to_string(),
      "--prefetch-documents".to_string(),
      "true".to_string(),
      "--prefetch-icons".to_string(),
      "--prefetch-css-url-assets".to_string(),
      "--max-discovered-assets-per-page".to_string(),
      "10".to_string(),
      "--max-image-urls-per-element".to_string(),
      "2".to_string(),
    ],
    "supported prefetch flags (and their values) should be extracted"
  );
  assert_eq!(
    pageset_args,
    vec![
      "--prefetch-images".to_string(),
      "--prefetch-embeds".to_string(),
      "--prefetch-video-posters=1".to_string(),
      "--max-images-per-page=5".to_string(),
      "--foo".to_string(),
      "bar".to_string(),
    ],
    "unsupported flags should remain in pageset args"
  );
}
