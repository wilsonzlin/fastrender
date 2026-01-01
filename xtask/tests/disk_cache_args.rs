use xtask::extract_disk_cache_args;

#[test]
fn extracts_all_disk_cache_flags() {
  let extra = vec![
    "--pages".to_string(),
    "example.com".to_string(),
    "--disk-cache-max-bytes".to_string(),
    "123".to_string(),
    "--unrelated".to_string(),
    "--disk-cache-max-bytes=456".to_string(),
    "--disk-cache-allow-no-store".to_string(),
    "--viewport".to_string(),
    "1200x800".to_string(),
  ];

  assert_eq!(
    extract_disk_cache_args(&extra),
    vec![
      "--disk-cache-max-bytes".to_string(),
      "123".to_string(),
      "--disk-cache-max-bytes=456".to_string(),
      "--disk-cache-allow-no-store".to_string(),
    ]
  );
}

#[test]
fn does_not_forward_non_prefixed_flags() {
  let extra = vec![
    "--disk-cache".to_string(),
    "--cache-dir".to_string(),
    "fetches/assets".to_string(),
    "--timeout".to_string(),
    "30".to_string(),
  ];

  assert!(extract_disk_cache_args(&extra).is_empty());
}

