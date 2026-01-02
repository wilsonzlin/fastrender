use xtask::{extract_pageset_extra_arg_overrides, PagesetExtraArgsOverrides};

#[test]
fn extracts_pageset_extra_arg_overrides() {
  let extra = vec![
    "--foo".to_string(),
    "--no-disk-cache".to_string(),
    "--no-fetch".to_string(),
    "--pages".to_string(),
    "discord.com".to_string(),
    "--user-agent=TestAgent".to_string(),
    "--accept-language".to_string(),
    "en-US".to_string(),
    "--viewport".to_string(),
    "1200x800".to_string(),
    "--dpr=2".to_string(),
    "--fetch-timeout".to_string(),
    "60".to_string(),
    "--render-timeout=7".to_string(),
    "--shard".to_string(),
    "0/4".to_string(),
    "--bar".to_string(),
  ];

  let (filtered, overrides) = extract_pageset_extra_arg_overrides(&extra);
  assert_eq!(
    filtered,
    vec!["--foo".to_string(), "--bar".to_string()],
    "wrapper-level knobs should be stripped from forwarded args"
  );
  assert_eq!(
    overrides,
    PagesetExtraArgsOverrides {
      pages: Some("discord.com".to_string()),
      shard: Some("0/4".to_string()),
      user_agent: Some("TestAgent".to_string()),
      accept_language: Some("en-US".to_string()),
      viewport: Some("1200x800".to_string()),
      dpr: Some("2".to_string()),
      disk_cache: Some(false),
      no_fetch: true,
      fetch_timeout: Some("60".to_string()),
      render_timeout: Some("7".to_string()),
    }
  );
}

#[test]
fn extracts_pageset_extra_arg_overrides_disk_cache_enabled() {
  let extra = vec!["--disk-cache".to_string(), "--pages=example.com".to_string()];

  let (filtered, overrides) = extract_pageset_extra_arg_overrides(&extra);
  assert_eq!(filtered, Vec::<String>::new());
  assert_eq!(
    overrides,
    PagesetExtraArgsOverrides {
      pages: Some("example.com".to_string()),
      shard: None,
      user_agent: None,
      accept_language: None,
      viewport: None,
      dpr: None,
      disk_cache: Some(true),
      no_fetch: false,
      fetch_timeout: None,
      render_timeout: None,
    }
  );
}
