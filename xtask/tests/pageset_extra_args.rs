use xtask::{extract_pageset_extra_arg_overrides, PagesetExtraArgsOverrides};

#[test]
fn extracts_pageset_extra_arg_overrides() {
  let extra = vec![
    "--foo".to_string(),
    "--pages".to_string(),
    "discord.com".to_string(),
    "--user-agent=TestAgent".to_string(),
    "--accept-language".to_string(),
    "en-US".to_string(),
    "--viewport".to_string(),
    "1200x800".to_string(),
    "--dpr=2".to_string(),
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
    }
  );
}

