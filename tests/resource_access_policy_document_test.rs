use fastrender::api::ResourceContext;
use fastrender::resource::{origin_from_url, ResourceAccessPolicy};
use fastrender::ResourceKind;

#[test]
fn iframe_documents_ignore_same_origin_subresource_policy() {
  let policy = ResourceAccessPolicy {
    document_origin: origin_from_url("https://example.test/"),
    same_origin_only: true,
    ..ResourceAccessPolicy::default()
  };
  let ctx = ResourceContext {
    policy,
    document_url: None,
    diagnostics: None,
    iframe_depth_remaining: None,
  };

  assert!(
    ctx
      .check_allowed(ResourceKind::Document, "https://other.test/frame.html")
      .is_ok(),
    "iframe document loads should not be blocked by same-origin subresource policy"
  );

  assert!(
    ctx
      .check_allowed_with_final(
        ResourceKind::Document,
        "https://example.test/frame.html",
        Some("https://other.test/frame.html"),
      )
      .is_ok(),
    "document loads should remain allowed even when redirected cross-origin"
  );

  for (kind, url) in [
    (ResourceKind::Image, "https://other.test/image.png"),
    (ResourceKind::Stylesheet, "https://other.test/style.css"),
  ] {
    let err = ctx
      .check_allowed(kind, url)
      .expect_err("cross-origin subresources should still be blocked");
    assert!(
      err.reason.contains("cross-origin"),
      "unexpected policy message: {}",
      err.reason
    );
  }
}

#[test]
fn document_loads_still_apply_mixed_content_and_file_restrictions() {
  let origin = origin_from_url("https://example.test/").expect("origin");

  let ctx = ResourceContext {
    policy: ResourceAccessPolicy {
      document_origin: Some(origin.clone()),
      block_mixed_content: true,
      same_origin_only: true,
      ..ResourceAccessPolicy::default()
    },
    document_url: None,
    diagnostics: None,
    iframe_depth_remaining: None,
  };
  assert!(
    ctx
      .check_allowed(ResourceKind::Document, "http://other.test/frame.html")
      .is_err(),
    "expected mixed-content iframe document load to be blocked for HTTPS documents when block_mixed_content is enabled"
  );

  let ctx = ResourceContext {
    policy: ResourceAccessPolicy {
      document_origin: Some(origin),
      allow_file_from_http: false,
      same_origin_only: true,
      ..ResourceAccessPolicy::default()
    },
    document_url: None,
    diagnostics: None,
    iframe_depth_remaining: None,
  };
  assert!(
    ctx
      .check_allowed(ResourceKind::Document, "file:///etc/passwd")
      .is_err(),
    "expected file:// iframe document load to be blocked for HTTP(S) documents when allow_file_from_http is disabled"
  );
}
