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
    diagnostics: None,
  };

  assert!(
    ctx
      .check_allowed(ResourceKind::Document, "https://other.test/frame.html")
      .is_ok(),
    "iframe document loads should not be blocked by same-origin subresource policy"
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
