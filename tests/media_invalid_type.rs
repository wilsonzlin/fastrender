use fastrender::style::media::MediaQuery;

#[test]
fn rejects_unknown_media_type() {
    // CSS spec: unknown/unsupported media types are invalid and cause the query to fail.
    let q = MediaQuery::parse("@media foobar and (min-width: 500px)");
    assert!(q.is_err(), "unknown media type should be invalid");
}

