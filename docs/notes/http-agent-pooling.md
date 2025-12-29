# HTTP agent pooling

- `HttpFetcher` now owns a reusable `ureq::Agent` so sequential subresource fetches reuse TCP/TLS connections instead of paying a handshake per request.
- The agent is rebuilt when timeouts change to keep behavior identical to the one-off configuration.
- Run `cargo bench --bench http_agent_pool` to compare the pooled client against a fresh agent per request on a local keep-alive server; the pooled path avoids re-establishing connections and is the code path used in production fetches.
