# HTTP client pooling

- `HttpFetcher` owns reusable HTTP client state so sequential subresource fetches reuse TCP/TLS connections (and cookies) instead of paying a handshake per request.
  - HTTP/1.1 backend (`ureq`): pooled `ureq::Agent`.
  - HTTP/2-capable backend (`reqwest`): pooled `reqwest::blocking::Client` with a shared cookie jar.
- The `ureq` agent is rebuilt when timeouts change because ureq configures timeouts at the agent level; `reqwest` timeouts are set per request so the client can stay pooled.
- Run `cargo bench --bench http_agent_pool` to compare the pooled `ureq` agent against a fresh agent per request on a local keep-alive server. (The benchmark uses `http://` so it exercises the `ureq` path; `https://` requests would select `reqwest` under `FASTR_HTTP_BACKEND=auto`.)
