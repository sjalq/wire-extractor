# wire-extractor

Freeze a Lamdera app’s `ToBackend` / `ToFrontend` as one `Protocol.elm`, generate Wire3 proofs, run them.

```bash
node path/to/wire-extractor/bin/wire-extractor.js
# node .../wire-extractor.js --project /path/to/app
# node .../wire-extractor.js extract-only -o Protocol.elm
```

Needs: `elm-review`, `elm-test-rs`, `lamdera`.

Writes `tests/Protocol.elm` + `tests/ProtocolWireProof.elm`, then:

`protocol-encode → app-decode → app-encode → protocol-decode` (real `w3_*`, exhaustive + fuzz).

Multi-app smoke (optional): `node scripts/prove-compiling-projects.js --git-root ~/git`

MIT · https://github.com/sjalq/wire-extractor
