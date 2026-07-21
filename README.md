# wire-extractor

One command: freeze a Lamdera app’s `ToBackend`/`ToFrontend` wire surface as `Protocol.elm`, generate Wire3 property proofs, and run them.

```bash
node path/to/wire-extractor/bin/wire-extractor.js
# or:  node .../wire-extractor.js --project /path/to/app
```

Needs: `elm-review`, `elm-test-rs`, `lamdera`.

Writes `tests/Protocol.elm` + `tests/ProtocolWireProof.elm`, then runs:

```text
protocol-encode → app-decode → app-encode → protocol-decode
```

(byte-identical Wire3 via real `w3_*` codecs; exhaustive ctors + fuzz where args are kernel types.)

```bash
# protocol file only, no tests
node .../wire-extractor.js extract-only -o Protocol.elm
```

Multi-app smoke (not the main CLI):

```bash
node scripts/prove-compiling-projects.js --git-root ~/git
```

MIT · https://github.com/sjalq/wire-extractor
