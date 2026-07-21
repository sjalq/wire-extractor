# wire-extractor

Extract a **single `Protocol.elm`** module from any [Lamdera](https://lamdera.com) app containing only:

- `ToBackend`
- `ToFrontend`
- types reachable from those message payloads

Then **prove Wire3 identity** against the live app codecs with exhaustive samples and property-based tests.

Analysis and codegen are **Elm** (`elm-review` + `elm-syntax` + `Elm.Docs`). Node is a thin CLI around that.

## Install

```bash
# clone / submodule
git clone https://github.com/sjalq/wire-extractor.git
# or: git submodule add https://github.com/sjalq/wire-extractor.git tools/wire-extractor

# needs on PATH:
#   elm-review (>= 2.10)
#   elm-test-rs
#   lamdera   (compiler for w3_encode_*/w3_decode_*)
```

## Usage

From a Lamdera app root (directory with `elm.json` and `Types.ToBackend` / `Types.ToFrontend`):

```bash
# Protocol only
node path/to/wire-extractor/bin/wire-extractor.js extract -o Protocol.elm

# Protocol + generated proof module
node path/to/wire-extractor/bin/wire-extractor.js extract \
  -o tests/Protocol.elm \
  --proof tests/ProtocolWireProof.elm

# Full prove: extract, write tests/, run property + exhaustive Wire3 proofs
node path/to/wire-extractor/bin/wire-extractor.js prove --project .
```

`prove` writes `tests/Protocol.elm` and `tests/ProtocolWireProof.elm`, ensures `tests/` is a source directory and `elm-explorations/test` is a test dependency, then runs:

```bash
elm-test-rs --compiler lamdera tests/ProtocolWireProof.elm
```

## What the proof checks

For each `ToBackend` / `ToFrontend` constructor (minimal payload) **and** property-fuzzed constructors with kernel-only args:

1. `Protocol.w3_encode_*` → bytes
2. `Types.w3_decode_*` succeeds
3. `Types.w3_encode_*` → **same** Wire3 byte list
4. `Protocol.w3_decode_*` succeeds and re-encodes to the same bytes

Codecs are the real Lamdera-generated `w3_*` functions, not hand-rolled serializers.

## What is included / excluded

| Included | Excluded |
|----------|----------|
| `ToBackend`, `ToFrontend` | `FrontendMsg`, `BackendMsg` |
| Nested wire payload types | `FrontendModel`, `BackendModel` |
| Inlined Auth/domain types on the wire path | Unrelated modules |
| Kernel imports (`Url`, `Dict`, …) | Opaque types redefined incorrectly |

Opaque package types (e.g. `SeqDict`) are **imported**, not reinvented. Package types that would collide on constructor names when flattened are externalized the same way.

If two **project** types on the wire share a constructor name (e.g. both define `Noop`), a single-module freeze is impossible without breaking Wire3 tags; extract fails with a clear error.

## Layout

```
bin/wire-extractor.js     # CLI (extract | prove)
review/                   # elm-review project (all analysis/codegen)
  src/
    ReviewConfig.elm
    ExtractWireProtocol.elm # rule + JSON extract
    ProtocolIR.elm          # IR, closure, Protocol + proof emit
```

## License

MIT
