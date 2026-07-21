# Generic trip-ups (single-module wire freeze + proof)

## Fixed in codegen
- **Type parameters** on custom types (`Nonempty a` applied as `Nonempty PlanChanged`) — substitute before inventing ctor args.
- **Type aliases** — expand body (with subst), do not invent fake DUs.
- **Opaque Lamdera IDs** (`Effect.Lamdera.SessionId`/`ClientId`) — use `sessionIdFromString` / `clientIdFromString`; never assume `T(..)` is exported.
- **Kernel empties** — `[]`, `Nothing`, `Dict`/`Set`/`Array`/`SeqDict`.empty, `Time.millisToPosix`, fixed `Url`, `Http.BadUrl`.
- **Effect.Time.Posix** — same sample as `Time.Posix`.
- **External imports** — `exposing (TypeName)` only (opaque-safe).

## Clear failures (no silent broken proofs)
- **Unconstructable sample** — `Debug.todo "wire-extractor cannot invent sample: …"` counted into extract `errors`; prove fails.
- **Project ctor name collisions** — two project types share a constructor name (e.g. both `Noop`); cannot flatten without breaking Wire3 tags.
- **Package ctor collisions** — externalize package types so project types keep names.
- **Recursive DU with no nullary ctor** — cannot invent finite value.
- **Function types on the wire** — rejected.
- **Unparseable sources** — elm-review parse failure.
- **Missing/ambiguous roots** — no unique `ToBackend`/`ToFrontend` after Evergreen filter.

## Environment
- Prove needs `elm-review`, `elm-test-rs`, `lamdera`.
