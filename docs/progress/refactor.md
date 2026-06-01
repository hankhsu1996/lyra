# Refactor

Tracks architectural debt and cleanup work that has been deferred because the immediate task did not
need it -- but that we have agreed should land eventually. Distinct from the per-feature progress
files (which track in-flight language-feature work); this file is the queue of "architecturally we
know this is wrong, here is the target shape, here is what triggers picking it up".

Each entry states:

- The current shape (what is awkward today)
- The target shape (what it should look like)
- The trigger (when to pick it up)

Entries get checked off as their PRs land. When the last entry lands, the file is deleted.

## Sub-Steps

- [ ] R1 -- Promote `Builtins` (canonical TypeIds for `int`, `bit[0]`, `string`, `void`, `realtime`)
      out of `UnitLoweringState` and onto `mir::CompilationUnit`. Today the table lives on the HIR
      -> MIR lowering state, which makes the canonical TypeIds invisible to any other MIR consumer.
      The literal-synthesis helpers that sit on `UnitLoweringState` (currently just
      `MakeInt32LiteralExpr`, sibling forms expected) follow it: they move into `lyra::mir` as free
      functions taking a `mir::CompilationUnit&`. **Why deferred**: no non-HIR-to-MIR consumer
      exists today; the lowering-state-scoped factory is sufficient until a second consumer arrives.
      **Trigger**: when a MIR-to-LLVM lowering (or a MIR-level optimization pass) first needs the
      canonical builtin TypeIds; at that point the per-consumer copy temptation is the smell that
      forces this move.

- [ ] R2 -- Realign the `Var<T>` wrapping gate so the cpp backend stops mixing structural type
      classification with runtime-capability gating. Today `IsObservableScalarType` returns true
      only for `IsIntegralPacked()` and the backend uses it to silently skip the wrapper for every
      other value type (`string`, `real`, unpacked array). The two questions it tangles together --
      "should this field semantically be a value slot at all (vs an object / sub-hierarchy)?" and
      "does the runtime currently support change-tracking for this T?" -- have different homes: the
      first is an intrinsic MIR type classification (value family vs object family); the second is a
      runtime capability gap that should manifest as an explicit `kUnsupported...` diagnostic at the
      AST -> HIR / HIR -> MIR sensitivity-lowering sites when a `@(...)`, `wait`, or `always_comb`
      sensitivity references a type whose `Var<T>` specialization does not yet exist. Target shape:
      backend wraps every value-typed field uniformly in `Var<T>`, leaving object- family fields
      plain; runtime grows minimal `Var<String>` / `Var<double>` / `Var<vector<T>>` specializations
      (storage + assign at first, waiter paths throw `kUnsupportedFeature` until filled in); AST ->
      HIR rejects the unsupported sensitivity uses upstream with a source span. The backend then
      becomes a pure renderer, never asking a capability question. **Why deferred**: backend
      currently sidesteps the issue silently and the sensitivity rejection at AST -> HIR for
      non-integral leaves already keeps things consistent in practice; the proper fix touches
      runtime, sensitivity lowering, and backend rendering together. **Trigger**: when the first
      non-integral runtime change-tracking surface lands (named-event subscription, string
      `wait (s == "x")`, real value-change detection) -- at that moment the `IsObservableScalarType`
      shortcut becomes actively wrong and the layering fix has to happen alongside.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`control-flow.md`,
  `operators.md`, etc.).
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
- The pre-reset surface re-implementation backlog. That lives in `architecture-reset.md`.
