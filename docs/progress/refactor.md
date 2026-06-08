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

- [x] R3 -- Collapse the runtime's dual hierarchy into a single object tree. The mirrored
      `RuntimeScope` tree and the bind-time `RuntimeBindContext` are gone; there is now one runtime
      `scope` base (named after the LRM VPI object model, with `instance` and `gen scope`
      specializations) carrying name, parent, services, the process list, and the observed-region
      machinery. Every emitted scope class extends it and stays thin; the scheduler walks the real
      object tree through the base. The kind is carried by the specialization type, not a side enum.
      Identity is fixed at construction; bind only wires services, creates processes, and recurses.
      `services_` lives once, generate-scope classes are no longer bare, and the observed region
      drains for every scope -- so a deferred check (and combinational settle) inside a nested
      instance or a generate block now works. **Follow-up**: the `instance` / `gen scope` layer is
      empty today and exists to mark the divergence; it is populated when Stage E (ports) lands --
      ports live on `instance`, never on `gen scope`.

- [ ] R4 -- Store an instance array as a fixed-size array, not a growable vector with a side dim
      list. Today `Child c[2][3]` lowers to a nested `VectorType` (`std::vector`) and the
      per-dimension element counts ride as a separate `dims` list on the construct statement; the
      backend peels that list into nested container loops at render time. The extent is a
      compile-time constant, so the storage should be a fixed-size array (`std::array`) carrying its
      extent on each type layer -- a new array type symmetric with `VectorType`, with the count
      living in the type (one container layer per dimension) rather than in a side list. Each
      pre-sized slot is then assigned directly, with no grow step. This is the same
      size-belongs-in-the-type, one-layer-per-dimension shape the unpacked-array decision fixes; the
      flat dim list is the rejected form. **Why deferred**: the vector form is behaviorally correct
      and the instance-array feature ships on it; the array-type refinement is contained but not
      needed for correctness. **Trigger**: when a second fixed-extent object-collection consumer
      appears.

- [ ] R5 -- Resurrect a smoke-suite tier so the emit-cpp path gets CI gating. Today
      `tests/suites.yaml` carries only the monolithic `architecture_reset` suite, and the
      `cpp_tests` target is tagged `requires-host-cxx` while CI's `Test` job filters that tag out --
      so every PR merges without any automated check that the emitted C++ still compiles and runs. A
      toolchain-compat regression (runtime header or emitted code reaching for a stdlib feature
      absent from the floor toolchain), or an emit-vs-runtime ABI break, is only caught when a
      contributor runs `cpp_tests` locally. Target shape: a `cpp_run_smoke` suite defined in
      `tests/suites.yaml`, populated by `cpp_run_smoke`-tagged cases -- one representative per
      distinct emit codegen family (integral / unpacked array / process / `$display` / loop / etc.),
      5-10 cases total, target wall-clock under 30 seconds. A dedicated CI lane runs that suite via
      the existing `cpp_tests` target with whatever toolchain `ubuntu-latest` ships -- no explicit
      LLVM install. The lane is the anchor for a (yet-to-be-written)
      `docs/decisions/emit-cpp-baseline.md` that pins the supported toolchain floor. The shape
      mirrors the pre-reset `aot_smoke` suite (see `archived/tests/suites.yaml`); the only
      adaptation is regex-on-path becoming tag-on-case, since cpp is currently the only backend and
      tags travel with cases more naturally than external catalogs do. **Why deferred**: this is its
      own focused PR (decision doc + suite entry + tag application + CI lane); folding it into a
      feature PR makes both diffs noisy. Full `cpp_tests` in CI remains separately gated on the LLVM
      JIT backend replacing per-case clang invocations (expected 5-10x faster), at which point a
      sibling `jit_smoke` / `jit_full` pair can be added on the same pattern. **Trigger**:
      standalone -- can be picked up at any time. The longer it waits, the larger the window for an
      unspotted toolchain-compat regression to land in emit or runtime headers.

- [ ] R6 -- Consolidate the "synthesize an expression of canonical type X" helpers that lowerings
      reach for whenever they need a temporary, sentinel, or computed bound. The HIR -> MIR side
      already exposes `UnitLoweringState::MakeInt32LiteralExpr(int64)` and has ~8 consumers across
      `lower_expr.cpp`, `lower_stmt.cpp`, and `lower_deferred_check.cpp`. The AST -> HIR side has no
      equivalent public helper: `expression/lower.cpp` carries an anonymous-namespace
      `MakeIntegerLiteralExpr` that takes a slang `IntegerLiteral&` (no raw-int64 entry point) and
      an anonymous `MakeRefExpr` for procedural / structural / loop var refs. New consumers that
      can't see those private helpers (most recently `statement/foreach.cpp`) reroll their own
      copies inline -- `MakeInt32Constant` + `MakeInt32LiteralExpr` + `MakeProcVarRefExpr` are all
      sitting in foreach's anonymous namespace today, and similar copies will accumulate at every
      future desugar site that needs a synthetic counter, sentinel, or width-constant. Target shape:
      a small set of public helpers on each layer's `UnitLoweringState` mirroring the MIR-side
      `MakeInt32LiteralExpr` -- minimally a raw-int64 int32 literal expr builder, a bool literal
      expr builder, and ref-expr builders for the three var families -- with the `IntegralConstant`
      construction (the masked-word layout) factored into a single function rather than copy-pasted
      at every site. **Why deferred**: foreach's local helpers are correct, scoped to one TU, and
      shipping them inline kept the immediate PR focused. The duplication only becomes painful when
      the next consumer arrives. **Trigger**: when a second AST -> HIR lowering needs a raw-int64
      int32 literal expr (or a synthetic ProceduralVarRef expr), at which point the per-consumer
      copy is the smell that forces extraction. Related to R1, which is the further move of
      promoting the canonical `Builtins` table and these helpers from the lowering-state scope to
      the IR (`mir::CompilationUnit` and the AST -> HIR analogue) -- R6 is the prerequisite cleanup;
      R1 is the architectural promotion.

- [ ] R7 -- Move the literal-fold peephole from the C++ backend into HIR-to-MIR. Today
      `RenderConversionExpr` in `src/lyra/backend/cpp/render_expr.cpp` carries a
      `TryFoldLiteralIntegerConversion` helper that detects "MIR `ConversionExpr` wrapping a
      `mir::IntegerLiteral` whose source and destination shapes are equivalent (same width, same
      signedness, no X/Z loss)" and renders the literal directly in the destination shape, skipping
      the runtime `PackedArray::ConvertFrom` call. That fold is a pure semantic decision driven
      entirely by MIR data, so per `lowering_boundaries.md` Core Invariant 5 ("downstream layers do
      not re-derive upstream decisions") and `mir.md` Core Invariant 5 ("dumper is the golden-test
      surface, lossless wrt MIR structure"), it belongs at the HIR -> MIR boundary, not in the
      backend. Target shape: `LowerHirConversionExprProc` (and its structural twin) recognises a HIR
      `ConversionExpr` whose operand is a `hir::IntegerLiteral` with a foldable shape pair and emits
      a `mir::IntegerLiteral` directly in the destination shape -- no `mir::ConversionExpr` node is
      ever materialised for the no-op case. The backend's render path then becomes pure
      `(src_kind, dst_kind)` dispatch with no peephole; the MIR dumper shows exactly what the
      backend will emit (no "node exists but renders to nothing" surprises). **Why deferred**: the
      current backend-side fold is correct and the scan-family PR that exposed it
      (`feature/     scan-family-corners`) should not also rewrite the conversion-lowering path --
      the MIR dump goldens that exercise this fold need separate scrutiny. **Trigger**: standalone
      -- can be picked up at any time. Highest leverage is just before any other change to
      `LowerHirConversionExprProc` (or any new constant-folding pass on MIR), because both want a
      MIR free of redundant `ConversionExpr` nodes.

- [ ] R8 -- Unify the callable forms onto one concept. Today a process (`mir::Process`), a
      subroutine (`mir::StructuralSubroutineDecl`), and a closure (`mir::ClosureExpr`) are three
      separate types whose bodies are all the same `ProceduralScope`; the code already notes the
      duplication ("a subroutine is a callable peer of a process"). Each hardcodes a fixed point in
      three orthogonal axes: how the body binds outer state (the enclosing object only / named
      parameters / captured values), whether the body suspends (coroutine vs plain), and whether it
      is an anonymous value or a named declaration. The combinations in use today are partial --
      process is (object-only, suspends, value-spawned-at-startup), function is (params, no-suspend,
      named, returns), task is (params, suspends, named), closure is (captures, no-suspend,
      anonymous value). A fork-join branch is the missing fourth-axis combination -- an anonymous
      value, with captured state, that suspends -- which no form provides. This cut fills it the
      minimal way, by completing `ClosureExpr` with a suspend axis (so a closure value may be
      suspending or not), which unifies the NBA / `$strobe` closure and the fork branch under one
      type but leaves the three-way duplication between process, subroutine, and closure standing.
      Target shape: a single callable concept carrying a `ProceduralScope` body, a list of bound
      inputs that unifies parameters and captures behind one binding-mode axis, a suspend flag, and
      an optional result type; the five forms become instances differing only in their axis values
      and in how the referencing site invokes them (spawn at startup, call, submit, spawn
      concurrently). **Why deferred**: this rebases the whole process / subroutine / closure
      machinery across lowering, MIR, the dumper, and the backend; the fork cut needs only the
      suspend axis on `ClosureExpr` and reaches it without touching processes or subroutines, so
      folding the full merge into a feature cut is scope explosion. **Trigger**: when a further
      feature needs yet another axis combination, or when a change has to be made three times across
      the duplicated forms; the suspending closure this cut introduces is the first concrete
      cross-form driver, so the unification now has a real motivation rather than being speculative.

- [ ] R9 -- Split `ProcessLoweringState` along the fact-vs-traversal-state line so const-correctness
      can be restored to expression lowering. Today the class mixes two categorically different
      kinds of information. The first kind is "facts about this process" -- HIR-to-MIR var bindings,
      the static-frame scope, the time resolution, the accumulated static-locals list. Facts are
      filled at statement-declaration lowering and are read-only from that point on; expressions
      never legitimately change them. The second kind is "where is the lowering pass currently
      standing" -- the procedural depth counter, the active capture sink for closure-body lowering.
      That is traversal state: it is pushed on entry into a nested scope and popped on exit, and
      stays balanced across any one full lowering. Because both kinds shared one class, the
      `const ProcessLoweringState&` qualifier on expression-lowering signatures was honest for facts
      and accidentally worked for traversal state only because ordinary expressions do not open
      scopes. The qualifier broke the moment an expression-form construct that creates a procedural
      scope appeared (the `$sscanf` / `$fscanf` closure-IIFE), forcing a binary choice between
      smuggling the mutation through `mutable` back doors and dropping the qualifier from the entire
      expression lowering layer. We chose the latter as the temporary shape, which means "expression
      lowering does not change process facts" is now a docstring convention rather than a
      signature-level invariant. Target shape: separate the two kinds. Facts live on an immutable
      object handed in by reference -- it is the process's identity, set once by upstream lowering
      and read by everyone below. Traversal state lives on a separate, explicitly mutable object
      pushed/popped via RAII guards; closure-body construction installs its capture sink there. The
      LowerExpr signature then carries the right qualifier on each: const on facts, mutable on
      state. The invariant "expression lowering does not change process facts" is restored at the
      signature level, and the existing mutation pattern for traversal state stops needing to argue
      with `const`. **Why deferred**: the fact-vs-state split rebases every `Lower*` function
      signature across the lowering layer and every site that constructs or extends the lowering
      objects; folding it into the scan-family PR doubled the diff without architectural gain on
      that PR's terms. **Trigger**: when a second expression-form construct that opens a procedural
      scope appears (any future closure-IIFE expression -- the user-function-call-with-output-args
      case, for instance), or sooner if the loss of the const-on-facts invariant produces a real
      mistake.

- [ ] R10 -- Investigate the lowering layer's organization of fact vs state at the layer level (not
      just inside `ProcessLoweringState`, which R9 already tracks). The smell surfaced from
      scan-family work: helpers that compose multi-step MIR construction end up with long parameter
      lists that thread
      `(unit_state, structural_scope_state, process_state, procedural_scope_state)` through every
      call -- typically four references where only one is the operation's real input. The
      fact-vs-state split is not currently first-class in the layer's organization. Some objects are
      pure lookup -- HIR-to-MIR var bindings, type tables, builtin type IDs, the static-frame scope;
      those are facts and want `const` on every signature that uses them. Others accumulate across
      the lowering -- the procedural scope being built up via add-expr / append-stmt / append-local
      methods; those are state and are already builder-shaped at the method level, but the surface
      around them is unstructured (free functions threading the builder plus its facts through every
      parameter list). Two observations from the scan PR sharpen the question. First: the builder
      shape is already partially present (`ProceduralScopeLoweringState` exposes `AddExpr` /
      `AppendStmt` / `AppendIfThen` / `AppendLocal` / `Finish` -- a clean builder API). Second: the
      awkwardness in helper signatures is not the builder's fault; it is that every helper has to
      thread the surrounding facts back in separately to do anything builder-shaped on the state. A
      clearer layer-level organization might separate the two kinds explicitly -- facts handed in by
      const reference once and read freely, state handed in by mutable reference and operated on
      through builder methods -- so helpers either operate on a fact (no state argument, no mutation
      possible) or compose state operations through the builder (fewer threaded-through references).
      **Target shape unknown.** Possible directions include: a lowering-context struct that splits
      the fact bundle and the state bundle into two distinct types; encapsulating multi-step body
      construction into builder methods on a closure-body builder type whose constructor captures
      the facts once; or some pattern this codebase has not yet adopted. The right shape probably
      emerges from a second concrete driver, not from an abstract design pass. **Why deferred**:
      there is no clear target yet, and the current shape, while verbose, is auditable -- every
      helper signature spells out exactly which facts it reads and which state it mutates, which is
      the codebase's standing rule. Refactoring without a clear target risks importing a worse
      pattern. **Trigger**: investigate when (a) a second feature area accumulates a similar
      verbose-helper smell, or (b) R9 lands and exposes a natural layer-level split that this entry
      can ride on, or (c) a concrete confusion (someone touches the wrong piece because the facts
      and state were not clearly separated) makes the cost real.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`control-flow.md`,
  `operators.md`, etc.).
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
- The pre-reset surface re-implementation backlog. That lives in `architecture-reset.md`.
