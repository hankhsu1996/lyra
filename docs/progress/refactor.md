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

- [ ] R6 -- Consolidate the "synthesize an expression of canonical type X" helpers that lowerings
      reach for whenever they need a temporary, sentinel, or computed bound. The HIR -> MIR side
      already exposes `UnitLoweringState::MakeInt32LiteralExpr(int64)` and has ~8 consumers across
      the `expression/`, `statement/`, and `deferred_check_cascade` files. The AST -> HIR side has
      no equivalent public helper: `expression/lower.cpp` carries an anonymous-namespace
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

- [x] R7 -- The literal conversion keeps its faithful shape in MIR; constant folding is the
      downstream optimizer's job, not HIR-to-MIR's. A conversion of an integer literal to a
      same-width / same-signedness destination (e.g. a 2-state literal feeding a 4-state target) is
      the same operation as the conversion of a non-literal value to that destination, so MIR
      represents both the same way -- a `mir::ConversionExpr` over the operand -- rather than
      folding the literal case into a re-typed literal. Folding only the constant case would
      special-case it on whether the operand happens to be a literal, which is an optimizer's
      concern, not a MIR structural one; MIR is the program in primitives, and the LLVM backend (the
      load-bearing target) folds the constant conversion for free. What was wrong was the C++
      backend's render-time peephole that inspected a conversion's operand to decide whether to fold
      -- a renderer making a semantic decision; that peephole is removed, and `RenderConversionExpr`
      is now a pure `(src_kind, dst_kind)` map. The C++ medium may emit a runtime conversion for the
      constant case, which is acceptable -- it faithfully mirrors the MIR. See
      `decisions/conversion-folding.md`.

- [ ] R8 -- Unify the callable forms onto one concept. Today a process (`mir::Process`), a
      subroutine (`mir::StructuralSubroutineDecl`), and a closure (`mir::ClosureExpr`) are three
      separate types whose bodies are all the same `ProceduralScope`; the code already notes the
      duplication ("a subroutine is a callable peer of a process"). Each hardcodes a fixed point in
      three orthogonal axes: how the body binds outer state (the enclosing object only / named
      parameters / captured values), whether the body is a coroutine, and whether it is an anonymous
      value or a named declaration. The combinations in use today are partial -- process is
      (object-only, coroutine, value-spawned-at-startup), function is (params, plain, named,
      returns), task is (params, coroutine, named), closure is (captures or params, anonymous
      value). A fork-join branch is a closure with a coroutine result type, distinguished from a
      synchronous closure by that type, not by a flag on the node -- so a closure already spans both
      coroutine and plain without a suspend axis. Target shape: a single callable concept carrying a
      `ProceduralScope` body, a list of bound inputs that unifies parameters and captures behind one
      binding-mode axis, and a result type (the coroutine type when the body suspends); the five
      forms become instances differing only in their axis values and in how the referencing site
      invokes them (spawn at startup, call, submit, spawn concurrently). **Why deferred**: this
      rebases the whole process / subroutine / closure machinery across lowering, MIR, the dumper,
      and the backend; the fork work needed only a coroutine result type on the closure and reached
      it without touching processes or subroutines, so folding the full merge in would be scope
      explosion. **Trigger**: when a further feature needs yet another axis combination, or when a
      change has to be made three times across the duplicated forms.

- [x] R9 -- AST-to-HIR migration to the class-based organization defined in
      `docs/architecture/lowering_organization.md`. The `*LoweringState` god-objects are gone;
      per-task-instance `ModuleLowerer`, `StructuralScopeLowerer`, `ProcessLowerer`, and
      `CompilationLowerer` classes hold facts and registries. `ScopeStack` and `fork_branch_depth_`
      are absorbed by `WalkFrame`. All helpers across `expression/` and `statement/` migrated; no
      transitional shape remains.

- [x] R10 -- HIR-to-MIR migration to the class-based organization defined in
      `docs/architecture/lowering_organization.md`. Every handler is now
      `(Lowerer&, WalkFrame, node)`; the `WalkFrame` value type carries `current_compilation_unit` /
      `current_structural_scope` / `current_procedural_scope` / `static_frame_scope` /
      `procedural_depth` / `active_closure` / `active_index_binding` and every write goes through
      `frame.current_*_scope->Add...` uniformly. `ModuleLowerer`, `StructuralScopeLowerer`, and
      `ProcessLowerer` hold facts and registries only -- no borrowed pointer to in-flight IR, no
      delegate `Add` / `Allocate` wrapper, no ambient `Set*` / `Enter*` / `Leave*`.
      `ProceduralDepthGuard` is gone. The dead `facts.hpp` is deleted. `state.hpp` has been split
      into `module_lowerer.hpp` / `structural_scope_lowerer.hpp` / `process_lowerer.hpp`. AST-to-HIR
      `StructuralScopeLowerer.scope_`, `ModuleLowerer.hir_unit_`, and `ProcessLowerer.body_` are
      likewise off the lowerer. `~1400` local-variable sites renamed (`unit_state` -> `module`,
      `scope_state` -> `scope`, `proc_state` -> `process`, `proc_scope_state` -> `proc_scope`,
      etc.). The per-LRM-family subsystem split ships as its own focused cut; see R13.

- [x] R11 -- Remove the `mutable owned_temp_counter_` escape hatch on `RenderContext`. Today the C++
      backend's temp-name counter is a `mutable` field reached through a pointer from every
      `With*()` descendant; the escape hatch exists because every `Render*` helper takes the context
      by `const&`, but the counter genuinely mutates. Target shape: pull the counter out of
      `RenderContext` entirely. Each callable-body render entry (`RenderProcessMethod`,
      `RenderSubroutineMethod`, `RenderConstructor`) declares a local
      `std::size_t temp_counter = 0;` and threads `std::size_t& temp_counter` down the
      statement-handler chain. `RenderClosureExpr` opens its own fresh counter for the closure body.
      The `mutable` keyword disappears from the backend; const-correctness on `RenderContext`
      matches reality (it carries facts and walk position, both immutable per descent). **Why this
      minimal scope**: the rest of the lowering-organization contract (class-based pass shape with
      `WalkFrame`) does not transfer cleanly to the render layer -- rendering is mechanical
      translation, not semantic lowering, so the per-task state model is fundamentally different
      (see R18). The mutable escape hatch is the one shape that is unambiguously wrong from any
      vantage. **Trigger**: standalone -- can be picked up at any time.

- [x] R12 -- A signal read / write is an explicit access call in MIR, not a render-time decision.
      The observable-storage wrapper is a first-class MIR type (sibling to the owning-pointer and
      vector wrappers): a signal field's type is the wrapper and `RenderTypeAsCpp` maps it straight
      to `lyra::runtime::Var<T>`. The access half is now explicit too -- HIR-to-MIR emits an
      observable `Get` call for a value read and a `Set` / `Mutate` call for a write, so the backend
      renders each like any other call and never asks "is this observable" at access time. The
      value/storage duality -- a signal is read as a value but its cell is the target of a write or
      a reference binding -- is carried by which call wraps the cell, not by a render-time branch.
      The backend's `IsObservableScalarType` predicate is gone. See
      `docs/decisions/value-type-concepts.md`. R2 still reworks the same observable surface from the
      capability-gating angle (wrap every value type uniformly, push unsupported change-tracking to
      explicit diagnostics).

- [x] R13 -- HIR-to-MIR per-LRM-family subsystem split. Per-kind handlers are now grouped by
      semantic family in
      `include/lyra/lowering/hir_to_mir/expression/{operators, calls, references,     selects, aggregates, assignment, inside}.{hpp,cpp}`,
      `expression/system/{print, scan, sformat,     file_io, diagnostic, timescale, control}.{hpp,cpp}`,
      and `statement/{blocks, branches, loops,     timing, fork_join, assignment, flow}.{hpp,cpp}`.
      The procedural and scope-level expression dispatchers are class methods
      (`ProcessLowerer::LowerExpr` / `LowerStmt`, `StructuralScopeLowerer::LowerExpr`); the
      for-generate-header vs generate-control distinction lives on `WalkFrame::loop_var_mode`.
      Subsystem files include only the pass-class headers and their own family header, so adding a
      kind touches three files (subsystem header, subsystem implementation, dispatcher switch) and
      leaves the pass-class header untouched.

- [x] R14 -- The `return` / `co_return` choice is carried by MIR, not re-decided in the backend.
      `return` and `co_return` describe the same operation (exit the callable); whether a given
      `mir::ReturnStmt` renders as one or the other depends only on whether its enclosing callable
      is a coroutine. `mir::ReturnStmt` carries that as `is_coroutine_return`, set at HIR-to-MIR
      from the enclosing callable body's coroutine kind; the cpp backend reads it at each return
      rather than inheriting a render-time walk-state flag. A closure needs no coroutine field of
      its own -- whether a closure body suspends follows from how the closure is used (a fork-branch
      reference spawns it as a coroutine; a deferred submit runs it synchronously), which is itself
      explicit in MIR per `architecture/mir.md`.

- [x] R15 -- Give `mir::Process` a `name` field, so the C++ method name, static-frame struct name,
      static-frame field name, and any future LLVM-IR function symbol all flow from a single
      MIR-level identifier instead of each backend re-deriving "process_N" from a position-in-scope
      iteration index. `mir::StructuralSubroutineDecl::name` already plays this role for
      subroutines; processes are anonymous in SV (LRM 9.2) so HIR-to-MIR synthesises a positional
      identifier (`"process_0"` etc.) and threads it into the lowering so the returned `Process` is
      constant -- no post-hoc mutation. The `WithStaticFrame(...)` install in the cpp backend now
      reads `process.name + "__static"` rather than computing from an iteration index; the
      walker-state propagation of the frame field name continues until R18 dissolves the walk frame.
      **Trigger**: scheduled in front of R18.

- [x] R16 -- Give every MIR callable body an explicit `self` first binding -- `body.vars[0]` is a
      procedural-var of borrowed-pointer-to-enclosing-scope type, named `self`. Route every
      class-member access through a new `mir::MemberAccessExpr { receiver, var }` whose receiver
      reaches `self` via `DerefExpr(ProceduralVarRef(self))`. Today four distinct receiver
      mechanisms coexist -- method `this` (process / subroutine / constructor bodies), fork-branch
      `(M* self)` parameter, NBA / `$strobe` / scan closures' `[this]` or `[=, this]` capture, and
      `mir::StructuralVarRef`'s implicit-receiver render -- and the cpp backend dispatches between
      them through `RenderContext` walker state (`ReceiverObject()` / `WithReceiver(...)` /
      `DeferredByValueCapture()` / `MemberPrefix()`). The same dispatch would have to be re-derived
      by every future backend (LIR / LLVM-IR). Target shape: `body.vars[0]` is uniformly `self`
      across every callable form, but how it is supplied follows each form's natural binding
      mechanism -- a process / subroutine / constructor body receives `self` as its first formal
      parameter (the caller supplies), while a closure carries `self` as its first by-value capture
      (the enclosing scope snapshots its own self at construction). `mir::SelfScopeExpr` is removed
      (its job was to denote "the current receiver, whatever that is" -- precisely the
      implicit-context shape this refactor eliminates). C++ emit per form: process / subroutine /
      constructor bodies as `static auto <name>(M* self, ...) -> ... { ... }`, with the C++
      constructor delegating its body to a `static init(this)` call; closures as
      `[self = <enclosing self>, cap1 = ..., &cap2 = ...](closure_params) -> R { ... }` -- every
      capture is name-explicit, the clause never contains `[this]`, `[=]`, or `[&]`.
      `CreateProcesses()` remains a virtual instance method (C++ requires it) and emits
      `AddProcess(kind, process_N(this))`. The receiver-related `RenderContext` machinery disappears
      in lockstep. See `docs/decisions/callable-receiver.md`. **Trigger**: scheduled in front of
      R18.

- [x] R17 -- Selector and packed-struct field access lower to explicit
      `Call(ArrayMethod{kElementAt|kSlice})` / `Call(AssociativeMethod{kRead|kElementRef})`, and
      HIR-to-MIR materialises a borrowed `PackedArrayRef` chain with an explicit
      `Call(ArrayMethod{kToOwned})` wrap. `mir::ElementSelectExpr` and `mir::RangeSelectExpr` are
      retired; the render decides the emit shape from the callee alone. The runtime mirrors Rust's
      `ToOwned` trait -- ref types expose `ToOwned()` for materialisation, owning types expose the
      same name as an explicit copy.

- [x] R17a -- Signed-slice re-interpretation is explicit in MIR. HIR-to-MIR types a packed struct /
      union field slice with the runtime-honest unsigned signedness and wraps signed-field accesses
      with an explicit `ConversionExpr` re-tag to the declared field type. Render is a pure
      mechanical translation of `Call(kSlice)` / `Call(kToOwned)` / `ConversionExpr` independently.

- [ ] R17b -- **SUBSUMED by R26.** The narrow "make `Slice`'s `count` argument a `PackedArray`"
      target only fixes half of the slice asymmetry -- the underlying problem is that runtime
      containers carry no formal protocol forcing `Slice` (and every other shared family) to one
      signature shape. Original text retained for history: Stop window-projecting the `kSlice`
      `count` argument from an `IntegerLiteral` in the C++ backend. Today `RenderArrayMethodCall`
      peeks at the third argument of a `Call(ArrayMethod{kSlice})` and -- only when that argument is
      a literal -- emits a raw `N U` integer, because the runtime `Slice` signature takes
      `std::uint32_t`. The peek is a backend re-derivation that no other backend will replicate
      uniformly, violating `mir.md` Core Invariant 10. Target shape: the runtime `Slice` accepts a
      `PackedArray` for `count` and projects to `std::uint32_t` internally (matching how the
      `offset` argument already flows); render renders the `Call` mechanically with no peek.

- [x] R17c -- Render-side method-receiver dispatch is gone. `RenderMethodReceiver` retired and every
      method-call render path (`String` / `Array` / `Queue` / `Associative` / `Observable`) renders
      its receiver through plain `RenderExpr`. The read-vs-write decision lives entirely on the MIR
      receiver chain (LHS-side callees, `Deref(Mutate)` wraps) produced by HIR-to-MIR.

- [ ] R17d -- Lift the remaining backend-side post-process wraps in the method-call render paths
      into explicit MIR. Today `RenderArrayMethodCall` / `RenderQueueMethodCall` /
      `RenderAssociativeMethodCall` synthesise `lyra::value::PackedArray::Int(...)` around the
      result of size-style methods (`kSize` on Array / Queue; `kNum` / `kSize` / `kExists` on
      Associative) because the runtime returns `std::size_t` / `bool`, and `RenderStringMethodCall`
      projects integral string-method arguments through `static_cast<std::int32_t>((...).ToInt64())`
      because the runtime method takes `std::int32_t`. Both are backend re-derivations of facts MIR
      did not state, violating `mir.md` Core Invariant 10. Target shape: either the runtime APIs
      change to take and return `PackedArray` uniformly (the cleanest bridge), or HIR-to-MIR wraps
      the corresponding `Call(...)` with an explicit `ConversionExpr` so render is purely
      mechanical. After this, the five method-call render paths share an identical 5-step shape
      (receiver / `.` / name+`(` / args / `)`) and can collapse into one shared helper parameterised
      by the per-family member-name table. **Trigger**: batches with R17b as one render-cleanup cut.

- [ ] R18 -- Rewrite the MIR-to-C++ backend to free functions per node kind, with no
      `RenderContext`, no class hierarchy, and no `WalkFrame`. Once R11 has removed the mutable
      escape hatch, and R14 + R15 + R16 + R17 + R12 have closed every render-time decision that
      currently lives on the context, the render layer's per-task state collapses to a
      procedural-scope chain (for hops resolution within one callable body) and a temp counter --
      both local to one callable body, both plain locals in the callable's render function. Each
      callable kind (process, task, function, constructor, fork-branch, deferred closure) becomes
      one render function; per-node-kind handlers are free functions taking only what they read. The
      result is the mechanical-translation shape rendering should always have had: a rendering fold,
      not a construction pass. `lowering_organization.md` already reflects this distinction
      (invariant 9, the "Rendering Folds" section, and the narrowed `*Context` forbidden shapes);
      this cut brings the code to the fold shape the contract now describes. **Trigger**: R11, R12,
      R14, R15, R16, R17 have all landed; ready to pick up.

- [x] R19 -- LRM 10.5 variable initialization lowers to an `AssignExpr` statement at the top of
      `constructor_scope.root_stmts`, with the value being the user-supplied expression when present
      or the LRM Table 6-7 type default. `mir::StructuralVarDecl.initializer` is removed; MIR has
      exactly one mechanism for construction-time work (the statement list). `RenderField` emits a
      pure value-init declaration (`Var<T> name{};` or `T name{};`) with no inline initializer;
      `RenderContext::in_class_member_init_` and `WithClassMemberInit` are removed. The C++ runtime
      takes `RuntimeServices&` at `Scope` construction (Bind no longer wires services), and
      `PackedArray` / `UnpackedArray<T>` / `DynamicArray<T>` gain default constructors with a 0-bit
      sentinel shape that the first `AssignFrom` adopts -- so a constructor-body `Set` works without
      the deferred-bind dance. Vars whose MIR type is non-assignable (pointer / vector / object /
      external-unit-object / external-ref / event) are filtered out of the init-statement path --
      their declaration shape itself fixes the field at construction. See
      `docs/decisions/variable-initialization.md`.

- [ ] R20 -- **Runtime effects as generic calls** (design settled in
      `decisions/runtime-effects-as-generic-calls.md`). Move every runtime effect off the dedicated
      `RuntimeCallExpr` + payload-struct shape onto the one generic `CallExpr` (callee symbol plus
      an argument vector, with services threaded as a plain `self.Services()` argument), then retire
      `RuntimeCallExpr` and its payloads entirely. **Done**: `$finish`, the `$time` family, file
      I/O, the print-to-sink family, diagnostics (`$info` / `$warning` / `$error`), the
      string-format family (`$sformat` / `$swrite` / `$sformatf`), `$printtimescale`, and
      `$timeformat` (the no-argument reset form restores a design-global default the runtime
      resolves, so set and reset select different runtime entries). The closure-free effects are all
      on the generic shape now; the closure-bearing effects are carved out to R30 below, and
      retiring `RuntimeCallExpr` entirely depends on R30.

- [x] R21 -- **Closed after review: no rename, follow-on rejected.** The proposed rename of the
      HIR-to-MIR structural-var-read helper assumed its name tracked the MIR structural-var-ref expr
      arm that R16 dropped. It does not: like its procedural-var, cross-unit, and loop-var siblings,
      the helper is named for its HIR _input_ node, which still exists -- the cross-unit sibling
      likewise lowers to a member access and keeps its input-aligned name. Renaming this one helper
      for its MIR output would break that convention, so there is nothing to rename. The follow-on
      -- fold HIR's structural-var reference into a unified HIR member access -- is rejected on
      layering grounds: a bare SV name carries no receiver, so synthesizing the explicit `self`
      receiver is a HIR-to-MIR translation into MIR's generic vocabulary (settled in
      `decisions/callable-receiver.md`), not an HIR shape. HIR is SV-faithful (`hir.md` invariant 3)
      and must not carry that receiver, and HIR member access models LRM 7.2.1 packed-struct field
      selection -- a distinct construct from a named-variable reference, so the two cannot share one
      node. If any name is now misleading it is the MIR member-selector struct, which no longer
      names an expression; that lives in R22's vocabulary review, not here.

- [ ] R22 -- Reconsider the procedural-vs-structural variable naming and structure now that
      "structural variable" no longer appears as an `ExprData` arm. With class-member access
      generalised to `MemberAccessExpr(receiver, member)`, "structural var" is just "a member of the
      class" -- a fully general concept -- while "procedural var" is the body-local concept. The two
      were named as a symmetric pair when both were specialised expression forms; now the structural
      side has been abstracted into general member access, the asymmetry is awkward. Open questions:
      does `mir::StructuralVarDecl` need a different name (e.g. `ClassMemberDecl`)? Do shared
      structures that paralleled the two (e.g. `*VarRef`, `Lookup*Var`) still need to be parallel,
      or did some of them only exist to mirror an axis that no longer divides? **Trigger**: design
      discussion -- requires walking the structural / procedural axis across MIR vocabulary, dumper,
      lowering helpers, and backend, deciding what stays paired and what merges.

- [x] R23 -- "Materialise a reference into an owning value" is an explicit
      `Call(ArrayMethod{kToOwned})` node in MIR (Rust's `ToOwned` trait). HIR-to-MIR inserts the
      wrap at the read boundary; render is a mechanical translation. The `RenderExpr` /
      `RenderExprNatural` split and `ProducesPackedArrayRef` predicate are gone.

- [ ] R24 -- Dispatch the "sized collection" concept (`.size()` / `.num()`) through one resolver
      instead of branching on the concrete container at each call site. Today the element-count
      query is a separate enum value on every container's method family (`ArrayMethodKind::kSize`,
      `QueueMethodKind::kSize`, `AssociativeMethodKind::kSize` / `kNum`), and a caller that wants a
      count branches on the receiver type to pick the right one -- the `foreach` synthetic bound
      does exactly this (`isQueue() ? QueueMethodKind::kSize : ArrayMethodKind::kSize`). The concept
      is "any collection with an element count exposes `size()`", and the key modeling point (from
      how Rust generics, C++ templates + concepts, TypeScript interfaces, and Python protocols are
      implemented) is that under **static dispatch** -- which is our regime, the receiver's concrete
      type is always known at lowering -- the concept is a **compile-time resolver that is erased in
      the backend IR**, not a persistent IR node. So the per-container method enums stay (they are
      the concrete implementations, like Rust `impl` blocks / C++ members), and MIR keeps carrying
      the concrete per-type method; the concept lives only as a single lowering-time
      `ResolveSized(type) -> that type's size method` table -- the one place the type-to-method
      branch lives. The earlier mistake to avoid: extracting `size` into a flat
      `CollectionMethodKind` that removed it from each container's method set (that loses dimension
      one -- an array genuinely has a `size` method -- and persists a concept node in MIR, which the
      static-dispatch model says should be erased). **Why deferred**: only one consumer (`foreach`)
      today, so the resolver is a one-caller function -- the interface payoff is multiple generic
      callers sharing the table, which do not exist yet. **Trigger**: a second generic consumer of
      collection-size dispatch (a locator / reduction lowering, an array-querying system function,
      or a generic-algorithm pass). A separate, structural sub-question rides along: whether fixed
      unpacked and packed arrays should also be "sized collections" with a runtime `.size()` (a
      type-model decision, distinct from how `foreach` iterates them -- `foreach` over a fixed array
      uses its declared range and direction, not a count, so that split is a real semantic
      difference, not the same redundancy).

- [ ] R25 -- Collapse the per-data-type builtin-method render handlers onto the single generic
      `(receiver).name(args)` rule (R20's rule, now also carrying user-subroutine calls and the
      event family). **Value-model decision settled** (it was the blocker): an SV-facing runtime
      method's return and parameter types _are_ the SV value types; the representation bridge lives
      inside the method body, compiled once, an internal detail of the value layer. It is neither a
      MIR `ConversionExpr` (the call's SV type does not change, so there is nothing to convert) nor
      an emit-side wrap (forbidden by `decisions/integral-representation.md`: no native-scalar
      bridges in emit). This follows from `mir.md` invariant 10 -- the backend reads the call's
      stated result type and emits it, never re-deciding a representation. Internal native-count
      callers use a separate `Raw*` accessor. **Landed on this thread**: the integral-query surface
      (container `size`, associative `num` / `size` / `exists`, string `len` / `getc` / `compare` /
      `atoi` family, enum `num`) and the integral-argument surface (string index / byte args, enum
      next / prev step) now return / take SV value types directly; the member-shaped families
      (string, array, queue, associative non-traversal, enum instance) render through one generic
      handler whose only per-kind inputs are the member name and a mutates flag; all
      `PackedArray::Int(...)` result wraps and `static_cast<...>(ToInt64())` argument casts are gone
      from emit; user calls carry `self` and event trigger / triggered carry the engine handle as
      real arguments. **Remaining**: three families do not fit `(receiver).name(args)` and still
      need their own decision -- enum static methods (`Class::first()`, no receiver), associative
      traversal (a free runtime call still splicing the engine handle, the one remaining services
      fabrication), and value `$isunknown` (type-static, no call in the all-2-state case).
      **Structural prerequisite**: R29 -- it collapses the per-family enums whose existence forces
      R25's handler to keep per-family member-name tables; the per-family `Render*MethodCall`
      wrappers then collapse mechanically. R29 itself rests on R26 / R28. **Trigger**: continuation
      of `decisions/runtime-effects-as-generic-calls.md`.

- [ ] R26 -- Define runtime container protocols as explicit C++20 concepts, pin the
      currently-conforming surface, and complete `Sliceable` alignment. Today five containers
      (`PackedArray`, `DynamicArray`, `UnpackedArray`, `Queue`, `AssociativeArray`, plus `String` as
      a sixth member-shaped surface) share method families with no compile-time contract forcing the
      signatures into one shape. Most families already align de-facto (`ResetToDefault`, `ToOwned`,
      the equality / case-equal / bit-identical surface, the with-clause reduction and search
      families); two have drifted -- `Sliceable` is fixed in this entry, `Writable` defers to R28 (a
      pure rename pass, independent). The full protocol inventory derived from the audit:

      - `Sized` -- `Size() -> PackedArray`: Dynamic / Unpacked / Queue / AA. PackedArray opts out
        (its "size" is ambiguous between bit count, element count, and outer-dim count; SV does
        not expose `.size()` on packed types).
      - `Lengthable` -- `Len() -> PackedArray`: String (LRM 6.16.1 mandates the name).
      - `Indexable` -- `ElementAt(pos) -> ConstView`: Packed / Dynamic / Unpacked / Queue; String
        carries the LRM-named variant `Getc`.
      - `Writable` -- `WriteRef(pos) -> MutView`: Queue conforms today, AA's `ElementRef(K)` is
        the keyed sibling; the three array containers conform after R28's rename.
      - `AssocRead` -- `Read(K) -> ConstView`: AA only (the key is not a position; separate
        protocol from `Indexable`).
      - `Sliceable` -- `Slice(PackedArray, PackedArray) -> Window`: after this entry's
        alignment, Packed / Dynamic / Unpacked / Queue all conform structurally. The two
        arguments mean different things per LRM contract: Queue takes inclusive bounds
        `(lo, hi)` (LRM 7.10.1); Packed / Dynamic / Unpacked take `(offset, count)` because
        LRM 7.4.5 / 11.5.2 require canonical-fill at the type-fixed width even when bounds
        carry X/Z, and that width is not derivable from `(lo, hi)` alone. String's
        `Substr(i, j)` uses the queue's `(lo, hi)` shape but does not claim the protocol
        (the method name differs per LRM 6.16.8).
      - `Ownable` -- `ToOwned() -> Self`: Packed / Dynamic / Unpacked / Queue.
      - `Defaultable` -- `ResetToDefault()`: all five containers plus selected ref views.
      - `Equatable` -- `==`, `!=`, `CaseEqual`, `IsBitIdentical`: universal.
      - `Sortable` -- `Sort(F)`, `Rsort(F)`, `Reverse()`: Dynamic / Unpacked / Queue.
      - `Reducible` -- `Sum/Product/And/Or/Xor(F)`: Dynamic / Unpacked / Queue.
      - `Searchable` -- `Find/FindIndex/FindFirst/FindLast/Min/Max/Unique(F)`: Dynamic /
        Unpacked / Queue.
      - `KeyTraversal` -- `FirstKey/LastKey/NextKey/PrevKey -> optional<K>`: AA only.

      Target shape:

      - A new runtime header defines all 13 concepts in their final-aligned form
      - Every container header carries a matching `static_assert(<Concept><...>)` for each
        protocol it satisfies (12 of 13 pinnable after this entry; `Writable` waits for R28)
      - `Sliceable` alignment: `PackedArray`, `DynamicArray`, and `UnpackedArray` change their
        `Slice` signature's second argument from `std::uint32_t count` to
        `const PackedArray& count`. The runtime projects to `std::uint32_t` internally at the
        API boundary. Queue keeps its `(lo, hi)` form; both fixed-width and queue containers
        now share the structural `Slice(PackedArray, PackedArray)` shape.
      - HIR-to-MIR's three parallel range-unfold helpers collapse onto a single
        `UnfoldRangeBoundsToLoHi` that dispatches by container kind. The MIR `Call(kSlice,
        ...)` argument list carries `[base, offset, count_pa]` for fixed-width containers
        (count is a PackedArray-typed literal for static-width slices) and `[base, lo, hi]`
        for Queue.
      - The backend's `RenderArrayMethodCall` Slice argument peek (the only emit-side
        type-dependent argument projection) disappears; arguments render uniformly through
        `RenderExpr`.

      Subsumes R17b. After R26 + R28 lands, every signature drift becomes a compile-time
      failure rather than a silent regression. **Trigger**: standalone; before R28 / R29.

- [ ] R28 -- Align the `Writable` protocol's naming across the three array containers.
      `Queue::WriteRef(idx)` and `AssociativeArray::ElementRef(K)` carry the "write-side has extra
      semantics" intent (queue auto-pushes on out-of-bound write, AA creates the key);
      `PackedArray`, `DynamicArray`, and `UnpackedArray` overload the non-const `ElementAt(idx)`
      form for the same role. Target shape: rename the non-const `ElementAt` overload on the three
      array containers to `WriteRef`, aligning the protocol on `ElementAt(pos) -> ConstView` for
      read and `WriteRef(pos) -> MutView` for write. The `kElementAt` / `kWriteRef` enum asymmetry
      in MIR's `BuiltinMethodCallee` dissolves -- HIR-to-MIR's LHS-side dispatch produces
      `kWriteRef` for every array container, not just Queue. `static_assert(Writable<...>)` lands on
      all four array containers. **Trigger**: after R26 lands.

- [ ] R29 -- Collapse per-family MIR `BuiltinMethodCallee` enum redundancy. After R26 / R28 the
      containers share fully-aligned signatures, and the per-family enums (`ArrayMethodKind`,
      `QueueMethodKind`, `AssociativeMethodKind`) then double-encode "what is the receiver's
      container type" -- a fact MIR already carries on the receiver expression's type. For shared
      kinds (`kSlice`, `kElementAt`, `kSize`, `kReverse`, `kSort`, the with-clause reduction and
      search families) the per-family split is pure redundancy; for non-shared kinds (associative
      traversal, queue-only push / pop / insert, AA-only `Read` / `ElementRef` / `Exists`,
      string-only `Substr` / `Toupper` / `Itoa` etc.) the family identity is real. Target shape:
      audit each per-family kind, collapse the shared ones onto a single `BuiltinMemberKind` (or
      carry the C++ member name directly as a `string_view` -- the audit picks between these), and
      keep the non-shared kinds where family identity is real. The backend's five
      `Render*MethodCall` wrappers then collapse onto one generic `(receiver).name(args)` handler.
      Structural prerequisite for R25's clean landing. **Trigger**: after R26 / R28.

- [ ] R30 -- **Runtime effects as generic calls: the closure-bearing subset** (carve-out of R20,
      same decision). Each of these lowers to a generic `CallExpr` over a compiler-synthesized
      closure: the `$strobe` family (deferred print -- the last effect whose print items the backend
      still constructs inline), the scan family (`$sscanf` / `$fscanf`, whose write-through output
      slots are captured into the closure), and the synthesized non-blocking-assignment and
      deferred-assertion submits (no system-subroutine id -- these need a callee for
      compiler-synthesized effects). The MIR closure representation this once waited on has landed,
      and the closure builder (R31) is the construction vehicle. **Trigger**: continuation of
      `decisions/runtime-effects-as-generic-calls.md`.

- [x] R27 -- **Associative-array traversal output write-back.** `first` / `last` / `next` / `prev`
      (LRM 7.9.4 -- 7.9.7) lower to an immediately-invoked closure that reads the index into a plain
      local, runs a pure (engine-free) query that mutates it, and commits `index = temp` through an
      ordinary observable assignment -- so the LRM 4.3 update event fires in the assignment, not the
      query. The traversal query is now a plain container member rendered through the generic
      member-call rule; the backend no longer fabricates the engine handle or a reference wrapper
      for traversal. Closes R25's traversal carve-out (traversal now fits the generic member rule).

- [ ] R31 -- Converge every closure-construction site onto the one closure builder. Associative
      traversal (R27) and the scan family introduced and adopted a builder that owns the closure
      body scope, the `self` capture, and the capture sink: the caller fills the body through it and
      a terminal assembles the closure value, so the build-the-closure boilerplate lives in one
      place. The remaining sites still inline that scaffolding -- the fork-join branch (a coroutine
      closure with a by-value capture depth), the with-clause iterator (LRM 7.12.4, carrying an
      index binding), the non-blocking-assignment submit (manual value captures, no sink), and the
      deferred-assertion check. Converging them deletes the duplicated body-scope / self / sink /
      capture-assembly code and leaves one builder for every closure. Requires generalizing the
      builder with a coroutine result, a by-value capture depth, manual value captures, an index
      binding, and a bare closure-value terminal (for spawn / submit, versus the immediately-invoked
      call the synchronous sites use). **Trigger**: standalone -- the builder exists and the
      synchronous IIFE sites already use it; R30 builds its new closures on it too.

- [ ] R30 -- Unify the Expr- / value-construction helper naming, which is ad hoc today (`Make*` and
      `Build*` are both used for the same job). Establish one rule, grounded in the cross-language
      norm: every IR / value system separates a **pure node factory** -- assembles a node from
      ready-made parts, no scope or arena side effect, returns by value -- from a **scope-using
      builder** -- allocates sub-nodes into a scope and assembles, side-effecting. C++ `std::make_*`
      (pure) versus the builder pattern; LLVM `Type::get` / `ConstantInt::get` (pure, interned)
      versus `IRBuilder::Create*` (inserts into a block); rustc `T::new` versus `tcx.mk_*` (interns
      into the arena); MLIR / Clang `builder.create` / `Expr::Create(ctx, ...)` (arena). Keep both
      -- the pure-versus-stateful split is load-bearing, since it tells the reader whether the
      helper mutates a scope -- and pin the rule: `Make<Node>(parts...) -> Expr` is the pure factory
      (the caller does the `AddExpr`); `Build<Node>(scope-or-frame, inputs...) -> Expr | ExprId` is
      the scope builder (it does the `AddExpr`, with the context argument first); a type-producing
      variant is `Make<X>Type(...) -> TypeId`. Audit every construction helper and rename it to the
      correct side. **Trigger**: standalone naming cleanup.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`control-flow.md`,
  `operators.md`, etc.).
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
- The pre-reset surface re-implementation backlog. That lives in `architecture-reset.md`.
