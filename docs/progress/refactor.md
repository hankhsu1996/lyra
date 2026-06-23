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

- [x] R1 -- The canonical builtin TypeIds (`int`, `bit[0]`, `string`, `void`, `realtime`, and the
      runtime-library siblings) live on `mir::CompilationUnit`, populated by its constructor and
      read off the unit by every MIR consumer rather than scoped to the lowering pass. The
      literal-synthesis helpers are free functions in `lyra::mir` taking the canonical TypeId they
      synthesize against, so any MIR consumer can build a literal without reaching into a
      lowering-state factory. The former lowering-state table is gone with the lowering-state
      god-objects it used to sit on (R9 / R10).

- [x] R2 -- Non-integral value-change observability. The wrapping-gate realignment this entry
      originally described had already landed under R12: observable storage is a first-class
      `mir::ObservableType` (R12), `Var<T>` gates on the `LyraValue` concept rather than an
      integral-packed predicate (`decisions/value-type-concepts.md`), `IsObservableScalarType` is
      gone, and the runtime cell is uniformly generic over every value type (`PackedArray`,
      `String`, `Real`, `UnpackedArray<T>`, `DynamicArray<T>`, `Queue<T>`, `AssociativeArray<K,V>`).
      As a direct consequence of that generality, every implicit value-change construct --
      `always_comb` / `always_latch`, `@*`, `wait`, and continuous assignment -- already subscribes
      to and wakes on any value type with no per-type code: the change-detection hook is each type's
      `IsBitIdentical`, and a non-`PackedArray` cell fires its any-change waiters on every real
      change. The minimal `Var<String>` / `Var<double>` / `Var<vector<T>>` specializations this
      entry once anticipated were never needed (the single generic template covers all), and the
      "reject unsupported sensitivity uses at lowering" half proved unnecessary too -- the slang
      frontend already rejects every LRM-illegal value-change event source: an edge qualifier on a
      non-integral operand and an aggregate (non-singular) event expression both fail frontend
      binding (LRM 9.4.2, "event expressions shall return singular values"). The work was the
      inverse of the original framing -- two frontend lowering gates were over-broad and rejected
      forms the architecture and the LRM both allow. Both are now lifted: the `@(expr)`
      event-control path admits any value-change-observable operand as an any-change event (the
      legal `@(string)` / `@(real)` / `@(enum)`) while still requiring a packed bit-vector for an
      edge (the runtime classifies an edge only on a `PackedArray` cell), and the
      input-port-connection path admits any value type (the driver rides the generic
      continuous-assign path). `hir::Type::IsValueChangeObservable` is the single HIR-level
      predicate both gates share. A value-type x construct coverage matrix backs it (`@`, `wait`,
      `always_comb`, `@*`, continuous assignment, input port over string / real / enum / unpacked).

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

- [ ] R8 -- Unify the callable forms onto one concept. Today a process (`mir::Process`), a method
      (`mir::MethodDecl`), and a closure (`mir::ClosureExpr`) are three separate types whose bodies
      are all the same `Block`; the code already notes the duplication ("a method is a callable peer
      of a process"). Each hardcodes a fixed point in three orthogonal axes: how the body binds
      outer state (the enclosing object only / named parameters / captured values), whether the body
      is a coroutine, and whether it is an anonymous value or a named declaration. The combinations
      in use today are partial -- process is (object-only, coroutine, value-spawned-at-startup),
      function is (params, plain, named, returns), task is (params, coroutine, named), closure is
      (captures or params, anonymous value). A fork-join branch is a closure with a coroutine result
      type, distinguished from a synchronous closure by that type, not by a flag on the node -- so a
      closure already spans both coroutine and plain without a suspend axis. Target shape: a single
      callable concept carrying a `Block` body, a list of bound inputs that unifies parameters and
      captures behind one binding-mode axis, and a result type (the coroutine type when the body
      suspends); the five forms become instances differing only in their axis values and in how the
      referencing site invokes them (spawn at startup, call, submit, spawn concurrently). **Why
      deferred**: this rebases the whole process / method / closure machinery across lowering, MIR,
      the dumper, and the backend; the fork work needed only a coroutine result type on the closure
      and reached it without touching processes or methods, so folding the full merge in would be
      scope explosion. **Trigger**: when a further feature needs yet another axis combination, or
      when a change has to be made three times across the duplicated forms.

- [x] R9 -- AST-to-HIR migration to the class-based organization defined in
      `docs/architecture/lowering_organization.md`. The `*LoweringState` god-objects are gone;
      per-task-instance `ModuleLowerer`, `StructuralScopeLowerer`, `ProcessLowerer`, and
      `CompilationLowerer` classes hold facts and registries. `ScopeStack` and `fork_branch_depth_`
      are absorbed by `WalkFrame`. All helpers across `expression/` and `statement/` migrated; no
      transitional shape remains.

- [x] R10 -- HIR-to-MIR migration to the class-based organization defined in
      `docs/architecture/lowering_organization.md`. Every handler is now
      `(Lowerer&, WalkFrame, node)`; the `WalkFrame` value type carries `current_compilation_unit` /
      `current_class` / `current_block` / `static_frame_scope` / `block_depth` / `active_closure` /
      `active_index_binding` and every write goes through `frame.current_class->Add...` /
      `frame.current_block->Add...` uniformly. `ModuleLowerer`, `ClassLowerer`, and `ProcessLowerer`
      hold facts and registries only -- no borrowed pointer to in-flight IR, no delegate `Add` /
      `Allocate` wrapper, no ambient `Set*` / `Enter*` / `Leave*`. `ProceduralDepthGuard` is gone.
      The dead `facts.hpp` is deleted. `state.hpp` has been split into `module_lowerer.hpp` /
      `class_lowerer.hpp` / `process_lowerer.hpp`. AST-to-HIR `StructuralScopeLowerer.scope_`,
      `ModuleLowerer.hir_unit_`, and `ProcessLowerer.body_` are likewise off the lowerer. `~1400`
      local-variable sites renamed (`unit_state` -> `module`, `scope_state` -> `scope`, `proc_state`
      -> `process`, `proc_scope_state` -> `proc_scope`, etc.). The per-LRM-family subsystem split
      ships as its own focused cut; see R13.

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
      `docs/decisions/value-type-concepts.md`. The uniform wrapping this entry set up is what makes
      R2's residual small -- only two over-broad frontend event-source gates remain.

- [x] R13 -- HIR-to-MIR per-LRM-family subsystem split. Per-kind handlers are now grouped by
      semantic family in
      `include/lyra/lowering/hir_to_mir/expression/{operators, calls, references,     selects, aggregates, assignment, inside}.{hpp,cpp}`,
      `expression/system/{print, scan, sformat,     file_io, diagnostic, timescale, control}.{hpp,cpp}`,
      and `statement/{blocks, branches, loops,     timing, fork_join, assignment, flow}.{hpp,cpp}`.
      The procedural and class-level expression dispatchers are class methods
      (`ProcessLowerer::LowerExpr` / `LowerStmt`, `ClassLowerer::LowerExpr`); the
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
      iteration index. `mir::MethodDecl::name` already plays this role for methods; processes are
      anonymous in SV (LRM 9.2) so HIR-to-MIR synthesises a positional identifier (`"process_0"`
      etc.) and threads it into the lowering so the returned `Process` is constant -- no post-hoc
      mutation. The `WithStaticFrame(...)` install in the cpp backend now reads
      `process.name + "__static"` rather than computing from an iteration index; the walker-state
      propagation of the frame field name continues until R18 dissolves the walk frame. **Trigger**:
      scheduled in front of R18.

- [x] R16 -- Give every MIR callable body an explicit `self` first binding -- `body.vars[0]` is a
      local of borrowed-pointer-to-enclosing-class type, named `self`. Route every class-member
      access through a new `mir::MemberAccessExpr { receiver, var }` whose receiver reaches `self`
      via `DerefExpr(LocalRef(self))`. Today four distinct receiver mechanisms coexist -- method
      `this` (process / method / constructor bodies), fork-branch `(M* self)` parameter, NBA /
      `$strobe` / scan closures' `[this]` or `[=, this]` capture, and `mir::MemberRef`'s
      implicit-receiver render -- and the cpp backend dispatches between them through
      `RenderContext` walker state (`ReceiverObject()` / `WithReceiver(...)` /
      `DeferredByValueCapture()` / `MemberPrefix()`). The same dispatch would have to be re-derived
      by every future backend (LIR / LLVM-IR). Target shape: `body.vars[0]` is uniformly `self`
      across every callable form, but how it is supplied follows each form's natural binding
      mechanism -- a process / method / constructor body receives `self` as its first formal
      parameter (the caller supplies), while a closure carries `self` as its first by-value capture
      (the enclosing scope snapshots its own self at construction). `mir::SelfScopeExpr` is removed
      (its job was to denote "the current receiver, whatever that is" -- precisely the
      implicit-context shape this refactor eliminates). C++ emit per form: process / method /
      constructor bodies as `static auto <name>(M* self, ...) -> ... { ... }`, with the C++
      constructor delegating its body to a `static init(this)` call; closures as
      `[self = <enclosing self>, cap1 = ..., &cap2 = ...](closure_params) -> R { ... }` -- every
      capture is name-explicit, the clause never contains `[this]`, `[=]`, or `[&]`.
      `CreateProcesses()` remains a virtual instance method (C++ requires it) and emits
      `AddProcess(kind, process_N(this))`. The receiver-related `RenderContext` machinery disappears
      in lockstep. See `docs/decisions/callable-receiver.md`. **Trigger**: scheduled in front of
      R18.

- [x] R17 -- Selector and packed-struct field access lower to explicit built-in method calls for
      element access, slice, and the borrowed-to-owned materialisation. The dedicated element-select
      and range-select MIR nodes are retired; the render decides the emit shape from the callee
      alone. The runtime mirrors Rust's `ToOwned` trait -- ref types expose `ToOwned()` for
      materialisation, owning types expose the same name as an explicit copy.

- [x] R17a -- Signed-slice re-interpretation is explicit in MIR. HIR-to-MIR types a packed struct /
      union field slice with the runtime-honest unsigned signedness and wraps signed-field accesses
      with an explicit `ConversionExpr` re-tag to the declared field type. Render is a pure
      mechanical translation of `Call(kSlice)` / `Call(kToOwned)` / `ConversionExpr` independently.

- [x] R17b -- Slice's `count` argument flows as a `PackedArray` value end to end (subsumed by R26).
      The backend-side window-projection peek that emitted a raw native integer for a literal count
      is gone; render reads the call's argument list mechanically.

- [x] R17c -- Render-side method-receiver dispatch is gone; every method-call render path renders
      its receiver through the generic expression renderer. The read-vs-write decision lives
      entirely on the MIR receiver chain (LHS-side callees, mutate-deref wraps) produced by
      HIR-to-MIR.

- [x] R17d -- An SV-facing runtime method's signature carries the SV value types directly:
      size-style queries return a `PackedArray`, and integral arguments to string methods arrive as
      `PackedArray`. The backend post-process casts that wrapped a host integer back into an SV
      shape are gone; render reads call result and argument types straight from MIR.

- [x] R18 -- The MIR-to-C++ backend is a rendering fold: per-node-kind handlers are free functions
      that take only what they read, with no `RenderContext`, no render-pass class hierarchy, and no
      `WalkFrame`. The render layer's only per-descent state is an immutable walk position carrying
      the compilation unit and the enclosing-scope chain (for hops resolution within one callable
      body), copied on descent and growing no member per concept. Each callable kind (process, task,
      function, constructor, fork-branch, deferred closure) renders through one entry function. This
      is the mechanical-translation shape rendering should always have had -- a fold, not a
      construction pass -- matching the distinction `lowering_organization.md` draws (invariant 9
      and the "Rendering Folds" section).

- [x] R19 -- LRM 10.5 variable initialization lowers to an `AssignExpr` statement at the top of
      `constructor_block.root_stmts`, with the value being the user-supplied expression when present
      or the LRM Table 6-7 type default. `mir::MemberDecl.initializer` is removed; MIR has exactly
      one mechanism for construction-time work (the statement list). `RenderField` emits a pure
      value-init declaration (`Var<T> name{};` or `T name{};`) with no inline initializer;
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

- [x] R22 -- MIR's vocabulary is generic-software, not SystemVerilog: the structural / procedural
      axis words are gone from MIR. A structural scope is a **class**, a structural variable is a
      **member**, a procedural variable is a **local**, and a procedural scope is a **block**; their
      ids, references, accessors, fields, and dumper labels follow. HIR stays SystemVerilog-faithful
      and keeps its structural / procedural names, so the HIR-to-MIR lowering helpers that translate
      _from_ HIR keep their HIR-input-aligned names (the convention R21 settled) -- the rename
      touches what MIR _is_, not the translator that reads HIR. The member / local distinction (an
      instance field versus a body-frame variable) is the real software pair that replaces the
      former symmetric axis.

- [x] R23 -- "Materialise a reference into an owning value" is an explicit
      `Call(ArrayMethod{kToOwned})` node in MIR (Rust's `ToOwned` trait). HIR-to-MIR inserts the
      wrap at the read boundary; render is a mechanical translation. The `RenderExpr` /
      `RenderExprNatural` split and `ProducesPackedArrayRef` predicate are gone.

- [ ] R24 -- Dispatch the "sized collection" concept through one resolver instead of branching on
      the concrete container at each call site. R29 absorbed the cross-container collapse for the
      shared method (every container's element count is one `BuiltinFn` identity now), so the only
      remaining branch is `Size` (LRM 7.4.3 / 7.5 / 7.10.2 dynamic / associative / queue) vs `Len`
      (LRM 6.16.1 string-mandated name); a single lowering site handles it today and serves as the
      one place that translates "sized collection" to its container's element-count method. Under
      static dispatch the concept is a compile-time resolver and never persists as an IR node. **Why
      deferred**: only one consumer today, so the resolver is a one-caller function -- the interface
      payoff is multiple generic callers sharing the table, which do not exist yet. **Trigger**: a
      second generic consumer of collection-size dispatch (a locator / reduction lowering, an
      array-querying system function, or a generic-algorithm pass). A separate, structural
      sub-question rides along: whether fixed unpacked and packed arrays should also be "sized
      collections" with a runtime element-count query (a type-model decision, distinct from how
      `foreach` iterates them -- `foreach` over a fixed array uses its declared range and direction,
      not a count, so that split is a real semantic difference, not the same redundancy).

- [ ] R25 -- Two families still do not fit the generic `(receiver).name(args)` member-call rule and
      need their own lowering decision: enum type-static methods (no receiver -- the qualifier is
      part of the symbol identity, surfaced as a static-callee arm) and the value-static
      `$isunknown` query (type-static in the all-2-state case, constant-folded). The cross-cutting
      value-model decision is settled: an SV-facing runtime method's return and parameter types are
      the SV value types, the representation bridge lives inside the method body (an internal detail
      of the value layer), and the backend reads the call's stated result type and emits it
      mechanically. The integral-query surface and integral-argument surface flow through SV value
      types directly; the member-shaped families (string, array, queue, associative including
      traversal, enum instance) render through one generic handler; user calls carry `self` and
      event trigger / triggered carry the engine handle as real arguments. **Trigger**: continuation
      of `decisions/runtime-effects-as-generic-calls.md`.

- [x] R26 -- Runtime container protocols are pinned as explicit C++20 concepts in a single
      value-layer concept header; each container `static_assert`s every protocol it satisfies. Slice
      is aligned across the four conforming containers: the signature is
      `Slice(PackedArray, PackedArray)`, with Queue's two arguments meaning inclusive bounds (LRM
      7.10.1) and the three fixed-width containers meaning `(offset, count)` (LRM 7.4.5 / 11.5.2
      require canonical-fill at the type-fixed width, which is not derivable from `(lo, hi)`). A
      single HIR-to-MIR range-bounds unfolder dispatches by container kind; the call's argument list
      flows through render with no type-dependent argument projection. Signature drift on any pinned
      protocol is now a compile-time failure. Subsumes R17b.

- [x] R28 -- The read-vs-write access surface is aligned at the noun-level naming axis across every
      container and across both lowering and runtime: bare `Element` / `Slice` for read,
      `ElementRef` / `SliceRef` for write. Lowering carries the read-vs-write choice as one
      access-side parameter at the slice-builder boundary; the runtime concepts pin the
      bare-and-`Ref` pair as part of `Indexable` (write-side present on every keyed container) and
      split `Sliceable` from `SliceableRef` because LRM 7.10 defines no write-side queue slice. No
      separate `Writable` concept: the pair belongs with the read-side concept it shadows.

- [x] R29 -- Built-in method calls and runtime entries carry one flat closed-namespace identifier
      shared between HIR and MIR. Two MIR callee arms (instance, type-namespace-qualified static)
      replace the per-family variant. The receiver's MIR type drives backend calling-convention
      mechanically; SV-side `$isunknown` returns the SV `bit` type so no host-bool lift survives at
      the backend. See `decisions/builtin-call-identity.md`.

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

- [x] R31 -- **Every closure-construction site now goes through the one closure builder.** The
      builder owns the body scope, the `self` capture (captures[0]), and the capture sink; the
      caller fills the body -- by lowering HIR through the builder's frame (the sink turns
      enclosing-variable reads into captures) or by hand-snapshotting outer expressions by value --
      and a terminal assembles the closure value. It generalizes over a coroutine result (a fork
      branch yields the coroutine type via a `co_return` terminal), a by-value capture depth (a
      fork-scope local snapshots, a deeper enclosing variable aliases), per-invocation parameters
      (the with-clause iterator and index, LRM 7.12.4), manual by-value captures (the NBA submit and
      deferred-assertion check, which build their bodies by hand and so use no sink), and three
      terminals -- a value `return`, a `co_return`, and a bare void body. The fork-join branch,
      with-clause iterator, NBA submit, and deferred-assertion check all dropped their inline
      body-scope / self / sink / capture-assembly code; `mir::ClosureExpr` is now constructed in
      exactly one place. The result type left the constructor -- a synchronous terminal derives it
      from the result expression, since a closure's result type is just its returned value's type.

- [ ] R32 -- Unify the Expr- / value-construction helper naming, which is ad hoc today (`Make*` and
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

- [ ] R33 -- Remove the redundant `UnsupportedCategory` argument from the `diag::Unsupported`
      factory. The category is already declared once per `DiagCode` in the code table; the factory
      additionally takes a `cat` argument, stores it, and cross-checks it against the table,
      throwing an `InternalError` on mismatch. A call site that passes the wrong category therefore
      _crashes_ on the unsupported path instead of emitting the clean diagnostic it intended -- a
      latent landmine on every untested rejection. Eleven such drifts were found and corrected
      pointwise, but the design keeps re-arming the class: the call-site category is a second source
      of truth that can diverge. Target shape: the factory derives
      `category = DiagCodeCategory(code)` from the table alone, the `cat` parameter and the
      `RequireCategory` check are deleted, and the ~96 call sites drop their trailing category
      argument. This is the universal diagnostic-metadata shape -- Clang keys severity / group to
      the diagnostic id through TableGen records, Roslyn through a `DiagnosticDescriptor`, rustc
      through the diagnostic's own level: metadata is a property of the id, derived at emit, never
      re-supplied at the report site. The `diag_code.cpp` table already is that registry; the only
      flaw is the factory re-takes a value it should read. The same shape applies to the sibling
      `RequireKind` guard, which should fold in. The load-bearing principle: diagnostic construction
      is the graceful-degradation path and must be infallible -- it must never throw, least of all
      on the rarely-exercised unsupported branches. **Trigger**: standalone; mechanical sweep across
      every lowering file, so it lands as its own cut.

- [x] R34 -- Unify the procedural and structural AST-to-HIR expression-lowering paths. The two
      carried parallel handlers per expression family that built the same HIR node but guarded their
      accepted operand types independently, so the two drifted. Resolved as the AST-to-HIR slice of
      `generic-lowering.md` Phase 2: each context-free family is now one function template over the
      pass class, so there is a single guard and the drift cannot recur. The specific string
      element-select concern turned out to need no separate fix -- slang rejects an element of a
      dynamic type outside procedural code, so a structural string `s[i]` never reaches lowering,
      and the value realization (getc for a string base, element access for an array) was already a
      property of the node at HIR-to-MIR, applied regardless of origin.

- [ ] R35 -- Resolve the intra-unit part of a hierarchical reference at compile time as typed member
      access, not by name at construction. `reference_resolution.md` invariant 2 says an intra-unit
      reference -- including one into a same-unit named generate scope -- resolves at compile time
      as a constructed-object reference plus a compile-time offset. Today such a reference is
      lowered like a cross-unit one: a borrowed-pointer slot filled by a runtime by-name navigation
      (`GetChild` / `GetSignal`) from `self`, even though the whole path stays inside the unit and
      its layout is known at compile time. Only the genuine cross-unit hop (into another unit's
      internal) must stay by name. Target shape: the intra-unit prefix is typed member access, and a
      fully intra-unit reference is a plain member access with no runtime slot. **Blocker**: this
      needs a MIR member-access form that descends into an owned child object's member, resolving
      the member against the receiver's type. Member access today is hops-relative to the enclosing
      class and cannot descend, so this rests on a new member-access capability that touches the
      member-access model. **Trigger**: when that descend capability is designed and built.

- [x] R36 -- The container default-value slot fused the read-miss value with the discarded-write
      target, forcing the const read to scrub the slot a prior write may have dirtied before
      returning -- a const method that mutates. Split the read role from the write role so the read
      path is pure. See `../decisions/runtime-shape-and-default-value.md`.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`control-flow.md`,
  `operators.md`, etc.).
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
- The pre-reset surface re-implementation backlog. That lives in `architecture-reset.md`.
