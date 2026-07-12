# Refactor

Tracks architectural debt and cleanup work that has been deferred because the immediate task did not
need it -- but that we have agreed should land eventually. Distinct from the per-feature progress
files (which track in-flight language-feature work); this file is the queue of "architecturally we
know this is wrong, here is the target shape, and what (if anything) blocks it from landing now".

Each entry states:

- The current shape (what is awkward today)
- The target shape (what it should look like)
- What blocks it, if anything -- a named prerequisite that must land first

An entry with no blocker is doable now; it is unlanded only because no one has picked it up. Per the
project's clean-code-outranks-scope stance, an architecture-correctness cut lands when its code is
next touched, not when some future event is deemed to "trigger" it. The only honest reasons to leave
an entry open are a named unlanded prerequisite (it is blocked), or a cut large and cross-cutting
enough to warrant its own focused review.

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

- [x] R6 -- The synthetic-expression builders an AST-to-HIR lowering reaches for (a counter,
      sentinel, or computed bound) are public, pure, and have one definition each. A raw-int64
      `int`-typed literal builder folds the masked-word `IntegralConstant` layout into a single
      function, and one generic reference-expression builder wraps any named-value reference primary
      (procedural / structural / loop var) -- the three families build through the one builder
      rather than a per-file copy. A bool-literal builder was not added: no lowering synthesizes
      one, and surface without a consumer is not introduced.

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

- [ ] R8 -- Unify the callable forms onto one concept, per `../decisions/unified-callable-model.md`.
      Today a process (`mir::Process`), a method (`mir::MethodDecl`), the constructor block, the
      per-class resolve and initialize phase bodies, and a closure (`mir::ClosureExpr`) are separate
      constructs whose bodies are all the same `Block`, and the same fact ("is this a coroutine") is
      encoded several ways. The target is one callable concept -- callable code (a signature plus an
      internal body or an external symbol) and a callable value (code plus a bound environment) --
      with the result type carrying the call protocol and parameter direction normalized to data
      flow. It rebases callable machinery across lowering, MIR, the dumper, and the backend (where
      the per-shape `Render*` paths collapse into one generic function renderer), so it lands in
      staged cuts, each its own focused review:
  - [x] R8a -- The result type is the sole carrier of the call protocol. A method's coroutine-ness
        is read from its result type (a coroutine result is a task, a value / void result a
        function), not a side enum; `MethodKind` is removed. Behavior-neutral; existing task /
        function tests prove no regression.

  - [x] R8b -- Parameter direction normalizes to data flow. `output` / `inout` formals stop being
        reference parameters and become components of the callable's completion payload -- the
        explicit return (if any) followed by each `output` / `inout` value, normalized by count
        (zero is `Void`, one is a bare type, two or more a tuple), riding the result type and
        written to the caller's actual after completion -- so copy-out timing is correct under
        suspension; `ref` / `const ref` stay reference-typed parameters. The direction enum is
        removed (a parameter becomes a typed binding). This forces the coroutine result type to be
        parameterized (`Coroutine<T>`): a task's completion payload is its `T`. Two merge gates make
        the realization decoupled rather than a relabeled special case: (1) await is a typed,
        value-yielding form -- awaiting a `Coroutine<T>` yields `T`, and an output writeback is a
        projection of that value, so a pure suspension is the `Coroutine<Void>` case of the same one
        await, never a void-only statement with a hidden writeback convention; (2) the C++ backend
        realizes the payload through a hidden, caller-owned completion slot that is not part of the
        MIR signature (the monomorphic coroutine handle the scheduler relies on is untouched -- a
        payload-templated promise is deferred as a backend-only option). The two output-pack
        invariants from `../decisions/unified-callable-model.md` hold: an output / inout actual
        place is bound exactly once at call entry, and the completion slot outlives any write the
        callee can still make. Per `../decisions/unified-callable-model.md`.

  - [x] R8c -- Callable code versus callable value. A closure constructs a callable value (code plus
        a bound environment); a directly-invoked named callable receives its environment from the
        caller. `self` is the code's first parameter, bound into a value's environment when needed,
        not a privileged `captures[0]` slot.

  - [x] R8d -- `mir::Process` dissolves into a callable value registered at constructor time
        (per-instance, generate-dependent), with `initial` and `final` as distinct lifecycle
        registrations. `ProcessKind` is removed.

  - [ ] R8e -- A DPI import is a bodyless external callable: the foreign-symbol implementation form,
        the external twin of an internal body. The virtual-dispatch facet this entry once bundled is
        the object model's dynamic dispatch, now tracked in `object-model.md`; with the object model
        designed, the external-callable work is no longer gated.

  - [x] R8f -- The scope's callables render through one backend method path. A process body and the
        synthesized resolve / initialize lifecycle bodies join functions and tasks as one uniform
        callable: a static function over the explicit receiver `self`, with no per-shape kind tag.
        The per-shape backend renderers collapse into one mechanical fold that reads the body's
        fields (result type, name, parameters, body): `void` and the coroutine type are ordinary
        result types, `co_return` is a body statement rather than a render-time epilogue, and the
        receiver is always `self`. How a referencing site reaches a body -- a direct call, a process
        registration, an engine-dispatched lifecycle hook -- is separate dispatch plumbing (a thin
        virtual shim forwarding to the static body, the pattern the constructor already uses), never
        a property of the body. The two remaining backend-synthesized remnants -- process activation
        registration and the upward-reference member initializer -- retire under R8d and R40.

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
      `frame.current_block->Add...` uniformly. `ModuleLowerer`, `StructuralScopeLowerer`, and
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
      vantage.

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
      iteration index. `mir::MethodDecl::name` already plays this role for methods; processes are
      anonymous in SV (LRM 9.2) so HIR-to-MIR synthesises a positional identifier (`"process_0"`
      etc.) and threads it into the lowering so the returned `Process` is constant -- no post-hoc
      mutation. The `WithStaticFrame(...)` install in the cpp backend now reads
      `process.name + "__static"` rather than computing from an iteration index; the walker-state
      propagation of the frame field name continues until R18 dissolves the walk frame.

- [x] R16 -- Give every MIR callable body an explicit `self` first binding -- `locals[0]` is a local
      of borrowed-pointer-to-enclosing-class type, named `self`. Route every class-member access
      through a new `mir::MemberAccessExpr { receiver, var }` whose receiver reaches `self` via
      `DerefExpr(LocalRef(self))`. Today four distinct receiver mechanisms coexist -- method `this`
      (process / method / constructor bodies), fork-branch `(M* self)` parameter, NBA / `$strobe` /
      scan closures' `[this]` or `[=, this]` capture, and `mir::MemberRef`'s implicit-receiver
      render -- and the cpp backend dispatches between them through `RenderContext` walker state
      (`ReceiverObject()` / `WithReceiver(...)` / `DeferredByValueCapture()` / `MemberPrefix()`).
      The same dispatch would have to be re-derived by every future backend (LIR / LLVM-IR). Target
      shape: `locals[0]` is uniformly `self` across every callable form, but how it is supplied
      follows each form's natural binding mechanism -- a process / method / constructor body
      receives `self` as its first formal parameter (the caller supplies), while a closure carries
      `self` as its first by-value capture (the enclosing scope snapshots its own self at
      construction). `mir::SelfScopeExpr` is removed (its job was to denote "the current receiver,
      whatever that is" -- precisely the implicit-context shape this refactor eliminates). C++ emit
      per form: process / method / constructor bodies as
      `static auto <name>(M* self, ...) -> ... { ... }`, with the C++ constructor delegating its
      body to a `static init(this)` call; closures as
      `[self = <enclosing self>, cap1 = ..., &cap2 = ...](closure_params) -> R { ... }` -- every
      capture is name-explicit, the clause never contains `[this]`, `[=]`, or `[&]`.
      `CreateProcesses()` remains a virtual instance method (C++ requires it) and emits
      `AddProcess(kind, process_N(this))`. The receiver-related `RenderContext` machinery disappears
      in lockstep. See `docs/decisions/callable-receiver.md`.

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
      `constructor.body.root_stmts`, with the value being the user-supplied expression when present
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

- [x] R20 -- **Runtime effects as generic calls** (design settled in
      `decisions/runtime-effects-as-generic-calls.md`). Every runtime effect is now an ordinary
      `CallExpr` over a closed-namespace callee, services threaded as a plain `self.Services()`
      argument; `RuntimeCallExpr` and its payloads are gone. The closure-bearing subset (NBA submit,
      deferred-assertion submit, `$strobe`, `$sscanf` / `$fscanf`) is tracked separately under R30
      -- all of its members are now on the generic shape too. The remaining migration debt (families
      still routed via `SystemSubroutineCallee` rather than `BuiltinFnCallee` / `FreeFnCallee`) is
      the subject of R37.

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

- [x] R25 -- **Closed: both carve-outs resolved.** The two value-query families this entry set aside
      as not fitting the generic `(receiver).name(args)` member-call rule are both settled. Enum
      type-static methods (`first` / `last` / `num`, no receiver -- the type qualifier is part of
      the symbol identity) lower to the `BuiltinStaticCallee` arm R29 introduced and render as
      `Enum::method(args)`. `$isunknown` needs no special type-static / constant-fold path: it is
      the generic instance built-in call `(x).IsUnknown()` returning the SV `bit` type (1-bit
      `PackedArray`, LRM 20.9), now wired end to end -- recognized at AST-to-HIR by
      `KnownSystemName::IsUnknown`, lowered through the context-free call family in both procedural
      and continuous-assign positions (`../decisions/context-free-call-lowering.md`). The
      cross-cutting value-model (SV-typed runtime signatures, the representation bridge inside the
      method body, the backend reading the stated result type) is settled.

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

- [x] R30 -- **Runtime effects as generic calls: the closure-bearing subset** (carve-out of R20,
      same decision). The `$strobe` family, the scan family (`$sscanf` / `$fscanf`), and the
      synthesized non-blocking-assignment and deferred-assertion submits each lower to a generic
      `CallExpr` over a compiler-synthesized closure built through the one closure builder (R31).

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

- [x] R32 -- The Expr- / value-construction helper naming is pinned to one load-bearing rule: a
      `Make<Node>` is a pure factory (assembles a node from ready-made parts, reads at most a
      `const` frame for the body's `self` or a builtin type, interns nothing -- the caller does the
      `AddExpr`), and a `Build<Node>` is a scope builder (interns one or more child nodes into the
      scope as it builds, returning the top node detached or its id, context argument first); a
      type-producing variant is `Make<X>Type`. The distinguishing axis is the scope side effect, not
      whether the helper consults the frame -- a factory that reads `self` off a `const` frame and
      interns nothing is `Make`. A full audit of the construction helpers found the codebase already
      largely conformant; the few pure factories mislabelled `Build` were renamed to `Make`. The
      rule now lives as an invariant in `lowering_organization.md` (Expression-Builder Helpers), so
      it outlives this entry.

- [x] R33 -- A diagnostic's metadata is a property of its code, derived at construction, never
      re-supplied at the report site, and construction never throws. The per-kind factories and the
      `RequireKind` / `RequireCategory` cross-check guards (which threw on the rarely-exercised
      unsupported path) are gone; one kind-neutral surface (`diag::Fail` for the recoverable-failure
      path, `diag::Make` for report-and-continue) reads the kind from the code table. The
      consumer-less `UnsupportedCategory` classification axis is removed in full rather than
      retained. See `../decisions/diagnostic-construction.md`.

- [x] R34 -- Unify the procedural and structural AST-to-HIR expression-lowering paths. The two
      carried parallel handlers per expression family that built the same HIR node but guarded their
      accepted operand types independently, so the two drifted. Resolved as the AST-to-HIR slice of
      the generic-lowering work: each context-free family is now one function template over the pass
      class, so there is a single guard and the drift cannot recur. The specific string
      element-select concern turned out to need no separate fix -- slang rejects an element of a
      dynamic type outside procedural code, so a structural string `s[i]` never reaches lowering,
      and the value realization (getc for a string base, element access for an array) was already a
      property of the node at HIR-to-MIR, applied regardless of origin.

- [x] R35 -- Realize hierarchical references through the routing the architecture docs prescribe:
      one semantic shape per reference, per-segment classification by layout visibility, route
      execution in Resolve, endpoint committed in Seal, hot path reads only sealed endpoints. Today
      the lowering uses three parallel mechanisms keyed off the frontend's lexical-form
      classification and on source order: a downward by-name SDK install at construction, an upward
      wrapper registered at construction and resolved later, and the typed enclosing access used for
      bare names. These collapse to one route in the target shape. See
      `../decisions/hierarchical-reference-routing.md` and
      `../decisions/binding-graph-resolution.md`. The work bundles into one PR landing as five
      checkpoints:
  - [x] Structural-first lowering pipeline. Every class's structural shape -- its members, its owned
        children, the signal registrations it contributes to the runtime tree -- is complete in MIR
        before any body lowers. A body, a process, an initializer, or an install statement may reach
        a peer class's structural members through the artifact's identity model; the lowering order
        guarantees the peer's shape is visible when the referring body translates.
  - [x] Reference install runs in Resolve. Every cross-instance reference's install code emits into
        the resolve body, not the constructor body. The constructor allocates the instance shell and
        constructs its children; resolution runs after the runtime tree is built. Forward and
        backward reference directions stop differing in when they install.
  - [x] The upward-reference runtime wrapper retires from IR vocabulary. The wrapper used today to
        defer upward references' resolution is no longer carried as an MIR type, an HIR variant, or
        a vocabulary item the lowering names. Every reference's slot becomes a uniform borrowed
        pointer to its target's cell, filled by ordinary resolve-time install code.
  - [x] Layout-visible route segments install typed. A route segment whose source and target classes
        are both owned by this artifact emits as a typed member-access chain; the runtime SDK is
        reached only for segments that cross the unit boundary. A reference whose entire route is
        layout-visible installs with no SDK call; a mixed-route reference installs a typed prefix
        composed with an SDK suffix. An indexed hop on a layout-visible segment falls back to the
        SDK for its own hop only, then downcasts back to typed so the rest of the route stays
        layout-visible.
  - [x] Reference mechanism unified; lexical-form dispatch retired. The lowering produces one route
        shape per reference regardless of the form that named the target. The frontend's
        lexical-form classification is consumed only at AST-to-HIR for route synthesis and does not
        reach the route's mechanism dispatch. HIR path encoding is structural: each segment carries
        its name plus its per-axis indices, and the head carries its own indices, so no consumer
        needs to know a per-head-kind convention for where indices sit.

- [x] R36 -- The container default-value slot fused the read-miss value with the discarded-write
      target, forcing the const read to scrub the slot a prior write may have dirtied before
      returning -- a const method that mutates. Split the read role from the write role so the read
      path is pure. See `../decisions/runtime-shape-and-default-value.md`.

- [x] R37 -- Retire `SystemSubroutineCallee` from MIR's Callee variant set. R20 declared every
      runtime effect "generic" because each was an ordinary `CallExpr`; in practice the closure-free
      families still route through `SystemSubroutineCallee`, whose identity is an SV system task id
      -- an HIR / source-language concept that `mir.md` invariant 10 forbids carrying into MIR. Each
      affected family decomposes into MIR-layer-correct primitives whose receiver matches the
      operation's true subsystem ownership. The receiver is what fixes the layer; the SV system task
      id stays at HIR / support as the dispatch key but does not survive as a MIR callee identity.

  Sub-items, by family:
  - Pure value-layer (no receiver, render as `lyra::value::Name(...)`):
    - [x] `$sscanf` / `$fscanf`: pure `value::Scan(input, format, targets...)` + `files`-side
          `PeekBuffered` / `AdvanceFd` primitives for the file form. The MIR shape of the two SV
          system tasks differs by primitive composition, not by an enum tag on a unified call.
    - [x] `$sformat` / `$sformatf` / `$swrite[bho]?`: route to the existing `Format` method (the one
          print already uses) returning a string; for `$sformat` the call result is assigned to the
          output lvalue. The dedicated `LyraSFormat` runtime entry retires.
  - Diagnostic subsystem (`services.Diagnostic()`):
    - [x] `$info` / `$warning` / `$error` / `$fatal`: each decomposes to `services.Format(items)`
          for the message text, then
          `services.Diagnostic().Emit{Info,Warning,Error,Fatal}(origin,     text)` for the
          severity-tagged emit, with `origin` carrying the call's `file:line:col` so the dispatcher
          can prefix the message and key its rate-limit counter per site (LRM 20.10). The severities
          are distinct `BuiltinFn` methods (parallel to print's `Write` / `Writeln` split), not a
          single `Emit(severity, text)` with a tag arg. The unique / priority deferred-check cascade
          synthesizes its warning through the same broker. `$fatal` chains an implicit
          `kFatalFinish` after the emit; the engine flags the termination so `Run()` returns a
          non-zero exit code per LRM 20.10.
  - File-IO subsystem (`services.Files()`):
    - [x] `$fopen` / `$fclose` / `$fread` / `$fseek` / `$rewind` / `$ftell` / `$feof` / `$ferror` /
          `$fflush` / `$fgetc` / `$ungetc` / `$fgets`: each lowers to a `BuiltinFnCallee` method on
          the `files` broker (`files.Open`, `files.Close`, `files.Read`, ...). The runtime
          free-function `Lyra*` entries are retired; the `FileTable` methods are the only surface.
          `$fwrite` already rides the print pipeline (`kFormat` + `kWrite`); `$fputc` is not yet
          wired.
  - Engine forwarders on `services`:
    - [x] `$time` / `$stime` / `$realtime`: each lowers to a `FreeFnCallee` against the matching
          runtime entry with the engine handle and the calling scope's unit power as ordinary
          operands.
    - [x] `$finish` / `$exit` / `$stop`: `$finish` is on the same `FreeFnCallee` shape; the
          await-suspension wrapping is unchanged. `$exit` / `$stop` are not yet wired and pick up
          the shape when they land.
    - [x] `$timeformat` / `$printtimescale`: `$timeformat` lowers to
          `services.SetTimeFormat(units, precision, suffix, min_width)` (four-argument form) or
          `services.ResetTimeFormat()` (no-argument form), one method per form rather than an
          arity-driven branch. `$printtimescale` desugars at lowering time: the scope name, unit
          power, and precision power are all compile-time facts of the enclosing scope, so the full
          "Time scale of (...) is X / Y" message is assembled into a string literal and routed
          through `services.Files().Writeln(STDOUT_FD, msg)`, the same sink-write path that
          `$display` lands on. The `LyraTimeFormat` and `LyraPrintTimescale` runtime free functions
          retire; the timescale runtime header is deleted.

- [x] R38 -- `Format` moved from `RuntimeServices` to the value layer. The per-item value-format
      walk is now the pure free function `value::Format(items, time_format)`; the engine's
      `$timeformat` state is reached through a `TimeFormat` reader on `services` and threaded into
      the call as an explicit operand at lowering, so the format step holds no engine state of its
      own. The four format sites (`$display` family, `$info` family, `$sformat` family, the
      deferred-check cascade) share one lowering builder that assembles the call and its
      `TimeFormat` operand. This makes "pure value ops live at `lyra::value`, engine state is
      reached through `services`" consistent everywhere, closing the last exception R37 left open.

- [x] R39 -- The implicit `Ref<T>` access is lifted into explicit MIR calls, reusing the observable
      cell protocol rather than inventing a parallel one. R12 lifted observable-cell reads / writes
      / mutations into explicit `BuiltinFnCallee` (`kGet` / `kSet` / `kMutate`) calls at HIR-to-MIR
      so the backend renders them mechanically. A `ref` / `const ref` formal (LRM 13.5.2) and a
      by-reference capture (LRM 6.21) carry a `RefType` whose access -- by
      `reference-as-data-type.md` F3 -- is the same cell protocol (read, update-event `Set`,
      partial-write `Mutate`). The lift therefore routes through the same three callees: the callee
      names the access; the receiver's `RefType` (vs `ObservableType`) selects `Ref<T>` vs `Var<T>`
      at render, exactly as `builtin-call-identity.md` D3 prescribes -- no `RefType`-specific
      callee. The root cause was a single typing asymmetry: a reference-to-a-ref-binding was typed
      with the unwrapped value, so the auto-`kGet` wrap and the `Set` / `Mutate` write-routing
      (which already admit every cell-type, including `RefType`) skipped it, forcing the render path
      to re-derive ref-ness from the slot type and inject `.Get()` / a spliced engine handle -- a
      `mir.md` invariant 10 violation. Typing the reference with its `RefType` slot makes the
      existing lift catch it; whole writes, compound / partial writes, and increment / decrement
      through a ref are now all well-defined (the render previously bailed on the latter two). The
      runtime `Ref<T>` gained `Mutate`, and `ScopedMutation` now commits through a `Ref<T>` so
      observable and reference partial writes share one handle. See
      `decisions/value-type-concepts.md` and `decisions/reference-as-data-type.md`.

- [x] R40 -- Retire the construct- / control-stmt shapes that the backend completes from outside
      MIR. `ForkStmt`, `ConstructOwnedObjectStmt`, and `ConstructExternalUnitStmt` each name a
      runtime entry the render path looks up by stmt kind and string-injects a `self->Services()`
      argument the MIR did not state. `ConstructOwnedObjectStmt` additionally emits a companion
      `RegisterChild` call after the construction -- a runtime mutation that exists nowhere in MIR.
      `DelayStmt` has already retired this way: `#N` now lowers to `AwaitStmt` over a
      `FreeFnCallee{kDelay}` call whose argument vector states services, the literal duration, and
      the calling scope's precision power. Apply the same frame to the remaining three: each stmt
      decomposes to `AwaitStmt` plus a generic `CallExpr` (or a sequence of them) whose argument
      vector is complete; services flows as an explicit operand; companion runtime registrations are
      MIR statements of their own. The stmt-kind-as-runtime-entry-tag layer disappears; the
      constructor receiver pattern aligns with `decisions/callable-receiver.md`.

- [x] R41 -- Push the sensitivity-leaf subscription target into MIR. Each `SensitivityRead` carries
      an explicit observable-pointer expression chosen at HIR-to-MIR: an `AddressOf` of the cell
      member, or a bare borrowed-pointer slot. The render-side type-kind dispatch is gone.
      Address-of itself was lifted to MIR via the new primitive (see
      `decisions/address-of-primitive.md`).

- [x] R42 -- Retired `RuntimeNavCallee`. The three by-name scope operations (`kRegisterSignal` /
      `kGetSignal` / `kGetChild`) now ride `BuiltinFnCallee` with the signal / child name as a
      regular `StringLiteral` argument and the index list as an `ArrayLiteralExpr`. The `kGetSignal`
      cast is lifted to a new MIR `PointerCastExpr` primitive whose destination type is the call
      site's slot type; the backend emits `static_cast<T>(...)` mechanically from a stated MIR fact.
      The `kGetChild` index conversion moved into the runtime (which now accepts
      `std::span<const value::PackedArray>` and calls `.ToInt64()` itself), so the render side has
      no `.ToInt64()` injection and no `std::array{...}` wrapper. Every call -- regardless of callee
      variant -- now renders as `fn(rendered_args...)`.

- [x] R43 -- Replace the constructor-of-pointer fallback. The expression set grew a `NullLiteral`
      primitive that the default-value lowering emits directly for borrowed-pointer members; the
      `ConstructorCallee` render path no longer pattern-matches on empty-args-against-pointer.

- [x] R44 -- Swept the pre-existing code-comment doc-reference violations alongside the policy
      landing. Every comment in `src/` and `include/` now states the code's own contract; no
      `docs/`, decision-doc name, or "invariant N" back-references remain.

- [x] R45 -- Unify MIR's call shape. `mir::Callee` is a 3-arm
      `std::variant<Direct, Indirect,     Construct>`; the former 6-arm split (`MethodRef` /
      `BuiltinFnCallee` / `BuiltinStaticCallee` / `FreeFnCallee` / `ClosureRef` /
      `ConstructorCallee`) collapsed because the first four were the same generic-language concept
      "a direct call to a named symbol" -- instance method, type-static, free function are not three
      call kinds, just three signature / qualifier shapes of one direct invocation, the way Rust
      sugars `Vec::push(&mut v, x)` to `v.push(x)`. The arm split let `mir.md` invariant 10 ("a node
      field that no backend's realization reads, or that restates what the node's structural context
      already fixes") be violated -- the LLVM backend's realization does not consult the arm; for
      the C++ backend, instance-form / free-form is a per-id render fact, not structure. Landed as:
      `Callee = variant<Direct, Indirect, Construct>`, where
      `Direct { target, qualification: optional<ScopeQualifier> }`. `target` is the symbol identity
      (`variant<MethodId, BuiltinFn>` today; R49 unifies into one `CallableId`). `ScopeQualifier`
      starts as `variant<TypeQualifier{TypeId}>`; R50 adds `Namespace{...}` for future SV packages.
      `MethodRef`'s `hops` field retires -- the receiver becomes explicit
      (`Deref(LocalRef(self_at_hops))` in `args[0]`), the receiver's type pins the enclosing class
      whose `methods` arena names the `MethodId`. Render mode is read off the callee's signature: a
      self formal (built-in: per-`BuiltinFn` metadata table; user method:
      `MethodDecl.code.params[0]`) drives `args[0].name(rest)`; a qualification drives
      `Q::name(args)`; neither drives `<backend_ns>::name(args)`, where `<backend_ns>` is per-id
      backend metadata (the C++ namespace a runtime helper lives in, no MIR-level meaning). The
      `decisions/builtin-call-identity.md` paragraph that justified the instance / static / free
      split as "structural at MIR" for backend convenience is rewritten -- the split was an
      invariant-10 violation, not a structural fact. Reserves the seat for `Virtual` (R8e) without
      inventing it now: gated on R47, a future `Virtual { slot, static_receiver_type }` arm slots in
      as an additional `Callee` arm with no change to the others.

- [x] R46 -- MIR's value-reinterpretation primitive is one `CastExpr{operand}` carrying no kind
      axis. The destination type is stated on the enclosing `Expr::type`; the source type is read
      off the operand; the `(source, destination)` type pair fully determines the realization
      (integral resize / sign-handling, integral-to-real, real-to-integral, packed-to-enum,
      pointer-to-different-pointee). Backends consult the type pair to select the emit (C++ chooses
      among `static_cast` and `lyra::value` helpers; LLVM picks `zext` / `sext` / `trunc` / `fptosi`
      / `bitcast` / `inttoptr`). The earlier `mir::ConversionExpr` (mirroring HIR's LRM-defined
      `ConversionKind`) and `mir::PointerCastExpr` (the backend type-erasure bridge) retire; HIR's
      `ConversionExpr` + `ConversionKind` stay as SV vocab in HIR, and the 5 HIR kinds collapse to
      the one MIR primitive at HIR-to-MIR. The kind axis Clang's AST carries exists because Clang
      drives LLVM codegen directly; Lyra's MIR is a higher layer and the (src, dst) type pair
      already names the same dispatch.

- [ ] R49 -- Unify callable identity. Today `mir::Direct::target` is `variant<MethodId, BuiltinFn>`
      -- a built-in is a closed-enum global id; a user method is a per-class arena id. As DPI
      imports, future SV class statics, and future package-level free functions land, each currently
      brings a new identity space (the historical pattern is one variant arm per origin). The target
      is one `CallableId` space whose entries name a `CallableDecl` carrying signature,
      implementation form (internal body / external symbol / built-in intrinsic), receiver
      convention, and per-backend render metadata. `mir::Direct::target` becomes a single
      `CallableId`; the `MethodId` / `BuiltinFn` distinction collapses into the declaration's
      implementation form. Backend tables (`BuiltinFnCppName`, `BuiltinFnCppNamespace`) re-key by
      `CallableId`. **Gated on**: R8e (external callable form) and a co-design with DPI's
      symbol-contract structure -- the `CallableDecl` shape is the same one those features need.

- [ ] R50 -- Add `Namespace` qualifier to `mir::ScopeQualifier`. Today the type carries one arm
      (`TypeQualifier{TypeId}`) because SV only exposes type-as-namespace at the source level
      (`MyEnum::first`, `PackedArray::FromInt`). SV packages (LRM 26) expose namespace-as-namespace
      (`P::pkg_func`, `P::MyEnum::first`); for a package-qualified call the call site provides a
      `NamespaceId` instead of (or composed with) a `TypeId`. Adds `Namespace{NamespaceId}` and a
      `Path` composition for the cross-cut case. **Gated on**: SV package support landing in HIR --
      this is a refactor of MIR's existing qualifier shape to match the feature's needs, not an
      independent migration.

- [ ] R47 -- The object model is designed: a module instance, a generate scope, and a SystemVerilog
      class are one generic nominal object type, differing only in which base they extend, which
      reference reaches their instances, and which lifecycle they participate in -- not a
      module-specific object set against a class-specific one. The contract is
      `../architecture/object_model.md` and the resolved trade-offs are
      `../decisions/object-model.md`. The staged implementation -- generalizing the module-side
      object model, then putting SystemVerilog classes on it -- is tracked in `object-model.md`. The
      design gate this entry held over SV classes and over R8's virtual-dispatch facet is lifted.

- [x] R48 -- An object type names its class directly, so emit resolves the owning class in O(1).
      `ObjectType` carries a unit-unique `ClassId`, and the backend recovers a member / method
      reference's owning class by a direct `unit.GetClass(class_id)` lookup rather than matching the
      receiver's `ObjectType` against every class reachable from the render position. The flat
      unit-level class table this needed exists (class identity is assigned into the unit registry),
      so the former linear search per access -- the one remaining place the backend re-derived
      information the lowering already had -- is gone.

- [x] R51 -- Class construction is stated as a protocol on the class rather than side fields on the
      class or render conventions in the backend. `Class::constructor` is a `ConstructorDecl`
      pairing (a) the ctor's callable code, stored as an ordinary `MethodDecl` in `Class::methods`,
      with (b) explicit base and per-field initialization ordered before the body runs. The C++
      render composes the mem-init list from those stated facts alone; the interim
      `Class::ctor_prefix_params` / `Class::params` / `Class::base_init` side fields are removed,
      the prefix params flow through the ctor's own signature, and `ParamRef` (dead vocabulary tied
      to the removed `Class::params`) is gone. Value transfer to the base call is an explicit
      `MoveExpr` MIR primitive that the C++ backend translates mechanically to `std::move(...)`; an
      alias-style handle (services / files / diagnostic) is exempt via `Type::IsAliasHandle` rather
      than a render-side pattern match, so the render learns no runtime type name. The previous
      `HierarchySegment` copy per scope construction is elided as a consequence; the LLVM path
      treats `MoveExpr` as pass-through and defers transfer to LIR liveness.

- [ ] R52 -- Collapse the closure environment and the activation frame onto MIR's value/reference
      spine. The design round this entry once held is resolved: the contract is
      `../architecture/compiler_generated_storage.md` and the trade-offs are
      `../decisions/closure-environment-and-activation-frame.md`. The resolution: a closure and a
      promoted scope are two distinct nominal categories sharing only the field substrate -- the
      closure is an anonymous concrete callable value (`ClosureType`: capture fields plus one invoke
      body), the promoted scope a named aggregate reached through `Shared<>` (`StructType`: fields,
      no invoke); the callable value additionally has an erased `Callable<Sig>` level reached
      through an explicit erasure, introduced only with a heterogeneous consumer. Neither category
      is a `mir::Class` -- `mir::Class` stays the one object IR, and the frame is a plain
      `StructType`, not a `mir::Class` specialization. The value/reference spine is the top split --
      a single root over value records and reference storage is explicitly not built. Staged cuts,
      each its own review:
  - [x] R52a -- First-class callable value type. A closure's MIR type stops being its call result
        type. This cut first introduced a signature-only callable type as that first-class type;
        R52b corrects the direction -- the concrete per-site record is the callable value and is
        directly callable, so the signature-only type is removed there, and the erased
        `Callable<Sig>` with its explicit erasure returns only when a heterogeneous consumer
        requires it. Realizes the first-class callable type `../decisions/unified-callable-model.md`
        committed to.

  - [x] R52b -- The closure is a per-closure-site nominal value record. The closure-only
        capture-storage IR (`CaptureId` / `CaptureRef` / capture-init) and the interim value-tuple
        environment both retire; a captured read is ordinary field access over the closure receiver
        by a stable field id keyed from the binding origin, so the invoke body is never rebuilt when
        the physical field layout is canonicalized. All field access -- object member and closure
        capture alike -- routes through one field-access resolution, so no per-receiver-kind branch
        is scattered across consumers. The record is directly callable: a call resolves its
        signature from the record's invoke, and the signature-only callable type an earlier cut
        introduced is removed here as dead surface (it had a producer but no consumer). The invoke
        receiver is a read-only borrow of the record -- value / move semantics and receiver
        mutability are independent axes, so read-only is this cut's invoke contract, not a permanent
        property of the category. A closure value's field initializers are pure reads of
        already-materialized captures, so a side-effecting capture source is sequenced before
        construction and evaluation order stays independent of layout order. The C++ backend
        realizes the record as a lambda whose captures are derived solely from the record's fields
        and layout; the runtime's coroutine and callable consumer shapes are backend realization,
        not part of the closure model. `binding_and_capture.md` identity and forwarding are
        unchanged. This cut reuses the existing field vocabulary; renaming it and extracting the
        shared substrate is deferred to R52c. Supersedes the earlier value-`TupleType` form of this
        cut (an intermediate state now being migrated to the record).

  - [x] R52c -- The activation frame splits out of `mir::Class`, and closure and scope are pinned as
        two distinct nominal categories. The promotion box stops being a baseless `mir::Class` and
        becomes a plain `StructType` (fields, no invoke) reached through `Shared<>`; the closure is
        a separate `ClosureType` (capture fields plus one invoke body). Neither is a `mir::Class` --
        `mir::Class` stays the one object IR (`object_model.md` invariant 1), and the two categories
        share only the field substrate, not a class base. This is where the narrow shared
        field-storage substrate (field declaration, field id, field access, field init) is extracted
        for the object, the frame, and the closure together, and the field-access vocabulary is
        renamed from member to field -- done here because only now are all three field-bearing
        categories concrete, so the abstraction is validated rather than imagined. A scope struct's
        emission nesting is an explicit list on the nesting class (`mir::Class.structs`), iterated
        mechanically by a nesting backend.

  - [ ] R52d -- Generalize the HIR-to-MIR capture / lifetime policy -- gated on a new escaping
        construct, not doable now. The three capture forms (snapshot value, live-place alias
        `Ref<T>`, retained frame `Shared<StructType>` plus a slot projection) are already chosen per
        site from its source semantics, and no generic "deferred means snapshot" default exists: a
        fork-detached branch snapshots its own block-item locals and retains an escaping enclosing
        automatic through a promoted shared scope (LRM 6.21); a postponed `$strobe` aliases its live
        cells and reads them at postponed fire (LRM 21.2); a non-blocking assignment snapshots its
        right-hand value at submit (LRM 10.4.2). The one fork-shaped part is the escape / promotion
        trigger -- which automatic must be retained -- and that is correct for every construct
        lowered today: a fork-detached branch is the only legal capture that both aliases an
        enclosing automatic and can outlive its owner scope. The frontend rejects an automatic in a
        traced (`$strobe` / `$monitor`) context; a non-blocking assignment snapshots rather than
        aliases; a suspended coroutine keeps its own frame; assertions are not yet lowered.
        Generalizing the trigger beyond fork is therefore gated on a new legal construct that both
        aliases an automatic and outlives its owner scope (a coroutine-based process handle, a DPI
        callback, a class-held process) -- introduced with that construct, never ahead of one. Keep:
        capture form carried by field type, whole-scope promotion, per-site lowering, and
        copy-before-escape for snapshots.

    **Interacts with**: R47 (object model), R51 (a class with a body), R8 (callable model).

- [x] R53 -- Finish the front-end reference / sensitivity translation boundary
      (`../decisions/front-end-semantic-boundary.md`). The correctness repairs and the move to route
      cross-scope references by layout visibility (order-independent, from a whole-unit declaration
      pass) have landed. The boundary is realized across three staged sub-items:
  - [x] R53a -- One route translation for every reference consumer. Value read, value write, and
        change observation (including a dependency read only inside a called function) route through
        one translator keyed on the reader's elaborated position and the target symbol; each segment
        is classified by layout visibility, so enclosing, downward-child, sibling, cross-module, and
        upward-out-of-unit references all resolve the same way with no dependence on lowering order
        or cross-unit-slot dedup. The reader's elaborated position is recovered from its enclosing
        structural scope, so a read nested in a procedural block or fork branch routes from that
        scope. The frontend's resolved reference path is provenance only, not a routing authority.
  - [x] R53b -- The declaration pass covers every addressable identity. Generate, instance, and
        named procedural block identities are all registered before any body lowers, so a forward
        cross-scope reference to any of them resolves the same layout-visible route regardless of
        declaration order. A named block's head identity is its SV label (a hierarchical-reference
        head, LRM 23.9), registered directly from the elaborated scope members; the body pass grows
        no structural identity.
  - [x] R53c -- Endpoint binding. A value read, a value write, and a change observation of one
        target reach it through one bound runtime endpoint: a reference that crosses a scope
        boundary seals to a per-instance endpoint in the resolve phase and the hot path dereferences
        it, rather than re-navigating the parent chain on each access; only a target on the reader's
        own scope is a direct member. The endpoint's access protocol is bound from the target's
        semantic kind -- a variable's observable cell, a resolved net, a reference member -- carried
        by the endpoint's type, so one binding serves every consumer. A port connection reaches the
        child's port member through the same route as any hierarchical reference: an input or output
        port over its cell, a `ref` port bound once to the peer's cell through the one
        reference-store, with no port-only route species and no port-only alias path. Pairs with the
        endpoint-capability decisions (`../decisions/net-driver-resolution.md`,
        `../decisions/reference-as-data-type.md`).

- [x] R54 -- Runtime `RunDesignHost` consolidates every invariant host-boundary concern (argv
      parsing, plusarg collection, engine construction, `BindDesign`, exception mapping). The
      emitted host is a design-supplied `$root` builder plus a one-line `main` hand-off, so new
      host-boundary concepts (seed CLI flags, a VCD sink path, a simulation deadline, verbosity,
      signal handling) grow runtime C++, not the emitter's string surface. The builder body itself
      remains the C++ backend's allocation form (composed by the emitter from the root class name
      and hierarchy-segment type rather than rendered from a single MIR/LIR root-construct
      artifact); unifying it with the JIT's allocation shape waits for instance-representation
      unification.

- [x] R55 -- A receiver-less associated callable names its own owner. An instance method's call
      target recovers its declaring class from its receiver; a type-associated callable -- a DPI-C
      import today, an SV class static method later -- has no receiver, so a bare slot id left the
      owner implicit, meaning the caller's own class. A call to a callable declared in an enclosing
      scope therefore had to be rejected rather than resolved against the wrong namespace. The call
      target now names the owning class and the slot, so an import declared at module scope is
      callable from a generate block. The declaring scope's depth is a compile-time lookup distance,
      resolved once during lowering and absent from the target; nothing is reached at run time,
      because the callable has no body and no receiver. Declarations stay in the declaring type's
      associated namespace (`../decisions/dpi-foreign-boundary.md`,
      `../architecture/object_model.md` invariants 7 and 8); only the target identity gained what it
      was missing, and that identity survives the later collapse of the method and static-callable
      identity spaces into one callable identity (R8).

- [ ] R56 -- A foreign symbol has no single record. An external callable's symbol -- its linkage
      name and purity -- is copied into every declaring type's associated namespace, so one
      link-time global can hold several declarations in one compilation unit and emit one
      `extern "C"` prototype per declaration. The frontend guarantees the copies agree (two imports
      of one C identifier with mismatching signatures are rejected), so the emitted code is correct
      today; this is redundancy, not a defect. It is not fixed by deduplicating in the render: a
      render entry is a mechanical function of one node and makes no decisions, so a redundancy the
      render must hide is a redundancy in MIR (`../architecture/backend_contract.md`). Target shape:
      the unit holds one record per foreign symbol, keyed by its linkage name, that the
      type-associated declarations reference -- the single source of truth a generated ABI header
      and the JIT's external-symbol resolution both read. Deferred until one of those consumers
      exists, so the record is built for a real reader rather than to hide duplicate prototypes.
      **Blocker**: none.

- [ ] R56 -- A condition is a value; reducing it to a control predicate is an explicit conversion,
      not a backend's contextual one. MIR already owns the primitive: `BoolCastExpr` reduces any
      operand to a host bool, and a real or chandle `!` lowers through it. But `IfStmt`,
      `WhileStmt`, the loop forms, and `ConditionalExpr` all carry a raw SV-typed condition, and the
      C++ backend recovers the predicate by handing that value to C++'s contextual conversion -- a
      passthrough render that works only because every value type happens to expose
      `explicit operator bool`. The semantic fact "this value is read as a predicate here" is
      therefore stated nowhere in MIR; each backend rediscovers it from the operand's type at the
      branch site, which is exactly the re-derivation `mir.md` invariant 10 forbids, and an LLVM
      backend must synthesize a different test per type (`icmp ne ptr null` for a chandle, a runtime
      call for a `PackedArray`) with no MIR node telling it to. The asymmetry is visible today: `!h`
      on a chandle carries an explicit `BoolCastExpr` while `if (h)` does not, for no reason other
      than that `!`'s result must be re-shaped to a 1-bit integral. Target: every condition context
      carries its condition already reduced through `BoolCastExpr` at HIR-to-MIR, and the backend's
      condition-rendering passthrough disappears -- render emits the cast like any other node. No
      new node kind is needed; the cut is to make the existing primitive mandatory where a predicate
      is consumed, and to settle each value type's predicate semantics against LRM Table 11-1 while
      doing so. **Blocker**: none.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`control-flow.md`,
  `operators.md`, etc.).
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
- The pre-reset surface re-implementation backlog. That lives in `architecture-reset.md`.
