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

  - [ ] R8b -- Parameter direction normalizes to data flow. `output` / `inout` formals stop being
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

  - [ ] R8c -- Callable code versus callable value. A closure constructs a callable value (code plus
        a bound environment); a directly-invoked named callable receives its environment from the
        caller. `self` is the code's first parameter, bound into a value's environment when needed,
        not a privileged `captures[0]` slot.

  - [ ] R8d -- `mir::Process` dissolves into a callable value registered at constructor time
        (per-instance, generate-dependent), with `initial` and `final` as distinct lifecycle
        registrations. `ProcessKind` is removed.

  - [ ] R8e -- External callables and virtual dispatch, gated on the object-model design (R47): a
        DPI import is a bodyless external callable; a virtual call is an explicit call form resolved
        against the object's vtable layout.

  - [x] R8f -- The scope's callables render through one backend method path. A process body and the
        synthesized resolve / initialize lifecycle bodies join functions and tasks as one
        `mir::MethodDecl` carrying a `MethodForm` -- a static function over an explicit `self`, or a
        virtual instance method that overrides a runtime-base slot and reaches the scope through the
        implicit receiver. The per-shape backend renderers collapse into one that reads the method's
        fields (result type, name, parameters, body): `void` and the coroutine type are ordinary
        result types, `co_return` is a body statement rather than a render-time epilogue, and the
        receiver is spelled from the form. `mir::Process` now carries only its activation kind and
        the body method; the two remaining backend-synthesized remnants -- process activation
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
      propagation of the frame field name continues until R18 dissolves the walk frame.

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
      member-access model.

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

- [ ] R39 -- Lift the implicit `Ref<T>` access into explicit MIR calls. R12 / the
      value-type-concepts cut lifted observable-cell reads / writes / mutations into explicit
      `BuiltinFnCallee` (`kGet` / `kSet` / `kMutate`) calls at HIR-to-MIR so the backend renders
      them mechanically. The sibling axis -- a procedural `ref` / `const ref` formal (LRM 13.5.2),
      whose binding kind is `ParamDirection::kRef` / `kConstRef` and whose MIR type is `RefType{T}`
      -- is still implicit: a read of such a local is silently rendered with a `.Get()` suffix, and
      a whole-cell write through one conjures an engine handle argument the MIR call does not carry.
      Both are `mir.md` invariant 10 violations of the same shape the observable axis just resolved.
      Apply the same lift: HIR-to-MIR emits `BuiltinFnCallee{kRefGet}` / `BuiltinFnCallee{kRefSet}`
      whose argument vector is complete (services included). Compound / partial writes through a ref
      formal become well-defined in the same step (today the render path bails). See
      `decisions/value-type-concepts.md` for the pattern this generalizes.

- [ ] R40 -- Retire the construct- / control-stmt shapes that the backend completes from outside
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
      member, a bare borrowed-pointer slot, or a `Call(kAsObservable, ...)` for an upward reference.
      The render-side type-kind dispatch is gone. Address-of itself was lifted to MIR via the new
      primitive (see `decisions/address-of-primitive.md`).

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

- [ ] R45 -- Unify MIR's call shape. Today `mir::Callee` is a 7-arm `std::variant`
      (`SystemSubroutineCallee` / `MethodRef` / `BuiltinFnCallee` / `BuiltinStaticCallee` /
      `FreeFnCallee` / `ClosureRef` / `ConstructorCallee`); four of those arms (System / Method /
      Builtin / BuiltinStatic / FreeFn -- 5 actually) are sub-categorizations of the same structural
      concept "a direct call to a named symbol", differing only in C++ rendering convention. The
      `mir.md` identity ("a generic programming-language IR") admits three call shapes: direct
      (named symbol), indirect (computed callable, currently `ClosureRef`), and type-driven
      (constructor of the result type, currently `ConstructorCallee`). The 5-way splintering is SV
      organizational scheme leaking into MIR -- a real architectural mismatch. Target:
      `Callee = variant<Direct{symbol}, Indirect{expr},     Constructor{}>`; per-symbol metadata
      (instance-method-form, namespace path, qualifier) moves into a backend-side table extending
      today's `BuiltinFnCppName`. Each backend reads the symbol id and consults its own table --
      LLVM doesn't consult at all, since `call     @symbol(args)` has no method/free distinction.
      **Blocker**: R37 still in flight retiring `SystemSubroutineCallee`; do that first so the
      call-shape unification sees the final set of named-symbol callees.

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

- [ ] R47 -- Design the object model that SV classes need, distinct from the module / scope object.
      The MIR concept currently named a "class" is a compiled module / scope, carrying
      module-specific structure (an elaboration and construction graph, owned children, generated
      members, lifecycle registrations). An SV class (LRM 8) needs heap allocation, handles and
      null, inheritance, dynamic dispatch, object identity, and a reference-lifetime policy. The
      likely factoring is a shared object type (fields, methods, and a virtual / dispatch layout
      where applicable) plus a separate module-specific instance / elaboration plan -- "a module is
      an object type plus an instance plan," not "an SV class is a module in another mode" -- so
      module construction policy does not contaminate heap classes. **Design first**: this is the
      gating prerequisite for SV classes and for the virtual-dispatch facet of R8
      (`../decisions/unified-callable-model.md`). No implementation until the object model is
      designed.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`control-flow.md`,
  `operators.md`, etc.).
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
- The pre-reset surface re-implementation backlog. That lives in `architecture-reset.md`.
