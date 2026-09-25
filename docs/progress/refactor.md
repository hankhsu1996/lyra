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
      legal `@(string)` / `@(real)` / `@(enum)`), and the input-port-connection path admits any
      value type (the driver rides the generic continuous-assign path). An edge carries no
      restriction of its own either: binding admits only an integral operand, and every integral
      value is one packed vector at run time, which is what classifying an edge needs.
      `hir::Type::IsValueChangeObservable` is the single HIR-level predicate the value-change gate
      uses. A value-type x construct coverage matrix backs it (`@`, `wait`, `always_comb`, `@*`,
      continuous assignment, input port over string / real / enum / unpacked).

- [x] R3 -- Collapse the runtime's dual hierarchy into a single object tree. The mirrored
      `RuntimeScope` tree and the bind-time `RuntimeBindContext` are gone; there is now one runtime
      `scope` base (named after the LRM VPI object model, with `instance` and `gen scope`
      specializations) carrying name, parent, services, the process list, and the observed-region
      machinery. Every emitted scope class extends it and stays thin; the scheduler walks the real
      object tree through the base. The kind is carried by the specialization type, not a side enum.
      Identity is fixed at construction; bind only wires services, creates processes, and recurses.
      `services_` lives once, generate-scope classes are no longer bare, and a deferred check (and
      combinational settle) inside a nested instance or a generate block now works. **Follow-up**:
      the `instance` / `gen scope` layer is empty today and exists to mark the divergence; it is
      populated when Stage E (ports) lands -- ports live on `instance`, never on `gen scope`.

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

- [ ] R8 -- A closure's code is not a callable declaration. Every other body a class owns -- a
      process, a lifecycle phase, a subroutine, an imported foreign symbol -- is one declaration
      kind reached by one identity, so a consumer asking "what does this class call" has one answer.
      A closure is the exception: its code is carried inside an expression, so the same body reached
      through a closure and through a call is two shapes to a backend, a dumper, and anything that
      walks a class's callables. The target of the unified model is a callable _value_ being code
      plus a bound environment, where the code half is the same declaration everything else uses.
      Rebasing it touches lowering, MIR, the dumper, and the backend, so it lands in staged cuts,
      each its own focused review:
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

- [x] R12 -- Whether storage is observable is a fact of its type, not a render-time decision. The
      observable-storage wrapper is a first-class MIR type (sibling to the owning-pointer and vector
      wrappers), so a signal field's type is the wrapper and the backend maps that type straight to
      its runtime cell without ever asking "is this observable" at an access. The value/storage
      duality -- a signal is read as a value, while its cell is what a reference binds to -- is
      carried by the type and by whether an access reaches through the cell, never by a render-time
      branch. The uniform wrapping this entry set up is what makes R2's residual small -- only two
      over-broad frontend event-source gates remain.

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

- [x] R14 -- The `return` / `co_return` choice is stated by MIR, not re-decided in the backend. Both
      describe the same operation (exit the callable), and which one a return is depends only on
      whether its enclosing callable completes as a coroutine. That is the callable's call protocol,
      carried by its result type, so every backend reads it from there; no return statement,
      closure, or lowering frame restates it as a flag of its own.

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
      capture is name-explicit, the clause never contains `[this]`, `[=]`, or `[&]`. The body that
      creates a scope's processes registers each one over the receiver it was handed. The
      receiver-related `RenderContext` machinery disappears in lockstep. See
      `docs/decisions/callable-receiver.md`.

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
      as not fitting the generic `(receiver).name(args)` member-call rule are both settled. An
      enumerated type's methods (`first` / `last` / `num`, no receiver) are answered from the
      enumeration's own declared members, so the call the source wrote reaches a question put to the
      member list the unit states for that enumeration. `$isunknown` needs no special
      type-associated or constant-fold path: it is the generic instance built-in call
      `(x).IsUnknown()` returning the SV `bit` type (1-bit `PackedArray`, LRM 20.9), now wired end
      to end -- recognized at AST-to-HIR by `KnownSystemName::IsUnknown`, lowered through the
      context-free call family in both procedural and continuous-assign positions
      (`../decisions/context-free-call-lowering.md`). The cross-cutting value-model (SV-typed
      runtime signatures, the representation bridge inside the method body, the backend reading the
      stated result type) is settled.

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
      shared between HIR and MIR, replacing the per-family variant. The receiver's MIR type drives
      backend calling-convention mechanically; SV-side `$isunknown` returns the SV `bit` type so no
      host-bool lift survives at the backend. See `decisions/builtin-call-identity.md`.

- [x] R30 -- **Runtime effects as generic calls: the closure-bearing subset** (carve-out of R20,
      same decision). The `$strobe` family and the synthesized non-blocking-assignment and
      deferred-assertion submits each lower to a generic `CallExpr` over a compiler-synthesized
      closure built through the one closure builder (R31).

- [x] R27 -- **Associative-array traversal output write-back.** `first` / `last` / `next` / `prev`
      (LRM 7.9.4 -- 7.9.7) lower to a block expression that runs a pure (engine-free) query and
      commits the index it answers with through an ordinary observable assignment -- so the LRM 4.3
      update event fires in the assignment, not the query. The traversal query is a plain container
      member rendered through the generic member-call rule; the backend fabricates no engine handle
      and no reference wrapper for traversal. Closes R25's traversal carve-out (traversal fits the
      generic member rule).

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
      rule now lives as an invariant in `lowering_organization.md` (Node-Builder Helpers), so it
      outlives this entry.

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
    - [x] `$sscanf` / `$fscanf`: a pure value-layer scan plus `files`-side `PeekBuffered` /
          `AdvanceFd` primitives for the file form. The MIR shape of the two SV system tasks differs
          by primitive composition, not by an enum tag on a unified call.
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

- [x] R39 -- An access through a `ref` reuses the observable cell's model rather than a parallel
      one. A `ref` / `const ref` formal (LRM 13.5.2) and a by-reference capture (LRM 6.21) carry a
      reference type, and reaching what that reference stands for is the same access as reaching
      what an observable cell stands for -- only the type differs, and the type is what each backend
      maps to its own realization. The root cause was one typing asymmetry: a reference to a
      reference binding was typed with the unwrapped value, so the general access path skipped it
      and the render had to re-derive ref-ness from the slot type. Typing the reference with its own
      type makes the general path catch it; whole writes, compound and partial writes, and increment
      / decrement through a ref are now all well-defined, where the render previously bailed on the
      last two.

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
      regular `StringLiteral` argument and the index list as an element list. The `kGetSignal` cast
      is lifted to a MIR cast whose destination type is the call site's slot type, so the backend
      emits the conversion mechanically from a stated MIR fact. The `kGetChild` index conversion
      moved into the runtime (which now accepts `std::span<const value::PackedArray>` and calls
      `.ToInt64()` itself), so the render side has no `.ToInt64()` injection and no
      `std::array{...}` wrapper. Every call -- regardless of callee variant -- now renders as
      `fn(rendered_args...)`.

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
      `Callee = variant<Direct, Indirect, Construct>`, where a direct call carries the symbol
      identity -- which `mechanical-translation.md` T11 unifies into one space -- and the object it
      dispatches on, where it has one. `MethodRef`'s `hops` field retires -- the receiver becomes an
      explicit expression the call carries, and its type pins the enclosing class whose arena names
      the callable. Render mode follows the callee: a receiver drives the instance form, and a
      callee that binds no object is spelled from its own declaration, which is per-id backend
      metadata with no MIR-level meaning. The `decisions/builtin-call-identity.md` paragraph that
      justified the instance / static / free split as "structural at MIR" for backend convenience is
      rewritten -- the split was an invariant-10 violation, not a structural fact. Reserves the seat
      for `Virtual` (R8e) without inventing it now: gated on R47, a future
      `Virtual { slot, static_receiver_type }` arm slots in as an additional `Callee` arm with no
      change to the others.

- [x] R46 -- A cast below the front end is one node stating two types: what the value comes from and
      what it is read as. Every conversion that reshapes a _value_ -- integral resize, real <->
      integral, packed <-> string -- is a library call and not a cast at all. HIR keeps SV's own
      conversion vocabulary, which is where an overloaded source conversion is resolved, so what
      reaches MIR is one concrete type pair. Settled by `decisions/cast-is-a-pair-of-types.md`,
      which reversed the several-primitives shape this entry first landed and says how a backend
      refuses a pair it cannot realize.

- [x] R83 -- Naming a capability wrapper's storage is place formation, not a call. A bare wrapper
      place denotes the wrapper and a dereference of it denotes the storage it represents, so a
      write that descends into a part starts from that dereference and a by-reference lending lends
      that storage -- while rebinding a reference stays a store into the bare place, structurally
      distinct from writing through it where before only the choice of lowering path told them
      apart. Reading what a wrapper holds and replacing the whole of it act on the wrapper rather
      than naming its storage, so each stays an ordinary call. The mutation proxy a partial write
      interposed is gone, and with it the execution backend's pattern-match that recovered the
      destination it stood for. Each backend now supplies the access protocol from the place's type
      through one dispatch. The store no longer carries a runtime handle, which removes the handle
      from the deferred-assignment closure, from the package initializer, and from the cell store
      the execution ABI exposes.

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
        categories concrete, so the abstraction is validated rather than imagined. A scope struct is
        reached by the one name its identity carries in the unit, and a backend emits the unit's
        structs by iterating its registry.

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

- [x] R54 -- One runtime entry consolidates every invariant host-boundary concern (argv parsing,
      plusarg collection, engine construction, binding the design, exception mapping), so a new
      host-boundary concept (seed CLI flags, a waveform sink, a simulation deadline, verbosity,
      signal handling) grows runtime C++ rather than the emitter's string surface. A design's whole
      contribution to the emitted host is two names: the entry its root unit publishes for making an
      object, and the label that root carries. Nothing about the allocation is composed by the
      emitter any more -- the root is asked for the way every referrer asks for an instance -- so
      what is left of the shell is a one-line hand-off, and what still keeps it from being ordinary
      generated behaviour is tracked in `emit-readability.md`.

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
      and the JIT's external-symbol resolution both read. That first reader now exists: the ABI
      header collapses the copies by linkage name as it collects them, which is correct but is the
      consumer doing what the record should have done. **Blocker**: none.

- [x] R57 -- A condition is a value, and reducing it to a control predicate is an explicit
      conversion rather than a backend's contextual one. Every condition context -- `if`, the loop
      forms, the conditional expression -- carries its condition already reduced to a machine
      boolean at HIR-to-MIR, so a backend emits the reduction from a stated node instead of handing
      the value to whatever its target language does with it contextually. That passthrough worked
      only because every value type happened to expose a boolean conversion, and left the execution
      backend to synthesize a different test per type with nothing telling it to. Each value type's
      predicate semantics is settled against LRM Table 11-1.

- [ ] R58 -- A design unit's definition reference has two realizations. Semantic modeling already
      states it once: a unit's definition is a class-level constant, and the constructor hands the
      base its address, so a unit installs its own definition. The C++ backend renders exactly that.
      The execution backend does not read it: it drops the constructor's base initialization, and
      the code generator instead re-derives a definition symbol from the child's type at every
      construction site, so a child unit's definition is supplied by its caller rather than by the
      unit itself. The two backends therefore disagree on who owns a definition reference, and the
      same concept is realized once in semantic modeling and once, independently, in the code
      generator. **Target shape**: one realization -- the unit installs its own definition, both
      backends render that, and cross-unit construction names a unit rather than its definition.
      **Blocker**: none identified, but it moves the definition out of the construction ABI, which
      touches how a unit definition is published and filled; it warrants its own review rather than
      riding along with a value-domain or storage cut.

- [ ] R59 -- An enum is a nominal type, the way a struct or a class is. Every other named type
      carries its name on a nominal declaration reached from the type by id -- a struct's name is
      `GetStruct(struct_id).name`, a class's is `GetClass(class_id).name` -- so the backend renders
      both through one uniform lookup. The enum is the lone exception: it is a structurally-interned
      value type (`EnumType{base, members}`) with no nominal identity and no name. Because it is
      interned by structure, two identical enum declarations collapse to one type -- contra LRM
      6.19, where each enum declaration is a distinct type -- so no one source name can live on it.
      The emitted name is instead a unit-level side lookup keyed by the enum's `TypeId`
      (`nominal_type_names`), populated by an enum-only branch in the HIR-to-MIR typedef pass and
      consumed by a dedicated enum-name renderer rather than the uniform struct / class path. That
      side table is not a stray smell to patch: it is the correct consequence of structural
      interning -- one shared interned enum backs several typedefs, so it has no single intrinsic
      name. Target shape: an enum is interned by declaration identity, its name and member set live
      on a nominal enum declaration reached from the type by id, and the type mapping resolves an
      enum's name the same way it resolves a struct's or a class's; the unit side table, the
      enum-only typedef branch, and the dedicated enum-name renderer all retire, and distinct enum
      declarations become distinct types. The enum's value representation -- its base packed shape
      -- stays intrinsic to the value type (an enum value is a packed integer, not a reference like
      a struct / class instance), so the integral-packed value helpers that read an enum's base
      without the unit are untouched; this cut is nominal identity and name, not moving the value
      representation onto the declaration. **Blocker**: none unlanded, but it is a cross-layer
      type-interning change -- HIR and MIR both intern enums structurally today, and the source name
      is currently captured only after canonicalization has stripped the naming typedef, so the name
      must be resolved at declaration time -- large and cross-cutting enough to warrant its own
      focused review.

- [ ] R60 -- The C++ backend's value-emission entries name runtime library identifiers directly and
      inject prologue statements the MIR does not state. `../architecture/backend_contract.md`
      confines every runtime library type literal to the type-mapping dispatch and confines every
      runtime library identifier (wrapper type, helper function, helper struct, method spelling) to
      "MIR types and MIR calls that map onto them". The current backend violates this in three
      related ways -- render composes the identifier as a string, render fabricates the operation
      the identifier realizes, and render injects a receiver-recovery or initialization statement
      that the callable body does not carry. The pattern shows up in enough render entries that a
      fix is not one edit, but the shape is uniform: the runtime library form of every MIR primitive
      is stated in one central mapping (the peer of the type-mapping dispatch, extended to value
      forms), and every prologue statement the wrapper needs sits in the MIR body as an ordinary
      statement the render walks. Concrete sites today:
  - Class construction of a managed reference renders as inline `lyra::runtime::GcNew<...>` from a
    Construct call whose result type is `ManagedRefType`.
  - A coroutine closure's captures reach its body as an argument list the render site composes,
    because a capturing coroutine lambda would dangle once a spawned branch outlives the
    construction site. The MIR node states those captures as fields, so the render site converts one
    into the other and the two closure kinds stop looking alike: a synchronous closure is a value
    that is invoked, a coroutine one is a value that already is its coroutine. Construct-then-enter
    is what the execution backend does with both, so the divergence is this render site's alone: a
    capturing coroutine lambda would dangle in the target language, which is why the captures become
    an argument list here.
  - The instance-method render injects `Self self = this;` before the body -- another prologue the
    MIR body does not state, so every method-form callable's `self` binding is a render-side
    convention rather than a stated MIR fact.
  - A value-emission entry composes its output as a format string carrying the callee, the
    punctuation, and the argument grouping together, so how many arguments a call has and which
    brace group a type owns are decided per site. The grouping half of that has already produced a
    target-language ambiguity that reached the emitted text. The syntactic wrappers a value-emission
    entry is meant to compose are now available as combinators over rendered parts; the sites that
    still hand-write the punctuation should route through them, which also leaves the callee as the
    one thing a value-form dispatch has to supply.
  - The compilation unit's enum types render through hand-composed
    `class E final : public lyra::value::Enum<E> { ... };` in the header, with the runtime library
    type spelled directly and the class boilerplate structured only by the render text. The enum
    declaration form belongs behind the type-mapping dispatch or as its own emission concept, not a
    hand-written template. **Target shape**: one mapping per axis. A runtime library type is named
    exactly once, in type mapping (already the shape today). A runtime library operation is named
    exactly once, in a peer value-form dispatch a value-emission entry looks up by MIR primitive
    kind. A prologue statement -- receiver recovery, self binding -- is a MIR statement in the
    callable body a HIR-to-MIR lowering emits, so render walks it like any other statement.
    **Blocker**: none. The value-form dispatch exists and a concatenation and a replication reach
    the target through it, so each site left is an addition to a table that is already there rather
    than a new backend surface.

- [x] R61 -- A value-aggregate interior write reached MIR as a write onto a nested lvalue
      expression, and each backend recovered the owner and the selectors by walking that expression
      and consulting each receiver's type. Two backends deriving one semantic fact is the shape
      `../architecture/mir.md`'s Forbidden Shapes name, and it made every new container family
      arrive as another per-type branch. A write target is now built rooted where the write lands,
      as one call per level of the descent, each naming the entry the lowering settled from the type
      that level descends into. Each backend is a fixed function of those calls: the C++ backend
      reaches the part in place, the execution backend reads the whole value and rebuilds it,
      because that is what its representation allows. The nested write encoding, the target
      decomposition, and its per-type predicate are gone. Every interior write -- struct component,
      union member, packed slice, packed or unpacked element, string character -- and every
      increment through one takes the same path on both backends. The model is recorded in
      `../decisions/value-projection-write.md`, and how it is stated in
      `../decisions/value-descent-as-named-calls.md`.
  - [ ] A `ref` / `output` / `inout` actual bound to an interior is rejected on the execution
        backend today. Landing it needs a reference into a value to exist as a runtime value on both
        backends, able to cross a suspension. A nonblocking assignment into an interior is not one
        of them and already runs on both: what it captures is a reference to the owner, not to the
        part, and the descent is restated over that capture with its coordinates snapshotted, which
        is what LRM 10.4.2 asks for and needs no reference into the value at all.
  - [ ] The queue and associative-array interior writes connect to the same path once those value
        domains are realized on the execution backend.

- [x] R69 -- An evaluation of several steps standing where only an expression may was a closure
      invoked where it was built. That is what a language without the construct is forced into, and
      it is a substitute: it manufactures a callable value nobody holds, spends a function boundary
      on something that is not a function, and -- because a closure carries coroutine-ness in its
      result type -- makes every sequencing site structurally indistinguishable from a body that may
      suspend. The boundary also silently reinterprets a return among the steps, the defect
      `../decisions/foreach-lowering.md` had already met from the other direction. MIR now has the
      construct: a block expression, sequencing and nothing else, whose steps do not return. Every
      site that sequenced -- a call writing back to its actuals, the scan family, the plusargs and
      randomize families, the foreign-import boundary, associative-array traversal -- builds one,
      and the closure is left to the bodies that genuinely escape. The reasoning, and why one node
      reaches further than making each control construct value-producing, is in
      `../decisions/block-expression.md`.

- [ ] R62 -- Diagnostics reaches its source files through a hand-rolled pool. The source manager
      keeps its files in a plain sequence, mints a file identity from that sequence's running count,
      and reserves zero to mean "no file", so every reader tests the identity for the reserved value
      and the lookup answers with a pointer that may be null. Two different facts are fused into one
      integer: which file, and whether there is one. An identity a pool confers should answer only
      the first; whether a source span has a file is the span's own question and belongs in an
      optional. Fusing them leaves the identity type unable to state what it is, moves the check to
      every read, and leaves the pool's bounds hand-written -- the unnamed-pool shape
      `../architecture/lowering_organization.md` names under Pool Selection. Target: the files are a
      pool like any other, minting the identity and answering totally, and a source span carries an
      optional file identity so a span with none says so. No prerequisite blocks it, but the span
      type is read across most of the compiler and pinned by every case that asserts a diagnostic's
      text, so the optionality change is broad enough to warrant its own focused review.

- [ ] R63 -- A function's basic blocks are a bare sequence indexed by a typed identity, so the
      identity's bounds are unchecked. The reason was real: a block used to be filled in place after
      it was appended, which the append-only pool contract forbids. That reason is gone -- the pass
      that builds a function now holds its open blocks in its own shape, where a block with no
      decided exit is representable, and appends only finished blocks, in order. What remains is a
      typed identity indexing a plain vector, which is the shape Pool Selection rules out. Target: a
      function's blocks are a pool, and the building pass takes block identities from a typed
      allocator the way the declaration stages take identities before their pool exists.

- [ ] R64 -- Two counters confer one callable identity. The stage that takes identities before any
      declaration settles mints a class's callable identities from a typed allocator, whose stated
      contract is that it is the authority for that id space and nothing downstream re-derives it.
      The class's own callable pool then mints the same range again, one slot per signature. The two
      agree because both count from zero over the same sequence, and nothing checks that they do: a
      callable synthesized between the two points would shift one sequence and not the other,
      silently. Correct today, but by arrangement rather than by fact. Target: the pool that will
      own the identities is the one that confers them -- created where the identities are taken and
      carried into the class, so the later stage adopts a pool instead of re-minting its range.

- [ ] R65 -- The terminator for a block control never reaches has no producer in practice. Its only
      source is the step that closes blocks the lowering left open, which selects it for a
      value-returning body; across the whole test corpus it is never produced. Either the case
      cannot arise -- because every block of a value-returning body is already closed by the time
      that step runs, which would make the selection dead and the close unconditionally an implicit
      return -- or it can arise and nothing exercises it. Both are answers; neither is established.
      An IR node with no producer is a claim about the language nobody has checked. Target: settle
      the derivation, then either drop the terminator and the selection, or add the case that
      produces it.

- [ ] R66 -- "Sees every unit" is spelled as a plain sequence. A per-unit lowering takes one unit
      and nothing else, which is what lets units lower independently; above it, the design-root
      assembly, the host-main emission, and the foreign-boundary header each take a bare sequence of
      units. Per-unit independence is the property incremental and parallel compilation rest on, and
      today it holds because nobody has passed the sequence downward -- a convention, not a fact the
      types carry. Target: the whole-program view is a named type, so work that reads the whole
      program is identifiable by signature and a per-unit pass cannot acquire that reach by a
      parameter change nobody notices.

- [x] R67 -- A program-global cell is storage the execution session owns and hands out by name,
      reached by the same load and store any other cell is. A package or `$unit` variable is named
      by its linkage symbol rather than through a scope, so its storage hangs under no scope and the
      member-storage path every other variable takes does not describe it; a place now opens at that
      symbol and dereferences it. A class's statics are the same shape and still refuse, but on the
      publishing side rather than here -- nothing mints the symbol -- which `execution-backend.md`
      carries.

- [ ] R68 -- MIR's homogeneous sequence vocabulary has no producer. `VectorType`, the sequence value
      that builds it, and the projection that reads one element are declared, translated to LIR, and
      handled by both backends, while nothing in any lowering produces one. A node no pass emits is
      a claim about the language that nothing tests: every arm handling it is unexercised, and the
      first producer would discover whether those arms were ever right. Target: settle whether a
      homogeneous sequence is a value MIR needs -- the associative literal its type doc names as the
      motivating use is built another way today -- and either give it the producer that use implies
      or drop the vocabulary and the arms that carry it.

- [ ] R70 -- MIR-to-LIR's expression lowering claims every expression yields a value. An operand is
      an instruction's input, and a void-typed expression has none to give, so the entries that meet
      one improvise: a void builtin call and a void await each hand back an operand belonging to
      something else, which is a value nothing put there and no consumer should read. Void is an
      ordinary type of the language above -- a call to a subroutine that returns nothing is one --
      so the mis-statement is here: this layer restates MIR in a machine model's vocabulary, and in
      that vocabulary a computation that produces no value is ordinary rather than exceptional.
      Target: let expression lowering answer "no value" outright, so the improvised operands go. Not
      an operand alternative -- an operand is an input, and "no value" never is one; every consumer
      would grow an arm for something it can never receive.

- [ ] R71 -- MIR's block fuses an expression arena with a statement sequence, and only the sequence
      half is a block. A callable's bindings are pooled once for the whole callable, because a local
      declared in a nested sequence is still that callable's local; its expressions are pooled per
      sequence instead, for no matching reason. Two consequences follow. An expression identity is
      meaningful only beside the sequence that minted it, so every id numbers from zero in every
      sequence and reading one against the wrong sequence yields a valid but different expression --
      a whole class of silent mistake that a callable-wide identity cannot spell. And which sequence
      an expression will belong to has to be settled before the first operand is lowered, because an
      expression cannot move afterwards, which is why a lowering that may or may not need a sequence
      has to predict that from the declaration rather than discover it while building. The fusion
      shows plainly where a class-level constant, which is an expression tree with no callable and
      no statements at all, still has to be carried as a sequence: its producer reaches past it to
      the arena, and its consumer borrows an unrelated callable to read it back. Target: pool
      expressions where bindings are already pooled, give the constant its own tree, and leave the
      sequence holding statements alone -- at which point the name it already has is the right one.
      Wide: every lowering that writes into a sequence, both backends, and the dump.

- [ ] R72 -- A switch over a closed set is checked for completeness only where nobody wrote a
      catch-all. `-Werror=switch` already compiles Lyra's own targets and is a full completeness
      gate, but the compiler stops applying it the moment the switch carries a `default:`. So
      whether gaining an alternative breaks the build is settled per site by whether someone reached
      for a catch-all while writing it, and nothing reports which sites made which choice. A
      catch-all also leaves the one arm that cannot say two things: a refusal covering several
      alternatives at once gives them all a single message, and the ones that deserved a different
      sentence read identically to the ones that did not.

      `exhaustive-alternative-consumption.md` weighed the blanket ban on a catch-all and rejected it,
      and its reason holds where it was aimed: a registry -- 242 builtins, or the format specifiers
      -- cannot answer one yes-or-no question by listing every member, so a rule firing there is
      noise, and that decision's own follow-up found the registry's real answer elsewhere (one
      declaration per entry, holding every property at once). What it did not settle is the rest. A
      dozen alternatives a lowering dispatches on is not a registry, and there a catch-all buys
      nothing the enumeration does not; the earlier objection that enumerating slang's sets makes
      every dependency upgrade break the build is not what decides it either, at roughly one upgrade
      a year.

      The flag that would do it is `-Werror=switch-enum`, and measured 2026-09-09 it fires at 27
      switch sites -- 13 over enums Lyra declares and 14 over slang's, the largest in play naming 41
      alternatives. It cannot tell a dispatch set from a registry, so it fires on exactly the sets
      the decision protected; that distinction is per-site, so it belongs where A013 already draws it
      rather than in a flag. `-Werror=switch` stays whatever shape this takes -- it owns the separate
      "case label naming no enumerator of the switched type" diagnostic, which `-Wswitch-enum` does
      not report. Nothing else substitutes: clang-tidy carries no such check (its nearest,
      `bugprone-switch-missing-default-case`, is the opposite rule and was measured to fire only on
      non-enum switches, so it does not collide), and `-Wcovered-switch-default` -- the only thing
      that flags a catch-all left on an already-complete switch -- is Clang-only and so cannot join a
      set that also compiles under the remote image's GCC.

      Target: say where a catch-all is the wrong shape in terms the policy check can decide, then
      enumerate those sites. Completeness is not the whole of it either -- whether each arm says the
      right thing is the first searchable smell in `design-process.md`, and no mechanism sees that.
      Wide: both backends, the JIT, the value layer, and six lowering families.

- [ ] R73 -- The runtime ABI is named mechanically and defined by hand. Which symbol an operation
      publishes comes from the closed sets that already spell it, so a symbol cannot be composed
      from a string; the definition behind that symbol is written out one function at a time. Of the
      688 entries the check counts, 520 lead with a value representation and differ from a sibling
      only in which one -- each a cast, a read, and one call -- so an operation added to a family
      costs a function per representation, and a representation added to the set costs one per
      operation of every family that names one. The sampled-state work added 66 in a change whose
      whole subject was three storages, and the DPI-marshaling work 21 in front of code the C++
      backend was already calling.

      The count alone is not the argument, since the functions are short and a check holds each
      entry's prototype, definition and binding to the other two, which is what makes the shape
      survivable rather than dangerous. What decides it is that the two halves of one contract are
      derived differently: one side cannot be spelled wrong and the other is retyped per member of a
      set the first side already enumerates.

      Target: a family that varies only by value representation states its body once over that set,
      so adding an operation or a representation is one edit. Nothing blocks it. The check stays
      whatever shape this takes -- it also covers the standalone entries, which no generation would
      reach.

      The net-strength work is another measurement of the same cost, taken after the one above: one
      new install operation and one new operand on an existing attach cost twenty edited or written
      functions across the four representations a net may hold, in a change whose subject was six
      net types. The number is small enough to write by hand and that is the point -- nothing about
      it is a decision, so nothing about it is review.

- [ ] R74 -- Which accesses a storage defines is spread across the sites that need the answer rather
      than stated where the storage is. A LIR type is classified into a storage kind in one place,
      which is right; but the pairs that do not exist -- a net taking a store, a driver installing a
      representation, either of them retaining what a time slot moved away from -- are guards raised
      one at a time inside the symbol minter, and there are four now where there were two. Each is a
      throw rather than something the caller could not have spelled.

      The same answer is also reached by two overlapping classifiers. One asks which capability
      wrapper a type is, for an access made through a place; the other asks what values a storage
      holds, for an entry named by the representation of what it holds. The second is the first plus
      the storages that are not capability wrappers, so a storage joining the second set has to be
      read against both to know it did not join the first by accident.

      Target: a storage says which accesses it defines, so one it does not define is unspellable
      rather than thrown on, and the question "what storage does this operand reach, and what does
      it hold" is asked once. Nothing blocks it; the two readings sit in one file today, which is
      what keeps them in step and is also why the split is easy to miss.

- [ ] R75 -- Nineteen switches over a closed set of alternatives still carry a `default:`, so for
      each of them the compiler's exhaustiveness check is off and gaining an alternative compiles
      silently. The style contract has forbidden this all along and a person had been catching it by
      hand; the count is what says that was never the mechanism, and writing the check found more
      than twice what hand-catching had. The architecture policy now fails on a new one, and carries
      the standing ones as a record that fails equally when an entry is fixed and left listed, so
      the list only ever shrinks and is the authoritative statement of what remains. Nothing blocks
      picking them up.

      The record holds fourteen entries for those nineteen switches, because it is keyed by the
      file and the set being switched on so that an entry survives the code moving. Where one file
      switches over one set more than once, the entries share a key: fixing one of them leaves the
      key satisfied by its neighbour, and only fixing all of them frees the entry to be dropped.
      That is coarser than one entry per switch and it is still monotone -- nothing new can hide
      behind a listed key, since a key is listed only where a switch already carried a `default:`.

      Target: the record empty and the rule enforced by nothing but the compiler. Most are
      mechanical -- write the arms out -- but they are not one job. A `default:` over a large
      enumeration where the switch answers for a handful of members is a design question rather
      than a transcription: the set being asked is a subset, and what states which members are in
      it should be the same place that declares them, not a switch that silently answers "no" for
      whatever is added later. Expect at least one entry to turn into its own cut.

- [x] R76 -- A SystemVerilog name reaches the emitted C++ through one total, injective map, and the
      map's plain branch is exactly the set of spellings C++ accepts as a name. A design may spell a
      declaration with any printable non-space character (LRM 5.6.1); C++ admits fewer and reserves
      some of what is left, so a name it refuses is escaped to a reserved prefix and its bytes
      rather than repaired, because repairing maps two declarations that differ onto one token.

      Four ways a name used to reach emitted text without that map, each a legal program producing
      output that does not compile, and none of them reported by anything. The plain branch tested
      whether a name was spelled out of identifier characters, which every keyword is, so eleven
      positions broke on an ordinary name -- a design element (it becomes a namespace), a variable, a
      port, a signal an interface declares, an instance, a class, a property, a method, a
      package-level subroutine, and a subroutine's arguments and locals. A package variable was
      defined without the map and referenced through it, and a package subroutine the other way
      round, so each spelled one declaration two ways across a unit boundary -- which breaks on an
      escaped name as well, and is invisible from either end alone. And a unit's own program-global
      cells were written straight out.

      Escaping is decided by what C++ refuses outright, so its keywords and the alternative
      spellings of its operators are escaped, and so are the identifiers it gives a meaning of their
      own, which cost only how a name reads. What it reserves to an implementation rather than
      refusing -- a name carrying a double underscore, or an underscore before a capital -- stays
      plain: such a name compiles, and the compiler already spells names of its own that way.

      The execution backend runs every one of these correctly, because what it links is not a C++
      identifier, so this was the C++ backend's alone. `tools/policy/check_emitted_names.py` is what
      keeps the map at one point, and it fails closed: a name an emitter reads is a violation unless
      it is an argument of the map, an argument of a call that looks something up rather than
      writing it, or admitted with the reason it is neither.

- [ ] R77 -- The value layer states every aggregate operation twice, once for each realization. A
      product, a union and a fixed-size unpacked array each exist as a monomorphized template the
      C++ backend instantiates and as a type-erased class the execution backend holds, and the two
      carry the same algorithm: recurse into the components, apply the operation, put the results
      back. Which realization a backend uses is settled (`decisions/jit-aggregate-realization.md`)
      and is not what this entry disputes; what it disputes is that the algorithm is written per
      realization rather than once over "a value made of parts".

      The cost is per operation rather than per type, which is why it grows. Adding net domination
      and a shape-preserving fill -- two operations -- cost seven implementations each: one packed,
      three monomorphized aggregates, three erased ones. Every operation the value layer has ever
      gained paid the same, and nothing about the second copy is a decision: the erased one differs
      from the template one only in reaching its parts through a variant rather than a pack.

      Target: an aggregate's per-part operations are stated once against how it reaches its parts,
      so a new operation is one implementation plus whatever a leaf type states for itself. Nothing
      blocks it. The obstacle is that the two families expose their parts differently -- an index
      sequence over a type pack on one side, a vector of erased values on the other -- so what has
      to be found first is the one surface both can answer, and that is a design question rather
      than a transcription.

- [ ] R78 -- Whether a pairwise operation requires two values to have the same shape is decided per
      operation. The runtime product checks the component counts agree in its equality and its case
      equality and does not in its net resolution or its domination; the erased array family is
      split the same way. Every one of them indexes the other value by position, so the ones that do
      not check read out of bounds where the ones that do report. The states that would reach it are
      unreachable today -- a net fixes the shape of every contribution to it, and an assignment
      fixes the shape of a comparison's operands -- so this is a shape argument rather than a bug
      report.

      Target: how a pairwise operation over parts obtains its pairs is stated once, so whether the
      counts agree is asked once rather than per operation, and the answer for a shape that cannot
      arrive is the same everywhere. Blocked by nothing, and R77 is where it naturally lands: the
      one surface that hands out the pairs is the place the question belongs.

- [x] R79 -- A declaration the source never wrote carries no name. Its identity is the position it
      sits at, and being reachable by an identifier is a relation its owner holds, which only what
      the source declared takes part in. The compiler therefore mints nothing into the design's own
      name space, and a backend answers "what is this called" by asking whether anything names it
      rather than by reading a spelling it was handed.

      Before it, a lowering joined a source name to a word of its own and the result was neither
      kind of name. `int foo_borrowed_handle;` beside a block named `foo` emitted a class with that
      member declared twice; a design element named for an enclosing one and a block inside it
      collided with the scope built for that block; and two block-local variables could reach one
      spelling, because the mechanism that kept synthesized locals apart renamed a colliding one and
      never rechecked what the rename produced. All legal SystemVerilog, no diagnostic, and emitted
      text that does not compile.

      It reached further than the count suggested. One rule crossed eight kinds of declaration --
      a field, a class, a gathered scope and its members, a local, a construction parameter, a
      class-level cell, a namespace variable -- two IR layers, both backends, and the composition of
      program-wide symbols, where a part now says which of the two ranges it is in. What the rule
      paid for itself with is deletion: every mechanism that existed to keep minted names apart is
      gone, because a position is distinct by being one.

      Two defects it exposed that nothing was looking for. `$printtimescale` (LRM 20.4.2) named the
      scope with a word the compiler had composed, printed to the user as simulation output; it now
      reports the design element, and what the clause actually asks for -- the hierarchical path --
      is a run-time fact this layer cannot answer. And the emitted construction protocol spelled its
      receiver one way in the signature and another in the body, which held only while both reached
      for the same word.

      The execution backend answered all of it correctly throughout, because what it links is not a
      C++ identifier, so only the C++ projection could see the failure while the unsoundness was
      everyone's.

- [x] R84 -- The front end records the identifier a variable was declared under and records none for
      a variable it introduced itself, so R79's rule holds from the layer that answers what the
      design wrote. A held right-hand side of an intra-assignment delay (LRM 9.4.5), a `foreach`
      bound and its continuation flag, a crossing result and a forwarded one had each carried a word
      of the compiler's own, under three conventions, two of which are ordinary identifiers a design
      may also declare.

      It was filed as a subject of its own and that was wrong. The rule is R79's, and the reading
      that separated them -- that the front end owes source fidelity rather than a target spelling
      -- describes why the answer takes the shape it does, not why it is a different question.
      R79's decision record states that a name relation admits only what the source declared; while
      the front end supplied names for what it synthesized, that sentence was false in the tree, and
      an invariant its own change violates is worse than none.

      Doing it turned up a position nothing else had: the record a unit keeps about each of its
      packed types was named in a layer with no target, and a package declaring a variable spelled
      like one of those records emitted C++ that does not compile. It is the same defect as every
      other position on the axis, it had gone unseen through the whole of R79, and it was reachable
      only by treating the two as one subject -- which is the argument against splitting a rule by
      the layer it happens to land in.

- [ ] R85 -- A policy check's own allowlists go stale silently, so a check keeps passing for a
      reason that stopped being true. Each of these scripts carries lists naming paths, functions or
      expressions that are exempt, and nothing confirms any entry still matches anything: an entry
      whose subject was renamed or deleted stops exempting what it was written for, and the summary
      line each script prints goes on counting it. Four scripts carry such lists -- the exception
      policy has four, the architecture policy three plus four hard-coded file paths, the runtime
      ABI check four hard-coded paths, and the render-name check its emitter list.

      The class has already cost once: a rule named the directory a `catch(...)` allowlist covered,
      the entry point moved, and the sentence went on naming a directory the gate no longer meant --
      which reads like a typo and is dead configuration. Where the exempted subject no longer
      exists, the check passes over it for the wrong reason.

      The shape of the fix is settled and proven in one place: the emitted-name check now reports
      any entry of its three lists that nothing in the scanned files reaches, and that report was
      verified in both directions -- fabricated dead entries make it fail and name them, removing
      them makes it pass. Each remaining script wants the same, in its own terms. Blocked by
      nothing.

      Two rules were considered here and rejected on measurement rather than on judgement, which is
      worth recording so they are not re-proposed. A check for a variant alternative nothing
      constructs finds nothing: 91 variants and 521 alternatives were examined and every one is
      constructed somewhere. A check for an alternative built at exactly one site finds fourteen,
      all of them legitimate single-producer cases, so it would either stand red or need an
      allowlist of its own -- which is the defect above. What neither catches is the case that
      prompted them: an alternative built only by the classifier's own unreachable fallthrough,
      which is a reachability property and not a syntactic one.

- [ ] R80 -- The C++ backend has no diagnostic channel, so every construct it does not realize is
      either an internal error telling the reader to report a bug, or a refusal stated somewhere
      other than where the backend meets it. The renderer is a fold whose every entry answers with a
      string, and nothing in it carries a result that can fail, so the only refusal it can raise is
      a throw, and the one throw type admitted for a gap is the one reserved for a compiler
      invariant. Every gap the backend has today is therefore hidden by the front end refusing
      first, which holds only for as long as the two sets coincide.

      The first form that broke the coincidence is answered where the backend is asked rather than
      where it renders: the emit entry reads off the unit's own types whether the unit settles where
      a name lands, and declines the unit whole. That is a capability statement, correct and cheap,
      and it does not generalize -- a form with no footprint in the type pool has nowhere to be read
      off, so the next one needs the channel rather than a second pre-check. Two pre-checks would be
      the same decision in two places, which is the shape to avoid rather than repeat.

      Not blocked. The execution backend already returns a diagnostic from the same depth, so what
      is missing is the C++ side's plumbing and not the contract. Weigh it against the C++ backend's
      remaining life: threading a result through the fold touches every entry in it and collides with
      any other work in render.

- [x] R81 -- A question asked of a closed set answers for every alternative it has, and no arm
      stands for the ones nobody named. Every remaining arm is one of three answers -- a value, a
      refusal naming the construct, or a statement of why that alternative cannot arrive -- and the
      third is what most of them are, which is why writing them out reads as long and is not
      transcription. The record of standing exceptions is gone rather than empty, so the next
      generic arm is new by construction.

      **What the record was hiding is a legal program that aborted the compiler.** Printing an
      associative array whose index is a wildcard reached the arm answering "what does a value of
      this type start out as" for every type nobody had written, and told the user to report a bug
      rather than naming a construct. The walk such a printed form is built from needs a value of
      the index type, and a wildcard index is not a data type at all (LRM 7.8, 7.8.1) -- which is
      also why the standard forbids walking such an array by index. That construct is refused by
      name now, settled one layer above where it aborted by the work that gave a declared type its
      own readings, so what this entry is left holding is the mechanism rather than the case: an
      alternative nobody named can no longer turn a legal program into a bug report, whichever
      program reaches one next.

      **What emptied the rest was not transcription.** Four sites asked one question in four
      spellings -- which declaration a type names -- so the question moved onto the type, beside the
      ones already stated there, and each site now dispatches on the five alternatives that answer
      it rather than on every type there is. The sites that were left genuinely had something to say
      per alternative: which value domain the runtime realizes, what crosses the foreign boundary,
      what a place opens, what a memory task accepts.

- [ ] R82 -- Every value domain names its two realizations the same way: the monomorphized one takes
      the domain's own word and the erased one takes that word behind `Runtime`, so a reader meeting
      either knows there is a pair and what the other is. The class-handle domain is the one that
      does not -- its two realizations carry unrelated words, and the domain's own name matches
      neither -- so nothing in any of the three names says they are one concept, and a reader has to
      be told.

      What makes it more than cosmetic is where the domain's name goes. It is the word the
      library's symbols are composed from, so the pair is not renamed without renaming the entries
      the generated module calls, and it is the word the semantic layers use for the type as well.
      Straightening it therefore crosses both IRs, both backends, the runtime and the ABI at once,
      for no change in behaviour -- which is why it is written down rather than taken.

      Not blocked. Read it together with whatever is live on name composition, since the two touch
      the same question from opposite ends: this is one concept wearing three words, that is one
      word standing for two kinds of name.

- [ ] R86 -- The C++ backend decides a class method is pure virtual from the absence of a body
      alone, where the language it is writing makes that marker legal only on a method that also
      takes part in dynamic dispatch. Both facts are stated on the declaration and only one is read,
      so the emitted text is well formed today for a reason no reader of that site can see: the one
      lowering that leaves a class method bodyless is the one that marks it virtual, and nothing
      holds those two together. A bodyless method reaching the render without a dispatch role emits
      a marker its enclosing declaration cannot carry, which fails in the target's own compiler
      rather than in anything that gates a merge.

      Not blocked, and it is a one-line reading rather than a design: the site asks for both facts
      instead of one. It is written down rather than taken because it belongs to declaration
      rendering, which the subject that found it does not otherwise touch.

- [ ] R87 -- A `for` whose init declares more than one binding emits C++ that does not compile. MIR
      states the init as a list, and the C++ render spells each declaration with its own `auto`, so
      two of them land as `for (auto a = ..., auto b = ...)`. Nothing produced two until a lowering
      tried, and the failure is in the emitted text rather than in a check, so it surfaces as a host
      compile error naming no construct.

      The target shape is the render refusing what it cannot spell, since a backend meeting IR it
      does not realize returns `diag::Unsupported` rather than emitting text that will not build.
      Whether the render should instead spell several declarations -- one `auto`, comma-separated,
      which is legal C++ only when the deduced types agree -- is the open half: MIR admits inits of
      different types and C++ does not, so the answer may be that the IR is what should say only one
      declaration is allowed.

      Not blocked, and nothing produces it today: the foreach lowering declares the odd binding
      ahead of the loop for exactly this reason, and the assignment-pattern rendering now does too.
      Found by writing the second one.

- [ ] R88 -- The execution backend publishes no entry for the guard a tagged-union member access
      carries (LRM 11.9), so an ordinary read of a packed tagged union's member refuses there with
      `the runtime library publishes no entry named lyra_rt_packed_require`. Reproduced on a
      four-line program whose only content is `pt.Bits`; the C++ backend runs it.

      The target shape is the entry, beside the other value-domain entries. What makes it worth an
      entry here rather than a note is that the guard is the general "yield the receiver when a
      condition holds, raise otherwise" operation, so every construct the language checks while
      evaluating an access meets it, not only this one.

      Not blocked. It sits in the execution backend's own surface rather than in any lowering.

- [x] R89 -- A compile holds every whole-design representation at once, so its peak memory is their
      sum and not the largest of them. Measured on a generated design: the peak tracks the total
      source and does not move when the same content is split into four units or into two hundred
      and fifty-six, which is what a sum looks like and a maximum does not. Two of the three are now
      released where their last reader finishes -- the elaborated front-end tree, and each unit's
      HIR as that unit is lowered -- which took 28% off a lowering run's peak and 19% off a dump's.

      Nothing below HIR is held any more. Each unit is lowered, handed to whoever asked for it,
      and released before the next one starts, so a compile no longer returns a bag of every
      stage's artifacts and the command that wanted them drives the loop itself. What the steps
      reading across the design get instead is a small record each unit publishes: which namespace
      it brings up and against what, which foreign names it takes part in with their prototypes,
      and whether a backend can realize it. A prototype crosses in a pool the record carries,
      because whoever reads it does so where the producing unit's arenas are gone.

      The same record settles two questions that had been answered separately, each by walking a
      whole unit: which foreign names the program publishes, and which of them the program itself
      must define. It also carries the one fact a backend needs before it writes anything, so
      every unit can be refused before the first byte is written and a run that meets a gap names
      every gap rather than the first.

      Measured on a generated design of five megabytes of source: lowering it to MIR peaked at
      1371 MiB and now peaks at 613, and emitting the whole C++ project costs 608 -- which is to
      say rendering and writing the design now costs one unit more than lowering it. What remains
      linear in the design is HIR, which is still built for every unit in one pass before any of
      it is consumed; that is the next term and a different subject.

      Deriving the published facts before any body lowers is a further step and a different gain --
      it lets the design root be assembled while unit bodies are still being lowered. That one
      belongs to the signature workstream rather than here.

      **The record is gone, which is where this ends.** It was a bridge rather than a destination: a
      design's link-level unit is a referrer like any other, and what it may read is the signatures
      of the units it references. The three facts that were not on any signature have moved to the
      parties that run after compilation -- an order the namespaces settle among themselves as they
      run, a symbol every declaring unit defines and the assembling party keeps one of, and a
      foreign name space the build unions from per-unit fragments -- and the fourth thing the record
      carried, whether a backend could realize a unit, turned out not to be a program-level fact at
      all. The link-level unit is now synthesized from the named tops and their signatures alone,
      and every unit goes from HIR to a finished artifact with nothing reading across.

- [x] R90 -- How far a lowering runs is no longer a value. A request is made by calling for what it
      reads -- the elaborated source, the design's HIR, every unit modelled semantically, every unit
      in the form something runs -- and what comes back is that and nothing optional, so no caller
      asks whether a product it requested is there. The only absence left is at the outermost
      boundary and means the run failed, which is what the collect-and-continue rule already
      requires. `decisions/the-request-names-its-products.md` holds the derivation and the survey.

      A command now states its depth once, by calling for what it reads. It used to state it twice:
      a switch over the command kinds answered how far the front end had to run, and a second switch
      ran the command and read what it liked, with nothing holding the two together. A command that
      asked for less than it read compiled and reported a compiler bug at run time.

      A compiled unit's two halves -- the body and the metadata defining it -- now travel as one
      value, so the execution session takes a sequence of units rather than two it had to index in
      step. That coupling was stated in prose on the entry it crossed.

      **Doing it falsified the reading three consumers had written down.** Each guarded its
      per-unit work with a test for a missing executable body, explaining that a namespace has
      none. Every unit has one, a package included, because a package's variable initializers and
      its subroutines are code like any other -- so all three guards were always taken, and dumping
      a design with a package shows the package's own executable unit. The comments were a day old.
      Had they been believed rather than checked, the honest-looking fix -- skip the units that root
      no objects -- would have dropped every package's initializers from what a session loads.

- [x] R91 -- Converting a packed value between widths, or between the two-state and the four-state
      domain, is word-wise. A destination word is the source's bits where the source reaches and the
      padding bit everywhere above, settled by one mask, so each destination word is written exactly
      once and the whole value costs what its words cost rather than what its bits cost.

      It used to be spelled one bit at a time: clear the destination bit by bit, copy the
      overlapping bits one at a time, then sign-extend the rest the same way. The separate clear was
      needed only because the copy wrote a subset of the destination; writing every word removes it,
      and with it the whole per-bit accessor surface of the view layer, whose only caller these
      conversions were.

      **Measured before and after, same case and same amount of work.** On the mixed
      arithmetic-and-control case, conversion was **over four tenths** of the run's instructions and
      is now **under one tenth**; on the clocked-pipeline case, about a sixth and now a fiftieth. One
      conversion of a value in the tens of bits went from thousands of instructions to a couple of
      hundred, and the first case as a whole runs in about two thirds of the instructions it did. The
      per-bit write was the single largest self-cost entry in both programs before the change; what
      stands at the top now is constructing and range-checking views, which
      [performance.md](performance.md) already predicted.

      Two things this deliberately did not take. A conversion still builds its destination as an
      all-unknown value and then overwrites every word of it, so the default-value fill is a wasted
      pass; removing it needs a way to build a value without initializing it, which is separable
      because the conversion writes every word either way. And a source or destination that does not
      start at a word boundary got no word-wise path here -- conversions never produce one, so
      nothing this entry did needed it; R128 built one.

      **The reason this entry gave for leaving that alone was wrong, and a profile said so.** It
      read "the part-select paths that would use it do their own word arithmetic already", which was
      the opposite of what those paths did: both looped one bit at a time, and on 2026-09-23 they
      measured at three tenths of the wide-bitwise case. The claim was never checked against the
      code it described -- the two functions sat a page away from the one this entry rewrote -- and
      being a reason to do nothing, nothing later disagreed with it.

- [ ] R92 -- Composing a runtime call's engine handle is spelled at roughly twenty lowering sites
      rather than once. Each interns the handle expression itself and then builds the argument list
      around it, so the convention that the handle comes first is held by every site repeating it.
      The sites that append a plain effect statement now have one emitter; the rest produce a value
      and would need a companion for the expression form, which is what makes this a shape to settle
      rather than a mechanical replacement.

      Not blocked. Written down rather than taken because converting the value-producing sites
      changes how a chained runtime call is built everywhere, which is wider than the subject that
      made the duplication visible.

- [x] R93 -- Two units that reference each other produce a C++ project that does not compile. Each
      unit became one file carrying its declarations and its bodies together, so a unit reaching
      another included that unit's whole file; where the reaching went both ways the two files
      included each other, the second include was a no-op, and whichever file the compiler entered
      first met a body naming a unit it had not seen.

      A unit now emits its declarations and its bodies as two artifacts, and the program is formed
      by compiling each translation unit and linking the results. The declarations reach another
      unit through a pointer, so they name the class without its file -- with one exception, a class
      they extend, which the target language needs whole. That is the only edge one unit's
      declarations have to another's, and a cycle in those edges has no target-language form and is
      refused.

- [ ] R94 -- A choice the standard leaves open is pinned by a conformance case, which is the one
      thing that corpus says a case may not do: a case states what the standard requires, so it is
      valid under any conforming simulator. The order two packages' variable initializers run in is
      such a choice -- IEEE 1800 states the barrier four times (6.7.3, 6.8, 10.5, 26.2) and an order
      nowhere -- and a case checks the value that only one order produces. It is not the only one of
      its kind; a pass over the corpus asking "would a conforming simulator be free to answer
      otherwise" is what finds the rest.

      What makes this hard to simply delete is that the behaviour is deliberate and worth guarding:
      an initializer reading another package's variable sees that package's value, which is what
      every established simulator answers. Removing the case loses the only thing standing between
      that and a silent regression. **Target shape**: somewhere for a case that pins a choice of
      ours, kept apart from the corpus that answers to the standard, so neither has to carry the
      other's contract. Until there is one, such a case says in its own text which of the two it
      is.

- [ ] R95 -- A body a unit's namespace owns takes no LIR function identity, so a call to one inside
      the very unit that defines it is lowered as a reach for a linker symbol rather than as a call
      to a function the unit holds. Every class body is reserved an identity before any body is
      lowered, which is what lets a call name the callee directly; the namespace's own bodies are
      appended as they are lowered instead, so nothing can name one and a caller composes the symbol
      string a second time.

      The symbol is composed from the same parts at both ends, so the two agree and nothing is
      wrong today. What it costs is that an intra-unit call carries a string where an identity
      exists, and the string is the one thing that cannot be checked: a caller and a definition that
      compose it differently fail at link time with no compiler in between. Reserving an identity
      per namespace body before the bodies are lowered, exactly as a class's are, makes the call a
      `FunctionTarget` and leaves the symbol to the definition alone.

- [ ] R96 -- Nothing drops a body no reachable body calls, and there is now a set of bodies for
      which that question is both cheap and decidable. A unit's callables include the readings every
      declared type owns, which exist because the type does rather than because anything reads a
      value of it, so a design that prints nothing still carries one per aggregate, container and
      enumeration it declares. Measured on a 47-unit RISC-V core: 337 such bodies, and the sites ask
      for none of them, because that design writes no assignment-pattern conversion and no
      enumeration method anywhere. That count includes three bodies per enumeration that no longer
      exist -- an enumeration's methods are now questions put to a member list the unit states as
      data -- so what is left per enumeration is its assignment-pattern text; re-count before
      quoting a number.

      What makes this decidable where a general dead-code pass is not: a unit callable that no
      identifier answers to is exactly one no other unit can name, so reachability from the unit's
      named and foreign-linked entry points settles it with a call-graph walk and no analysis. What
      makes it worth doing at MIR rather than in either backend is that both consume the same
      callables, and the emitted C++ translation unit -- already the slowest thing in an iteration
      -- is where the unused ones would otherwise land.

      Deliberately not folded into the change that created the set: what a layer states and what an
      optimizer removes are separate, so the readings are correct whether or not this exists.

      **What it costs is worth sizing before anyone estimates the work.** That same core's
      unoptimized build measures 2:59 of host-compiler CPU, and a figure of 1:42 recorded for it
      earlier in the same window does not reproduce -- which is consistent with the emitted text
      having grown, though nothing here has attributed it. Whoever takes this should measure the
      build with and without the uncalled bodies rather than counting them, since the count is an
      upper bound on what removing them delivers and says nothing about what they cost to compile.

- [ ] R97 -- A question about a closed set can also be answered by a chain of one-alternative tests
      falling off its end, and no check sees that one: the set gains an alternative, the chain
      answers "no" for it, and nothing fails. It is not one layer's habit -- a type's own family
      predicates are written that way in two IR layers, and so is the container test in the
      default-value lowering. Seventy-two sites reach for a single alternative that way, and how
      many of them are a chain over a whole set has not been counted.

      Target: count them, then separate a family predicate, whose members a total answer stated
      beside the type should give, from an ordinary test for one alternative, which is what a call
      site is entitled to write. The other three spellings of the same opt-out each have a check
      already, so what this needs is the rule said in terms a check can decide.

      Not blocked.

- [ ] R98 -- The benchmark harness treats a rejected probe as a failure rather than as a bound, so
      one case reports no rate at all and the whole run exits non-zero. The harness raises a case's
      amount of work until a measurement reaches its target duration, extrapolating the next amount
      from the slope of the previous two. The case whose work is a declared bit width has almost no
      slope -- being flat in width is exactly what that case exists to show -- so the extrapolation
      asks for far more than the curve needs, and nothing clamps it to what the language permits.
      The probe lands above the maximum packed width, the front end rejects the design, and the case
      gives up instead of stepping back toward the last amount that built.

      Measured 2026-09-16: it asked for 16,992,729 bits against a maximum of 16,777,215, while a
      build at two million bits takes about five and a half seconds against a twenty-second target
      -- so an amount that would have converged very likely exists below the ceiling and the harness
      simply never probes one.

      **Target shape**: a probe the tool rejects is information about the upper bound, not the end
      of the search -- step back toward the last amount that built and continue from there. A work
      axis with a limit the language fixes says so, so no probe is spent past it. A case that
      genuinely cannot reach the target reports the rate it did reach, which is what the harness
      already does for a case too slow to get there at one unit.

      Found while measuring R91, which lowered this case's per-bit build cost and so raised what the
      extrapolation asks for. That is why it surfaced then; it is not what put the ceiling within
      reach.

- [ ] R99 -- Nobody owns the rule that a packed value's bits above its declared width are clear, so
      every step along a value's path re-establishes it. Masking the top word is a few percent of a
      profiled run on its own, and the passes that do it are not independent: a conversion masks its
      destination, the constructor it is handed to masks again, and an operator masks its result
      buffer before handing it to that same constructor. A deliberately broken conversion that skips
      its own mask changes no observable answer in the corpus, because a later pass covers for it --
      which is what an unowned invariant looks like from the outside.

      **Target shape**: name the point at which the rule holds and say so once -- most likely the
      construction of a value, which every path already goes through -- so that a step in the middle
      may assume it and stop restating it. Until that is written down, the masks are all load-bearing
      by ignorance and none of them can be removed safely.

      Found while measuring R91: with no document stating the rule, a defensive mask cannot be told
      from a necessary one, and the test that would separate them does not exist either.

- [ ] R100 -- The AST-to-HIR declare pass builds a class's whole declared shape and then holds it in
      the pass's own private state rather than recording it where a peer can read it, so the unit's
      class registry answers nothing about a class until that class's bodies have lowered. The
      staged-lowering decision states the opposite -- a declared shape is a query target the moment
      the declare pass records it -- and names the lowerer-owned in-progress declaration among the
      shapes it rejects, which is what this is, one layer above where that decision was applied.

      Today nothing asks for a peer's contents that early, because the one early asker needs a name
      and now gets it from the identity. The cost is that the next thing needing a peer fact during
      publication has no place to read it and the failure is an abort rather than a compile error.

      Target: the declare pass records what it built where any consumer resolves it by id, and the
      body pass attaches bodies to it -- the same two-artifact shape the layer below already has.

      Not blocked.

- [ ] R101 -- What compiling a design's foreign source is gets answered twice, once per backend's
      build, and the two answers differ in ways nothing reconciles. Both walk the same classified
      link inputs and both compile each in the language it was written in; one asks for
      position-independent code and the other does not, and only one names the C++ standard for a
      source written in C++. Neither is obviously wrong for its own backend, which is what makes the
      divergence invisible: each reads as deliberate where it stands, and no case exercises a C++
      foreign source on either path.

      Target: one statement of what compiling a foreign source is, which both builds read. What is
      genuinely per-backend -- position-independent code is, because one backend links an object
      into a running process and the other into an executable -- belongs beside the difference that
      demands it rather than duplicated into the whole recipe.

      Not blocked. Found while splitting the C++ build into per-unit compiles, which is what made
      the first copy state a standard it had previously inherited from a shared command line.

- [ ] R102 -- A variable's storage on the execution backend lives for its body's whole execution,
      where the language says it lives for the scope that declared it. A variable of an inner block
      exists, as storage, from the moment the body starts; entering the declaration begins its
      contents afresh and leaving the block ends nothing. The other backend gives the scope's own
      extent, because its target language does.

      Nothing observes the difference today: SystemVerilog has no destructor, so a value ending
      later than its scope is not a behaviour any program can see, and what a spawned process may
      still reach is carried by a separate mechanism rather than by the declaring scope's storage.
      What it costs is memory held longer than the language requires, which grows with the number of
      declarations in a body rather than with anything a program does.

      That last clause is too kind, and the hold on a promoted scope is the counter-example. It is
      taken where the block is entered rather than where the body starts, so a body that enters such
      a block in a loop takes one hold per iteration and lets go of none until the whole execution
      ends -- growth in what the program does, not in what it declares. Measured at 685 bytes per
      entry over 200000 iterations, beside a baseline that already retains an order of magnitude
      more per iteration for other reasons.

      And that one is worse than a cost, because dropping the hold where its scope ends is half of
      what the shared wrapper the semantic layer names already means, not a target's spelling of it.
      A target that does not emit that drop has not realized the construct, so the two stop agreeing
      on what it means rather than only on how they represent it -- which the layer contract does not
      admit. What is missing is a lowering rather than a new operation to state, and an end one
      target emits while the other takes it from its own language already has a shape here, in how a
      body's declared storage is opened and closed. It is a piece of work because it moves how such
      a local lowers on both targets at once: whichever gets the end for free must stop emitting it
      twice.

      Target: the extent a variable's storage has is the extent its declaration has, on both
      backends. This is the same axis as ending it on every way out and was deliberately not taken
      with it -- ending at a scope exit needs the scope exits to be enumerable in the same way the
      body's are, which is a separate piece.

      Not blocked. Found while giving every declared variable one storage.

- [ ] R103 -- Two things own values for one execution: the storage a body's variables live in, and
      the store that carries what an execution completes with. They are the same relation -- a
      described set of storages owned by something whose lifetime is one execution's -- realized
      twice, and the second is what is left of a mechanism the first replaced.

      Target: one of them. A completion value is storage its caller allocates and the callee writes
      into, so the likelier answer is that it stops being a store of its own and becomes what it
      already is elsewhere, rather than that the two merge.

      Not blocked. Found while giving every declared variable one storage; the store survived
      because nothing else it carried moved in the same cut.

- [ ] R104 -- A requirement whose whole content is that the run **fails** cannot be stated in the
      corpus. A case is held to an exit status and a sentinel, and both directions of that pair mean
      "the checks ran and passed", so a program IEEE 1800 requires a tool to reject _at run time_
      has nowhere to be written down. The report directives cover the case where a run reports and
      still succeeds; the rejection directive covers the case where a tool refuses the program
      before running it. Between them sits everything the standard makes a fatal simulation error,
      and the DPI-C disable protocol is the first place it bit: three checks LRM 35.9 obliges a
      simulator to implement, each verified by hand in both directions and none of them held by
      anything afterwards.

      **Target shape**: a directive for a case whose run must end in a stated failure, checked
      against the diagnostic the way a rejection already is. It shares the rejection's shape -- no
      sentinel, text the run has to name -- and differs only in when the failure happens, which is
      what makes it a third state of the same axis rather than a second corpus.

- [x] R105 -- One way of leaving a body instead of two. Both targets now hand a departure to the
      language they emit and touch nothing in between; the gate that asked at every point an
      execution regained control, and the branch it left by, are gone. What made the split look
      necessary was a property that turned out to be two: passing through a generated frame, which
      already worked, and catching in one, which nobody had emitted. Only the second was missing.

- [x] R106 -- A command acts on every option it is given, or refuses it by name. Each row of the
      command table states the options that command acts on, so a command added later has to say
      what it takes, and one check refuses every option given outside that set, naming the commands
      that do take it -- `dump ast --release` and `emit cpp --backend llvm` are refused where they
      used to run and act on nothing. What is refused is a function of the command alone: an option
      a command acts on stands where this time it changes nothing, as `--no-pch` does on the backend
      that compiles no C++, because a refusal that depends on the combination is one a caller cannot
      predict. The half-fix this entry was written against -- one option refused while four are
      ignored -- is what the per-command set rules out, since every option is judged by the same
      row.

- [x] R107 -- A class handle read after its process parks survives on the execution backend. A
      variable of class type is now storage its execution owns, like a variable of every other type
      whose value the body does not hold the whole of, so nothing a wait does unreferences the
      object.

      **The axis this entry first recorded was wrong, and the correction is the part worth keeping.**
      It read the two faces -- a memory fault and a member reached through a null handle -- as
      belonging to two kinds of body, a process and a task. They belong to which storage the handle
      sits in: a class property, a module's own variable, a block promoted out of its frame and an
      element of an aggregate variable were all correct, while a variable of automatic lifetime, a
      subroutine's formal, and a local of a class method were not. The last is every local of every
      method, since a class method's lifetime is automatic whatever encloses it (LRM 8.6), so the
      broken side was the ordinary case rather than a corner of it.

      The repair this entry expected to be unavailable was not the one needed.
      `decisions/a-body-holds-a-value-or-its-execution-stores-it.md` has what decided it.

- [ ] R108 -- A `fork` reached from a static variable's initializer aborts as a compiler bug on both
      targets, on a program the front end accepts at exit 0. LRM 13.4.4 lets a function hold a
      `fork ... join_none`, and nothing stops a static initializer calling such a function; both
      targets then report that there is no process to parent the branch to. Whether an initializer
      evaluated before time zero is inside a process at all is the question underneath, and the
      standard's answer is what decides whether this is a missing process or a program that should
      have been refused -- either way it is a stop with an invariant's message on accepted source,
      which the error policy does not allow.

      Target: settle what an initializer runs inside, then either give it one or refuse the program
      where it is accepted. Not blocked. Found while probing where a promoted scope's storage
      belongs when the body holding it cannot suspend.

- [ ] R109 -- The runtime type holding a block of storage over a definition is named for one of the
      two lifetimes that use it. It is what a class object's properties live in and what a block
      promoted out of its frame lives in, and those are the two regimes this project is most careful
      to keep apart: one ends by reachability, the other with the last hold on it. Nothing is wrong
      with one type serving both -- the operation reaching a member of one is identical, and giving
      each its own would be two entries differing only in a cast -- but the name states a regime
      rather than what the type is, so reading it tells a reader the wrong thing about half of what
      it holds.

      Target: a name for the storage rather than for one of its owners. What makes this its own
      change rather than a rename in passing is that the name reaches the C++ backend's emitted
      text, so it is held by the emitted-name policy and costs a full host-compile run to move.

      Not blocked. Found by widening the type's second user without being able to widen its name.

- [ ] R110 -- A closed set of alternatives read by a chain of type tests is the one spelling of that
      shape nothing checks. The project settles that such a set is consumed so that gaining a member
      breaks the build, and two policy rules hold it: one over an enumeration read by comparison,
      one over a visit carrying an arm that names no alternative. Neither reaches a predicate built
      as `type.Is<A>() || type.Is<B>() || ...`, which opts out of the mechanism exactly as the other
      two do -- a type added later takes whichever side named it nothing, silently.

      It is not hypothetical: one such predicate decided which of a body's variables get storage, and
      answered wrongly twice. The first time it omitted every container, and a process declaring one
      and using it after a delay was dropped with no output and no diagnostic. The second time it
      omitted the class handle, and a legal program died of a memory fault. Both were found by
      reading the sentence above the list against the list rather than by anything failing.

      Target: the enforcement reaches this spelling too, or the shape is made unwritable. What has to
      be settled first is how to tell it apart from a legitimate one -- a predicate asking whether a
      type is one of three related alternatives is ordinary and must stay writable, while one
      claiming to partition the whole set is the defect, and the two look alike from the outside.

      Not blocked. Found by fixing the second wrong answer and asking what would have caught it.

- [x] R111 -- Sampling an expression whose value names an object answers on both targets. A tick
      settles which object the variable named, a history keeps that as a reference so a past tick
      answers with an object still there to be reached through, and the four functions the value
      admits -- the sampled value, a past one, and the two that compare across ticks -- answer over
      it as they do over any other kind of value.

      **What this entry recorded as an abort was a named refusal, and the correction is worth
      keeping.** On the execution backend the stop names the runtime entries that were never
      published for the domain; the internal error it also described sits in the runtime's own
      history path, which only that backend reaches, and the C++ target answered correctly the whole
      time because its host compiler instantiates a history for whatever type reaches it. The
      asymmetry was the finding: a storage family is universal over the kinds of value on a target
      that compiles per type, and only as wide as what was published on one that calls a library
      built beforehand.

- [ ] R112 -- One sampled expression against one clocking event keeps a history per place it is
      written rather than one. The design says a history belongs to the pair and is as deep as the
      deepest read of it asks for; what is built is one per call, so four reads of the same variable
      under the same clock produce four histories, four synthesized processes waiting on that clock,
      and four commits per tick. Where the value names an object each of those is also a separate
      reference holding it, so what an object's life is extended by is multiplied by how often the
      source happened to ask.

      Target: the pair decides, and the depth is the largest any of its readers named. What has to
      be settled first is when two written expressions are the same one, which is a question about
      comparing expressions rather than about sampling. Not blocked. Found by reading the emitted
      C++ for a case that reads one variable four ways.

- [ ] R113 -- A variable of automatic lifetime named by an event control or by a sampled value
      function is resolved as though it named a signal of the enclosing scope, and refused as a
      hierarchical name that reaches none. The message describes neither the construct nor what is
      missing, on both targets and for every kind of value, and the two constructs meet it through
      one resolver.

      The standard asks for little here: LRM 9.4.2 puts no restriction on what an event control
      watches, and LRM 16.5.1 makes an automatic variable's sampled value its current value and a
      past value of one the current value too -- so what is owed on the sampling side is a plain
      read rather than any storage. Target: such a name resolves to the storage it is, and whatever
      remains unsupported is refused by naming the construct. Not blocked. Found by probing the
      reason a progress entry gave for this being refused, which the standard contradicts.

- [ ] R114 -- A kind of value a storage family was never published for is reported to the user as a
      missing runtime library symbol. The check is real and fires at the right moment, but its
      audience is this compiler and its own library, and what it prints is a list of internal entry
      names -- so someone who wrote an assertion over a class handle is told that
      `lyra_rt_managedref_cell_sampled_load` does not exist. The error policy asks that an operation
      a legal program requests and this tool does not carry out be refused by naming the construct.

      Target: the refusal names the construct and the kind of value, and the symbol list stays where
      it helps, which is a build of this compiler rather than a run of someone's design. What has to
      be settled first is where that refusal belongs, since the check runs where only symbols are
      left. Not blocked. Found by meeting it on a legal program.

- [ ] R115 -- The `null` literal is typed as the opaque handle, and a comment says every handle
      absorbs it. That holds where a target compiles per type and an implicit conversion covers the
      difference; it is false where the kind of value decides which runtime object a value is,
      because an opaque handle holds a bare pointer where a class handle holds a share of ownership.
      Two comparisons therefore have to bring such an operand to the handle's type before stating
      themselves, and a third that forgets reads one object as the other.

      Target: a literal that names no object carries no kind of value of its own, so whatever
      consumes one states the type it is read at and the shape stops being forgettable. Not blocked.
      Found by a legal four-line program dying in a comparison entry.

- [ ] R116 -- A scope's construction is entered through one prototype every class shares, and the
      execution backend works out which of a unit's functions that applies to by reading the class
      table the other way round. Three sites ask -- where the function is declared, and twice where
      its parameters are bound -- so the prototype is decided by a consumer rather than stated by
      what it consumes.

      Target: the lowering states the signature it means. A construction's parameters past the ones
      every construction shares arrive as one run of values, and reading them apart is then an
      ordinary body doing what its own parameter list says, with nothing anywhere asking what kind
      of function this is.

      Blocked on the execution IR having no way to read one element of a run of plain values. It
      can build one -- that is how every entry taking a span is called -- but the read side exists
      only for values a runtime domain owns and for a sequence of object handles, so a body handed
      a run cannot take it apart. That read is the prerequisite, and it is worth having on its own:
      a form that can only be written is one generated code can hand on and never use.

      Two of the parameters that prototype carries are dead, and this is where they go. A
      construction is handed the parent the scope hangs under and the identity it is reached by,
      which the object it is handed already holds -- the runtime installed both before entering it.
      Nothing reads them: the only thing that would is the base construction, and a scope's base is
      the runtime's own object tree, which the lowering enters no constructor for. So the entry
      takes the scope and the values its class is parameterized by, and the two dead parameters go
      with the rest of this. Not blocked on the read above, but not worth doing apart from it
      either, because both change the same prototype.

- [ ] R117 -- The semantic layer states the elaborated blocks of a loop generate wherever they are
      not one body, rather than stating the loop the source wrote. That layer exists so that what
      the user wrote has exactly one answer, and the user wrote a loop; the blocks are what the
      front end produced to answer its own questions. Everything awkward about the lowering follows
      from taking them as the form.

      Target: the semantic layer states the loop and one body, always. What makes one body enough is
      that an elaboration-time value reaches it as a construction input rather than as a constant,
      and a body that needs a constant anyway refuses -- falling back to the concrete form, which
      stays only as the correctness escape the top-level objectives require. No question about the
      source is asked, because there is nothing to choose between.

      What was blocking it is done: a name reaching `g[k]` now means the k-th elaborated block, and
      which compiled scope that is, is resolved where the construct is realized. So no pass commits
      to a form before the blocks exist, and the prediction is gone with everything it needed --
      every block is lowered, the lowered scopes are compared with each other, and one of them is
      kept where they agree. That costs what the prediction cost, because both forms already lowered
      every block, and it removes the failure the prediction could not be relieved of: a hole in it
      refused a legal program as a compiler bug, which the top-level objectives forbid.

      What remains is the rest of the target. The semantic layer still states the blocks where they
      did not lower alike, rather than stating the loop and letting a body that needs a fixed value
      fall back on its own; and the half that widens what can be shared is still every site that
      stops folding the index.

      The commonest reason blocks did not lower alike is gone: a conditional written inside the
      loop selects a different alternative at different indices, and the construct now holds the
      conditionals the source wrote, nested as the source nested them, so the construction runs
      them against the index rather than being handed the answer. Each condition is stated once,
      and the ordering the standard gives a `case` is kept as the order of its items rather than
      restated on each of them. That reaches every conditional the language has: a two-alternative
      `if`, an `if ... else if` chain, a `case`, a conditional that produced no block at some
      index, and one written inside another's selected side, whose blocks belong to the outer
      construct. What cannot come from an index that did not select an alternative is its body,
      which is why the bodies are still read together across the indices.

      One position on that half is measured and has a tension worth stating before anyone moves it.
      A block declaring a class keeps the blocks apart, because a class takes an identity of the
      whole unit and one is minted per elaborated declaration, so blocks alike in everything else
      hold different ones. That identity also carries which block declared it, and it has to: a
      unit holds its classes in one flat name space, a loop's blocks answer to the construct's
      label and their index rather than to labels of their own, so without the index every block's
      class reached the same identifier -- which the C++ backend emitted as several definitions of
      one class and the host compiler refused. So the index is load-bearing for the blocks kept
      apart and would name one block while serving all of them if they were shared. Sharing here
      means the identity stops being settled before the blocks are compared, which is the same
      shape as keying a record by what it states rather than by which declaration spelled it.

      Measured, because the argument for the shape was twice made from an assumption instead. At 256
      iterations the whole semantic and generic-IR pipeline is under half a second and the elaborated
      duplication inside it costs 0.019s; the optimizer and code generation cost 4.2s and compiling
      the emitted target 16.8s. Duplication is free everywhere above the artifact, so lowering every
      block costs nothing worth weighing -- and the speed argument that twice chose a worse shape was
      about a few percent of the cost.

      Why the previous shape could not be finished, rather than merely improved, and what the field
      does instead, are in `one-body-built-at-every-index`.

- [ ] R119 -- The positions where this compiler still settles an elaboration-time value into the
      artifact, rather than supplying it to the artifact. Each is a place the front end had an
      answer available and it was taken; the question each owes is whether a different value there
      would be a different class, or the same class holding a different value.

      A sampled value's depth was on this list as settled, on the grounds that the count decides
      how much history is kept and two counts are therefore two shapes. That was wrong: the storage
      is a run-time sequence sized from a number it is handed, and a read reaches an entry by a
      number it is handed, so neither the count nor the distance is stated by any type. Both are
      now supplied at construction, and a loop whose blocks reach back by their own index is one
      body. What the mistake looked like is worth more than the fix -- the clause really does fix
      the count, and the count really does decide how much is kept, and the conclusion still did
      not follow.

      A declared width sits in the type, so two widths are two types and a repeated structure whose
      packed dimension is written from its own index is compiled once per index. That is a
      consequence of where the width lives rather than a position this entry took, and where it
      should live belongs to whoever owns the value representation; the decision record says so and
      names the entry that settles it. Three reasons this entry once gave for treating it as a
      position of its own are withdrawn -- a constant-width fast path that no longer exists, a claim
      that the front end discards the dimension's expressions, and an appeal to how rare the
      construct is.

      A container's declared extent sits in the type the same way, and both of them -- a
      fixed unpacked extent and a queue's declared bound -- are ordinary members of that answer
      rather than positions to move. What they turned up instead was a defect in how one body is
      decided, which is R117's subject and is recorded there.

      Audited and needing nothing. An assignment pattern's keys are lowered already, whichever form
      the pattern takes; where a key names a member or a type rather than a value, it names
      something the class is made of, which is where it belongs. A reference's coordinate arrives
      already resolved, because the front end hands back the index and not the expression behind it.
      Three further positions are fixed by the standard itself -- a port's declared default (LRM
      23.2.2.4), a structure member's default (LRM 7.2.2), and a port reference's coordinate (LRM
      23.2.2.1) -- and each is written in a unit's header or in a type declaration, which a
      parameterization already separates into its own artifact, so reading one there cannot cost a
      second artifact.

      What has already moved, as the worked examples: a dimension query's index, both of a sampled
      value's numbers -- the depth its history runs to, and the distance a read reaches back -- and
      which alternative of a conditional generate stood, which the construction now asks rather
      than being told.

- [x] R118 -- A loop generate's blocks are one compiled body whatever form its step is written in.
      Of the seventeen operator forms LRM 27.4 admits for a step, twelve used to fall back to one
      built block per index -- every compound assignment operator, including the one the clause
      names where it allows the index array to be sparse. What decided it was read off the step's
      spelling before any block was looked at.

      The step is now the expression the source wrote, placed in the construction for its effect on
      the index, so nothing asks what form it took. That took one decision, which is that an
      expression a construction evaluates may write: both lowering boundaries had refused an
      assignment there on the stated grounds that the language admits none outside a procedure, and
      a loop generate's step is exactly such a position. A sparse index array reaches the shared
      body for the first time, carrying the value the genvar held rather than an ordinal.

- [x] R120 -- On the execution backend, a body that does not suspend holds every value it builds
      until it returns, so a run's footprint grows with the amount of work rather than with what the
      design declares. Values crossing into generated code are owned by the scope wrapping the call
      that made them and released when that scope ends; a process body that loops without waiting is
      one such scope for its whole duration, so nothing is released while it runs.

      Measured 2026-09-17 on the representative compute-block case under a 8 GB address-space cap:
      it completes at 20 table passes and is killed for memory at 50, identically before and after
      the constant work landed, so this is neither caused nor relieved by it. A case whose body waits
      on a clock releases per stretch and runs 60,000 cycles in the same cap without difficulty,
      which is what places the cause at the scope's extent rather than at the total.

      **What it costs beyond the ceiling itself**: the throughput of this backend cannot be compared
      against the other one on any case whose body does not wait, because the case ends in memory
      before it runs long enough to time. That is why the two backends' figures under runtime
      performance are taken on different cases and do not compare.

      **Target shape**: a value whose last reader is inside the stretch that made it does not have to
      outlive the statement that made it. The scope's extent is right for what crosses a call
      boundary and wrong as the lifetime of every intermediate a loop produces, and separating those
      two is the question -- not a larger arena, which moves the ceiling and keeps the growth.

      Resolved by removing the scope's ownership of values altogether rather than by separating
      the two extents inside it: every value is built in the frame of whoever made it and ended at
      the end of its full-expression, as a C++ compiler ends a temporary
      (`../decisions/a-value-lives-in-its-makers-frame.md`). A loop of a million function calls now
      holds 4.5 MB, the same as at a hundred thousand.

- [ ] R121 -- Every runtime entry the execution backend can call is registered in one function, and
      that function is long enough that the editor reports its size when it grows. It is a flat list
      of name-to-address pairs with no branching, so what the report is about is length rather than
      anything a reader has to follow; the cost is that the answer to it is always to look away,
      which is the habit a warning cannot afford.

      Target: the registration is grouped so that each group is the set of entries some one thing
      publishes, and adding a family touches the group that owns it. What has to be settled first is
      what the groups are, and the answer is probably what already names the entries -- the value
      representations, the wrappers, the operations that name themselves -- rather than a split by
      size. Not blocked. Found by adding one family to it and reading what the editor then said.

- [ ] R122 -- Asking for the sampled value of storage lent by reference is refused on both targets
      wherever that storage is not a subscribable variable, and for one of the two things that can
      be lent there the standard says what the answer is. LRM 16.5.1 gives a variable its value in
      the Preponed region and excepts an automatic variable, whose sampled value is the value it
      holds and whose past value is that same value. A reference may name either an automatic
      variable or a class property, and what it carries is which form of storage it is -- whether
      anything subscribes -- never which kind of variable, so the answer the standard fixes for one
      of them cannot be told apart from the answer that would be wrong for the other.

      Target: the automatic variable answers with the value it holds, and the property is refused by
      naming what is missing, which is a value nothing retains for it. What has to be settled first
      is where the distinction lives, since it is a fact about the declaration and the thing carried
      to the callee is an address. Not blocked. Found by reading the two targets' answers side by
      side and noticing they disagreed.

- [ ] R123 -- The record fixing how generated behavior reaches the runtime
      (`../decisions/generated-behavior-boundary.md`) sketches that boundary with a shape the
      runtime no longer has: a per-unit definition record held apart from a scope's, an instance
      kind distinct from a scope, and user dispatch as a table still to come. All three moved when a
      unit's object became a promise and a realization -- one definition record, carrying the
      dispatch table, read off whatever kind of value holds the class. The record's principle is
      intact and only its shape sketch is stale, which is the worst combination: it reads as
      current, and someone building against it builds against a boundary that is not there.

      Target: the sketch says what the runtime holds, or the record says outright that it fixes the
      principle and not the shape. Not blocked. Found while sweeping what a unit promises, which is
      what merged the two definition records.

- [x] R124 -- A value of a class is one shape, and nothing above the runtime asks which kind of
      value it is. That shape holds the class's record and the storage that class asks a value of it
      to own; an instance in the design hierarchy and an object the program built are that shape
      plus what differs between them -- the lifecycle each joins and the reference that reaches it.
      So every entry that acts on a value through its class takes the value and nothing about its
      kind, and the entries are one family rather than one family per kind.

      The two had been apart, each holding the record at an offset of its own in types sharing no
      base, which made every such entry exist twice and obliged whoever emitted a call to work out
      which to name first. They differed by less than that suggested: reaching a member was the only
      entry that genuinely read differently, and only because a hierarchy instance was allowed to
      skip naming the class declaring the member -- which the general form already covers by
      starting that class's storage at zero. The narrow entry was the general one with an
      optimization folded in, and folding it in was what obliged every caller to know what it held.

      The shared shape has to be where a value's address points, not merely somewhere inside it: an
      entry is handed an untyped address and reads the class off it, and a kind that declares a
      virtual no type below it declares takes that address for its own table pointer. So the shape
      extends the base an object already carries, that base is where the destructor is declared
      virtual, and every kind extends the shape by single inheritance -- which puts all of those
      addresses at one place and leaves what class a value is stated once rather than once per kind.
      Declaring it lower instead compiles, and passes everything except what builds emitted text:
      the kinds a target derives from the shape are written by this compiler rather than found in
      its own sources, so a search for them inside the tree comes back empty and says nothing.

- [ ] R125 -- A referrer reads the part of a unit's signature it named, so editing a class it never
      named moves nothing it compiles. What it still reads is the unit's name set: every class the
      unit promised is announced where the unit's namespace is declared, so adding or removing a
      class re-emits every referrer of that unit even where none of them could name the new one.

      That announcement exists so a declaration can reach a sibling class through a reference
      without the sibling being complete. Which siblings a given class needs announced is a property
      of that class, so the announcement could sit with the class that needs it rather than with the
      unit -- after which a unit's name set stops being something a referrer reads at all. What has
      to be settled first is what a class names by reference that is neither its own unit's nor
      already reached through a promise, because that is the set each class would announce, and
      nothing enumerates it today.

- [ ] R126 -- A class answers where a property lives and which body fills a dispatch position in one
      of two ways: by walking what it extends and asking the target that laid the value out, or by
      reading the flat schema a runtime-owned realization built for it. Which one a class uses is
      installed when that realization runs. A class whose values stand in the design hierarchy is
      not realized that way -- the declaring unit states its definition outright -- so it keeps the
      walking answers while its values hold runtime-owned storage the walk cannot reach.

      Nothing asks it that way today, because a value standing in the hierarchy is reached as its
      own address and the entry for that reads the flat schema without consulting the class at all.
      So there are two statements of one answer that agree everywhere anyone currently looks, which
      is the shape that stays wrong until someone looks somewhere else. Target: a class states one
      answer whoever built it, and every entry asks the class rather than reading past it -- which
      also settles whether the indirection belongs on the member-access path at all, since today it
      is avoided by not asking. Not blocked. Found while merging the two into one value of a class.

- [ ] R127 -- A design element's signature lists the classes it declares, under a field whose own
      definition is the classes another unit may name. Another unit may name none of them: a class a
      design element declares is a type of that element's instance (LRM 6.22), which is why a
      reference to one is reached by asking the declaring scope rather than by naming the class.

      What keeps the listing from doing harm is that it carries no members, so every reader that
      looks for one falls through to asking by name -- an emptiness doing the work a statement
      should. A reader that asked a different question of the same listing would get an answer that
      looks authoritative and means nothing, which is what a published member's type did until its
      class was taken off it at publication. Target: what a unit publishes says which classes it
      offers, and a design element offers none, so nothing downstream reads an entry to find it
      hollow. Not blocked. Found while a referrer's artifact asked to read a class that has no
      readable form.

- [x] R128 -- A run of bits moves between two packed values a word at a time. Taking a run out of a
      value and writing one back used to be a loop over the run's bits, each iteration dividing,
      taking a remainder, shifting twice and writing one bit. Both now step a destination word at a
      time, reading the source shifted by the difference between the two offsets and merging under
      one mask, so neither end has to sit on a word boundary -- which is the word-wise path R91
      recorded as missing and unneeded.

      This was the same defect R91 removed from width and domain conversion, in the two functions a
      page away from it that were not looked at. R91's own text asserted these paths did their word
      arithmetic already; being a reason to do nothing, nothing ever contradicted it.

      **Measured at the same work on the wide bitwise case, which is the one case outside the corpus
      band**: 934,135,373 instructions and 367,261 iterations/s before, 713,634,459 and 469,661
      after -- **24% fewer instructions and 28% more work per second**. Moving a run is now under a
      tenth of that run, from three tenths.

      **The order the two halves landed in is worth more than either number.** The word-wise rewrite
      on its own took the instructions to 750,594,459 -- a fifth of them gone -- and the rate did not
      move at all, 365,797 against a 367,261 baseline. What moved it was deleting a bounds check
      this same change had added: 3% of instructions, and all 28% of the time. The check took a
      `string_view` for a message only a failure would print and composed a `std::string` on the
      throw path, which was enough to keep the mover from inlining into its two callers, so every
      run paid a call. **A profile counts instructions and only a benchmark counts time**, and here
      they disagreed by twenty points; the same trade is recorded on `base/fixed_array.hpp`, where a
      refactor that changed no data structure spent an entire optimization by hiding what the
      compiler could see.

      The check could not have fired either. Both callers work out the overlap between the run and
      the value before saying what to move, so it compared a bound against the arithmetic that had
      just produced it -- which is the shape already recorded for the view range check that was
      removed rather than made cheaper.

      What stands at the top of that case now is making a value at all: blanking one, zeroing its
      words, masking the bits above its width. That is what tops the representative block too, so
      the two no longer disagree about where the time goes.

- [x] R129 -- What an emitted project is given of the runtime is the surface a design compiles
      against, and nothing else. It used to receive every header this compiler has -- the semantic
      layers, the front end, both backends -- none of which anything in the project includes: 413
      headers and 2.11 MB where the surface is 132 and 1.01 MB.

      **The build had always stated the set correctly, and the compiler was not reading it.** The
      surface is named as a set here, and what the compiler did was resolve one member of it and take
      the directory around that member. Where a member physically sits is not something the naming
      controls: a source file sits where this repository keeps it, so the directory around it is the
      whole tree. What the set said was never consulted -- and nothing about the resolution looked
      wrong, because the file it named was the right file.

      So the surface is staged under a root that holds it and nothing else, the way the runtime
      library a project links already was. Then the directory around any member is the set by
      construction, and the walk that reads it cannot reach what the set excludes. The lesson is the
      general one: **navigating by parent directory from a resolved file leaves whatever the build
      arranged**, and it is the staging rather than the care taken at the call site that makes such a
      walk answer correctly.

      Two things it was costing beyond the size of the directory, both now closed. A prepared header
      is named by the bytes of the tree it was built from, so editing any header of this compiler --
      a lowering, an IR node -- renamed the entry and made the next build of any design prepare one
      again, which during development is most builds; that tree now holds the surface alone. And a
      project handed to someone else no longer carries the compiler's internals.

- [x] R130 -- What the merge gate spends its time on is the corpus again. Measured 2026-09-22, the
      CLI suite had become the longest target in the gate at 117 s of processor time, roughly four
      times the conformance run beside it, and the only argument recorded for it being there was a
      written claim that the host-compiling targets outside the corpus "cost a fraction of a minute
      between them" -- off by about five times.

      **The cost was not repetition, which is why the answer was not a cleanup.** Six of the sixteen
      cases build an emitted project and were the whole of the cost, and no two of them can share a
      prepared header: what one holds is bound to the paths of the headers it was made from, and
      every emitted project carries its own copy. Nothing was being answered twice.

      So they were split off by subject, and the subject happened to be the cost. What those six ask
      is whether an emitted project still builds and runs, which is what the corpus through that
      path asks, so they are answered on the same schedule and under a name that says so. What is
      left gates and costs seconds, and the cases that only reached the host compiler because
      running a design defaulted to it now ask for the backend that needs none. The exposure this
      accepts is a regression in the shipped recipe reaching `main` and being found within a day --
      the same trade that path already had, and whoever could cause it is already told to run that
      schedule before committing.

- [ ] R131 -- Which run-time checks belong inside the expression that carries them. A guard the
      language requires to run as part of evaluating an access rather than ahead of it (LRM 11.3.5)
      is stated once, as an operation over any value that yields what it was handed, so the access
      composes onto it instead of naming its subject twice. Exactly one construct is built on it:
      reading a tagged union's member against a tag it does not hold (LRM 11.9).

      What is unasked is whether that is the only one. The clause is about short-circuiting, so it
      reaches every check a short-circuited operand must not raise -- and the other run-time checks
      a design can fail are today written wherever each one happened to be needed rather than
      against that question. The answer may be that this construct really is alone, and that is
      worth establishing rather than assuming: a check hoisted out of the expression it belongs to
      is wrong in a way no case that passes can show, because the program that would see it is one
      that must not run the operand at all.

- [ ] R132 -- The list of what the runtime library publishes, which every generated module is
      checked against before it is linked. Every entry is named there, one line apiece, split only
      into what the engine publishes and what a value does: a thousand lines whose structure inside
      each half is the order someone happened to add things in. It is the third place an entry is
      written, after its prototype and its definition, and the one that carries no information --
      the name and the function it reads a shape off are the same name twice.

      A policy script holds it together rather than the code doing so: it is what catches an entry
      declared and never listed, a name listed against another entry's function, and a name listed
      twice.
      That the check exists is the finding. A binding that restates its own name is derivable from
      the two places that do carry information, and a list nobody can read is where the mistakes
      that check looks for come from.

      Not attempted here. Whatever replaces it decides how this surface is organized, which is the
      runtime ABI's own question rather than a caller's, and grouping the list by hand first would
      spend the same reading twice.

- [ ] R133 -- A compiled artifact is a function of the design, and the front end's own containers
      are the one thing that can break it without anyone writing an error. They key on where a
      symbol was allocated, so iterating one yields an order that differs between two runs of one
      design; carry that order into what is emitted and the compiler answers differently the second
      time, which nothing a simulated program can observe will report.

      One such leak is closed, at the boundary where the front end's reads become identities this
      compiler minted, and a check now compiles every corpus design twice and compares what was
      written. That is an instance plus a detector: nothing stops the next one being written, and
      the detector reports only what some design happens to exercise. Target: the leak cannot be
      written. Every container keyed by a front-end pointer answers lookups and cannot be iterated,
      so a pass that wants an order has to key on something the design decides -- which is what the
      rule already says, stated so that the compiler holds it. About forty-six declarations, almost
      all of them caches that never wanted iteration. Not blocked. Found while settling why one
      design emitted two programs.

- [x] R134 -- Where a function an emitted unit calls is defined. This entry was written as "where a
      value operation is defined", on the reading that a unit's remaining object was copies of the
      operations a design performs on values: 78% of the symbol bytes left in a design's unit after
      the change that stopped a unit copying the classes it derives from, with a unit of 14,281
      bytes still producing a 612,288 byte object holding 12,678 bytes of code.

      **Both halves of that reading were wrong, and what replaced them is settled in
      `decisions/a-published-operation-is-compiled-once.md`.** The copies were not of the operations
      a design performs on values: the arithmetic a design calls is defined in the library already,
      and what a unit was copying was the library's own entries -- a wait, a delay, a fork -- and
      the families written over the value domains, and the constructors and destructors of what the
      runtime defines. Moving all of it costs an optimized build under 1% by instruction count, so
      the trade this entry described, against the measured worth of letting the optimizer read the
      runtime, was between two things that do not meet.

      What is left is a unit's own: its classes, the value operations it performs, and the families
      over value types its design composed. A 32-unit design's objects went from 26.6 MB to 6.7 MB,
      and a test reading the emitted objects fails on anything of the runtime's beyond that.

- [ ] R135 -- The rule that a lowering states the expression rather than the answer the front end
      computed is enforced by matching two spellings of taking that answer. The property it stands
      for is what the value varies with: an artifact's identity has a closed set of axes -- the unit
      and its parameterization -- and a value varying with anything else, a repetition index or a
      position in the hierarchy, must not be settled into what is compiled. A spelling is a proxy
      for that and admits whatever it does not enumerate.

      It has already cost one position. A parameter's value reaches the front end's answer through a
      third spelling, so a constant a repeated block worked out from its own index was folded into
      the artifact for as long as the rule stood, while the record the rule serves said every such
      position had been dealt with and was read as the evidence that it had. That one is closed, and
      the next accessor the front end gains is outside the pattern on the day it lands.

      What is owed is the enforcement and not the practice. Every site that reads a symbol's settled
      value -- four of them, counted rather than estimated, covering a specialization's own identity,
      an enumeration's members twice, and a unit's parameter -- now says in a comment what its value
      varies with, and all four vary with an axis an artifact already has. The remaining six matches
      of the same accessor are literals, where the value is what the source wrote and there is no
      second thing to prefer.

      So the open part is the design of a check, not a sweep. Neither mechanism the current one has
      can express the property: a spelling cannot, as above, and the per-file exemption cannot
      either, because one file holds both the body lowering the rule is for and two of the four
      legitimate reads. A per-site marker comment would work and is a per-site suppression, which is
      the ladder this codebase declines to climb, so the answer is something else and finding it is
      the subject. Not blocked, and small in surface rather than large -- what makes it its own
      change is that it needs an answer nobody has, not that it touches much. Found while settling
      why a block deriving a constant from its index compiled once per index.

- [ ] R136 -- Which lifecycle phase brings a scope's declaration up is decided in two places, and
      neither can see the other's answer. One region installs the index a loop counts with and the
      value construction supplies; a second, further down the same body, installs a net and runs a
      variable's assignment. Each reaches its cases by asking whether a declaration is of one kind,
      so a kind neither region names gets no phase at all: it is given storage and then nothing ever
      brings it up, and the build stays green because no consumer of the set is exhaustive.

      That happened while a constant a generate block settles for itself was being added, and it is
      the argument rather than the anecdote: the shape cannot report its own omissions. Making one
      region exhaustive was tried and does not work -- it would have to state something about the
      kinds the other region owns, and whatever it states is either a second copy of that answer or
      contradicts it, since a net really is installed in the constructor and reads as though it waits
      for the initialize phase. Target: the phase a kind comes up in is stated once, over the whole
      set, so that adding a kind fails to compile until its phase is named. Two regions of one long
      body, and a phase model stated once rather than inferred from where the code sits. Not blocked.

- [x] R137 -- The text a backend produces is written into the artifact it will be read from. Every
      render entry used to answer with a string, which whoever asked for it then copied into the
      string it was building, so a byte near the bottom of an expression was copied once for every
      level above it and the punctuation between the parts was a format string parsed while the
      program ran, once per node. A renderer now takes the destination and writes into it, in the
      order the text will be read, and answers with nothing.

      **The emitted text is identical, which is what makes this checkable rather than argued.**
      Emitting the whole conformance corpus before and after gives the same bytes for all but four
      of 8,216 files, and each of those four is a case whose emitted text varies between two runs of
      one unchanged compiler -- the same one binary produced three different answers for one of them
      in eight runs. Two synthetic designs emit the same bytes to the byte.

      **Measured as instructions, because a wall-clock reading on a shared machine says more about
      the machine**: over one emission of 1.3 MB of C++ from many small units, 2,083,178,409
      instructions before and 1,758,903,215 after, **15.6% fewer**; over 2.0 MB whose expressions
      nest two hundred deep, 6,023,100,029 and 4,987,880,584, **17.2% fewer**. Interleaved wall clock
      on the first of those at full size, five rounds on a loaded host: 2.97 s against 2.54 s for
      10.5 MB, so about 4.1 MB/s where it read 3.5.

      **A prediction written before the work came out weaker than it was written.** The cost of an
      expression was said to be its size times its depth, which predicts the deep design gaining far
      more than the shallow one; the gain is 17.2% against 15.6%, and what the depth actually decides
      is the cost per byte -- 3,066 instructions per emitted byte deep against 1,578 shallow -- which
      both shapes pay alike.

      What is not measured is an optimized build of the compiler itself. Both readings are from the
      default build, where this compiler's own code is inflated and the library primitives this
      change removes are not, so neither figure is the one a released compiler would show.

      **Names and type spellings were left out on first landing, and they were the larger half.**
      They were kept as values on the ground that a name has readers besides the artifact, which
      grounds deciding one in one place and not building one at every mention. Measured with an
      optimized build over 256 distinct unit specializations, the format calls left there were
      30.5% of the run; with names and types written too, 683,818,727 instructions became
      507,852,755, **25.7% fewer**, and the design nesting two hundred deep 4,137,785,696 and
      3,420,803,905, **17.3% fewer**. The whole corpus emits the same 6,412 files to the byte.

      [../decisions/rendered-text-is-written-once.md](../decisions/rendered-text-is-written-once.md)
      holds what a destination owns and how a name and a type are written. What stops the shape
      coming back is that the entries answer with nothing, so writing it no longer compiles, and a
      policy rule refuses a format call anywhere in the backend.

- [x] R138 -- The three intermediate-form dumps compose their text the way the C++ backend used to.
      Each node answers with a string and whoever asked for it copies that string into the one it is
      building, so a byte near the bottom of a deeply nested form is copied once per level above it.
      It is the same shape R137 removed, one subsystem over, found by searching for the decision
      rather than by reading the diff.

      **Measured, and there is nothing here to fix.** An optimized build dumps the middle form of
      the 48-unit RISC-V core -- 45.8 MB -- in 0.36 s, against 0.06 s to check the same design. The
      printing is 63% of that run, about 200 MB/s, and what it spends is the formatting library
      parsing its format strings at run time and small allocations; copying, the cost this entry
      was opened on, is 5%. A third of a second on the largest real design is below what anybody
      waits for, so rewriting the three dumps would buy nothing a reader can feel. The figures
      quoted below were an unoptimized build and included lowering.

      **What is not the same is the requirement.** R137 answers to the end-to-end iteration budget,
      because what it writes is the artifact a build then compiles. A dump is written only when
      somebody asks for one and nothing downstream reads it, so no budget names it and the rule R137
      established -- that a backend writes into the artifact it will be read from -- does not reach
      a subsystem with no artifact. Deciding whether the discipline extends to what a developer asks
      for is a question of its own, and it has to be answered before the policy rule's scope can
      move; that is why this is an entry rather than part of R137.

      What says it is worth answering rather than assuming: dumping one synthetic design's middle
      form wrote 32.9 MB in 6.7 s, against 10.5 MB of C++ in 2.7 s for the same design. Both figures
      include lowering, which is why neither is a rate for the printing alone -- getting one is the
      first step, and it is the step that decides whether there is anything here.

- [x] R139 -- The source backend refuses what it cannot realize. The unit's refusals travel with the
      scope every render entry already holds; an entry with no form for a node reports it as
      unsupported, writes nothing for it and goes on, so one run reports every such node, and a run
      that reported anything writes no file. Continuing inside a body is sound here where the
      lowering's is not, because a render entry never reads another's text. The cast of a pair and
      the spelling of a null are type-mapping answers that can say "not realized", which is how the
      two silent answers the entry below records became refusals. Found by: an `event` given `null`
      emitted C++ the host compiler rejected; it is now refused as unsupported.

      What the entry said when it was opened, kept for the reasoning:

      The source backend has no way to refuse. It holds twenty-two invariant violations and
      not one statement that a construct is beyond it, where the execution backend holds ten; and
      the settled reading of a conversion says outright that a target "refuses a pair it does not
      realize", with every unrealizable pair answered as unsupported. The execution backend does
      exactly that -- it passes a value through only where the two representations are the same
      machine type, handles two families, and refuses the rest. The source backend, after one
      special case, writes the target language's own conversion notation for **every** other pair
      and asks nothing, so a pair nobody implemented is not a diagnostic: it is an error in
      generated code, arriving from a host compiler that never heard of the design.

      **What makes it structural rather than an oversight is that there is no channel.** Every
      render entry answers with nothing, and before that answered with text; neither can carry a
      refusal, so the backend could not refuse even where it wanted to, and an unrealizable node has
      only two exits -- write something plausible, or report a compiler bug for a program that is
      merely unsupported, which the error policy forbids by name.

      The fix is derivable to its edge. The driver already has the channel: what writes one unit
      answers with a result, and the break is one call below it, where a unit's emission answers
      with its artifacts and nothing else. Granularity is settled too -- a run collects the gaps of
      one stage and produces nothing, so the units of an emission collect and a refusal ends the
      unit it was met in, because there is no text to carry on with.

      Not taken with the change that found it. Refusing is orthogonal to where rendered text goes:
      the conversion neither created this nor made it worse, and what the channel should be is its
      own decision rather than a consequence of that one. What it does share is the surface, so
      whoever takes it rewrites the same signatures a second time -- which is the argument for
      taking it soon rather than for taking it then.

- [ ] R140 -- The benchmark over the corpus is how simulation speed is tracked, and it has gone
      unmaintained. Three defects are measured. The runner uses the compiler it finds rather than
      building it, so a run after a pull measures a stale one -- once as twenty-one identical
      failures that named neither a cause nor a file. A case whose emitted program does not compile
      reports none of the host compiler's error. And the whole corpus, 21 cases at a two-second
      target, ran past ten minutes and prints nothing until it ends: stopped on 2026-09-23 with 20
      cases done, every one of them lost. Whether each case still measures what its own header says
      it measures has not been checked since the cases were written.

      Target: a run builds what it measures, a case that fails names why, and each result is kept as
      it arrives. Not blocked.

- [ ] R141 -- A unit-definition record is stated in MIR for the one backend that may not realize it.
      A scope's runtime definition reaches the C++ backend as a constant MIR holds, built at
      HIR-to-MIR out of runtime-library record types; it reaches the execution backend as something
      composed from the declarations a LIR class carries, because MIR-to-LIR refuses those types
      outright. Every fact the record holds -- the timescale, the entries, the callables and classes
      a scope answers for -- is already a declaration at every layer; what the record adds is only
      the runtime struct's shape they are bundled into, and bundling into a runtime struct is
      realization rather than language.

      So the record is the C++ backend's realization, living in MIR, and the test that shows it is
      whether the shape would still be right with the other backend alone: with only the execution
      backend, nothing reads it. It is there because that backend sits at MIR, where the backend
      contract forbids a render entry to compose what it emits, so what it emits has to be stated
      upstream -- a concession the architecture makes to the transitional backend on purpose.

      Target: not routing the record through MIR-to-LIR. That would put a runtime struct's shape
      into the target-neutral layer, which is the violation rather than the cure. The record leaves
      MIR together with the backend it exists for: when the C++ backend retires, HIR-to-MIR stops
      building it and the record types go with it, and every backend realizes the definition from
      the declarations the way the execution backend already does.

      What costs meanwhile is drift. A field added to the runtime's definition has two derivations
      to reach, and they fail differently when one is missed: the C++ side changes a constructor's
      arity, which no emitted program then compiles past; the execution side gains a declaration
      entry, and a missing one leaves the field at its default without any failure at all.

      Not blocked. Found while reading which layer each piece of this record lives at, and what
      each layer is for.

- [ ] R142 -- A prepared header is prepared only for clang, so a build under any other compiler pays
      the runtime headers in full in every unit. That was a cost on a developer's machine and is now
      the main cost of the C++ corpus, which runs on remote executors whose compiler is the
      platform's own GCC: a case there took 20 to 30 seconds where the same case with a prepared
      header takes 2 to 5, and the whole corpus took 280 seconds across 48 shards where it took
      about 135 across 16 locally.

      Target: prepare the header in the form the compiler in hand reads -- GCC reads its own,
      found beside the header it stands for -- under the same rule that a prepared header is
      offered and never required. What decides the shape is that the two compilers look the header
      up differently, so the name it is kept under and how a compile is pointed at it both belong
      to the compiler, not to the store.

      Not blocked. Found when the C++ corpus moved to remote executors.

- [ ] R143 -- An expression the source itself nests deeper than the C++ compiler accepts reaches the
      source backend's output nested just as deep, and the unit is refused. The render now encloses
      an operand only where its position needs it and writes an `else` holding one statement as that
      statement, and the lowering writes each arm of an if-else-if after the one before rather than
      inside it, so a chain written flat -- a long sum, a set membership test, the items of a case,
      an if-else-if or a chain of conditional expressions whatever their arms test -- stays flat
      whatever its length. What is left is nesting the program itself states: the front end accepts
      expressions parenthesized to a depth of 1024 by default, and clang refuses brackets nested
      past 256, so a program between the two compiles on the execution backend and not on this one.
      Not observed in any design yet.

      Target: the nesting of the emitted text is bounded whatever the source nests. Verilator's
      answer is to count depth over each statement's expressions and hoist a subexpression past the
      limit into a temporary ahead of the statement (its V3Depth pass, limit 240 for clang). Here
      that is a statement the render would write that MIR never stated, so it belongs upstream of
      the render or not at all -- which is the question to settle first. Not blocked.

- [ ] R144 -- The check of the middle form holds one rule, and the rules that would have caught a
      lowering passing the wrong value are not among them. Every unit is checked where it is
      produced, and what is checked is that a body which is not a coroutine never suspends. Below
      the middle form a pointer no longer says what it points at, so the middle form is the last
      place a value of the wrong kind is visible at all.

      Two defects found together show what that costs. A class extending one declared in an
      enclosing scope handed its base the wrong instance: the constructor declared a parameter
      pointing at the module's instance and was given the generate block's. And a static method
      forking a process read its instance from a local of the method, which the forked body does not
      have. Both were wrong in the middle form, visibly, and neither was caught there: the source
      backend failed only in the host compiler, which the default gate does not run, and the
      execution backend ran both and read the wrong memory -- one of them a wrong answer rather than
      a crash.

      Established compilers check every intermediate form against the rules its consumers rely on:
      LLVM's verifier requires, among other things, that a call's argument types match the callee's
      prototype, and GHC's `-dcore-lint` type-checks its core language between passes.

      Target: the check gains the rules the two defects broke -- what a call or a construction
      passes agrees in number and type with what its callee declares, the base construction
      included, and a local a body reads is one that body declares -- then a member access against
      the class that declares the member, and an assignment's two sides. Not blocked. A rule should
      state what a shape means rather than how it is spelled today, since the check reads every
      shape the middle form has and meets whatever reshapes one.

- [ ] R145 -- The execution backend passes a runtime entry's span by value as two words placed one
      at a time, which agrees with the host's C ABI only while two integer argument registers are
      free for it: the ABI places a struct that no longer fits the remaining registers wholly on the
      stack, and the generated call puts its first word in the last register and its second on the
      stack. An entry that took a pointer and three spans read its third span's length from nowhere
      and failed its first allocation. The policy check on the runtime ABI now refuses any prototype
      whose span lands past the sixth integer register, so the mistake cannot be written, but the
      limit it enforces is this backend's and not the library's: an entry that genuinely wants more
      spans is reshaped to fit.

      Target: the generated call passes an aggregate the way the host ABI classifies it, which is
      what clang's own argument lowering does (a struct that does not fit is passed in memory, by
      value, as the callee expects), and the policy rule goes with the limit. Not blocked.

- [ ] R146 -- Declaring each unit and lowering its bodies to its first semantic form read the front
      end one unit at a time. A unit's bodies are the first step of its own pipeline, so no unit's
      first form waits for another's and the barrier holds only declarations; what is left on one
      thread is the reading itself, which overlaps with the other units' later stages rather than
      preceding them. Measured on the RISC-V core at `-j 4` before this: about 0.7 s of a 7.2 s
      build; on a loop generate whose blocks lower alike, 49% of the run.

      Several units cannot read the front end at once, and it was built and measured to find out.
      It elaborates what a reader first touches, and a unit reads past its own body wherever a name
      lands -- another instance's port connections, a port's default -- so what the units read cannot
      be elaborated before they start. Frozen after its own pass, the front end stopped two corpus
      designs at an allocation even with every unit's own body elaborated first. Elaborating every
      instance first is what turning instance caching off does, which triples the largest measured
      design's front end.

      Target: the front end computes lazily and synchronizes that computation itself, the way
      rustc's query system does -- a read that finds its answer shares it, a read that must compute
      it marks it in progress, and a second reader of the same part waits -- so every unit reads it
      at once and the lock around each unit's reading goes. Computing everything before the units
      start is not the target: that is the instance-caching cost above. It is a change to the fork
      this project builds, across its lazily filled fields, its compilation's tables and its
      allocator. Parsing the sources is on one thread for a related reason: the fork has parallel
      parsing switched off, and its own `-j` would collide with this command line's. Blocked on
      that fork.

- [ ] R147 -- On the execution backend a run-time error that ends the run leaves a suspended body
      without running any of the cleanups it passes, and the body's frame is then released at the
      last wait it made, which runs what was owed there instead. A block the body left after that
      wait is left a second time, and one it entered after it is never left. Nothing observable goes
      wrong today only because leaving a disable target that is not the innermost is ignored rather
      than refused; made strict, a named block holding the last wait followed by an error in a later
      block aborts the run instead of reporting it, and nothing else in the gate changes. The C++
      backend runs those cleanups, as destructors, and so does clang for every exception.

      Two answers were tried and are not it. Letting a landing treat an error as a departure no
      region claims changes nothing for most errors, because the runtime calls that raise them are
      ordinary calls with no landing; and it breaks the release of a suspended foreign stack, which
      travels the same way and which an export's entry, landing every departure, then stops. What
      it takes is a call that can raise an error stated as one that can leave its caller, as clang
      states every call that is not known not to unwind, and the stack release told apart from an
      error where a landing decides to stop something. Not blocked.

- [ ] R148 -- On the execution backend, reading one element of a variable copies the whole variable,
      and writing one element copies it, rebuilds it and copies it back. The representative compute
      block runs 42 table passes a second there against 4,650 on the C++ backend (2026-09-24, both
      optimized); a profile of it puts 31% of the time in a cell read copying a 1024-element array,
      26% in the store copying it back, 18% in rebuilding it around one element, and 22% in ending
      the copies. The C++ backend reads the same cell through a reference and writes the element in
      place.

      The reason the execution backend did otherwise is gone: it read the whole value because a
      value lived behind a handle the runtime owned, with no interior the generated side could
      reach. A value is now an object in storage whose address the generated side holds, so a read
      can answer with a reference to the storage, as `Get() const&` does, and an element write can
      land in the element, as the decision on storage owning its value already asks.

      The same premise is what gives every such variable a cell in the execution's store rather
      than a slot of the frame. The MIR predicate choosing it, and the record that one storage per
      variable argues from, both reason that the holder has only a handle into storage the runtime
      releases -- which stopped being true when values moved into the frame. What still holds is
      that a variable needs storage a reference can bind and that survives a suspension, and a
      frame object has both: the coroutine passes carry it across a suspension, and its address is
      stable. So the question this entry answers includes whether a variable is a frame object,
      which would make a read an address and remove the copy above at its root.

      Target: a read of a variable's value, and a step to one of its parts, answer with the storage
      they reach -- the entry's answer is stated as the storage it was handed, as it already is
      for a guard -- and only a value the program keeps past the full-expression is copied. Not
      blocked.

## Out of Scope

- Per-feature workstreams. Those live in the dedicated feature files (`operators.md`,
  `processes.md`, `mechanical-translation.md`, etc.). Making MIR state each semantic fact once, so
  that no backend re-derives one, is tracked there rather than here.
- One-PR cleanups with no architectural shift. Those land directly without a tracking entry.
