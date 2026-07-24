# Ambient runtime capability surface

Date: 2026-07-20 Status: accepted

## Context

Runtime effects (`Diagnostic`, `Files`, `TimeFormat`, `SubmitNba`, `SubmitPostponed`,
`SubmitObserved`, `ScheduleAtTime`, ...) are declared on `RuntimeEffects`, the narrow surface a
generated body may reach. The concrete `Runtime` (a `final : public RuntimeEffects`) adds the host
orchestration methods -- `BindDesign`, `Run` -- that are not visible through the effects view. Every
emitted body reaches an effect through a MIR builtin call.

The previous shape emitted every access as `(self)->Services()`, where `self` was the callable
body's receiver. `self.Services()` was a generic `CallExpr` on a services builtin taking `self` as
its argument; the runtime realization threaded through a `Scope::Services()` method on the
receiver's C++ class.

This shape works for a body whose receiver is a `runtime::Scope*` -- a module process, a module
structural subroutine, an owned lifecycle hook. It fails for three body kinds that legitimately
exist in SV:

- **Package function / task** (LRM 26.3): no receiver. `frame.current_class` is null at lowering, so
  `MakeSelfRefExpr(frame, frame.current_class->self_pointer_type)` null-dereferences during
  HIR-to-MIR.
- **Plain SV class method** (LRM 8.6): receiver is a `C*` where `C` does not inherit
  `runtime::Scope` (only `is_scope_tree_node` classes do). Emit succeeds; the C++ compiler then
  refuses with `no member named 'Services' in 'C'`.
- **Class static initializer / static method** (LRM 8.9 / 8.10): no receiver, same failure mode as
  package function.

`mir.md` invariant 11 already admits receiver-less callables: "A type-associated (static) function
has no receiver and no `self` binding; the receiver is a property of an instance method, not of
every callable." The receiver-based scheme silently contradicted that invariant by requiring a
receiver for every runtime access.

The immediate motivation was an Ibex bring-up crash on `prim_cipher_pkg`'s intra-package call chain
and on `prim_secded_pkg::is_width_valid`'s nested `unique case`. Tactical patches (lazy-dispatch on
writeback subroutines; a `frame.current_class != nullptr` guard silently dropping the runtime
warning) kept Ibex compiling but left the bug class in place and deviated from LRM 12.5.4.

## Decision

**Runtime access is an ambient thread-local handle, not a receiver-borne value.**

Concretely:

1. `BuiltinFn::kCurrentRuntime` (zero argument, renders `lyra::runtime::current_runtime()`) replaces
   the previous receiver-consuming services builtin. Every runtime effect that previously chained
   through `(self)->Services()` now chains through `current_runtime()`.
2. `current_runtime()` returns `RuntimeEffects&`, reading a `thread_local RuntimeEffects*` the
   attached `Runtime` publishes for its full lifetime through a `CurrentRuntimeGuard` field.
   Lifetime is Runtime attachment, not per-execution boundary: elaboration phases, scheduler-
   dispatch windows, and host inspection code all see the same handle. See the header comment on
   `runtime::current_runtime()` for the full contract.
3. **Two-tier class hierarchy at the runtime library.** `RuntimeEffects` is a non-polymorphic base
   declaring only the capability surface generated code may reach: I/O sinks, scheduler verbs, time
   queries, ambient execution queries, deferred-check submit.
   `Runtime final : public RuntimeEffects` adds the host boundary -- `BindDesign`, `Run` -- that is
   not visible through the effects view. Runtime is the sole concrete implementation; the split
   expresses the capability boundary at the type level without a vtable.
4. **`Scope` carries no dynamic scheduler state.** `Scope`'s `Services()`, its stored services
   handle, its `processes_` vector, its `RegisterInitial` / `RegisterFinal` / `ForEachProcess`
   methods, and the receiver-based `AbiRegister*` shims are all removed. `Scope` contributes
   structural identity only (parent + segment + child scopes + registered signals + generated
   behavior handle).
5. **Process ownership moves to the runtime.** `Runtime::processes_` (primary
   `vector<shared_ptr<RuntimeProcess>>`) plus `Runtime::processes_by_scope_` (secondary
   `unordered_map<Scope*, vector<RuntimeProcess*>>`, kept in lockstep) hold every process. The
   `RegisterInitial` / `RegisterFinal` MIR builtins become namespace-qualified free functions
   (`lyra::runtime::RegisterInitialProcess(scope, coro)`) that push into that registry through
   ambient reach. The by-scope index answers the hierarchical queries (LRM 9.7 `disable`, LRM 9.6.1
   `wait fork` descendant walk, scope teardown, `%m` attribution) directly, without scanning the
   full registry.
6. **Design tree is a distinct type.** `Design { unique_ptr<Scope> root_ }` is the single nominal
   type for "an elaborated scope tree". `Runtime::BindDesign` takes `unique_ptr<Design>`, so a
   mid-tree `Scope` cannot be mistaken for the whole design. Tree-global traversal (`ForEachScope`)
   lives here rather than accumulating on `Scope`.
7. **Ambient execution identity is a single atomic step per resume.** `ProcessExecutionGuard` sets
   the ambient current process and its owning scope together -- publishing them as one pair.
   `ScopeExecutionGuard` sets only the current scope, for the extent of an elaboration walk that
   runs a body outside any process. The two-guard pattern (a separate process guard and a separate
   scope guard the caller had to remember to install together) is retired: a resume site cannot
   install a mismatched pair, and the ambient scope is always the ambient process's owning scope
   when a process is executing.
8. **`SubmitObserved` reads ambient current scope with no fallback.** Because every generated code
   entry point installs either a `ProcessExecutionGuard` (process resume) or a `ScopeExecutionGuard`
   (elaboration walk) before running a body, `current_scope` is always set when a runtime effect can
   fire. `SubmitObserved` reads it directly to key its per-scope coalescing map (LRM 16.14.6
   last-write-wins per `(scope, site_id)`); it does not need a root-attribution fallback.

`self` retains its role in `mir.md` invariant 11: the per-instance handle for instance-method
bodies. What it loses is its second job as the runtime-access route. The two concerns were
incidentally coupled under the module-only design; they are semantically independent.

## Rejected alternatives

- **Thread a runtime handle through every callable signature.** Every callable that transitively
  touches runtime would declare it as an explicit parameter; every call site would pass it in. Under
  this model, module bodies (previously `self.Services()`) and non-scope bodies would converge on
  "explicit callable input".

  Rejected because the runtime is a program-global singleton (one Runtime per program image,
  permanent design assumption per `north_star.md` inv 4/5 discussion). Threading a program-global
  through every call site is redundant plumbing that neither the caller nor the callee benefits from
  -- every caller would pass the same singleton value. The alternative also inherits the transitive
  needs-analysis problem `callable-receiver.md:113-123` rejected for `self`: adding a `$display` to
  a body would cascade into a signature change and force callers to recompile, violating incremental
  compilation (`north_star.md` inv 4).

  Always-include-runtime-handle (parallel to always-include-self) avoids the needs-analysis cascade
  but pays per-callable plumbing cost on every trivial helper that touches no runtime, and requires
  each of three backends (C++, LLVM/JIT, future) to implement the marshaling independently.

- **Give SV classes and packages a runtime companion object that carries services.** Bring plain SV
  class instances and package namespaces into the `Scope` hierarchy so `(self)-> Services()` would
  resolve everywhere. Rejected because it forces every SV class instance (a potentially
  heap-abundant object) into the runtime-tree representation, mixing the object-model layer with the
  scheduler layer -- exactly the coupling `runtime-effects-as-generic-calls.md` rejected under
  "Runtime ops as methods on `Scope`". Also introduces a synthetic runtime object for packages that
  today have none, purely to give the runtime handle a container.

- **Capability-typed `ProcessLowerer` split.** Model the six body kinds (module process, module
  structural subroutine, class method, class constructor, class static_init, package function) as
  separate lowerer types, each carrying only the ambient facts its body can use. Rejected because
  the observed bug class funneled through exactly two helpers (the services-call builder and the
  enclosing-scope lowerer) and the crash count did not motivate a type-system- level refactor. The
  concern -- lowering helpers reaching ambient state that some body kinds lack -- is real, but
  addressed adequately by localizing runtime reach into `current_runtime()` and by fixing the
  specific eager-deref site in the enclosing-scope lowerer.

- **Global variable, not `thread_local`.** Simpler by one qualifier; effectively equivalent under
  today's single-simulation-thread model. Rejected because `thread_local` is the honest general form
  (single-thread is its n=1 special case), matches the ambient pattern for current-process /
  current-scope publication, and pre-empts silent cross-thread corruption when a DPI-C callback runs
  on a foreign thread pool. Cost of `thread_local` on modern hardware (one segment-register
  indirection per access) is not measurable at scheduler granularity.

- **`RuntimeEffects` as a virtual polymorphic interface.** Give the capability surface a vtable so
  alternative concrete implementations (a `MockRuntime` for unit tests, a stable-ABI DPI-C boundary)
  can be swapped in. Rejected because there is exactly one concrete implementation (`Runtime`,
  declared `final`), runtime-effect methods are called on hot paths (`Now`, `SubmitNba`, `Files`)
  where a vtable indirection is a real cost, and no consumer today needs alternative dispatch.
  Non-polymorphic inheritance with a protected `Runtime` destructor gets the type boundary without
  vtable overhead; if a genuine multi-impl need arises later, adding `virtual` to the effect methods
  is an additive change to the header.

- **A single Runtime god-class, no `RuntimeEffects` split.** Generated code sees the whole
  `Runtime&`; `BindDesign` / `Run` are visible from an effect body. Rejected because ambient reach
  still leaves a capability-boundary question: an emitted body could call `BindDesign` or manipulate
  `queues_` if the type does not forbid it. The split has zero runtime cost and makes what generated
  code may vs may not touch a type-checkable fact.

- **`SubmitObserved` on `Runtime` with a single global Observed queue.** Simplest attribution model.
  Rejected because it drops the tree-order drain and the per-(site, scope) coalescing that LRM
  16.14.6 relies on for anti-glitch semantics; two module instances each running the same package
  function's unique check would collapse into one queue entry rather than firing independently.

- **`SubmitObserved` attributed to a synthesized package scope when the caller is package code.**
  Rejected because packages are naming / storage units, not dynamic execution owners. Two module
  instances calling the same package check would both attribute to one package pseudo-scope,
  breaking `%m` and multi-instance ordering. Dynamic attribution to the ambient current scope
  matches SV semantics: LRM 9.6 gives fork branches no separate scope, and the ambient identity for
  the executing body is the natural owner of its side effects.

- **`SubmitObserved` throws when no current process exists.** Cleaner contract; forces every
  deferred-check-emitting body to be inside a process. Rejected because LRM 10.5 admits
  `unique case` inside a variable initializer (which runs at Initialize phase, no process present).
  Under this alternative Ibex-style vendor code that puts unique/priority in an initializer would
  abort at simulation start; under the accepted rule the elaboration walk installs
  `ScopeExecutionGuard` around each per-scope body, so `current_scope` is set and the same code
  fires its warning in the first Observed region, matching the LRM 12.5.4 "shall issue a warning"
  requirement.

- **Per-generated-call-scope `CurrentRuntimeGuard`.** Publish the runtime only for the extent of
  each runtime-to-generated call, not for the Runtime's whole lifetime. Rejected because host
  inspection code and elaboration phases run outside a generated-call boundary yet still need the
  runtime; the per-call guard would either add overhead (a guard per call) or force every such site
  to install one manually. Runtime-attachment lifetime matches the ambient pattern for
  current-process / current-scope publication, and mirrors the existing `current_process` and
  `current_time` lifetime.

- **Independent `ProcessExecutionGuard` and `ScopeExecutionGuard` at every resume site.** The
  earlier shape had a resume site install two separate guards in sequence (one for the process, one
  for its owning scope). Rejected because the two identities are logically paired at a process
  resume -- the ambient scope IS the ambient process's owning scope -- and two separate guards let a
  resume site install only one, or install a mismatched pair. A single `ProcessExecutionGuard`
  publishes both atomically; a distinct `ScopeExecutionGuard` is the correct shape for elaboration
  walks where no process exists. Two-guard-at-every-site is a legacy of the receiver-based scheme
  where scope was reached through `self`; the paired-guard is the clean expression of "who is
  running this body".

## Consequences

- MIR: `BuiltinFn::kCurrentRuntime` (zero arg) is the runtime-access primitive.
  `mir::MakeCurrentRuntimeCallExpr(effects_type)` is the constructor. `mir::RuntimeEffectsType` is
  the type of the ambient handle; `builtins.effects` names it on `BuiltinMirTypes`. The
  `runtime-effects-as-generic-calls` decision holds: runtime access is still a generic `CallExpr`
  threaded into runtime free functions; only the source of the handle value moved from
  `self.Services()` to `current_runtime()`.

- Runtime library: `RuntimeEffects` declares the capability surface;
  `Runtime final : public RuntimeEffects` adds `BindDesign` / `Run` and holds every mutable
  simulation datum. `Design` owns the elaborated scope tree. `Scope` is a pure structural node (no
  scheduler state). `Runtime::processes_` + `Runtime::processes_by_scope_` hold every process, both
  indexes updated in lockstep on register / spawn. `ProcessExecutionGuard` publishes ambient
  process + scope atomically at each resume; `ScopeExecutionGuard` publishes only scope during
  elaboration walks. `CurrentRuntimeGuard` publishes on Runtime construction, restores the prior
  value on destruction so nested constructions in tests cannot leave dangling TLS.

- Backends: C++ renders `current_runtime()` as `lyra::runtime::current_runtime()` in the
  `lyra::runtime` namespace group. LLVM binds it to a zero-argument runtime symbol
  `lyra_rt_current_runtime`. Both backends translate mechanically with no per-body-kind branching.
  `RegisterInitial` / `RegisterFinal` render as free-function calls
  `lyra::runtime::RegisterInitialProcess(scope, coro)`, not as receiver methods.

- HIR-to-MIR: `BuildCurrentRuntimeCallExpr(unit)` (zero frame dependency) is the site every
  runtime-effect lowering calls to obtain the runtime handle. The receiver-based construction helper
  (previously reached via a frame's `current_class`) is retired. The MIR-side runtime scope ctor
  prefix no longer threads a runtime handle in; child-instance construction stops passing it; the
  JIT ABI's `lyra_rt_make_unit` drops its runtime argument; the emit shim `BuildRoot()` becomes
  zero-argument; `simulation_entry.hpp::RootBuilder` becomes `std::unique_ptr<Scope> (*)()`.

- Callable-receiver invariant: `callable-receiver.md`'s "every SV callable's first binding is
  `self`" rule stays intact for the callables it was written for -- instance-method-shaped bodies.
  It never held for package functions or static class methods; `mir.md` invariant 11 admits that.
  Runtime access is now decoupled from `self` entirely; a body that has `self` for its own
  per-instance state reads the runtime through `current_runtime()` like every other body.

- Ibex bring-up: the tactical `frame.current_class != nullptr` guard in `branches.cpp` (which
  silently dropped runtime warnings for package function unique/priority) is removed. The warnings
  now fire correctly, attributed via `RuntimeEffects::SubmitObserved` under the ambient current
  scope.

- Not addressed here: class `static_init` executed at C++ `dynamic-static-init` lifetime
  (pre-`main`, pre-Runtime). Static initializer bodies that transitively use the runtime still fail
  because there is no attached Runtime on any thread at that point. Fixing this requires moving
  static_init from C++ static-init-order lifetime into the runtime elaboration lifecycle
  (post-Runtime-construction, pre-initial). Orthogonal to the ambient-runtime mechanism; tracked
  separately.

## Cross-references

- `docs/architecture/mir.md` invariant 11 -- static functions have no receiver; the previous
  services-via-self scheme silently assumed otherwise.
- `docs/architecture/north_star.md` inv 4 / 5 -- incremental and parallel compilation as first-class
  constraints; the driving reason threading was rejected.
- `docs/decisions/callable-receiver.md` -- the explicit-`self` callable shape. Preserved for
  instance-method bodies; narrowed to what it was ever true for.
- `docs/decisions/runtime-effects-as-generic-calls.md` -- runtime effects as ordinary `CallExpr`.
  Preserved; only the runtime handle source changed identity.
- `docs/decisions/unified-callable-model.md` -- callable code and callable value split, `self` as
  `params[0]` for instance methods. Preserved; the runtime handle is not a per-callable fact.
- `docs/decisions/generated-behavior-boundary.md` -- generated-call scope as ambient dynamic context
  per runtime-to-generated boundary. Existing precedent for accepting ambient runtime state; this
  decision extends the same principle to runtime capability access.
