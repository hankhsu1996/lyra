# Unified callable model

Date: 2026-06-24 Status: accepted as the target model; not yet implemented (gap tracked in
`../progress/refactor.md` R8 and the object-model entry).

## Context

MIR carries four callable shapes as separate constructs: `Process`, `MethodDecl` (covering both SV
functions and tasks), the constructor (a bare body block on the class), and `ClosureExpr`. Their
bodies are all the same `Block`, yet the same fact is encoded several different ways -- most visibly
"is this a coroutine," which is implicit-and-always for a process, a `MethodKind::kTask` enum for a
method, and "the result type is the coroutine type" for a closure. By this layer's own rules a field
that restates what the structure already fixes is a defect (`mir.md`), and a backend must never
re-derive a fact MIR did not state (`mir.md` invariant 10). The split violates both.

The reduction this decision commits to: a callable is **one concept**, and the apparent variety is
either its _type-level signature_, its _environment binding_, the _referencing site's_ use of it, or
_backend realization_ -- never a `kind` tag on the body. The discipline that keeps the reduction
honest is a placement test (below): every per-callable-looking fact must be assigned exactly one
home, and three of the four homes are by definition _off_ the callable body.

This model is the conceptual peer of how Rust, C++, and Python treat callables: a function item is
code; a closure is code plus a captured environment; "async" is the return type (`async fn f() -> T`
is `fn f() -> impl Future<Output = T>`), not a separate kind of function.

## The placement test (the governing discipline)

Every fact that looks like it belongs to a callable is placed in exactly one of four homes. The
first two are structure _on_ the callable; the last two are _off_ it.

1. **Intrinsic signature** -- the callable's type-level identity: its parameter list (each
   parameter's _type_, where the type encodes value-versus-reference) and its result type (which
   encodes the call protocol and the returned data). This is what a first-class callable _type_
   carries.
2. **Environment representation** -- how the body reaches state around it: a directly-invoked
   callable receives its receiver and arguments from the caller; a callable _value_ (closure, bound
   method, registered process) owns its bound environment.
3. **Invocation-site policy** -- how a referencing site uses it: call-and-await, fork with a join
   mode, submit to a region, construct, dispatch statically versus virtually, register for a
   lifecycle phase. Lives on the referencing node and on class-level layout/plan structure.
4. **Backend realization** -- how a target mechanically emits it: calling convention, value
   marshaling, the param-versus-capture realization of a bound environment. Either desugars to
   ordinary MIR primitives at HIR-to-MIR, or is a backend-side table keyed by the called symbol.

The test that condemns a design: if a fact in homes 3 or 4 appears as a field on a callable body,
the `kind` enum has merely moved. The information in `MethodKind` / `ProcessKind` does not vanish
under this model; it redistributes to where it is _read_ -- the call protocol into the result type
(read by the call site to decide `await`), the lifecycle phase onto the registration (read by the
scheduler) -- and none of it returns to the body.

## Decision

### A callable is code; a callable value is code plus a bound environment

Two structural concepts, not one:

- **Callable code** (a declaration): a signature plus an implementation form, which is either an
  **internal body** (a `Block`) or an **external symbol** (a foreign declaration with no body; see
  DPI below). The receiver `self` is the code's first ordinary parameter -- a code-level receiver,
  not a special slot.
- **Callable value**: a reference to callable code plus a **bound environment**. A closure
  constructs a callable value with a captured environment; a registered process is a callable value
  that binds the instance receiver; a stored/bound method is a callable value that binds its
  receiver. Whether a bound environment field is a snapshot or an alias is the field's _type_ (a
  value type versus a reference type), never a separate capture-kind axis.

A _direct_ call references code and supplies every parameter (including `self`) per invocation; it
constructs no value. A callable value materializes only where a callable is deferred, stored,
forked, submitted, or otherwise invoked later by a site that does not supply the environment. This
is the same axis the call-shape unification draws between a direct named-symbol callee and an
indirect computed-callable callee.

`self` as `captures[0]` is removed: `self` is a code-level parameter that a callable value _binds_
into its environment when needed. Whether a bound `self` is realized as a passed parameter (a
directly-spawned process, whose instance outlives every invocation) or a by-value captured field (a
closure that outlives the frame that built it) is a backend realization choice driven by lifetime --
home 4, not a MIR distinction. A process is, at the MIR level, a closure whose environment binds
only `self`.

### The result type is the call protocol, and it is parameterized

A callable's result type is the sole carrier of how the callable is entered and completed. A
coroutine result type means "entered and completed through the coroutine protocol" -- the invocation
yields a coroutine object the site must `await` or `spawn`. The result type is **parameterized**:
`Coroutine<T>` carries the value the completion yields, so a task that produces outputs has a result
type with a real payload (see parameters, below), not a content-free marker.

Two facts are deliberately **not** the result type:

- "The body can suspend" is a body/effect-analysis result used for validation or optimization, not
  the definition of the result type. A `final` block runs in zero time and never suspends, yet is
  entered through the coroutine protocol so the scheduler holds a uniform handle; its result type is
  a coroutine type because of the _protocol_, not because the body suspends.
- "Returns a value versus void" is the result type's payload (`Coroutine<void>` versus
  `Coroutine<T>`, or a plain value versus void for a synchronous callable), not a separate kind.

`MethodKind` and the process's implicit always-coroutine are removed: function-versus-task and
process-coroutine-ness are all the result type.

### Parameter direction is data flow, not a direction enum

SV parameter directions normalize fully into the signature's data flow, with no `ParamDirection`
enum and no callable-level field:

| SV formal     | Normalized MIR contract                              |
| ------------- | ---------------------------------------------------- |
| `input T`     | a parameter of type `T`                              |
| `ref T`       | a parameter of type `Ref<T>` (a true live alias)     |
| `const ref T` | a parameter of type `Ref<const T>`                   |
| `output T`    | a component of the result's output pack              |
| `inout T`     | an input parameter `T` plus an output-pack component |

`output` / `inout` are **not** modeled as aliases. SV `output` / `inout` copy out at task
_completion_, not on each write; modeling them as a live `Ref<T>` would let a caller observe an
intermediate write while a task is suspended, which is the wrong semantics. Instead the formals are
ordinary body locals, and the callable's result carries an **output pack** (a tuple, an existing MIR
primitive) assembled at every return. The call site receives the pack after completion and writes
each component to the caller's actual place:

```text
task t(input int a, output int b, inout int c);
  -- normalized contract:
  t : (self, int a, int c_in) -> Coroutine<Outputs{ b: int, c: int }>
  -- call site:
  outputs = co_await self.t(a, c_snapshot)
  b_place := outputs.b
  c_place := outputs.c
```

This makes copy-out timing correct by construction (the writes happen after completion), keeps
copy-out ownership at the call site (the caller owns the actual place -- home 3, not the body),
needs no per-exit-path copy-out epilogue (the output pack rides the existing return mechanism), and
is the concrete reason `Coroutine<T>` must be parameterized rather than a marker. Only `ref` is a
true alias and therefore the only direction that becomes a reference-typed parameter. (Lowering
details -- a function with outputs in expression position, and `output` under an abnormal `disable`
termination -- are HIR-to-MIR concerns, not part of this contract.)

### Internal versus external callables: DPI is a structural form

A callable's implementation form is a sum, exactly mirroring how an object is either intra-unit or
external-unit:

- **Internal**: a body plus an environment model.
- **External**: a foreign-symbol declaration -- a signature plus a linkage name, foreign language,
  and calling convention -- with no body. A DPI import is this.

This is a legitimate structural variant, not a redundant discriminator: it changes whether a body
exists, how the symbol resolves, and where the implementation is produced. The external **symbol
contract** (linkage name, language, calling convention, import/export identity) is explicit MIR
structure on the external declaration, because more than one backend must read the same fact and
none may re-derive it (`mir.md` invariant 10). Only the mechanical value marshaling is backend
realization (home 4). A DPI export is an ordinary internal callable plus an export registration.

### Lifecycle registration is per-instance and constructor-time

A process is not a class-level static list. Because generate lowers to constructor-time control flow
(`mir.md` invariant 3), a process registration can depend on generate conditions, generate-for
multiplicity, and instance-specific construction. The scheduler's truth is therefore a
**per-instance, instance-bound activation**, produced by constructor-time code, exactly where child
objects are constructed. A class may hold a registration _descriptor_; what the scheduler ultimately
holds is an instance-bound callable value created during construction.

A finalization (`final`) hook is structurally distinct from a startup (`initial`) registration, not
another value of one "phase" enum: a startup registration is instance lifecycle setup, a
finalization registration is a shutdown hook. They are separate lifecycle registration kinds. (This
registration-level distinction is invocation-site policy -- home 3 -- and is legitimate; it is not a
classification on any callable body.)

### Virtual dispatch (forward-looking, gated on the object model)

When SV classes land, virtual dispatch is an explicit MIR distinction at the call site -- a virtual
invocation naming a receiver and a slot, distinct from a direct invocation naming a callee -- so no
backend re-derives "is this virtual" from the receiver's type. The vtable slot-to-implementation
mapping is a property of the **class type layout**, not of any callable: which class, hierarchy, and
slot use an implementation is layout, not callable identity. A vtable (runtime object type layout)
and a lifecycle registration (instance scheduling plan) are different structures and are kept
separate. This subsection is the target shape only; it is gated on the object-model design below.

## Deferred: the object model is the gating prerequisite

This decision settles the callable contract. It does **not** settle the object model, which is a
separate design and the prerequisite for SV classes and virtual dispatch. The MIR concept currently
named a "class" is a compiled module/scope, carrying module-specific structure (an elaboration and
construction graph, owned children, generated members, lifecycle registrations). An SV class needs a
different set of capabilities (heap allocation, handles and null, inheritance, dynamic dispatch,
object identity, reference-lifetime policy). The likely factoring is an `ObjectType` (fields,
methods, and a dispatch/virtual layout where applicable) shared by both, plus a separate
module-specific instance/elaboration plan -- "a module is an object type plus an instance plan," not
"an SV class is a module in another mode" -- so that module construction policy does not contaminate
heap classes. Designing this is a separate effort tracked in `../progress/refactor.md`; the callable
model rides on its result.

## Rejected alternatives

- **`output` / `inout` as reference-typed parameters with body copy-in/copy-out.** Semantically
  wrong for a suspending task: a live alias exposes intermediate writes during suspension, and would
  also copy a caller value _into_ an `output` formal (SV `output` has no copy-in). The output-pack
  return form fixes copy-out timing, the no-copy-in of `output`, and the abnormal-termination case
  uniformly. Only `ref` is a genuine alias.

- **A flat, payload-less coroutine type as the end state.** A single classification _can_ be a type
  (so is `bool`), so the objection is not "one bit is not a type." The objection is that a
  payload-less coroutine type cannot express a completion contract that yields outputs; once
  `output` / `inout` normalize to an output pack, `Coroutine<T>` is a concrete requirement, not
  aesthetic purity.

- **"Callable = body + captures" without the code/value split.** Treating a closure as a peculiar
  method declaration conflates a code declaration with a runtime value, forces the `self`-as-first-
  capture special case, and prevents one body from having different bound environments. Separating
  callable code from callable value removes all three.

- **Process registration as a static class-level `(callable, phase)` list.** Cannot express the
  per-instance, generate-dependent registrations that constructor-time elaboration produces. The
  registration is constructor-time, instance-bound work.

## Consequences

- `Process`, `MethodKind`, `ProcessKind`, `ParamDirection`, and `self`-as-`captures[0]` are removed.
  Their information redistributes to the result type (call protocol), the signature (data flow),
  class-level layout/plan structure (vtable, lifecycle registrations), and backend realization.
- `Coroutine<T>` becomes parameterized.
- MIR gains a first-class callable type (the type of a callable value) and an internal/external
  implementation-form sum.
- The C++ backend and the dumper each render one callable concept (code, plus the separate
  value-construction path for closures) instead of four.
- The SV-class object model is the next design; virtual dispatch and any heap-class callable detail
  are gated on it.

## Cross-references

- `architecture/mir.md` -- invariant 10 (no re-derivation), the no-redundant-field rule, and the
  Callables enumeration this decision recasts.
- `architecture/callable.md` -- the canonical callable contract; the capture model is part of it.
- `architecture/scheduling.md` -- processes and tasks as coroutines; the MIR place model.
- `decisions/callable-receiver.md` -- the prior `self` model this decision moves from a per-form
  receiver to a uniform code-level parameter.
- `decisions/address-of-primitive.md` -- MIR's place model (the reference class is Rust MIR / LLVM
  IR), which supports reference-typed `ref` parameters.
- `progress/refactor.md` -- R8 (the implementation gap) and the object-model design entry.
