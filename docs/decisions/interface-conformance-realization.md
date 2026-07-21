# Interface-class conformance realization

## Date

2026-07-20

## Status

Accepted

## Why this decision matters

SystemVerilog interface classes (LRM 8.26) reach the object model as one concrete base plus a set of
interface conformances (`object_model.md` invariant 3). The C++ backend renders a conformance as
public inheritance of an abstract type, and relies on the target language's own virtual dispatch to
route each contract slot to the implementation the class provides.

That works when the class provides the implementation locally. It breaks for LRM 8.26.2 _inherited
satisfaction_ -- a class implements an interface whose pure-virtual method is satisfied by a method
inherited from its concrete base, with no local redeclaration. The interface's abstract type and the
inherited implementation land in sibling base subobjects, and the target language does not let one
sibling base override another's virtual, so the class stays abstract and a call through it is
ambiguous. A realization is required.

This entry pins two things: how inherited satisfaction is realized, and how much of the "which
method fills which dispatch slot" relation the IR states now versus later. The second was contested
and is the load-bearing part.

## Findings that shaped the decision

- **The frontend resolves conformance; it does not persist a satisfier map.** Slang's conformance
  check locates each interface method's implementation by name over the flattened base chain and
  validates the signature, but for a regular class it uses the result only for diagnostics and keeps
  no queryable interface-slot-to-implementation mapping. The resolution -- including the inherited
  case -- is available by re-running slang's own by-name lookup at translation time; the class it
  returns is already conformance-validated.
- **The backend renders; it does not fabricate.** A backend render entry is a fixed function of one
  IR node and never invents an expression from IR data (`backend_contract.md`). A forwarding call
  must therefore exist as a real IR expression produced during lowering, not be synthesized by the
  backend from a bare relation.
- **A synthesized method override is an established mechanism.** The post-construction lifecycle
  bodies are already synthesized overrides with no source form, rendered like any method
  (`object_model.md`). A synthesized realization method is not a special case.
- **The full slot-to-implementation table is layout, and no backend reads it today.** Which slot an
  implementation fills is class-type layout; the physical dispatch table is LIR/runtime
  (`object_model.md`, `unified-callable-model.md`). The C++ backend never builds a dispatch table --
  it recovers a method name and lets the target language dispatch -- so it never reads "which slots
  does this method fill." The one place the IR states a method's participation is its single-slot
  resolved dispatch role.

## The decision

**D1. Inherited satisfaction is resolved at AST-to-HIR and realized as a synthesized forwarding
method.** For each directly-declared interface, for each contract method a class does not satisfy
locally but does satisfy through an inherited concrete-base method, translation synthesizes an
ordinary method on the class that forwards to the inherited implementation, using slang's own
by-name resolution (the class it returns is already conformance-validated) to find that
implementation. The forwarding method's dispatch role names the interface slot it fills, and its
body forwards to the inherited implementation. The backend renders it as an ordinary method; it does
not fabricate the forwarding expression. Forwarding methods are deduplicated by (implementation,
signature): one inherited implementation satisfying several same-signature interface slots yields
one method, not one per interface. There is no distinct "adapter" entity, marker, or type -- the
forwarding method is an ordinary synthesized method, the same mechanism a lifecycle body uses.

**D2. The forwarding body is a qualified, non-virtual base call, a general primitive.** The
forwarding body is a direct, vtable-bypassing call to the inherited base method, qualified by the
base type -- possibly cross-unit. This is a general IR call form and the same primitive a
source-level cross-unit `super` call needs; it is built as a general capability, not a bridge-only
special case.

**D3. A method's dispatch participation stays the single-slot resolved role; the full "one method
fills these N slots" representation is deferred until a backend reads it.** The IR continues to
state a method's participation as its single resolved dispatch role, and interface-slot filling
continues to rely on the target language's implicit override across sibling bases. The richer
representation -- a method naming every concrete and interface slot it fills -- is deferred. Its
trigger is the physical-vtable backend (a backend that builds the dispatch table from the IR): at
that point `backend_contract.md`'s mechanical-backend cross-check forces the full relation, and a
consumer exists to exercise and test it. Until then, materializing it is write-only state no backend
reads and no test validates. This keeps inherited satisfaction symmetric with local multi-interface
satisfaction, which already ships and records the same single-slot role.

## Rejected alternatives

- **A bare conformance binding the backend realizes.** The IR carries only a contract-slot to
  implementation relation, and the backend emits the forwarding override from it. Rejected: the
  backend would fabricate the forwarding expression from IR data, the forbidden shape in
  `backend_contract.md`. The forwarding call belongs to lowering, not render.

- **A separate conformance record parallel to the satisfying method.** Rejected as asymmetric: local
  satisfaction (a class that defines the method) already carries the relation on the satisfying
  method's dispatch role, with no separate record. A parallel structure only for the inherited case
  splits one concept across two homes; the synthesized forwarding method carries the relation the
  same way a local satisfier does.

- **An origin marker distinguishing a synthesized forwarding method from a source method.**
  Rejected: it contradicts the one-callable model (`unified-callable-model.md`). A synthesized
  lifecycle body is already an unmarked ordinary callable; the forwarding method is another. No
  consumer needs to treat it differently.

- **Building the full method-to-slots representation now.** Rejected as unexercised state: no
  backend reads slot filling today (the C++ backend defers dispatch to the target language; the
  physical table is LIR/runtime), so the representation would be write-only and untestable until the
  physical-vtable backend exists. The shape is anticipated; only its timing is deferred, keyed to
  the backend that consumes it.

## Consequences

- Inherited satisfaction compiles and dispatches correctly, symmetric with local satisfaction; a
  further-derived class that overrides the satisfied method dispatches to its own override through
  every handle type.
- A general qualified, vtable-bypassing base-call primitive lands, also enabling source-level
  cross-unit `super`.
- The full method-to-slots dispatch representation remains an open, deliberately-deferred gap, keyed
  to the physical-vtable backend.

## Relation to existing decisions

- `object-model.md` -- interface conformance is a relation distinct from the concrete base, and an
  override is a resolved relation a backend never re-derives by name. The synthesized forwarding
  method's resolved dispatch role is that relation.
- `generated-behavior-boundary.md` -- lifecycle and SV-virtual dispatch share a representation but
  are separate concepts; the synthesized-override mechanism this reuses is the same one lifecycle
  bodies use.
- `cross-unit-class-translation.md` -- the cross-unit base the forwarding method calls is resolved
  by the boundary translator this decision's forwarding call names.
- `unified-callable-model.md` -- the vtable slot-to-implementation mapping is class-type layout, not
  callable identity; the single-callable concept is why the forwarding method carries no origin
  marker.
