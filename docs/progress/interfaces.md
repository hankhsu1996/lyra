# Interfaces and modports

Tracks SystemVerilog `interface` (LRM 25): the interface as a compilation-unit kind, the interface
port that binds a module to an interface instance, the `modport` view that restricts and renames
what that port reaches, subroutines called across that boundary in either direction, and the virtual
interface that carries an instance in a variable.

Done when a module written against an interface, or against one of its modports, behaves as the LRM
requires: a member reached through the port reads, writes, and re-triggers a dependent process; a
modport restricts and renames what the port sees; a subroutine crosses the boundary in both
directions; and a virtual interface carries an instance through procedural code and class
properties.

The stage IDs (A1, B1, ...) are stable references. Stage letters **do** imply dependency order: a
later stage may not begin until what it depends on is settled, and the section below says which
items that is, since a stage rarely waits on all of another. Within a stage the items are not
ordered.

## Contracts

This workstream reasons from these architecture docs and does not restate them:

- `../architecture/compilation_unit_model.md` -- an interface is a compilation-unit kind alongside
  module and package, compiled from its own contents and reached by other units only through its
  interface of name and signature.
- `../architecture/reference_resolution.md` -- a reference is a route of segments, each classified
  by whether the emitting artifact owns its layout; ports, hierarchical references, and
  cross-instance sensitivity share one route mechanism.
- `../architecture/elaboration_lifecycle.md` -- connections are declarative facts at Build, routes
  execute at Resolve, endpoints commit at Seal, and nothing before Seal observes a binding.
- `../architecture/object_model.md` -- one nominal object type serves a module instance, a generate
  scope, and a class; the reference reaching an instance carries its lifetime, orthogonal to the
  object's category.
- `../decisions/hierarchical-reference-routing.md` -- one semantic shape for every hierarchical
  reference, per-segment classification, and the open target-category family this workstream
  extends.
- `../decisions/reference-as-data-type.md` -- a `ref` is a direction at HIR and an aliasing data
  type at MIR, filled once at Resolve, with one storage and no propagation delay.
- `../decisions/unit-signature.md` -- an interface publishes its members, so reaching one through a
  port is a name resolved where the referrer compiles rather than one the runtime answers while the
  design elaborates.
- `../decisions/interface-port-binding.md` -- what the port's declared type names, what its member
  holds, and why the interface a port carries is part of the module's specialization identity.
- `../decisions/published-member-placement.md` -- a published member sits at the position its
  signature states, derived independently by the declaring unit and by every referrer, so how many
  positions one member occupies is settled there rather than here.
- `../decisions/instance-array-multiplicity.md` -- a declaration standing for several objects is one
  member whose type carries the multiplicity, which is what an interface port carrying a range is
  over a borrowed pointer.
- `../decisions/specialization-identity.md` -- which interface a port carries is one of the
  selections a unit's identity is made of, read where the parent fixed it, so two connections naming
  different interfaces are two units and the frontend's own grouping of instances decides neither.

Stage B onward depends on the workstream tracked in `unit-signature.md` under this directory. An
interface exists to be written against, so its members are its signature; reaching them through a
port by a run-time query is the shape that decision forbids. Building the port before the signature
artifact exists would mean building it twice.

## Dependency order

```
A  The interface as a compilation unit
   |
B  The interface port
   |
   +---- C  Modports
   |        |
   +--------+---- D  Subroutines across the boundary
   |
   +---- E  Virtual interfaces
```

- A gates everything: until an interface instance exists in the object tree with its own parameters,
  ports, members, and processes, there is nothing for a port to bind to.
- B gates C, D, and E. All three reach an interface instance the module does not own, which is what
  the port's handle establishes; a modport is a view over that handle, an imported subroutine is a
  call through it, and a virtual interface is the same handle held in a variable. What they wait on
  is that handle and the route through it, which B1 and B2 settle; how many instances one port names
  is a separate question, so B7 blocks none of the three.
- C and E are independent of each other.
- D depends on the cross-unit subroutine endpoint, which is not interface-specific and is tracked in
  `hierarchy.md` (D9); the modport `import` and `export` forms are that endpoint reached through a
  port.

## Sub-steps

### Stage A -- The interface as a compilation unit

- [x] A1 -- An interface declaration is a compilation unit of the same kind as a module: it declares
      parameters, ports, variables and nets, subroutines, child instances, continuous assignments,
      and processes, and it is instantiated into the object tree as an owned child (LRM 25.2, 25.3).
      Its body needs no vocabulary a module's body does not already have; what makes an interface
      distinct is how other units reach it, which is Stage B onward.
- [x] A2 -- An interface's own port list connects like a module's, with the same syntax and
      semantics (LRM 25.4, 23.2.2): an `input` port reflects the enclosing scope's source
      continuously, an `output` port propagates a write outward, and a connection may be an
      expression or a constant. Only the nets and variables in the port list are connectable from
      outside by name or position; the rest of the interface's members are not.
- [x] A3 -- Interface parameters steer the instance the way module parameters do (LRM 25.8):
      distinct bindings yield distinct specializations, and a parameterized width reaches the
      interface's own declarations and its subroutine signatures.
- [x] A4 -- An interface is instantiated in every position a module is: several times, as an
      instance array, inside a generate block, and inside another interface (LRM 25.3). A module may
      be neither declared nor instantiated inside an interface, and an interface is never implicitly
      instantiated, both of which the frontend enforces.
- [x] A5 -- A hierarchical reference from a scope that can name the interface instance reads and
      writes its members, at any depth and through array indices, and a dependent process
      re-triggers when a member changes. This access is available regardless of whether the
      interface is also reached through a port, and regardless of which modports the interface
      declares (LRM 25.10).

### Stage B -- The interface port

- [x] B1 -- A module port whose type is an interface names an interface instance that lives
      elsewhere in the object tree. The port member is a non-owning handle to that instance's
      runtime scope, bound once during elaboration; it owns no storage of its own and introduces no
      propagation delay. This is the scope-level counterpart of a `ref` port, and it is a scope
      binding rather than a value connection: nothing is copied in either direction, and there is no
      reactive edge to arm.
- [x] B2 -- Every member access through the port is one reference whose route begins at that handle:
      the typed step to the port member, then the member at the position the interface's signature
      gave it, so a name the interface does not publish fails where the module compiles rather than
      while the design elaborates. Reads, writes, and change observation ride that single route, so
      a process in the module re-triggers when an interface member changes, and a write through the
      port is immediately the interface's value.
- [x] B3 -- The actual on the connection is an interface instance named in the instantiating scope,
      whether that is a local instance, an element of an interface array, or an instance reached by
      hierarchical name. LRM 25.3 forbids the hierarchical form from resolving through an arrayed
      instance or through a generate block, which the frontend enforces.
- [x] B4 -- A pass-through interface port: a module forwards its own interface port into a deeper
      child, so every port on the chain denotes one interface instance. This is a forwarding chain
      collapsed at sealing, and it is the reason the binding cannot happen while the subtree is
      being constructed.
- [x] B5 -- A generic interface port (LRM 25.3.3) leaves the interface unnamed in the module header
      and selects it at the instantiation site. It is the same handle reached by the same route;
      only the declaration is untyped. The LRM admits it in ANSI-style headers only and requires a
      named port connection to reach it, so an implicit connection is rejected.
- [x] B6 -- An interface port connection on an instance array binds each element's own handle (LRM
      23.3.3.5), whether one interface instance is replicated to every element or an interface array
      is mapped element to element.
- [x] B7 -- An interface reference in a module header carries a range, so one port names as many
      interface instances as the range has elements, the connection supplies an array of interfaces
      of that size, and selecting an element of the port reaches that one instance (LRM 25.3,
      23.2.2). The port is one published member whatever its multiplicity, which its type carries;
      the connection binds every instance it supplies at once, in the order the port's coordinates
      count them; and a name selecting an element spends the declared range where it is resolved, so
      everything below reaches an element by position.
- [ ] B8 -- A name continues past the port into what the interface itself owns: an interface may
      instantiate another interface (LRM 25.3), and a port bound to the outer one reaches the inner
      instance's members and enables its subroutines through that port. What it reaches is the inner
      instance belonging to whichever outer one the port was bound to, so the reach is the port's
      and not a name on the elaborated hierarchy -- which is why it refuses today rather than
      resolving to the instance one binding happens to name.
- [x] B9 -- Two instantiations of one module whose interface ports carry different interfaces
      compile to distinct units, at any multiplicity. What a port carries is settled by the
      connection (LRM 25.3), so a port declared with a range settles it exactly as one without a
      range does, and a module reached through two different interfaces is two members of a family
      rather than one unit serving both. Each compiles against the interface its own connection
      named, so a member reached through the port is the one that interface declares at that
      position.

### Stage C -- Modports

- [x] C1 -- A modport names a directional view of an interface, declared with the directions seen
      from the module that uses it (LRM 25.5). Selecting one -- in the module header, in the port
      connection, or in both, where the two names must agree -- changes which of the interface's
      members the port may reach and in which direction, and changes nothing about how a member is
      reached: the handle and the route are the modport-free ones. A port with no modport selected
      reaches every net and variable in the interface with direction `inout` or `ref`.
- [ ] C2 -- A modport expression gives a port identifier its own meaning inside the interface (LRM
      25.5.4): a part-select, an element, a concatenation, an assignment pattern, or a constant. The
      module's access through that identifier reaches the part of the interface's storage the
      interface named, so the route's leaf carries a projection the referrer did not write. Port
      identifiers live in their own name space per modport, and a modport port declared with no
      expression connects to nothing internal and is legal.

### Stage D -- Subroutines across the boundary

A call on a port with no modport selected belongs to this stage as much as an imported one does: LRM
25.7 makes every subroutine an interface declares callable through a plain port, and the `import`
form states which of them a restricted view offers. What separates the two directions is which side
declares the subroutine: D1 is a call this module makes on the interface, and D2 and D3 are calls
the interface makes on a module connected to it, which is why the second half does not follow from
the first.

A subroutine a hierarchical name enables (LRM 23.6) does not follow from it either, and the reason
is the unit boundary rather than the call: an interface promises its whole declared surface, so a
name on one is resolved where the caller compiles, while a module promises only its ports, so a
subroutine of one is reached -- if at all -- by a name the runtime answers while the design
elaborates. That form is refused, and it is tracked with the hierarchical-reference target forms in
`hierarchy.md` rather than here.

- [x] D1 -- A modport `import` makes an interface subroutine callable through the port, so a call on
      the port identifier enables that task or function on the bound interface instance (LRM 25.7).
      A port naming no modport reaches every subroutine the interface declares, and a scope that
      owns the instance enables one on it by hierarchical name. A task suspends its caller until it
      completes and an `output` formal is copied back at that completion, across the boundary as
      within one scope. Not yet: the prototype form of an import, which a modport needs where
      default argument values or binding by name are used, and whose argument number, types, and
      directions must match the declaration.
- [ ] D2 -- A modport `export` inverts the direction: the module connected through that modport
      defines the subroutine, and the interface -- or another module reaching it through the
      interface -- calls it. A connected module that does not define an exported subroutine, or
      defines one whose signature does not match the prototype, is an elaboration error (LRM 25.7).
- [ ] D3 -- `extern forkjoin` admits several modules exporting one task name (LRM 25.7.4): a call
      through the interface instance runs every definition concurrently as a fork-join block, and a
      call when no connected module defines the task reports a run-time failure of the design and
      returns with no effect. A `disable` naming the task through the interface instance disables
      every call; naming it through a module instance disables only that one. Functions may not be
      exported more than once, because a function always writes its result.

### Stage E -- Virtual interfaces

- [ ] E1 -- A virtual interface variable holds an interface instance (LRM 25.9). It is the same
      non-owning handle the interface port carries, held in a variable rather than a port member, so
      it is assignable during simulation and holds `null` until it is assigned. Assignment accepts
      an interface instance of the same type, another virtual interface of the same type, and
      `null`; equality and inequality compare against the same three. Using a null virtual interface
      is a failure of the simulated design and is reported as one.
- [ ] E2 -- A member access through a virtual interface resolves against the handle the variable
      currently holds. This is the one reference whose target is not fixed during elaboration --
      selecting the instance at run time is the construct's purpose -- so it resolves by name at
      access rather than reading an endpoint sealed once. LRM 25.9 confines such access to
      procedural statements: a virtual interface member appears in no continuous assignment and no
      sensitivity list, and a net is driven through one only by a procedural means the interface
      itself provides.
- [ ] E3 -- A virtual interface's type includes the interface's actual parameter values and,
      optionally, a selected modport (LRM 25.9). Assignment requires the parameter values to match;
      an instance or virtual interface with no modport selected may be assigned to one with a
      modport selected, and never the reverse. An interface carrying hierarchical references outside
      its own body, or ports that reference other interfaces, may not be used in a virtual interface
      declaration.
- [ ] E4 -- A virtual interface is passed as a subroutine argument, declared as a class property,
      and initialized from a constructor argument, which is what lets one transactor drive any
      instance conforming to the interface (LRM 25.9). It is never a port, an interface item, or a
      union member.

## Open questions

- The reference model leaves its set of sealed-endpoint target categories open and requires a
  decision entry per category (`../decisions/hierarchical-reference-routing.md`, D5). Neither Stage
  B nor Stage D introduced one. A member reached through a port ends at a member another unit
  published, which is the endpoint a downward port connection already seals to; a call on an
  interface ends at the object, which is an endpoint that already exists, with the callable named
  against what that unit published. Both are recorded in `../decisions/interface-port-binding.md`
  and `../decisions/calling-a-subroutine-on-another-units-object.md`.
- Stage E's access resolves per access rather than against an endpoint sealed once, because the
  target is chosen at run time. Whether the resolution memoizes per handle and name, and where such
  a cache lives so that it is a cache and not a second authority, is open.
- A modport expression (C2) states a projection at the interface rather than at the referrer, and
  that is what separates it from the rest of Stage C. A modport port written as a plain name is the
  interface item's own name used twice, so a referrer resolves it against what the interface
  published and needs to know nothing about the modport. One written as an expression renames a
  shape instead, and that name is on no signature, so the interface has to publish its modports for
  a referrer to resolve it at all. What such a view carries per port -- a published member, a
  projection of one, a concatenation, a constant, or nothing -- is open, and the answer decides
  whether a signature ever carries an expression across the unit boundary.

## Out of scope

- Clocking blocks, and with them modport clocking (LRM 25.5.5), virtual interfaces reached through a
  clocking block, and virtual interface modports selecting one (LRM 25.9.1, 25.9.2). No clocking
  block is supported in any scope; the construct is its own workstream, and every interface-side use
  of it waits for that rather than blocking this one.
- `program` blocks (LRM 24) and their interface ports. A program is a compilation-unit kind with its
  own scheduling region and its own termination semantics; interface support neither depends on it
  nor delivers it.
- Interfaces used as terminals in specify blocks (LRM 25.6). Specify blocks belong to the timing
  domain, which has no support in any scope.
- `inout` on an interface's own port list. It is a bidirectional net connection in the deferred
  net-connectivity domain tracked in `nets.md`, not a reference, and it is deferred there for every
  compilation-unit kind rather than for interfaces in particular.
- `defparam` reaching a parameter of an interface instance or its hierarchy. LRM 25.3 and 25.9 both
  carve out restrictions for it; `defparam` itself is unsupported, so the restrictions have nothing
  to constrain.
- An interface port on the module a simulation is run from. An interface port may not be left
  unconnected (LRM 23.3.3.4) and an interface is never implicitly instantiated (LRM 25.3), so
  nothing instantiates a top to connect one and the simulation has nowhere to begin. The module
  itself is supported, and is reached by instantiating it from one that owns the interface. Checking
  such a module reports what it always did; running or emitting one is refused with a diagnostic. A
  `ref` port is refused at a top under the same rule and is not particular to interfaces, so
  `hierarchy.md` carries it.
- Assertions, properties, sequences, and coverage declared inside an interface. They are legal
  interface items, and each follows its own workstream: what an interface adds is only that they
  appear in a unit of this kind.
- Extending the corpus for an interface reached from a `bind` directive or a configuration. Both are
  out of scope in `hierarchy.md` for modules and stay out here.
