# Specialization Identity by Content-Addressed Binding Hash

## Date

2026-06-23

## Status

Accepted

## Why this decision matters

A parameterized module instantiated with different bindings must behave according to each one's own
parameters, and where a binding decides what is compiled, compile to distinct artifacts that do.
Today a compiled unit's identity is the bare module name, so two specializations of one module (e.g.
`Reg #(.INIT(3))` and `Reg #(.INIT(7))`) carry the same name; emitting more than one collapses them
and the last one wins. This record fixes how a specialization is identified, named, and reached
across the unit boundary. It binds every parameterized instantiation and constrains how instances
handed different values share one unit.

## Findings that shaped the design

### F1. The identity must be a function of what the parent selects, not of the body

A cross-unit reference is resolved by the referrer, which cannot see the target's body: a unit
compiles against another unit's interface (name and signature), never its body or internal layout
(`compilation_unit_model.md` inv 8, `reference_resolution.md`). So when a parent constructs a child,
it must be able to name which specialization it wants from what it has -- which definition it
instantiates (F8) plus what it selected at the instantiation site -- without inspecting the child's
compiled code.

Consequence: the identity is `f(definition, selections)`, never `f(body)`. Fingerprinting the
lowered body is ruled out, because the consumer that must compute the same identity has no access to
it.

Two kinds of selection meet that test today. A parameter binding is one. An interface-port
connection (LRM 25.3) is the other: the parent names which interface instance the port carries, and
the module compiles against that interface's types at that interface's published positions, so two
connections naming different interfaces build different objects. Both are visible at the
instantiation site and neither requires reading the child.

### F2. The identity is a name, computed independently by both sides

Cross-unit access is by name against the interface; a design-global ordinal, coordinate, or table
that two units share to refer to each other is forbidden (`compilation_unit_model.md`,
`emission_model.md`, `identity_and_ownership.md`). The producer (the unit naming itself) and the
consumer (the parent naming the child it constructs) therefore compute the same name from the same
bindings by the same deterministic function; they agree with no global view. This rules out an
opaque key carried in a design-global map (the shape the previous iteration used).

### F3. A binding is one of two recursive structures, encoded structurally

A parameter binding is either a value -- slang resolves it to a `ConstantValue` of any type,
including unpacked aggregates (LRM 6.20.2) -- or a type -- a data type, recursive, including a
parameterized class with its own bindings (LRM 6.20.3). The identity encodes the structural content
of these, and excludes arena ids, source names, and source spans, because identity follows structure
not position (`identity_and_ownership.md`) and a fingerprint captures semantic meaning not spelling
(`incremental_build.md` inv 3). Two bindings alike in structure encode alike, a declared type
counting as the declaration it is (decision 2); an arena id, which is a unit-local insertion
position, is never part of the encoding.

### F4. A correctness-bearing equivalence may not be borrowed from a component that computes one for another purpose

`specialization_model.md` makes concrete elaboration the correctness baseline and sharing an
optimization admitted only when proven behavior-preserving. What supplies that proof is the question
this finding answers, because the frontend computes an equivalence over the same instances -- which
of them need not be elaborated twice -- and that answer is available for nothing.

It is not a proof. It answers a question about elaboration work rather than about compiled behavior,
it is free to be coarser or finer than this one, and it may change between releases. Taking it as
this compiler's answer leaves the baseline in the keeping of another component's optimization, and
the failure that admits is the intolerable direction: an artifact standing in for an instance whose
behavior differs from the one it was built for.

So the frontend's grouping is not an input to identity, and the artifact for a specialization
compiles a body elaborated for an occurrence in it. A frontend that splits more finely than this key
then costs nothing at all, and one that merges more coarsely costs the elaboration it meant to skip,
which is the price of the correctness baseline rather than a condition to handle.

The divergence is not hypothetical, and the shape it takes is worth carrying. A module whose
interface port carries a range, instantiated against two differently parameterized interfaces, is
marked a duplicate by the frontend while the two reach different types at the same member positions
-- so the relation that looked safe to borrow was already wrong on a construct ordinary RTL writes,
which is how a bundle of identical links between blocks is spelled.

Skipping the reading of an occurrence the frontend marked a duplicate is a different thing from
borrowing its relation, and a compile does it. It skips one only where its own comparison of what
the instantiation fixes finds the two one application, and takes for it what the body it duplicates
states. What holds that to this finding is a second lowering of every corpus design with every
occurrence read through its own body, which must agree with the first. Each divergence found that
way -- the interface range above, a real parameter compared as a number, a subroutine reached by an
upward name -- was also wrong as the frontend's own answer, and was closed there or covered by a
condition here.

### F5. The selections and the body come from the same instantiation

A body's contents are elaborated under some particular set of fixed selections: the types of the
expressions inside it are the types those selections produce. An artifact is therefore a pair -- the
selections fixed at an instantiation, and a body elaborated under those selections -- and both
halves have to come from the same one. An identity computed from one instantiation over a body
elaborated under another describes nothing that exists, however correct each half is on its own.

### F6. An identity has to settle everything below the instance, and where it stands is part of that

A unit's code names the class of every instance it builds. So two instances are one unit only if
everything below them is alike too, and an identity is sound only when it settles the identity of
every instance below. The comparison every instance is held to checks exactly this, and stops the
build on a legal design wherever it fails.

In C++ and Rust it holds with no effort: an instance is a definition and its arguments, the body is
a function of those, and what a body instantiates is found by substituting them going down (rustc's
monomorphization collector walks each body with its arguments applied). SystemVerilog differs in one
condition. A body's meaning also depends on where its instance stands, in two ways:

- **Something written elsewhere reaches it.** A `defparam` changes a parameter of any instance it
  names by a hierarchical path, and takes precedence over the instantiation's own assignment (LRM
  23.10, 23.10.1); a `bind` inserts an instantiation into the instances it names (LRM 23.11); a
  configuration chooses which cell an instance is and may set its parameters (LRM 33.4.1.6, 33.4.3).
  Each can reach one instance of a module and not another.
- **A name it writes lands outside it.** The upward search resolves a hierarchical name per instance
  (LRM 23.8), and the class of the scope it lands in decides what the writer compiles to.

Both are arguments the source does not write as arguments, and the identity has to hold them for it
to settle what is below. Each belongs to every instance it concerns: an effect to every instance
above the one it reaches, a name to every instance it leaves on its way to where it lands. A
lockstep pair shows the second: a core holds a stage that holds a controller writing
`u_core.hart_id`, and the design has the core once as `u_core` and once more under another name
beside it. From the first the name lands in the core itself, so it has left the controller and the
stage; from the second it also leaves the core and lands in the module holding both. The two
controllers differ, so the two stages and the two cores do too.

**An instance states what leaves it, and the instance holding it reads that statement and not its
body.** What follows for an instance from where it stands is one answer per instance, worked out
once and kept: its own body is read for the names that leave it and the instances it holds, and each
held instance is asked for its own answer, of which a name landing here stops and everything else
passes up under the held instance's path. So each body is read once, an answer is a function of its
instance alone and cannot depend on which instance was asked about first, and a unit compiled alone
asks for exactly the answers it needs. A key that searched below itself for these paid that search
each time it was worked out, which was the whole of what a loop's blocks cost to declare.

rustc collects a closure's captured variables this way: a query per closure walks that closure's own
body, and at a nested closure asks the same query of it and counts each answer as a use in the outer
body (`upvars_mentioned`, `rustc_passes/src/upvars.rs`). clang and slang instead walk outward from
the use over the scopes they have open, adding the capture or the name to each one passed
(`Sema::tryCaptureVariable`; `Compilation::noteUpwardReference`, after which slang declines to share
any body holding one, `DiagnosticVisitor::tryApplyFromCache`). That needs every enclosing scope open
at once, which one pass over one translation unit or one elaboration has and a unit compiled alone
does not; a walk of that shape was built here first and replaced for that reason.

The inputs are still read off the instance tree the parent already stands on, so F1 holds: nothing
needs a child's compiled body. Each is stated as itself, under the path from the instance to where
it was written, rather than as the name of the child it concerns. A child's name may hold the class
of an ancestor, where its name lands there, and an ancestor's name stated through its children's
names would then hold the child's; stating the input needs neither name. slang and Verilator answer
the override half by never sharing a body an override reaches, and the ancestors on its path with it
(slang `InstanceCacheKey::isEligibleForCaching`; Verilator clones a module per instance path when a
`defparam` lies beneath it). A content key keeps two instances overridden alike one unit, which a
path cannot.

### F7. Generic-language precedent points to injective mangling for a reason that does not bind us

C++ (Itanium ABI) and Rust (v0) encode template / generic arguments into an injective mangled symbol
name. They do so because the name must be demanglable for debuggers and must be self-contained for a
linker that matches symbols across separately compiled units with no global view, and they accept
that the name grows with argument complexity. Lyra needs neither property: its readable surface is
the MIR dump, not a demangled symbol, and determinism alone makes the producer and consumer agree on
a name without a global view. So Lyra can content-address (hash) the binding encoding where C++ and
Rust cannot.

### F8. A definition is found by more than its name

The front end tells one design element from another by three things: the library it was compiled
into, the scope declaring it, and its name. Two libraries may each hold a cell of one name (LRM
33.2.1, 33.3), and two modules may each declare a module of one name inside them (LRM 23.4, 3.13).
It holds that identity as an address, which serves inside one run and cannot be a name: a unit's
name is computed from one instance by the unit and by every unit naming it (F2), and a kept artifact
is found by it in a later run.

So the name spells all three, each by what is stable about it. A cell is written as a person writes
one, `cell` in the default library and `library.cell` in any other, which is the spelling a top
level and a configuration's use clause already take. A design element declared inside another is
named in the unit declaring it, `Outer::Inner`, the way everything else a unit declares is; it reads
that element's parameters (LRM 23.9), and its instance is handed none of them when it is built, so
each one it reads is fixed by the enclosing specialization.

Three other spellings of an address fail what a name is for. The declaration's file and position
moves with the checkout and with any edit above it. A hash of the declaration's text makes one unit
of one text in two libraries, whose children are searched for through different library lists. A
suffix added only where two names collide makes a unit's name depend on what else is in the design;
Verilator names that way (`V3LinkCells.cpp`, `readModNames`, `lib__LIB__name`) and can, because it
has read the whole design before it names anything. rustc starts every symbol with the crate, a hash
of its name and metadata beside the name, for the local crate too
(`rustc_symbol_mangling/src/v0.rs`, `print_crate_name`); clang prefixes an entity of a named module
with the module and one of the global module, which has no name, with nothing (`ItaniumMangle.cpp`,
`mangleModuleName`). The default library has a name, `work`, and is left out all the same: it is
what a bare cell name means everywhere a person writes one.

Which cell a held instance is belongs to its holder's key wherever the holder's text does not say
(F6). With no configuration the library search order is one for the whole build, so the text says.
Under one it follows where the instance stands: a library list is inherited by every instance below
the one it was set on (LRM 33.4.1.5), so two instances of one module bind one instantiation to two
cells with no rule naming either child. So the cell is stated for every instance standing under a
configuration, and not only for one a rule selected.

## The decision

1. **The identity is a key: the definition (F8), plus what the design fixed for the instance -- its
   parameter bindings, the interface each of its interface ports carries, every effect written
   elsewhere that lands below it (a parameter a `defparam` or a configuration sets, an instantiation
   a `bind` inserts, a cell a configuration chose), and the scope each hierarchical name written in
   it or below it lands in once it leaves the instance, each under its path from the instance
   (F6).** The key holds those as its parts, each named and each carrying the identity of what it
   was fixed to, and two keys are equal when their parts are. Together they settle the identity of
   every instance below, which is what lets a unit name the classes it builds. Nothing compares keys
   through a rendering of them. An effect written inside the instance's own text is the same for
   every instance of it, so stating it changes no sharing. A bound instance is named by the
   directive that inserted it -- the declaration holding the directive and its position among that
   declaration's binds -- since its connections are text of the directive.

   **The name is derived from the key** -- the definition as F8 spells it, plus a content hash of
   the key when anything was fixed. The producer and the consumer both build the same key from the
   same selections and so reach the same name. Folding to a name happens once, where a bounded
   identifier is needed, and never while a key is still being composed: hashing is lossy, and a
   component compressed early takes its collisions up with it, invisibly.

2. **The key's parts hold identities, not renderings.** A value's identity is its constant; a type's
   is its structure where it is built in or built of other types, and its declaration where it is
   declared -- a class, an enumeration, a structure or a union, packed or not, named or written in
   place (LRM 8.3, 6.22.1 c, d, h) -- stated as the unit that declares it and which declaration of
   that unit it is; an interface's is the name of the unit it instantiates, which is already how a
   unit is identified across the boundary, and a port carrying a range holds the units its instances
   are and which of them each position takes, never a list of them joined into one name. All of it
   excludes arena ids, source spans, and any name that does not participate in identity. Ordering is
   normalized so the result does not depend on traversal or enumeration order
   (`specialization_model.md` inv 6).

   **A value is held as one spelling that is both its identity and what the name is folded from.**
   The name is a hash of the key's bytes, so those bytes have to tell every two values apart
   whatever else holds them; once they do, comparing them is exact equality, and a second,
   structural comparison would be a second function that has to be exact too. What makes a spelling
   an identity is that each value's ends where it can be seen to end, so one joined into an
   aggregate never runs into its neighbour: a real is its bits in fixed-length hex, a string its
   length and its text, an aggregate its elements between brackets, and an integral its width,
   signedness and every bit, unknowns included. The front end's own `==` is not this equality -- it
   compares reals as numbers, calling 0.0 and -0.0 one value, and integrals after extending to a
   common width.

   This is how C++ and Rust spell a value argument in a symbol. The Itanium ABI encodes a literal by
   its type and value, a floating-point one as "a fixed-length lowercase hexadecimal string
   corresponding to the internal representation" (section 5.1.6.1), and prefixes every name with its
   length (`<source-name>`); Rust's v0 mangling ends each constant with `_`. Where the conditions
   differ is strings: Itanium spells a string literal "using their type, but not their value",
   because a C++ string literal is no template argument, while a SystemVerilog string is a value a
   parameter holds (LRM 6.20), so its text is part of the identity.

3. **The serializer is subset-agnostic; which selections feed it is policy.** A parameter every
   reference reads as a value enters only as being supplied, and its value reaches the instance at
   construction; one whose declaration writes it from such a parameter does not enter at all, since
   its value follows from theirs; every other parameter enters with its value
   ([a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)).
   The identity mechanism is the same for both; only the input differs. An interface-port connection
   is not on that axis: what it selects is the set of types and positions the module compiles
   against, which no constructor input can carry.

4. **The identity is computed at AST-to-HIR and carried by name thereafter.** HIR owns identity and
   frontend ids end there (`hir.md`); the cross-unit reference and the construct carry the name, and
   the backend renders it as the artifact's name.

5. **Readability is a separable, non-load-bearing decoration.** A human-readable prefix or comment
   may be added later for the emitted artifact; it never gates the identity, never has to be
   collision-free, and degrades gracefully on bindings it cannot render. The hash is the
   load-bearing identity.

6. **Every selection is read off the instance it was fixed for, and the frontend's own grouping is
   not read at all.** Which instances the frontend elaborated into one body is a classification with
   a different purpose, so it may inform how much work is done and never which artifact an instance
   belongs to. An instance whose key differs from the one an artifact was built from does not belong
   to that artifact, whatever the frontend grouped it with.

   **The body an artifact compiles is the one belonging to an instance its key was computed from.**
   A body elaborated for another application states different types at the same positions, so it is
   never a substitute; that the frontend elaborated one body for several applications is a statement
   about its own work and not about which artifact any of them belongs to. Compiling one of the
   separated sets for all of them is the merging error F4 names.

   Instances handed different values of a supplied parameter share one key, so one of their bodies
   is compiled for all of them. What admits that is not the frontend's grouping but this compiler's
   own: each of the others is lowered and compared with the unit, and one that differs keeps its
   definition out of the sharing.

7. **An instance's name is worked out once and kept, and keeping it changes no answer.** A key is
   built from every part fixed for its instance and folded, and every scope the instance holds and
   every unit naming it asks; asked afresh each time, a loop of N blocks builds its unit's key N
   times, and so does a loop of N instances each naming their parent. This is not the shared table
   F2 rules out: nobody agrees through it, and deleting it leaves every name what it was. What makes
   that true is one rule. A name asked while another is being worked out can differ from the one the
   instance has alone, where a hierarchical name it writes lands in an instance still being named,
   or the design element declaring it is one, or a type one of its parameters is fixed to is
   declared by one -- the instance's own body included, as for
   `localparam type T = <a type this module declares>` -- and is stated by how far out that instance
   is. An instance is among those being named for the whole of its key, since any part may ask for a
   name that leads back to it: an interface it carries may carry one standing inside it. So a name
   is kept only when it met nothing outside itself, together with the instances it looked for among
   those being named and did not find, and it answers a later asking only while none of those is
   being named. clang keeps a declaration's mangled name the same way and declines where the name
   "depends on whether the variable is referenced by a host or device host function"
   (`CodeGenModule::getMangledName`); rustc's symbol name is a query per instance, and its trait
   solver refuses a kept answer "if a nested goal of the global cache entry is on the stack"
   (`search_graph`, `candidate_is_applicable`), which is the rule taken here. Two keys folding to
   one name are refused where a name is kept.

   **A design element does not ask what it is called to name the scopes it declares.** It is handed
   its name when it is made, and names each generate block by joining it onto the scope holding it.
   Asking the design for its own name there is a unit reaching through a design-level lookup to
   answer a question about its own declarations.

   **The name stays a question answered on demand, and is not settled by one walk of the design.**
   Having the walk that collects the units name every instance, and refusing any later asking it did
   not cover, was built and taken back: it makes a table one pass over the whole design fills the
   thing every unit reads to refer to another, which is the shared table F2 rules out and what a
   query model cannot keep. An answer kept under the instance it is about is a memoized query, and a
   unit compiled alone can ask for exactly the names it needs.

## Consequences

- Distinct keys produce distinct names, hence distinct artifacts; the current name-collision
  collapse is fixed, including value parameters of any type (unpacked aggregates included), type
  parameters, and a module bound to different interfaces through one unparameterized header.
- Two instances agreeing on every selection share one artifact. Which instances the frontend
  elaborates into one body is a separate relation over the same instances, and neither relation is
  derivable from the other, so the two are compared rather than assumed to coincide.
- The hash's only failure mode is collision; a wide content hash makes it not a practical concern,
  and no global view is needed to guarantee that the producer and consumer agree.
- Sharing one unit across instances handed different values reuses the same serializer with a
  narrower input, so it discarded none of this work.
- Type-parameter identity reuses the recursive data-type structure already produced by type
  lowering; no parallel structure is introduced.

## A type is spelled by this compiler, never by the front end

The hash is self-owned, not the frontend's. slang exposes a value/type hash, but it folds raw
pointers for some type kinds (a virtual-interface type hashes the interface address) and ties the
result to a frontend-internal algorithm; identity is owned past AST-to-HIR (`hir.md`), so the
specialization hash is computed here, over content, with a fixed algorithm that is stable across
sessions (`incremental_build.md` forbids a pointer-derived or process-seeded key).

The same holds for the front end's rendering of a type, which every kind but a class and the
unpacked forms once answered with. It is not an identity. A declared type with no name of its own is
printed with a number counting the types the front end had made before it, so the rendering differed
between two instances of the module declaring the type, and moved when an unrelated type was
declared earlier in the design: the first ended a build of two such instances as two units of one
child, and the second moves a kept artifact's name under an edit that changed nothing about it.

**Which instance elaborated a declaration is left out of a type's identity.** The language makes a
type declared in a design element a different type in each instance of it (LRM 6.22), and the front
end's own match relation follows that. A unit is compiled once for every instance of it, so what it
hands a child has to be one answer for all of them; mirroring the per-instance relation would make
an artifact per instance. The front end enforces the rule where it makes a program illegal, and the
one place its answer reaches a compiled body is a comparison of two types (LRM 6.23). That answer is
fixed per instance like any other selection, so it is a part of the key in its own right once the
comparison is supported, and not a property of a type.

clang and rustc spell a type argument the same way: a record or an enumeration as the path of its
declaration, an unnamed one by a number among the unnamed types of its own context
(`ItaniumMangle.cpp`, `mangleUnqualifiedName`, `Ut <n> _`), and "all nominal types as paths"
(`rustc_symbol_mangling/src/v0.rs`, `print_type`), with tuples, arrays and references spelled from
their parts. Neither has an instance that makes a type, which is the one condition that differs and
is answered above.

Two declarations written alike are two types, so whatever is handed each is two units where the code
is the same. That follows the language, which lets a body read which declaration it was given (LRM
20.6.1, 6.23).

## Alternatives considered

**Injective mangling (C++ / Rust style): the binding encoding is the name.** Rejected. The name
grows without bound with binding complexity -- an unpacked array of structs or a deeply
parameterized type produces an enormous name -- and that cost is certain and always present. Lyra
gains nothing from the self-containedness that motivates it in C++ / Rust, because determinism
already makes the two sides agree, and it does not need demangling. Trading a certain, ever-present
cost (name growth) for a vanishing one (hash collision under a wide hash) favors the hash.

**Fingerprint the lowered body.** Rejected. The consumer that must compute the child's identity
cannot see the child's body under independent compilation (F1).

**Opaque key carried in a design-global map (the previous iteration's `ModuleSpecId`).** Rejected.
It resolves cross-unit references through a design-global table rather than by name, which
`compilation_unit_model.md` and `emission_model.md` forbid. The previous iteration's classification
insight -- fingerprint only code-shape facts, value parameters flow in as runtime data -- is what
[a-parameter-read-as-a-value-is-supplied-at-construction](a-parameter-read-as-a-value-is-supplied-at-construction.md)
does, but its mechanism is not.

**A readable name with scalar values baked in (e.g. `Reg__INIT_3`).** Rejected. It is specialized to
scalar value parameters and cannot express an aggregate value or a type parameter, so it breaks the
moment a non-scalar binding appears and forces a throwaway rewrite. Readability is recovered as a
non-load-bearing decoration on top of the general identity instead.
