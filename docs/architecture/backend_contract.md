# Backend Contract

## Purpose

Define the mechanical-translation contract a backend must satisfy when consuming per-unit MIR.

The primary backend is HIR -> MIR -> LIR -> LLVM IR. The C++ backend is a secondary realization,
which consumes the same MIR but renders to C++ source instead of descending through LIR and LLVM IR.
The C++ backend's emitted output is **how MIR is observed** -- a developer reads the emitted C++ to
validate that MIR's shape faithfully represents the source SystemVerilog through MIR's semantic
model ([`compiler_overview.md`](compiler_overview.md)).

Being secondary does **not** loosen the mechanical-translation discipline. The contract in this doc
is exactly the discipline the LLVM IR backend needs, in which every render rule is a fixed function
of one MIR node. If the C++ backend's render has to work out which operation a node names --
inferring a construction shape from a payload, matching on what an operand happens to be, reading
past the node for something the node did not say -- the LLVM IR backend has to work it out too, and
the two answers are held in step by nothing. Both backends pay the cost; the cost is a MIR design
failure visible from the backend side.

The C++ backend's render therefore serves as the cross-check on MIR shape. Any place where its
render is not a mechanical single-node translation is a place where the MIR-to-LIR lowering hits the
same obstruction. The bug is in MIR, not in render.

**The shape a value-emission entry has.** It is punctuation around the renders of its own children
and nothing else: a piece of the target's syntax with those renders filled into it. Every name it
would otherwise have to know comes from somewhere that owns naming -- a type's spelling from type
mapping, a runtime operation's from the runtime-entry declaration the layers above share, a
wrapper's access protocol from the place-access dispatch. So the shape is observable rather than a
matter of judgement: **no value-emission entry names a runtime library identifier at all**, and a
reader checks that by reading the entries rather than by reasoning about what they decide. A string
literal in one that carries anything but target syntax is the defect, whatever it was written for.

A backend in that shape holds no table of its own. Gaining a runtime operation, a value family, or a
container kind changes the shared declaration and reaches every backend at once; nothing in an
emitter has to be found and edited to match. That is the same property from the other side as the
rule that a backend never decides what a node means -- an emitter with nothing to look up has
nothing to decide -- which is why this shape and the invariants below are one statement read two
ways.

## Owns

- The principle that a backend render entry is a **fixed function of one MIR node**: input is the
  MIR node and its structural context (its children, the node consuming it); output is determined by
  that input alone. The recursion structure follows the MIR node tree; the render of each node
  composes target-language wrapping around recursive renders of its child nodes.
- The boundary between three backend entry kinds:
  - **Type mapping** -- one dispatch per MIR type variant returning a target representation (a
    target-language type literal for C++, a size + an LLVM type for LLVM IR). This is the only entry
    that names a target-language runtime library type. It also answers every other question a target
    asks about a type or a pair of types -- how a value of one is constructed, and how a value of
    one is written as the other -- and where the target answers such a question with a library
    function, the function's name is part of that answer. Among them is how a body is handed the
    object its receiver binding states, for every kind of body alike: whether the target hands it as
    its own implicit object or as a parameter, and whether the function is read-only, both read from
    the receiver's type alone, since answering either from the kind of body lets a read-only
    receiver lose its qualifier with nothing refusing it. Which object a call is entered on is the
    call side of the same question, and place access answers it.
  - **Place access** -- one dispatch per MIR type variant returning how the storage behind a place
    of that type is named as an lvalue, which is what a by-reference binding and the owner of a
    descending write both compose onto. This is the only entry that names a runtime library's access
    protocol. Reading what a wrapper holds and replacing the whole of it are calls rather than
    accesses, so neither is one of its answers. It also answers which object a call is entered on,
    given its receiver: a call's receiver may be a plain value, whose own member the call names,
    where a place is always a handle, so that answer is its own -- a handle designates the storage
    it addresses, and a value is the object itself.
  - **Value emission** -- the entries that translate MIR expression, statement, member, and body
    nodes into target-language form. They compose mechanical syntactic wrappers around recursive
    renders; they make no decisions about what the program means.
- The contract that MIR's primitive set is closed for backends: if a backend's render entry cannot
  produce its output from the node's structural fields plus subordinate renders, the missing
  semantic is upstream (MIR's primitive set or HIR-to-MIR's lowering), not in render.
- Where a text backend's output goes: every render entry writes into the artifact it belongs to, and
  answers with no text.

## Does Not Own

- Which primitives MIR has (see [`mir.md`](mir.md)).
- Which target language a backend chooses.
- The per-artifact and cross-unit boundary (see [`emission_model.md`](emission_model.md)).
- The runtime library a particular backend chooses to wrap target-side storage realizations in.
- Where the runtime library lives and how a binary locates it (see
  [`runtime_distribution.md`](runtime_distribution.md)).
- How a value lies below LIR and what code compiled before a design is told of a type
  ([`lir.md`](lir.md)).

## Core Invariants

1. **A backend render entry is a fixed function of one MIR node.** Two MIR nodes of the same kind
   with the same structural fields produce the same target-language output, every time, in every
   backend.

2. **A value-emission entry chooses a spelling, never an operation.** Write down what each arm of a
   branch emits. If a reader could tell the arms apart by running the program, the branch chose an
   **operation** -- a semantic decision MIR failed to state, which HIR-to-MIR then left render to
   fabricate. The fix is upstream: state it, and render becomes uniform. If only the target-language
   text differs, the branch chose a **spelling**, which is render's own business, provided it
   dispatches on a fact the node or its type states rather than on one it works out.

   A condition with more than one input is more than one branch. A conjunction, and an entry that
   takes a discriminating parameter beside the node, each carry a decision per input; judge every
   input on its own, because settling one leaves the rest choosing at the same site and the site
   then reads as though it were done.

   Presentation is neither, and has no claim: a branch that only makes the emitted text shorter or
   avoids a construct a reader would find redundant decides nothing, and the emitted artifact is not
   read for its looks.

   What the target's own grammar or its compiler's limits require is not presentation, and it is the
   backend's to answer. An operand is enclosed exactly where it holds together less than its
   position asks, a branch holding one statement is written as that statement, and an operator whose
   result is the object a member is reached on is written in the target's postfix form of it,
   because a target compiler bounds how deeply text may nest: a chain the program states flat -- a
   long sum, a run of operators of which the target has some as operators and the rest as functions
   of the value -- has to reach the target flat, or a program the language accepts is one the target
   refuses. An `if` in the `else` of another reaches it as `else if`, which opens no bracket per
   link, and is as deep as the source nested it. A `break` naming a statement reaches C++ as a
   `goto` to a label after that statement, since C++'s own `break` would leave the nearest `switch`
   rather than the loop it names. Each choice reads only the node's kind and its own children, so
   they are spellings.

   Different syntactic shape is therefore not the test, and reading it as one condemns the
   mechanical cases. A member reached through a pointer and one reached inline, a declaration that
   introduces a virtual slot and one that overrides it, an ordinary return and a coroutine's
   completion -- each is two spellings of one operation, dispatched on a fact MIR states, and each
   is exactly what this entry kind is for.

   Where an occurrence stands -- in value position or in target position -- is structural context,
   and dispatching on it is a spelling exactly when both readings are one operation seen two ways: a
   product answers with the component either way, so only the receiver's own form differs. Where
   they are not one operation -- where writing the part does something reading it does not, such as
   allocating the associative entry it names -- position selects an operation, and that is the same
   defect as selecting one from an operand's type. Run the test on it; "value category" is not a
   reason to skip running it.

3. **Type mapping is the only entry that names a runtime library type literal.** Every MIR type
   variant maps to a target-language type representation through one dispatch. Value emission
   entries render types only via that dispatch; they never compose a target-language type literal
   directly. A runtime library type's spelling lives at one place; nowhere else. For a type the
   library declares, that place is its declaration, which states the C++ type a holder names it by
   and the C spelling a DPI-C prototype names, and which each type mapping reads rather than
   restating. Where it holds one by value, every backend gives it the storage the declaration
   states, which the runtime checks every class each backend builds there against. Two MIR types map
   to one target type only where a value of either is held and operated on alike, and whatever tells
   them apart MIR states in its nodes rather than in the value: an enumeration and its base type,
   whose names MIR asks of a member table it states at the use; handles of two classes, whose class
   a dispatch or a cast states; a reference and a reference wrapper, whose rebinding is a node of
   its own. Whatever a target builds once for the one type is then right for both.

   A target also needs types for what MIR states as **structure** rather than as a value -- an
   extent whose exit is an effect, the root a managed object is emitted over -- and no MIR type
   variant names those, so no dispatch reaches them. They belong here even so, and for the reason
   this entry exists rather than by exception: each is a library type's spelling, and this is where
   a library type is spelled. What they are not is a dispatch. Nothing computes which one a site
   wants -- the site knows -- so each is named outright, and giving them a selector to be looked up
   by would be a lookup with no question in it.

4. **Place access is one dispatch per MIR type variant, exhaustive over capability wrappers.** MIR
   states that the storage a wrapper represents is reached by dereferencing the wrapper's place
   ([`mir.md`](mir.md) invariant 14); it does not state what that costs in a target. Each backend
   supplies it from the place's type through a single dispatch, sibling to type mapping, answering
   one question: how the storage behind a place of that type is named as an lvalue. Two things
   compose onto that answer -- a by-reference binding, and the place a write lands on -- and they
   are the same step, which is why there is one entry rather than one per consumer. Value emission
   asks that dispatch and never inspects the wrapper kind itself. Reading a wrapper's own value,
   rebinding it, and taking its address name the bare place and reach no protocol, so they have no
   entry here.

   Replacing the whole of what a wrapper holds, and reading it, reach no entry here either, for the
   opposite reason: each acts on the wrapper rather than naming its storage, so each is a call in
   MIR and is realized by the same entries that realize every other call. Their target-language
   spelling comes from where every call's spelling comes from, which is what keeps one runtime
   method named at one site (invariant 3). A write that descends is not among them: opening the
   wrapper for the write, and each step the write takes into a part, are calls, and only where the
   write lands -- the dereference of what those steps designate -- reaches this dispatch.

5. **Member declaration is a type at a position -- nothing else reaches member render.** A member's
   target-language declaration form is determined by its type and by what its class answers for that
   position: the type carries size, offset, and target type form, and the class answers with the
   identifier the source wrote or with nothing, which is what tells a render whether to spell the
   member from a name or from the position. Wrapper-typed members are no exception: any per-member
   construction state arrives later as ordinary MIR expressions in the constructor body, never as
   type payload that member render reads.

6. **The LLVM IR backend is the canonical cross-check.** When invariant 2 leaves a branch in doubt,
   ask: could a mechanical LLVM IR backend translate the same MIR node without working out what the
   node means? If not, the MIR shape is wrong. The C++ backend being secondary does not relax this
   check; it sharpens it, because the C++ backend's output is how MIR's correctness is observed.

   What the check predicts is the failure a fact MIR declines to state always produces: each
   consumer works it out alone, from whatever is nearest to hand, and the answers agree until the
   day one of them does not. Nothing holds them in step, and the consumer that answers differently
   is a wrong answer no reader is positioned to see -- which is why the search worth running is not
   "where does render branch" but "which question is answered in more than one place".

7. **The set of backends consuming MIR is open.** No MIR primitive and no contract entry is
   specialized for one backend. A new backend reads the same MIR; the only thing it brings is its
   own type-mapping and place-access dispatches and value-emission rules for its target's syntactic
   form.

8. **A value-emission entry names no runtime library identifier.** Every name it emits is either the
   target language's own syntax or the answer of a dispatch that owns naming: type mapping for a
   type -- including how a value of it is constructed and how it is written as another type -- place
   access for a wrapper's access protocol, and the shared runtime-entry declaration for an
   operation. The entry looks nothing up itself; it composes punctuation around what its children
   render to. This is invariants 2 and 3 read forward rather than as prohibitions, and it is what
   makes the contract checkable by reading a render entry instead of reasoning about it.

9. **A node renders as the one construct of the target that means what the node means, and writes
   out nothing that construct supplies itself.** MIR sits where clang's AST does, so for C++ that
   construct is the one clang's AST would hold for the node. Writing a node as two constructs
   composes an operation the node does not state, which is invariant 2's defect, and spelling what
   the construct supplies states one fact twice. A target whose construct supplies nothing -- one
   that calls the function the construct would have reached -- is handed the same fact from the
   type. How every target reaches a runtime entry is the shared declaration's to say (invariant 8),
   so a render that has to work it out names a defect in the declaration.

10. **A type's interface determines the operation, and a backend only exposes it in the form its
    target language asks for.** A type whose value's life is its own states in MIR, in its
    interface, the record its value is laid out as and its four operations on a whole value: its
    default and a copy, each answering a value, writing a copy over a value, and ending one. A move
    is no operation of a type's: it relocates a value whatever the type, and nothing ends its source
    ([`mir.md`](mir.md)). A type that states none has the steps its parts give it. A backend writes
    each operation as any declaration is written, from the declaration MIR states, and composes no
    declaration out of text of its own. Where the target's object model asks more of a type than the
    four operations say -- a C++ class must have constructors, assignments and a destructor, and C++
    destroys a moved-from object -- the target's library realizes that once for every type of a
    kind, as a class template whose members call the type's operations and relocate its record,
    leaving a moved-from object inert, so its destruction does nothing observable; the backend's
    type mapping names that template at the struct declaring the type's implementation, and chooses
    no operation, no adapter and no runtime name. Which operations the library asks of a type is the
    type's implementation of the library's traits, which MIR states and every backend reaches as it
    stands. What the library's own steps ask of an array of any kind is asked in the shapes MIR's
    declarations already have -- an element by reference, and where a write lands as an element's
    address or none and whether the write made it -- and what only the library needs, a lease on an
    element, it takes on the record, as the execution backend's runtime does. The record the
    execution backend hands the library for the type, and a construction of the type there, run the
    same operations, the execution backend building a copy in its return place. So the two are two
    ways of reaching one set of operations, never two implementations of them. Which artifact holds
    a definition already decided is a backend's packaging ([`emission_model.md`](emission_model.md)
    invariant 11), and choosing it decides nothing about what exists or what it does.

11. **A render entry writes into the artifact it belongs to, and each byte is written once.** The
    destination is handed down; every entry writes its own punctuation and asks its children to
    write theirs, in the order the text is read, and answers with nothing. Target syntax is written
    as bytes, and a formatting call only converts a value -- a number to its digits -- never places
    text. A name and a type are written the same way: each is decided in one place, which answers
    with what decides the spelling -- a source identifier, a kind and a position, a type -- rather
    than with the characters, and the one reader that needs the characters as a value, the name of
    the file a class is written into, writes them into a value of its own. What column a line opens
    at, the blank line between sections, and the depth a body opens at are the destination's, and a
    separator is paid by the first byte of the section it opens, so a section with nothing in it
    costs nothing and no entry answers in advance whether its own section is there. _Requirement:
    the north star's iteration time. When each node answers with its own text, a byte is copied once
    for every level above it and each node parses a format string at run time, so emission's cost
    follows how deeply expressions nest rather than how much text there is._ The dumps of the
    intermediate forms are not held to this: each is written only when a person asks for it, and no
    build waits on one.

## Boundary to Adjacent Layers

- [`compiler_overview.md`](compiler_overview.md) defines the pipeline (HIR -> MIR -> LIR -> LLVM IR)
  and the position of each backend within it.
- [`mir.md`](mir.md) defines the primitive set this contract realizes. A render entry that needs
  anything beyond the node's structural fields is one of two failures: a missing MIR primitive
  (extend MIR) or a missed HIR-to-MIR lowering (extend HIR-to-MIR). Render absorbs neither.
- [`lowering_boundaries.md`](lowering_boundaries.md) requires HIR-to-MIR to produce MIR that
  satisfies this contract: the output MIR must be mechanically translatable by any backend,
  including a mechanical LLVM IR backend.
- [`emission_model.md`](emission_model.md) owns the per-artifact and cross-unit boundary. Within an
  artifact, this doc owns the per-node translation rules.
- [`runtime_distribution.md`](runtime_distribution.md) owns where the runtime library lives; this
  doc owns how a backend may reference runtime library types (only through type mapping).

## Forbidden Shapes

Each entry names the invariant it breaks, and through it the requirement: that one MIR means one
program on every backend, with no fact decided in more than one place.

- **A value-emission branch whose arms differ in what the program does** fails invariant 2. It is
  the canonical render-side defect: render deciding what should be a MIR-level distinction. The fix
  is upstream, never inside render.
- **A value-emission branch that exists only to shape the emitted text** -- collapsing a construct
  the producer built, avoiding one a reader would call redundant, shortening a form -- fails the
  rule that the artifact is not read for its looks, and only that rule: it decides nothing and
  states nothing, so what it costs is a branch that must be read and kept correct, and what it buys
  is not something the artifact is for. Where the collapsed form is the right one, the producer is
  what states it. A form the target requires in order to accept the text at all is the backend's own
  answer (invariant 2).
- **A value-emission entry that composes a target-language type literal as a string** fails
  invariant 3: a library type's spelling then lives in two places, and gaining a type means finding
  every emitter that wrote it.
- **A value-emission entry that names a runtime library identifier directly** -- a wrapper type, a
  helper function, a helper struct, a method spelling -- fails invariant 8. The runtime library's
  identifiers appear at render through the MIR types and MIR calls that map onto them.
- **A value-emission entry that switches on a MIR type's payload to choose a non-mechanical emission
  form** fails invariant 2: type payload is for type mapping, and value emission acting on it reads
  value-layer data carried in a type.
- **A value-emission entry that branches on which capability wrapper a place's type is** -- an
  observable arm, a driver arm, a reference arm -- whether in value render, assignment render,
  projection render, or argument render, fails invariant 4: a wrapper's access protocol comes from
  the place-access dispatch, at one site, and spread across emitters it obliges every emitter to
  grow an arm each time a wrapper is added.
- **A member render entry that emits constructor arguments built from the member's type payload**
  fails invariant 5: a member is a type at a position, and construction state arrives as ordinary
  MIR primitives in the constructor body.
- **A render entry whose output depends on the node tree outside the node's own structural fields
  and its subordinate renders** fails invariant 1.
- **An architecture doc, a glossary entry, or a MIR invariant that names a backend's runtime library
  type by its target-language spelling** fails invariant 7: those names are one backend's choice,
  and architecture refers to MIR types and their type-mapping role.
- **A new MIR node or MIR primitive added to make one backend's render simpler** fails invariant 7:
  MIR's primitive set is uniform across backends, and a primitive earns its place by being a generic
  programming-language concept ([`mir.md`](mir.md)), not by being convenient for one backend.
- **A render entry that fabricates an expression from MIR data** -- composing constructor arguments,
  inlining a struct literal, deriving navigation steps from a payload -- fails invariant 2: render
  translates existing expressions, and an invented expression is HIR-to-MIR's job.
- **A render entry that writes one node as two target constructs, or spells what the one construct
  supplies itself** -- a `sizeof` beside an allocation, a receiver worked out from an argument, a
  member reference emitting a call, a conversion and a dereference -- fails invariant 9: such a node
  is standing for operations it did not state.
- **A second site deciding how a call reaches a runtime entry beside the one that reads the entry's
  declaration** fails invariant 6: the two answer one question and nothing holds them in step.
- **A render entry that writes a declaration no IR declaration states** -- a type, a constant or a
  function whose structure the render works out from data rather than translates -- fails invariant
  6: every declaration in the target output stands for one the IR holds, so that what the output
  shows is what the IR states, and a structure two backends need is decided once, before either
  consumes it. What the semantic IR should not carry, because it is a realization rather than a
  language concept, is materialized by a step both backends consume, not by one backend for itself.
- **A render entry that writes a class's special member, an adapter or a library name the shared
  declarations do not state for it** -- choosing which operation a step or a question calls, or
  supplying an argument the declaration does not have -- fails invariant 10: it decides what the
  class does, which the execution backend then decides again.
- **A render entry that answers with its text, or places text with a formatting call** fails
  invariant 11: each level above it copies the text again, and the format string is parsed while the
  compiler runs though it was fixed when the compiler was built.
- **A mechanism for one backend alone to realize something MIR states for both** -- a runtime
  formatter walking an aggregate the C++ host compiler can instantiate and the execution backend
  cannot, a description of a type handed to code compiled before the program -- fails invariant 7:
  the other backend then needs a second mechanism, and nothing holds the two to one answer.

## Notes / Examples

What a target's realization asks of a type's operations is that target's to state, and it is stated
over LIR, naming no target type, so the unit is complete before anything is emitted. The execution
backend hands the library records of a type: a type, an element type, the operations a call compares
or streams by, the function taking some positions, and what a net asks of a value's positions. What
each call, store, step and variable hands over, and how each is carried out, is one decision of the
target's lowering, the role of LLVM's `TargetLowering`, read by the query for what the unit needs as
it walks LIR and by emission as it arranges; a record is made whole, as a vtable collects every
method of its trait, and what a type's constant names of other types is one list both read. An
operation the query did not ask for is a compile-time defect at emission, never a failure at run
time, and one the language does not give the type fills its slot with the library's failing of it.
The C++ backend writes a type's implementation of the library's traits, as MIR states it, as a
struct declaring each item, and the type is the library's class at that struct -- over its core for
a run-time-sized container, over its elements in place for a fixed-size array -- whose members ask
each item and state no algorithm of their own (invariant 10); each item is defined once in the
type's own code file, and every other operation of a type -- a container's or a fixed-size array's
own methods and language traits' items, a tuple's -- is a function the code file calling it defines,
each a definition every unit states alike. Neither backend, nor the lowering to LIR, asks for a
type's whole interface, which would be choosing among its operations; each reads the implementation
MIR states and the operations its code names. So each operation of a fixed-size array -- its
equalities, its bit stream, the positions a net reads and writes -- is one body MIR states for its
type, which both backends compile, as rustc monomorphizes
`impl<T, U, const N: usize> PartialEq<[U; N]> for [T; N]` for every backend alike.

The canonical write rule for any value-emission entry: write it as if the target were LLVM IR. If
you cannot write a mechanical translation rule for it -- if your draft contains an `if` whose arms
produce different LLVM instruction sequences -- the MIR primitive set is incomplete. Fix MIR; render
then writes itself.

What invariant 8 looks like at the two sites that carry the weight. An expression entry is one arm
per node kind, each a piece of the target's syntax with the children's renders in it -- a binary
operator is its token between two renders, a dereference is the target's dereference around one, a
composite is the type-mapping answer around the renders of its parts. A call entry is two steps and
no more: what the target names the callee, and how it composes an object, a name, and the renders of
its operands. The callee's name for a runtime operation is one lookup in the shared declaration,
which already says whether a call site reaches the operation as a free function, as a method on the
object it acts on, or as a factory on the type it builds, or that no call reaches it because a
new-expression names it as its allocation function -- so the entry chooses nothing and knows no
identifier. The same declaration says how the entry reads each operand -- its planes with their
width, a quantity with its signedness, a value the entry keeps with its type, a count or a flag as
the machine value it is -- and a call is arranged from that declaration and from nothing about the
value in hand, as clang arranges a call from the callee's `CGFunctionInfo` apart from emitting it.

What invariant 9 looks like for a placement new-expression. For C++ the construct is the one clang's
AST holds, a `CXXNewExpr`, `::new (args) T(init)`: the target finds the allocation function by the
argument types and hands it the size, and the alignment of an over-aligned type, so the render
writes neither and names no function. It is written from the global scope so that a class's own
allocation function is never found first, and with the declaration of the library's allocation
functions and the unit's check that the one argument finds the function the node names, the
expression can find only that one. The execution backend calls the function the node names and hands
it the size and the alignment from the type's layout, as clang's code generation does; the alignment
is the least the storage meets, so both reach storage the value fits.

An operand is the one thing a call entry may not read past. Where a call carries something the
target spells somewhere other than its argument list -- a position fixed where the call is written,
which a typed target resolves where it resolves types -- that thing belongs to the callee rather
than among the operands, because reading an operand back to decide how to spell it is the same
defect as reading one to decide what the call means. The tell is arithmetic: a fact placed among the
operands that does not belong there costs one special case at every site that walks operands, and
the count of those special cases is the measure of how wrong the placement is.

Reading what a capability wrapper holds, and replacing the whole of it, are both calls, because MIR
states them as calls: each acts on the wrapper rather than naming its storage. Render composes them
the way it composes every other call, so each method's spelling comes from the one place every
runtime entry's spelling comes from, and no access entry writes a second copy of it. Neither render
knows the wrapper's name or which wrapper kind it is.

Writing part of what it holds descends through it: the wrapper is opened for the write by a call,
then one call per level, each naming the entry the lowering settled and taking the level above it as
its receiver, and the write lands where what those calls designate is dereferenced, with an ordinary
assignment there. Only that dereference reaches the place-access dispatch. How a target realizes
such a call is its own answer, a property of that target's value representation rather than a
decision taken per site, so it is not visible above render and no emitter chooses it.

Naming that storage as an lvalue is not a call: it names storage rather than operating on it, and it
is the one question the place-access dispatch answers. Two things ask it and both compose onto the
same answer -- a by-reference binding, and the place a write lands on.

Rebinding a wrapper renders as an ordinary assignment to the bare place, reaching no protocol and no
call, because MIR states rebinding and writing-through as different nodes.

A wrapper-typed member's construction follows the same shape. Construction state arrives as ordinary
MIR primitives in the constructor body (a `CallExpr` to an initialize method on the wrapper, with
primitive arguments -- string literals, array literals, member references); the field declaration
itself is uniform `<type> <name>{};` (C++) or its LLVM IR equivalent (a field at the offset its
class's layout gives it, begun by the library entry for its storage kind, plus a `call` to the
initialize method). Render never composes the wrapper's name or constructor arguments from type
payload.

When render needs to name a runtime library type to fabricate construction arguments, MIR is missing
the right primitive -- usually a way to express the per-member initialization as a call against the
wrapper's own API. The fix is to redesign the wrapper API so MIR can call it through existing
primitives (one initialize method per construction variant, taking flat primitive arguments), then
HIR-to-MIR emits the call, then render translates uniformly. The asymmetry between "some members are
default-initialized, some need construction" is expressed in MIR's expression set, not in render's
branch tables.

A constant is the value it is -- an integral value's bits at its type, a structure of the library a
part per member, an array, a view, an address. The unit holds each distinct one once and a use names
which, and nothing in MIR builds one. How a target writes a value of a type is a question about the
type, so type mapping answers it (invariant 3): a target with literals spells one, and a target that
lays values out as data states the bytes, each by one function from the value and its type that
knows no kind of constant. A backend defines the constants its own output names, each its own
artifact's. The unit's set is where a value gets one identity, and it is not a list of what an
artifact must contain, because an operation folded where it was built leaves its operands in the set
with no use.

How a definition MIR states is linked -- every body of the unit alike, one body for others to name,
or one body alone -- is one answer read off MIR's facts, and each backend spells it: a target's
linkage, or for C++ the namespace with no name, `inline` in a header, a weak definition in a code
file, and a definition in a code file of a type's own that every unit writes alike and the project
compiles once. A backend that worked it out for itself would be a second answer to one question, and
the two answers are held in step by nothing. What a target builds below MIR for its own realization
-- the records it hands the library of a type, the data it holds a constant as -- is linked as that
target decides. A definition every body states alike is kept in place of the others, and how a
target makes the kept copy mean what each dropped one did is that target's: LLVM's `weak_odr` asks
only that the copies behave alike, so each may read its own module's data, as clang's and rustc's
constants are each module's private data; C++ asks that every name in them denote the same entity
([basic.def.odr]/16), which a constant of one file's own is not and a static of an inline definition
is ([dcl.inline]), so the C++ backend writes a constant such a definition names in place, as a
static of its own. The text of such a definition is a function of what it states and nothing else: a
position the unit holds something at, which differs between bodies of one unit, appears in it
nowhere, as an Itanium symbol is composed from what it names and never from where a compiler kept
it.

A value-emission entry may not read _past_ its node to synthesize operands or pick a form: not an
enclosing declaration, not a sibling, not an expected destination, not a node reached by matching on
what an operand happens to be. Its own operands are a different matter and are named by invariant 1
as structural context -- an access asking whether the value it reaches is held through a pointer is
reading the input it was handed, not looking around. Nor does an entry fold two nodes into one: the
address of a dereference is translated as the two nodes it is, never rewritten into the
dereference's operand, since over a managed handle the address of the handle's object is not the
handle.

A relation the compiler established is not checked again on a path the program runs per access --
that a value has the width its type fixes, that a place names the kind of storage its node states. A
check of what the program computes, an index against its range, is the language's and is not this.
Such a check belongs where the compiler can make it, or to a construction that cannot produce the
bad state; repeated per access it costs every run of the program for a fact that was settled before
the program existed, which fails the north star's iteration time.

A type a callee is generic over is part of the callee, stated where the call is built, and what the
call answers with follows from it rather than the reverse. An entry whose operands do not settle
such a type -- a conversion's destination, the type some bits of a value are read or written at, as
many copies as fill a replication -- says so once, in its own declaration, and a call to it states
the type on its callee, beside the part a callee names. Each backend then tells the entry the way
that backend tells a type: one that resolves types where the call is written names the type there,
and one calling code compiled once for every type hands that code the facts of the type it needs.
Neither works the type out from the call's own: a write into some bits answers with a reference to
them, and the entry is called at the type of the bits. The unit's verification refuses a call to
such an entry that states no type, and a type stated to a callee generic over none. A count or a
width the type fixes is therefore never an operand, on a read, a write or a reference alike.

An operand that stands for a type survives only where its contents are an input: a value no type
states -- the `default:` a literal wrote for a missing entry of one associative array (LRM 7.9.11),
which two arrays of one type may state differently -- is handed over as the value it is, as the
operand of an operation of its own on the array the literal's entries built. What an element of an
array holds before anything is written to it is not such a value. Its type states it (LRM Table
7-1), the unit states the code that builds it once for that type, and a call that builds an array of
any kind -- a construction, `new[n]`, a conversion from one array kind to another, a concatenation
-- carries none, so each operation has one operand list whatever kind of array it answers. The same
holds of every other fact a type fixes: what a reduction answers over no entries, the index an
associative dimension reports where nothing is allocated and the shape a value read out of a stream
of bits takes are each its answer type's, and a queue's declared bound is the queue type's. Each is
a fact the type carries, so it reaches an entry with that type -- named where the call is written by
a backend that resolves types there, handed as the type's constant by one calling code compiled once
for every type -- and never as an operand. On the C++ backend nothing is emitted for an element's
default: the host compiler's default construction of the element type is the same statement. A value
passed for its type alone is the wrong way to say any of them: nothing downstream can tell such an
operand from one whose contents matter, so every backend builds contents it then discards, and every
runtime entry taking one advertises a parameter whose value it must document as unused. What a
declaration fixes and an entry needs as numbers -- an array's declared range -- is machine operands
of the call that needs it. An enumeration's member table is a constant like any other: the unit
holds it once as a value, a use names it, and it reaches render as that name, never composed by the
consuming call's render.

How a value reads under a format (LRM 21.2.1.6) is computation MIR states, never data a backend's
formatter walks. A type either decides nothing the value does not -- an integral, a real, a string,
a handle -- or decides a text beside the value, as an enumeration's name sits beside the integral a
radix conversion prints, or decides the only text the clause defines for it, as an aggregate does.
For the last two the compiler synthesizes one callable per source type, in generic MIR, whose body
builds the text -- a container's by a loop over its elements -- and a print carries what that
callable returns, so the library's formatter renders leaves only and an aggregate never reaches it.
The C++ host compiler could walk an aggregate and the execution backend's library cannot, so a
mechanism for either alone would be a primitive for one backend (invariant 7); stating the rendering
where the SystemVerilog type is still in hand needs neither. A format string the program computes
reaches no directive until it is parsed, so each operand carries every reading its type has, and an
aggregate, which has only its text, refuses by name a directive asking for another.

The leaves the library renders are rendered by one formatter per value type, reached through one
operand of erased type, as `std::format` reaches a `formatter<T>` through a `basic_format_arg`: a
value type gains its readings by stating its own formatter, and no list of types is kept anywhere to
be found and edited. A print's operand borrows the value it reads, which outlives the print, so
printing copies nothing it formats. What a reading depends on besides its operand is an operand of
the print as well -- what `$timeformat` last set (LRM 20.4.3), the scope's time unit a `%t` scales
against, the name a `%m` prints -- read where the call is written, so the formatter reads no state
of the run and every input of a print is stated at its call. A `%s` of an integral value reads the
value's own bytes (LRM 21.2.1.7), never the value converted to a string, a conversion that removes
every zero byte (LRM 6.16, [`storage.md`](storage.md)); what a zero byte prints is the ruling in
[`language_conformance.md`](language_conformance.md).

A source name reaches C++'s identifier space by one total, injective function, since an escaped
SystemVerilog identifier may hold any printable character (LRM 5.6.1) and C++ allows letters, digits
and underscores and reserves words of its own. A name that already reads as a C++ identifier, is no
reserved word and does not begin with the prefix the backend keeps for itself is written as it is;
every other name is that prefix, an escape marker and its bytes, each spelled so no two names meet.
What the compiler mints -- a local, a position where the source wrote no name -- is written into a
range of that prefix the escaped names never reach. Replacing a bad character instead would turn two
source names into one C++ name, which no reader of the output could catch. A name foreign C code
also spells, a DPI-C linkage name (LRM 35.4), is written exactly as given.

In the current implementation the C++ backend's render entries write into one destination type and
answer `void`, so an entry answering text does not compile where it would be written, and
`tools/policy/check_architecture.py` refuses `std::format` anywhere in the C++ backend. The LLVM
backend builds its artifact through LLVM's own interface rather than as text, so invariant 11, which
governs a text backend, does not reach it.
