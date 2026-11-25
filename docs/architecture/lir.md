# LIR

## Purpose

LIR is a self-contained, target-neutral executable model IR. Its conceptual peers are LLVM IR and
Rust's MIR, compiler IRs that express a program as a control-flow graph over typed values, with
explicit effect ordering and an explicit storage model, shaped so a backend can lower it to a
target. So the layer of rustc that Lyra's LIR corresponds to is the one rustc calls MIR, whatever
Lyra's own MIR shares with it in name; [`compiler_overview.md`](compiler_overview.md) maps every
layer. LIR is not a high-level programming language; it is what a program looks like once structured
control flow and the object-oriented vocabulary have been lowered into a control-flow graph a code
generator can consume.

LIR's vocabulary is what those execution-model IRs share:

- Basic blocks with explicit successor lists, forming control-flow graphs per callable.
- Typed values, each either a transient computed value or storage named by a place -- a base local
  plus a projection chain. Which one a local is, is explicit.
- A self-contained type graph: every value, place, and call is typed by a LIR-owned type identity.
- Low-level operations: arithmetic, comparisons, loads, stores, calls.
- Effect ordering within a block.
- An explicit protocol to the scheduling runtime: entry, suspend, resume, abandonment, and
  completion appear as CFG edges and call instructions, not as conventions implied by node order.

MIR is the source language LIR is lowered from. The semantic vocabulary of MIR -- structured `if` /
loop, expression-level operators, member access through a receiver, callables with parameter lists,
classes -- is translated at MIR-to-LIR into LIR's machine-execution vocabulary: structured control
flow becomes a CFG, expression trees become instruction streams, member access becomes a place
projection, callable invocation becomes a call instruction. Once LIR sees a value it sees a typed
result; it does not know or care that the same value was a structured expression in MIR.

LIR carries no source-language concept. SystemVerilog specificity is paid in full at HIR-to-MIR: by
the time the program is MIR it is already a generic programming language, with every SV-specific
construct -- an out-of-bounds guard, an event control, a non-blocking assignment, a generate region
-- already expressed as generic control flow, generic expressions, or runtime-library calls.
MIR-to-LIR therefore introduces execution-model facts -- a control-flow graph, logical storage
topology, scheduling edges -- without interpreting any source-language semantics. A name or a time
unit a class's definition holds for the library is data LIR carries and never reads.

LIR is target-neutral. It carries the logical shape of storage -- which local, which member, which
element a place names -- but not its physical realization. Byte sizes, alignments, field offsets,
object-header placement, and target calling conventions are not LIR facts; they are derived below
LIR from the LIR program together with the target data layout and the runtime ABI profile. Its
place-oriented form, where addressable state is a place and transient computation is a value, is
closer to Rust's MIR than to LLVM's fully SSA form; the move to SSA and to physical layout happens
below LIR, at LIR-to-LLVM.

## Owns

- Control-flow graphs: basic blocks, successors, edges.
- A self-contained type graph: LIR-owned type identities that type every local, value, place
  projection, return, and call argument. Each is translated from a generic-PL type at MIR-to-LIR and
  carries no live reference back to MIR. The unit's types are interned by their content, so a type
  reached by two routes is one identity and comparing identities is comparing types; the content is
  the whole type, since LIR holds nothing present for the front end's benefit, and a nominal type's
  content is its declaration, so two classes with the same members stay two types.
- The place vocabulary: a place is a base local plus a projection chain of member, dereference,
  element, index, component and slice steps, spelled inside the instruction that consumes it and
  never held. Every load, store, and address-of names a place by logical identity, and address-of is
  the one operation that turns the path into a value the program can retain. An element or component
  step reaches a part of storage -- an array's element, a structure's member, an untagged union's
  one storage, a tagged union's member, which lies at the union's own start. An index step reaches
  the entry of a machine aggregate at a machine integer the program computes, as rustc's
  `ProjectionElem::Index` does, and the aggregate's own bound holds it. A tagged union's member has
  no identity of its own, so nothing the program keeps is the address of one; MIR states that, and
  an address taken here for a transient access through the step is the union's own. A packed value's
  bits are a slice step, which only another slice step follows: a load of a place ending in it reads
  the bits where the value lies and a store writes them there, each step bounding the next, and
  since bits lie at no address of their own, no address is taken of such a place. Bits of the value
  a write in progress designates are stored through the designation's dereference, which names
  nothing else, so the write hears which bits move and wakes only what waits on them. A string's
  character is no step; the string's own operations read and write it. A window of several elements
  is no step either, because it is several elements rather than one storage, and storage behind an
  indirection is reached by a dereference step.
- When the bytes a frame keeps for a value are in use: from where a slot or a value is made, to a
  point stated on every way out of the extent it belongs to but unwinding out of the body, which
  ends every byte of the frame. The statement is a promise a target may act on -- the bytes may hold
  something else after their end -- so a value read after its storage's end is a miscompile, never a
  lost optimization.
- Whether a write initializes or assigns: a value put into storage holding none -- a slot's first
  value, one whose value was ended before, or a part of a body's return place, where an answer is
  built part by part -- is an assignment naming that storage as its destination, made there or moved
  in, and every other write is a store into the value already there. A value ended and built anew
  where it lies -- a tagged union whose live member changes -- is one statement doing both, so the
  storage holds nothing only within it. Both are stated, so nothing below works out which a write is
  from the shape of its place, as SIL states `store [init]` beside `store [assign]`.
- What each step of a value's life does for each type -- a copy into storage holding nothing, a
  relocation into such storage, which ends nothing of its source, a write over a value already
  there, an end -- and whether ending a value of a type does anything, answered once by one query:
  the value's bytes, the library's entry for what it is held as, a function the unit states built
  from the type's parts (a product's components in order, an array's elements, a tagged union's live
  member, a closure's captures), the body built for a run-time-sized container's type over the
  library's core, the definition a value opens with, or, for an owning pointer's end, what it points
  at, ended where the pointer ends. Every type the unit names whose step is such a function is given
  it, whatever else then uses it, since a type's declaration brings its steps; the operations of a
  type on a whole value, by contrast, are built only where something asks for one (Boundary below).
  And for each class the unit defines that a value is built of, how the parts it places begin and
  end and how an object of it is created whole and given its class, and ended. The step is stated as
  an operation -- a copy, a value built of parts, a value put in storage -- and the query says what
  it does, as rustc's `Drop` names a type's drop glue.
- Who ends a value a pointer owns: a uniquely owning pointer is an owned value with no copy, moved
  wherever it is handed on -- into a slot, to a caller, to a callee -- and ended by its last holder,
  which ends what it points at and gives the storage back. Storage of dynamic duration for a value
  is allocated holding nothing, initialized there, given back if the initialization departs, and
  only then owned. An object is created whole and owned from creation on, before its constructor
  runs, so a construction that departs ends its owner as any owned value is ended.
- The transient-value vocabulary: a computed value with a pure dataflow origin, not backed by a
  named memory location.
- Where every computed value goes, stated on the step that computes it as Rust's MIR states an
  assignment's place and a call's `destination`: a transient the step defines, or storage holding
  nothing yet that the value is made in and whose holder then owns it. A step that yields nothing --
  a store, an initialization, a frame's bytes coming into or going out of use -- names no
  destination, and an evaluation yielding nothing stands as the void value where one is wanted.
- Where a callable's answer goes: its return place, storage of the answer's type that every way out
  makes the answer in before returning, as Rust's MIR writes `_0`, whose value the caller then owns.
  Whether that storage is the caller's or the frame's is the target's convention for answering.
- Which element of a sequence of handles is taken out of it by value, at the position the program
  computes. A part of any other value is reached as a step of the place the value is held in.
- Logical storage topology: which local, member, element, or referent a place names, and the logical
  identity of every class member and callable a node refers to. A member step names the class that
  declares the member and the slot that class gave it; the type the place arrived at is checked
  against that class, never consulted to find the member (LRM 8.14). A member step reaches a class
  this unit compiles or a class another unit published, both member-bearing objects of this unit's
  own graph, and only what the artifact emits distinguishes them. A step into another unit's object
  reaches only a member its scope published, which leads that object's storage in the order the
  publication states; what lowering added to the object is reached by no step from outside it.
- Each closure the unit builds: its captures as members, and its one body. It is no class -- no
  base, no dispatch, no constructor, its captures initialized where a value of it is built -- and
  shares the member vocabulary with a class and nothing else, so a capture read is a member step
  through the body's receiver.
- Each operation of a type no declaration states it for that the unit asks for, by the type and the
  operation, with the function answering it, or none where the language gives the type no such
  operation. A body of one is linked within its artifact, since no other unit names it.
- Each class's relations as MIR stated them -- what it extends, what it introduces into dispatch and
  overrides, the interfaces it names and which behavior answers each behavior of every interface a
  value of it is also a value of -- and the contents of the constant the library reads for it, which
  are bodies, names, and plain data such as a scope's time unit and precision. The layout step below
  LIR reads these and decides every position; LIR carries them as data and decides nothing about
  them.
- Effect ordering within a block.
- Low-level operations: arithmetic, comparisons, machine conversions, loads, stores, calls.
- What an address fixed before the program runs names -- a function of this unit or of another, a
  constant of the unit, a class's definition, what ends its objects, the tables they dispatch
  through, storage no instance owns, a text -- stated one way wherever such an address stands: as an
  operand, as the base of a place, as a part of a constant, as the body a table's slot holds. It
  names what it reaches by the identity this layer gives it, as rustc's `GlobalAlloc` and clang's
  `LValueBase` do; storage no instance owns and another unit's entry are named by the symbol they
  link under, since this layer declares neither.
- Symbols a call reaches by name: a body of this program that another artifact holds, and a foreign
  symbol -- the linkage name of a callable defined outside the program. LIR states the name and the
  machine types the call crosses on; how the name is resolved -- a link line, an execution session
  -- is below LIR. The two are kept apart because the second is entered by another language's
  convention and no departure comes out of it, though the release of a stack waiting beneath it
  unwinds through it, so both are called as calls that can be unwound out of.
- What each function of the unit is said to be where it is declared, before its body exists: whether
  a call of it can be unwound out of, as LLVM's `nounwind` is an attribute of a function's
  declaration and rustc's `FnAbi::can_unwind` a fact of a callee apart from its body. A function
  lowered from a body takes what MIR declares of that body, and one this layer builds can be unwound
  out of unless it hands its place to an operation that cannot.
- Where unwinding goes in a body, as rustc's MIR states a call's unwind action: a call that can be
  unwound out of at a point where the body owes something on the way out names the first block of
  what is owed, and so ends its block; any other call is an instruction, unwinding from it leaving
  the body. A value whose type's end does nothing is owed no end, and since leaving the body ends
  every byte of its frame, a slot's use is owed only on a way out that stays in the body; every
  other obligation -- a value's end, a lease let go, a write in progress ended -- is owed on every
  way out. A block reached only by unwinding is marked as one. Unwinding ends where a region of the
  body, or the top of an execution, takes it -- a terminator asking the runtime what arrived and
  going on with a departure of the design, while anything else goes on unwinding as the same raise
  -- or by going on out of the body. What is unwinding is no value of the body, and how a target
  carries it -- landing pads, where it is kept while cleanups run, beginning and ending a catch,
  resuming -- is below LIR ([`nonlocal_control_flow.md`](nonlocal_control_flow.md)). The unit's
  verification holds two rules apart: a block reached only by unwinding calls nothing that can be
  unwound out of and is reached only by unwinding, and a function declared not to unwind lets no
  unwinding out of it.
- The boundary protocol to the scheduling runtime (entry, suspend, resume, abandonment, completion),
  and which bodies speak it: a body whose call protocol is the coroutine one says so in its result
  type, carried unchanged from MIR, and nothing else restates it.
- Where in the source each instruction and terminator came from, as rustc's MIR statements and
  terminators carry a span: that of the innermost MIR node naming one whose lowering emitted it, so
  what the code generator fails on or refuses is reported there.
- Where a call that can fail the design was written: the address of the unit's constant for that
  location, which is data like any other constant of the unit, read as a value of the library's
  location type, the one word the value is, as the call's last operand, ahead of what the target
  adds after every operand. A statement's location may also be an ordinary operand, which keys a
  coverage goal or names a procedure.

## Does Not Own

- Semantic ownership of objects, members, or callables (see [`mir.md`](mir.md)). LIR consumes the
  ownership, wrapper, and object-model decisions MIR fixed; it does not re-decide them.
- Physical layout: byte size, alignment, field offset, object-header placement, padding. These are
  derived below LIR from the logical topology, the target data layout, and the runtime ABI profile;
  Notes states the derivation.
- Target ABI classification: by-value versus indirect parameter passing, `sret` returns, register
  and stack placement, calling conventions, and the LLVM-facing encoding of a value.
- How a coroutine's frame, its resume state and the values that live across a suspension are laid
  out. LLVM's coroutine passes derive them from the suspensions LIR states.
- A live dependency on MIR. After lowering, LIR consults nothing upstream; every type and identity
  it uses is its own.
- Language-level constructs (loops as loops, assertions as assertions).
- The generate-region tree or hierarchy structure (see
  [`hierarchy_and_generate.md`](hierarchy_and_generate.md)).
- Identity rules for upstream references (see
  [`identity_and_ownership.md`](identity_and_ownership.md)).
- Target-machine details: register allocation, instruction selection, per-target calling
  conventions. These belong below LIR.

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose -- LIR is a
self-contained, target-neutral executable model IR. An invariant that cannot be re-derived from that
identity is the suspect, not the analysis.

1. LIR is a CFG-shaped IR. Every callable body is a graph of basic blocks with explicit successors.
   A block ends in exactly one terminator, which is what makes it a basic block rather than a
   sequence of instructions; there is no terminator meaning "not yet decided", so a pass that builds
   blocks holds the undecided state in its own shape and never in LIR. _Machine-execution
   consequence: control flow is the graph the codegen consumes; structured constructs do not survive
   at this layer._
2. Each LIR local either names storage or is a transient value, and which one is explicit. Which one
   a local is follows a canonical lowering rule, not the source language's notion of a variable: a
   local is a place -- named storage -- exactly when the canonical lowering names it as storage (a
   declared local, a value a part is reached in, a result that control-flow paths join at, or the
   callable's return place). A value computed once and consumed is a transient value with a pure
   dataflow origin. The rule fixes the storage topology; it does not minimize it -- recovering a
   register where the address is never truly needed is a separate derivation below LIR.
   _Machine-execution consequence: the codegen knows for every value whether it lives in memory or
   in a register-class temporary; no value's storage class must be inferred._
3. LIR fixes logical storage topology, not physical layout. A place names storage by logical
   identity -- a base local and a projection chain of steps naming a member, a referent, an element,
   or a component -- never as a byte offset, an address, or pointer arithmetic. The physical
   realization of that topology is derived below LIR. _Machine-execution consequence: one LIR
   program is valid for every target; the target-specific layout is a separate derivation, not a
   property baked into a place._
4. LIR identity is self-contained. Every type, class, member, and callable a LIR node names is a
   LIR-owned identity. Every MIR type entering LIR is fully translated to a LIR type or rejected at
   the MIR-to-LIR boundary; no LIR node carries a MIR id, index, or borrowed pointer as a live
   reference. _Machine-execution consequence: a finished LIR unit is dumpable, verifiable, and
   lowerable with no MIR present._
5. LIR consumes MIR's semantics and never re-derives them. It reads the ownership, wrapper, and
   object-model decisions MIR fixed -- whether a handle is shared, a reference is managed, a member
   is observable -- and acts on them. _Machine-execution consequence: lowering is one-way; LIR reads
   upstream decisions and acts on them, never re-decides them._
6. The runtime protocol is explicit: suspend, resume, abandonment, and each way a body is left
   appear as CFG edges, not as lowering conventions implied by node order. A suspension names both
   ways control can leave it -- the driver resumes the body, or ends it where it stands -- so what
   the second owes is in the graph rather than assumed; and an execution's body left by unwinding,
   or an execution completed by a departure, says so with a terminator of its own, as a returning
   one does. A suspension arranges nothing: the runtime calls before it build and register the wait,
   so a delay, an event control and a level wait differ only in those calls and share one suspend,
   and the stop acts on the execution the runtime has running, so no activation token appears in
   LIR. A coroutine body's entry is such a suspension, before its first statement, so whoever drives
   the body runs every stretch of it, the first included, and ending it there ends what it was
   handed and nothing else -- the initial suspend a C++ coroutine's promise states, here stated in
   the graph rather than left to each backend. _Machine-execution consequence: the scheduler's
   boundary is visible to the optimizer as edges, not as a side convention only the lowering knows,
   and a backend reads which bodies are coroutines from their result type alone._
7. LIR carries no source-language semantics. Every node is a generic machine-execution operation; no
   SV-specific shape -- an out-of-bounds guard, an index-validity predicate, an NBA region, an event
   control -- survives, because each was lowered to generic control flow or a runtime call at
   HIR-to-MIR. _Machine-execution consequence: a source-language concept reaching LIR is an upstream
   leak, never a LIR node to model._
8. A place is an access path, not an entity. It is spelled inside the instruction that consumes it,
   is not an operand and not a local, and does not cross a control-flow edge; address-of is the one
   operation that turns a path into something the program retains, and its result is an ordinary
   value. _Machine-execution consequence: what a callee receives, what a local holds, and what
   survives a suspension is always a value, so no consumer re-evaluates a path whose base or
   projection may have changed since._
9. The end of a value and the end of its storage's use are two statements. A value handed on leaves
   behind the bytes it was made in while it goes on existing, and a slot's bytes hold one value
   after another, so neither end implies the other. The statement says only how long a frame's bytes
   are needed. _Machine-execution consequence: those bytes are the frame's, because a value too
   large for a frame never reaches one -- MIR holds it through storage the program allocates -- so
   no layer below decides from a value's size where it is kept._
10. A write states what it writes and nothing wider. Bits written within a packed value that lies
    somewhere a write can land -- at a place, or within a write in progress -- are one statement
    naming where the value lies, the parts the bits are reached through, and the bits. No node
    states the value rebuilt in order to change part of it there, and no node reads a part out at
    its own type to write a part of that part. _Machine-execution consequence: what a write costs
    follows the bits it names and not the value they lie in, and a bit the program did not name is
    not written._
11. A type's identity is its content. Two requests for one content answer one identity, whichever
    route asked, so the verifier's and the code generator's comparisons by identity are comparisons
    of types. _Machine-execution consequence: a type one lowering builds and another translates from
    MIR are the same type, and no cache beside the pool is a second authority for an identity._
12. A cast between two packed values is between one type. It renames only what the program holds the
    bits to be; any change to the integral type -- its width, signedness or state domain -- is the
    reshape MIR states as a library call, and the unit's verification refuses a cast between two
    packed types that differ. _Machine-execution consequence: every cast is a pair of types a target
    writes in its own conversion syntax, and no value silently changes width at a cast because the
    reshape meant to precede it is missing._

## Boundary to Adjacent Layers

- Consumes MIR. MIR-to-LIR is the layer where the generic programming language becomes a
  machine-execution model: structured control flow (`if`, switch, loop, break, continue, return) is
  flattened into basic blocks and terminators, member access becomes a place projection, expression
  trees become instruction streams, callable invocation becomes a call instruction, and suspension
  (an await, a sensitivity wait) becomes suspend/resume edges with scheduler calls. Every MIR type
  and identity is translated into a LIR-owned one; a type MIR-to-LIR cannot translate is rejected at
  the boundary with a diagnostic, never passed through as a MIR id. MIR-to-LIR reads only a
  legalized unit, and every body MIR holds is concrete. A call of an operation of a type no
  declaration states it for is a function of the unit named on the first ask, and the body comes
  from outside this pass, which depends on neither the layer building it nor the target asking for
  it. MIR-to-LIR introduces every fact LIR owns and that MIR does not (CFG structure, logical
  storage topology, effect ordering, scheduling-protocol edges), and it introduces no
  source-language interpretation -- a lowering rule that must decide what a MIR node means is
  reading a defect in MIR, not doing LIR's job.
- Produces a self-contained LIR program for the layers below, complete: every function a unit
  declares is defined and every call of one states what it takes and answers, which verifying the
  unit checks in every build since no machine type below can tell two references apart, and what a
  target needs of a type's operations was asked for and lowered before the unit left here. Whoever
  drives a unit alternates lowering bodies and asking the target what the finished ones ask of the
  library's records, until nothing new is asked, as rustc's collector hands code generation a
  complete list; each body asked for is built as MIR, rewritten into the legalized phase and
  verified alone, and lowered, which may ask for more, and the set of types, constants and functions
  only grows while it runs. Emission then runs one way, on a unit no body is added to. Physical
  layout and target ABI are a separate derivation: the LIR program, the target data layout, and the
  runtime ABI profile together yield byte sizes, alignments, field offsets, and parameter-passing
  classification. LIR-to-LLVM consumes the LIR program and that derivation. It preserves the
  execution semantics LIR decided, while selecting the concrete representation and runtime
  realization the backend requires, and it is also where the place / transient-value form becomes
  fully SSA. The LLVM path never queries MIR; it is not a second semantic lowering.

## Forbidden Shapes

Each entry names the property of the identity in Purpose it breaks; the diagnostic for any new
forbidden shape is the same question.

- **A parallel identity or ownership system beside MIR's** fails invariant 5: identity is owned by
  the layer that introduced it, and a second system re-decides what MIR fixed.
- **A MIR identity reaching LIR as a live reference** -- a `mir::TypeId`, a class / member / method
  index, or a borrowed `mir::CompilationUnit`, held on a node or consulted in a later pass, or an
  un-translated MIR type passed through as a fallback instead of being rejected at the boundary --
  fails invariant 4: the unit is then not lowerable with no MIR present.
- **Byte offsets, addresses, alignment, padding, or LLVM-facing types on a LIR node or place** -- a
  place naming `base + N` instead of a logical projection -- fails invariant 3: the program would be
  valid for one target only.
- **Object or member semantics decided at LIR** -- which override a call runs, which interface
  behavior a method answers, where a member sits -- fails invariant 5: the first two are MIR's
  statements and the third the layout step's answer, and LIR only carries them.
- **A member found by searching the type a place arrived at** fails LRM 8.14, which binds a name to
  the class the access names, and invariant 5: the class and the slot are MIR's answer, carried.
- **Hierarchy navigation expressed at LIR** fails invariant 5: hierarchy is resolved by MIR and
  arrives as logical storage topology.
- **A structured construct reintroduced at LIR** -- a "foreach" node instead of a loop CFG -- fails
  invariant 1.
- **A source-language concept reintroduced as a LIR node** -- a guard, an index-validity predicate,
  an out-of-bounds default, an NBA region, an event control -- fails invariant 7; one reappearing at
  LIR is an upstream leak, never a LIR node to add.
- **A suspension terminator that names its wakeup source** -- a delay, an event or a level-wait form
  of it -- fails invariant 7 (the wait is a runtime call) and invariant 6 (a suspension arranges
  nothing, so one suspend serves every wait).
- **A flag on a function, a terminator or a node saying it is a coroutine** fails invariant 6's
  reading of coroutine-ness from the result type: a flag is a second statement of one fact, and it
  appears only where an upstream layer erased the protocol from the type, which is the defect to
  fix.
- **Checking at LIR that the program obeys a source language's ownership rules** -- that nothing is
  read after a move, that a borrow does not outlive its referent -- fails invariant 5: legality is
  decided where the program is still that language, and LIR's value model is value copy, a uniquely
  owning pointer moved wherever it is handed on, and explicit borrowed references. Deriving where
  storage is released is not this: it is a lowering computing a fact about the code in front of it,
  which is what an execution-model IR is for, and both peers named in Purpose do it.
- **A control transfer not stated as an edge** fails invariant 1 and invariant 6: the codegen and
  the optimizer rely on every transfer being one.
- **A call naming a block to unwind to where nothing is owed on that way out** fails the north
  star's iteration time, on the premise that a target pays for each landing whether or not it runs
  anything -- LLVM's instruction selection sends every block ending in an invoke, and every landing,
  through its slow path at `-O0` -- and it states an obligation the body does not have.
- **The extent of a catch, or what is unwinding as a value of the body, stated in LIR** fails target
  neutrality: it is one target's exception ABI, which has two realizations in LLVM alone (landing
  pads, and Windows' funclets), and handing a departure over once at a taking point leaves no extent
  to state.
- **Target ABI policy decided during LLVM lowering** -- parameter passing, `sret`, register / stack
  classification -- fails the separation of LIR from its physical derivation: those are facts the
  derivation fixes, and the LLVM step encodes them without choosing them.
- **The LLVM path querying MIR** for a semantic type, a class member, an object layout, or an ABI
  decision fails invariant 4: it consumes the LIR program and the physical-layout derivation only.
- **A basic block that crosses a callable boundary** fails invariant 1: each callable is one CFG,
  and a transfer between callables is a call.
- **A type identity minted per request**, or a cache keyed by a type's content standing beside the
  pool, fails invariant 11: one type reached by two routes then arrives as two identities, and a
  store or an address-of between them is refused by name.
- **A lowering deciding that a declared local needs no storage** -- substituting the value its
  declaration produced at every mention -- fails the rule that every declared variable is storage
  ([`storage.md`](storage.md) invariant 1), and is an optimization taken by a step that translates
  one node at a time and so cannot see every write that reaches the local; keeping a variable out of
  memory is the saving of whoever sees the whole function, whose failure is a slower program rather
  than a refused one.

## Notes / Examples

A MIR process becomes an LIR callable with a CFG. Its suspend points (for example, event controls)
appear as explicit CFG edges that hand control back to the scheduler, each after the runtime call
that builds its wait; the wait is held in a local of the body, so a stop reached again stops at the
same wait. The optimizer sees the suspend point as an edge it can reason about (does it
post-dominate any node, what values does it demand) rather than as a convention buried inside a
node's semantics. A level wait lowers to a re-check loop -- evaluate, continue if true, else build
the wait on what the evaluation reached and stop at it, then evaluate again on resume.

A structured `if` with then and else blocks becomes a conditional branch to two basic blocks that
rejoin at a merge block; a switch becomes one terminator naming the block for each value and the
block where none is named; a loop becomes a header, a body, and a back edge; a `break` becomes a
branch to the block after the statement it names, and a `continue` a branch to its loop's header.
The structured nesting MIR carried is gone; the CFG is the only control structure LIR has. This
flattening is the core of MIR-to-LIR -- a backend that targets a structured language (the C++
backend) never performs it, so it is work LIR introduces from scratch.

A callable's locals are the receiver `self`, its parameters, the automatic variables its body
declares, and the temporaries the lowering introduces. Every declared local has storage, and where
it lies follows from what it is: a variable MIR states as a cell, which a process may wait on, lies
among the body's variables, and every other local is a slot of the frame, which a reference binds as
it binds any storage and which the target keeps across a suspension. A source-level variable with
static lifetime is not a function local; it is a cell of whatever its declaration belongs to, which
for a body of the design hierarchy is a member of the enclosing class reached through `self` as a
place. The bulk of a program's state is member storage reached through receivers, not locals on a
frame.

LIR is not an SSA form. A local is either a transient -- computed once, consumed, with a pure
dataflow origin -- or a place, which is named storage: the frame's, or for the callable's return
place, wherever the target's convention for answering puts it. Which one a local is follows a
canonical lowering rule, not the source language's notion of a variable: a local is a place exactly
when the canonical lowering names it as storage -- a declared local, a value a part is reached in, a
result that control-flow paths join at, or the callable's return place (invariant 2). Everything
else is a transient. So there is no phi and none is needed: a value produced on more than one path
is a place each path writes, a conditional expression writes its result into a place from each arm,
and a loop-carried variable is a place the body updates. A parameter is a declared local whose first
write is the incoming argument: the argument arrives as a transient, and before the body's first
statement it is put in the parameter's storage -- a cell where MIR states one, a slot of the frame
otherwise -- whether or not the body ever writes it or takes its address. Recovering a register
where the address is never truly needed is a target-side derivation, not a property LIR must
establish: LIR states the storage topology, it does not minimize it.

The current execution backend realizes the statements of when a coroutine's slot is in use as
`llvm.lifetime.start` and `llvm.lifetime.end` at every optimization level, as clang does for a
coroutine. LLVM's coroutine split asks of every slot whether it is in use across a suspension, and
without those statements answers from every pair of the slot's uses, which costs the square of a
long body. Storage no way out ends -- the body's variables, the slot a landing keeps what is
unwinding in -- has a start and no end.

A closure lowers to a value of its declaration, built from its captures by an instruction of its
own; its one body is entered with a borrow of that value, or its box, as `self`, by its kind -- a
read-only borrow for a closure that only reads its captures, a mutable one for a closure that keeps
state between calls, the box for a body that takes the closure over. `self` is the first argument,
supplied like any other; LIR has no implicit receiver. A direct call to a statically known callable
passes its receiver and arguments to a named function. LIR knows only "call target" and "arguments";
which values a closure captures was decided in MIR. A closure, its body and its definition belong to
the body building it, and nothing outside that body names them, so all three are linked within the
artifact holding that body.

A place is the logical path -- `self.counter`, `*p`, `(*p).flag` -- the way Rust's MIR names a field
projection rather than `base + N`. The same place is valid on every target; only the derivation
below LIR turns it into an address. A member access lowers to a place whose projection names the
member by its logical identity, not by an offset. A read of an observable signal and a whole write
of one arrive as calls on its cell, and a partial write as a write in progress opened on it, because
HIR-to-MIR expressed them that way, and each lowers as a call, the same as any other. Whether a
plain-value member's place becomes a load or a store, and at what address, is the physical layer's
question, not LIR's.

An object member access -- reaching a member of an object through its receiver -- always yields a
place. What the use site does with that place -- read the value it holds, write a value into it, or
name where it lives -- is the use site's decision, never a property of the access. Some storage has
no first-class value in LIR at all: a storage cell, a scope, an object-tree node. Such a type is
address-only, and every operation over it consumes its address, so loading or storing a place of
that type is a lowering defect. Address-only is a fact about the storage object, not about how
values are represented: a packed value is an ordinary first-class value however the layers below lay
it out, and a place holding one is loaded and stored like any other. The cell that holds it is what
may only be addressed.

A place is the access path to independently addressable storage -- storage with an identity of its
own -- not merely something a source-level assignment can target. Path and identity are two phases,
and the split is what makes the vocabulary safe: the path is written where an instruction consumes
it and locates whatever its base and projections reach at that moment, while the identity a program
retains is the value address-of yields ([`storage.md`](storage.md)). Assignability and place-ness
are different questions, and which parts of a value are storage of their own is the language's
answer rather than this layer's. An element of an array, queue or associative array, and a member of
an unpacked structure, each have an identity a second name may denote ([`storage.md`](storage.md)),
so each is a place, which a load reads where it lies and a store writes into the object already
there. A member, and an element of a fixed-size array, is a step extending the place holding its
value; an element of a dynamic array, a queue or an associative array is the dereference of the
address its type's own body answers, as rustc reaches an element of a `Vec` by a call of
`Index::index`, and its place is the identity a name of it holds. Value semantics hold because a
copy of the aggregate copies its parts, so no two values share one and a write through one is seen
through no other. An untagged union is a structure of one component, its storage, and a member is no
part of it: MIR states a read of one as the storage's bits made into the member's type and a write
as a store of the member's bits into the storage, so nothing here knows a union has members. A
tagged union's member has no identity of its own and is still reached in place: a component step on
the union, at the union's own start, with a load and a store through it as through any step. A
tagged union's tag is read by its own instruction over the union's place, rustc's `Discriminant`,
and a whole union is built by one instruction naming the member and its value, which leaves every
other byte zero. Where the member is instead made where it lies by its own step -- a copy or a
relocation of the union, as a product's parts are each made where they lie -- a statement names the
member first, zeroing the storage and writing the tag -- rustc's `SetDiscriminant`, which its clone
of a coroutine states ahead of the fields it clones -- and the member is then made through a
component step. A value that lies in no storage is held in a local before a part of it is reached,
so no layer extracts a part of a value or rebuilds a value around one; the one element taken by
value is an element of a sequence of handles. Bits of a packed value are not parts of that kind
either: the integral's own operations read them out of a value and write them into one. Bits of a
packed value lying at a place are written where they lie, by a store to a place ending in a slice
step for each part the bits are reached through, outermost first, each saying where the part starts
within the one before it and the type that fixes how many bits it has; the value stored is the bits.
Bits lie at no address of their own, so a load and a store name such a place and no address-of does.
A part reaches nothing outside the part it is taken in, and a start that names no position writes
nothing; both are outcomes of the insertion the store is realized by, never control flow or a test
stated here. A step's direction is the access's: reading a missing element reads the default, and
writing one allocates or discards it by the container's own rule, which whatever realizes the step
applies -- arithmetic within a fixed-size array's layout, or, for a dynamic array, a queue or an
associative array, the body the unit states for the container's type, which the library's steps
within a write reach through the type's record. A write in progress is not a place but a value: the
write itself, which the writer owns and ends and which names no place, and each place designated
within it -- the whole of what it was opened on, and each part reached from there -- which borrows
the write. Each step MIR takes within a write is one call on a designation answering with the next,
and what the step does to the write -- an element step tells it what forming the element did -- is
the library's, as it is in the C++ rendering of the same MIR. Where the write lands is a call on a
designation, stated before anything is written there, that answers with the part's address; the part
is then storage like any other. A run of elements written within a write is a call on the
container's designation. Bits written within one are a store through the dereference of the
designation of the value they lie in, naming the same parts and bits a store at a place names; the
write keeps what those bits held and hears which of them moved. That is how the variable learns
whether the write changed it (LRM 4.3). Which library entry realizes a step, and whether it is an
instruction or a call at all, is a realization question answered below LIR; it never decides which
node the step is expressed as. A mutating method on a receiver -- a container's `delete`, a queue's
`push` -- changes the storage the receiver names in place.

### How a value lies below LIR

LIR states an integral type as MIR does: its width, its signedness, and whether it is two-state or
four-state. It names an operation on one by the language's operator and carries a constant of one as
its bits at its type. It does not carry how a value is stored; the derivation below LIR decides that
from the type, by the rules here. How a value lies is the same on both backends, since the C++
backend's value types lay a value out the same way; what the execution backend adds is what its
prebuilt library causes -- what code compiled once is told of a type, and how a holder of the
library's keeps a value. A choice of representation is justified by what each cost scales with for
any input, never by the designs that happen to have been measured, which only check the arithmetic:
an operation costs the bits it touches, a change propagates at the cost of the part that changed,
artifacts follow what one unit states, memory follows the declared bits by a constant factor, and
every decision reads one unit and the published declarations it uses. Each rule below serves one of
those bounds, or that a program the compiler accepts runs at every width it accepts.

**A value is its machine data, laid out flat at every size.** A two-state packed value is its value
bits in a storage unit of one, two, four or eight bytes, or in 64-bit words past that; a four-state
one is the same followed by an equal run of unknown bits, a position reading 0 as (0, 0), 1 as (1,
0), z as (0, 1) and x as (1, 1) -- the encoding DPI's `aval` / `bval` uses. Every position above the
width is clear in both planes whatever the signedness, because a write is a change exactly where
storage differs and a conversion between a signed and an unsigned type of one width changes no bit,
and both need one encoding per bit pattern. A fixed-size array is its elements one after another,
the element's size rounded up to its alignment apart, with no count, element type or header beside
them. A structure and a tuple are laid out as C lays out a record, each component at the first
offset its alignment allows after the one before; an untagged union is its storage; a tagged union
is its members overlaid, then its tag. The address of any of these is the address of its bytes,
wherever it lies, so a pointer, a reference and a designation each name the value itself. No value
owns storage of its own on account of its size: a value of a type too large for a frame is held
through a box MIR states ([`mir.md`](mir.md)), so what a call takes from the stack never follows a
declared width.

**An element of a fixed-size array is address arithmetic** -- a bounds test, a multiply and an add.
A position naming no element is the one path that enters the library: a read is answered with the
element type's default and a write lands on storage no read reaches (LRM 7.4.5), each asked of the
element type.

**An operator over a value of at most two machine words is instructions on its planes**, which the
code generator derives as a fixed function of the operand type, as rustc's code generation picks
`sdiv` or `udiv` from a type. Past two words each operator is a call of its type's own operation, a
body MIR states over the value's words and both backends compile, which loops over them, so the code
of one site follows no width. The library is called only for what no type's body states at every
width: an operator handed a value made elsewhere (text, a foreign buffer, a net's resolved
positions), a string's bits, and the few operations whose work is long at every width (a power, a
reversal of blocks, a selection of positions). A part of a value -- a select read, or bits written
into a place -- is reached by the words of each plane it spans where the value lies, as clang reads
a bit-field, so its cost follows the part. Each operator up to two words is stated once in the value
library, and that one statement is the C++ value type's operator, the execution backend's entry
where it calls one, and the compiler's constant folding, so a folded, an inlined and a called
operation cannot disagree; what holds the code generator's formulas and the bodies past two words to
it is a conformance case comparing every operator applied to variables with the same operator
evaluated as a constant expression by the front end, which shares no code with either.

**Code compiled before a design exists is told three things about the design's types and no others:
a number of bytes, a number of bits, and the address of a function the design's own code contains.**
It never interprets a description of a type to learn what a value is, because without the type it
can only allocate, move and copy bytes and do integer arithmetic over bits, and anything else means
something only for the type. A type crosses as a constant record laid out as C lays one out, which
the code generator and the library both read: the value's size and alignment and a table of what
every holder asks of a value -- copy, relocation, end, a write over a value already there, and
whether a write changed it -- plus what a kind of value adds (a fixed-size array's count and its
element type's record, a queue type's element and its bound, the function building an element type's
default). Each operation a caller asks by name is handed by the calling unit as a record of its own,
so no type carries an answer for a holder that never asks. Every unit handing a type over states its
record under a name the type settles, and the linker keeps one, as it keeps one copy of a template's
static data member, so one type is one object in the program. How an entry reads each operand is
declared with the entry -- its planes with their width, a quantity with its signedness, a value the
entry keeps with its type, a count or a flag as the machine value it is -- and a call is arranged
from that declaration and from nothing about the value in hand, as clang arranges a call from the
callee's `CGFunctionInfo`. A comparison answers one four-valued bit encoded as its two plane bits,
wherever it was compiled. A net's positions are words and a count, resolved by the same functions
that fold an integral value with the count as their width.

**What holds a value for the library is the library's part, then the value laid out by its type.** A
variable's cell, a net, a class's property, what a suspending call answers into: each is told the
value's width or its type's record where it is built, keeps it, and is told nothing more by the
entries acting on it afterwards. It keeps the value's address at a fixed offset whichever way the
value lies, so generated code reads and writes the value through one pointer, and a value too large
to lie in its holder is one address of storage the holder owns, taken where the holder is built. A
holder holds a valid value from the moment it is built, its type's start, so a reference, a binding
and a force name the value from the holder's construction on, before any initializer writes it
([`lifetime.md`](lifetime.md), [`object_model.md`](object_model.md)). What the library keeps of a
value for itself -- the value a variable held when a time slot began, a driver's contribution, a
history's entries -- is a value of its own, allocated once. Every value laid out by its type is one
domain of the library's, whatever kind of value the type is, and what the library does with one --
hold it, compare its bytes, resolve a net of it, keep its history -- it does by the record it is
told, as Go's runtime moves any value by one function told its type (`typedmemmove`).

In the current implementation a value past 1,024 bytes, and every value past the 16 KiB a frame
keeps of a body's values together, is held through a box; the widest packed type is the front end's,
2^24 - 1 bits.
