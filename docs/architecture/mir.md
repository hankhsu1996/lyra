# MIR

One part of this contract is not yet what the compiler does: a few switches still end in a catch-all
arm (invariant 19) -- among MIR's sets, the HIR-to-MIR lowering of the file input and output tasks
over the runtime entries it does not name; the rest, over the front end's sets and the format kinds,
are the ones `tools/policy/check_architecture.py` lists as standing under A020. The rules below bind
every layer regardless.

## Purpose

MIR is a generic software programming language IR. Its conceptual peers are C++, Rust, and Python --
high-level programming languages with rich type systems, first-class functions, structured control
flow, and object-oriented features. Among compiler IRs it sits where clang's AST and rustc's THIR
sit, and it is **not** rustc's MIR, which is a control-flow graph over places and corresponds to
Lyra's LIR ([compiler_overview.md](compiler_overview.md) maps every layer). MIR is not a hardware
description language IR and not a machine-execution model; it sits in the middle of the pipeline,
expressing the program as software in a generic programming-language vocabulary.

MIR's vocabulary is what those languages share:

- Expressions and statements.
- Function and method calls, including coroutines and closures.
- A type system covering value types, object types, and composing wrappers (pointer, reference,
  vector).
- Object-oriented features: classes with members, member access through an explicit receiver
  expression.
- Structured control flow: `if`, switch, loop, sequence, return; a region whose body can be left
  from anywhere within it -- including from a callable it invoked -- and whose continuation is the
  statement after it, together with the raise that sends an effect the region declines on to the one
  that does claim it; and a body paired with a cleanup that runs on every way out of it.

SystemVerilog is one source language that flows in through HIR; SV does not shape MIR's vocabulary.
SV-specific concepts -- signals as observable storage cells, NBA scheduling regions, event-control
semantics, generate as elaboration -- are translated at HIR-to-MIR into MIR's generic vocabulary:
variables whose type is a capability wrapper, calls against those wrappers' APIs for the operations
that act on the wrapper itself, dereference for naming the storage it stands for, runtime callables
the scheduler invokes, constructor-time logic that builds the object graph. Once MIR sees a value it
sees a programming-language expression with a programming-language type; it does not know or care
that the source was SystemVerilog. The same MIR could in principle express the same program reached
from any source language with comparable semantic surface.

MIR is also not the machine-execution model. Control-flow graphs, basic blocks, storage placement,
register allocation, and instruction-level effect ordering belong to LIR. MIR is high-level
structured code; the program is still readable as software at this layer.

The C++ backend renders MIR to C++ source, and the LIR-then-LLVM backend lowers MIR to a machine
model. Both consume the same MIR. Because MIR is a generic programming language, "the same MIR"
means the same program; the two backends differ only in how they represent each MIR construct (a
method call rendered as C++ method-call syntax versus lowered to an LLVM call instruction), never in
what the construct means.

## Owns

- Objects, members, and member access as first-class entities.
- A member as a type at a position in its object, and separately the identifier a source declaration
  reaches that position by. The type is the sole carrier of the member's storage shape and, for a
  member that holds a child, of its child-scope kind and cardinality.
- Machine data -- a machine boolean, a machine integer, a machine float, a borrowed C string, a raw
  pointer, the fixed-size contiguous aggregate of them, a read-only view of such an aggregate's
  entries, and a plain code address -- as ordinary types of the language, the way `bool`, `i64`,
  `[T; N]`, `*const c_char`, `&[T]` and `fn(A) -> R` are Rust's. A view is built from the address of
  the aggregate it views, and holds as many entries as that aggregate's type says. The family is
  closed on four axes: scalar, aggregate, view, and callable. The boolean is the scalar a predicate
  reduction yields and a condition consumes; it is not a one-bit integer, and not the source
  language's own one-bit value, so a question the language answers as a value -- `$isunknown` (LRM
  20.9) -- asks for the boolean and is taken into a `bit` where the call stands. Machine data is
  distinct from the simulation value types a source language's `int`, `real`, or `string` lower to:
  an integral type is a width, a signedness and a state domain (LRM 6.11), so `int` and
  `bit signed [31:0]` are one type, and it carries the language's own operators, where a machine
  integer is a number a runtime entry takes or returns. A real is laid out as the machine float of
  its width is, the IEEE 754 value LRM 6.12 makes it, and stays a type apart for the same reason: it
  carries the language's own conversions and `**`, and a variable holds it in a cell. `realtime` is
  that real (LRM 6.12), one type below HIR, so nothing past HIR tells the two keywords apart. Every
  value a runtime entry returns as a plain scalar, every table a runtime record reads as raw
  storage, and every value that crosses a foreign-call boundary is one of these.
- The type system: value types (integral, enumeration, real, fixed-size unpacked array, ...); one
  object type, naming a class by the identity the unit's class registry gives it, whichever unit
  defines it ([object_model.md](object_model.md) invariant 2); three composing wrappers, pointer,
  reference and vector, a pointer stating its ownership -- unique, shared or borrowed, the last two
  written `Shared<T>` and `Borrowed<T>` in these docs -- and a reference, as in Rust and C++, being
  bound once to a referent that exists elsewhere and never null, so holding one says only that the
  referent is read where it lies rather than copied; and five categories: the **tuple**, the
  structural product (positional; two with the same components are one type; the tuple of no
  components is the type of a value carrying no information, as Rust's `()` is); the **record**, a
  declared struct, union or tagged union (positional like the tuple, but its identity is its
  declaration, so two with the same members are two types, LRM 6.22.1); the **closure**, an
  anonymous concrete callable value (capture fields plus one invoke body, a distinct type per site);
  the **erased callable**, a closure of a type unknown where it is called, reached only through a
  box or a borrow, of a signature of machine types and nothing else
  ([compiler_generated_storage.md](compiler_generated_storage.md) invariant 10), and beside it the
  **erased value**, a value of a type known only where the program runs, reached only through a
  reference, as Rust's `dyn Any` is; and the **object** (`mir::Class`), the rich nominal object with
  methods and dispatch. A fixed-size unpacked array is an element type and a count, numbered from
  the left, as LLVM's array type is, one dimension per type node -- a further dimension is its
  element type, as HIR states it: the range the source declared is a coordinate system a select
  resolves where it is lowered, so two arrays of equivalent elements and one count are one type
  whatever ranges they were declared with (LRM 6.22.2). A component of a tuple or a record is
  reached the same way: by its position, as a field step on the place holding it, whether that place
  is a variable, a dereference or a value held for the full-expression. The closure and the object
  share a **field substrate** -- a field declaration, a field id keyed within one declaration, and
  one access node -- while staying distinct types; an access names the declaration it reaches into,
  so what the substrate shares is the vocabulary and never the identity. Structural versus nominal,
  and storage versus callable, are the ordinary generic-language distinctions (C++/Rust/LLVM carry
  them).
- Two references to an object that are values of the simulation, beside the machine pointer: a
  managed reference, whose object lives while some reference reaches it -- a class handle (LRM 8.3),
  and an event, whose value is a reference to the synchronization object the runtime defines, or to
  none (LRM 6.17, 15.5): it is held, copied, compared, nulled and stored as a handle is, and
  triggering it, asking whether it was triggered, making a new object and putting a wait on it are
  runtime calls handed the reference -- and one owning nothing (a `chandle`, LRM 6.14, whose pointee
  is `void`; a virtual interface, LRM 25.9, whose pointee is its interface's object). A variable
  holds either in a cell, defaults to null, prints and compares by identity, which no machine
  pointer does; a chandle crosses a foreign call as the machine pointer by a conversion, as an
  integral value crosses as its words. `null` has no type of its own: it reaches MIR only at the
  type its use gives it, which HIR states as a conversion -- the other operand's, or, formatted, the
  chandle's, since a handle naming nothing prints alike whatever type of handle it is (LRM 21.2.1.6)
  -- as the null of that type.
- One type for every type the runtime library declares, named by its declaration and the arguments
  the language gives it -- a type, or a value such as a queue's bound -- as rustc's `Adt(def, args)`
  names a definition of a library crate. A type is MIR's own only where MIR's constructs make, take
  apart or lay out its values, or a rule of the language reads its parts; every other type is the
  library's: a string, a value of any type, the containers whose size changes as the program runs --
  a dynamic array, a queue, an associative array -- a run of bits, a wildcard index, a weak
  reference, what a print or a wait is built from, the storage a scope, a timer or an assertion
  keeps, the facilities the runtime holds, the structures a unit states constants of, and the
  classes of the runtime's. Whatever MIR asks of one -- whether a variable may be declared to hold
  one (LRM 6.2), which of its arguments is a container's element type, its index type or a queue's
  bound, whether a temporary may stand in for it, whether it is storage reached only where it lies,
  how large a frame holding one is, the entry that builds what a declared variable of it starts as
  (taking no operands, called at the type it builds) and the constructor a construction of one
  reaches, the type of the values it holds where it is storage holding them -- it reads off that
  declaration, which states it once for every layer, and MIR holds no per-type answer of its own. An
  argument is read by the role its parameter plays, never by its position, and the verification
  refuses a type named with arguments its declaration does not take. A structure of the library
  whose members MIR reaches -- a dynamic array's core, a queue's core and the recycler its elements'
  storage comes from, an associative array's core, a structure a unit states constants of -- stays
  the library's. MIR names each member by its position, as it names a record's, and reads the
  member's type and place off that declaration, which lays it out, as rustc projects a field of an
  `Adt` another crate defines. A rule the language gives containers -- iterating by key or by
  position, leasing an element a call binds, what a queue's bound admits -- is MIR's to state, and
  it reads the element type, the index type and the bound as the arguments the declaration names
  them; a rule naming one container asks whether a type is declared by that declaration.
- A record is an unpacked structure, union or tagged union the source declared (LRM 7.2, 7.3), as
  rustc's struct, union and enum are one kind of declared type. It carries its kind, its members'
  types, the name another unit reaches it by and a method for each operation the language defines on
  its whole value -- equality, case equality, the bit stream both ways, the questions whether a
  value holds an unknown bit and whether two are the same bits, the positions a net reads and writes
  -- each applying the operation to every member and combining the answers, as a Rust type's
  `impl PartialEq` answers `eq`. A method's receiver is the value it is asked of, taken by reference
  as Rust's comparison operators take theirs; one asked of the type, as building a value from a
  stream is, has none. The declaration brings the methods, so they exist whether or not the
  declaring unit calls them, and a unit naming another unit's record states none and calls them
  through the declaration; an operation the type does not have has no method -- a real or a chandle
  member leaves no bit stream (LRM 6.24.3). A structure's type is the record and nothing else. An
  untagged union's record has one component, its storage: the bits of its widest member, 4-state
  where any member has a 4-state bit, as a packed union's are (LRM 7.3.1), with every member a view
  of them from the first position -- an integral member's bits, a real's IEEE 754 bits, a
  structure's and an array's parts in turn -- so a common initial sequence reads what any structure
  sharing it wrote (LRM 7.3), and a read through another member answers what its positions hold,
  which is no error (LRM 7.3.2). A read of a member makes the member out of the storage's first
  positions and a write replaces those positions by the member's; a literal names its member and
  zeroes every other bit, since every bit may be read; every whole-value operation reads the first
  member, as `%p` and streaming do (LRM 21.2.1.6, 11.4.14.1), except whether a holder changed and
  the positions a net holds, which are the storage's. A tagged union's members overlay one storage
  at its start, laid out by their own types since a tagged union may hold what owns something (LRM
  7.3), with the tag after them; held in an untagged union, its positions are the member it holds
  from the first and the tag above its widest member's, as a packed tagged union lays them out (LRM
  7.3.2). Every read and write of a tagged union's member is preceded by the check LRM 11.9 makes:
  the tag, read by its own node, compared with the member's position, and the design's error where
  they differ; the check and the whole read -- every select written below the member -- are steps of
  one evaluation, so no sibling operand retags the union between them, and a write is checked before
  it lands. A whole tagged value is built by storing the member and then the tag, and assigning one
  ends the old value first. A local a scope keeps past its end (LRM 6.21) is no struct: it is its
  own cell, reached through a `Shared<>` wrapper. A closure is not a struct-with-invoke but its own
  callable-value category.
- A packed structure or union the **source declared** is the single vector its members project onto.
  No member name is carried by any aggregate: an access names a member by its declaration-order
  position, the carrier LLVM IR and Rust MIR both use, and what the language reads a name for --
  rendering a value (LRM 21.2.1.6) -- is computation built from the source type before this layer
  ([backend_contract.md](backend_contract.md), how a value reads under a format). A reading the
  standard grants a type -- its assignment-pattern text (LRM 21.2.1.6) -- exists because the type
  does: it is a callable of the unit, declared for every type the unit names whose text the type
  decides, before any body is lowered and in the order the unit interns its types, so asking for one
  is a lookup that cannot fail, and its identity moves with no body's edit (north star invariant 4).
- An enumeration as a nominal type of its own, whose value is its base integral and whose member
  list -- each member's name and its whole value at the base type, x and z bits and every word
  included (LRM 6.19) -- is part of what the type is, because a value is asked for its member's name
  at run time (LRM 6.19.5). `first`, `last` and `num` are constants of that list; `name`, `next`,
  `prev` and whether a value is a member (LRM 6.24.2) are calls of the library's routines handed the
  member table the unit states as a constant, so the search and the wrap are written once for every
  enumeration. A value is a member where it is bit-identical to one. Below MIR every question that
  reads the member list has been stated as such a call, so LIR carries an enumeration as its base.
- The operations of a fixed-size unpacked array, which has no declaration to state them: one per
  operation the language defines on its whole value, built from its elements' operations, which a
  use calls by the type and the operation, handing each value by reference as a record's method
  takes it. The same for a tuple, built from its components' operations where every component has
  each. The same for a run-time-sized container -- a queue, a dynamic array, an associative array --
  for every operation the language gives it, its methods included: each body is a program over the
  element's type, an associative array's index type with it, and the library's core that keeps the
  container's storage, which it reaches by calling the core's entries, by reading and writing the
  core's members where the core is a structure of the library whose members MIR reaches, and by
  stepping to an element, and the program's own method call is a call of it. An element of a dynamic
  array, a queue or an associative array is a call of the type's own body answering where it lies,
  and every write through an index -- a position, or an associative array's index -- is built on one
  body answering where the write lands and whether it made the element there, as rustc reaches an
  element of a `Vec` by `Index::index`. Storage a body takes for an element is the body's until it
  commits the element there and the container's from then; building an element from a value runs no
  code of the design and fails only for want of storage, which ends the run, so no storage is left
  taken and uncommitted while the design goes on. An element of a fixed-size array is a call of the
  language's element entry, which the lowering below makes a step within the array. Such a type has
  an interface, stated from the type alone and from nothing else: the items of each trait of the
  library it implements, the items of each language trait it implements -- what a construct asks of
  a value handed to it as an operand: a memory task's memory (LRM 21.4), an open array formal's
  actual (LRM 35.5.6.1), a random variable walked part by part (LRM 18.4) -- and the methods of its
  kind's own inherent impl, each of a fixed signature or a family taken at a type argument, as
  rustc's trait impls and inherent impl for a generic type name their items, and for each the
  declaration -- what it takes and answers, with no body -- stated where it is asked, as rustc's
  `fn_sig` states an item's signature apart from its body. Every call of an operation of the type
  names one of them and hands it what its declaration takes, and each body is built from that
  declaration. A body that walks an array's elements counts a machine index below that array's own
  count and reaches each element by it with no check, to read or to write, as Rust's core reaches a
  slice's by `get_unchecked`; a position the program computed or is handed is checked against the
  count before it reaches one, by the element step or by the body itself (LRM 7.4.5). A type whose
  value's life is its own -- a dynamic array, a queue, an associative array -- states in its
  interface the record its value is laid out as and its four operations on a whole value: its
  default and a copy of a value, each answering the value it builds, as Rust's `Default::default`
  and `Clone::clone` do, writing a copy over a value, and ending one. A move is none of them: it
  hands a value on with its ownership whatever the type, and writing a moved value over another is
  ending the one there and then the move. A type that states none has the steps its parts give it,
  which the lowering below builds. Each operation's declaration states whether a call of it can be
  unwound out of, as a runtime entry's does and as clang's exception specification and LLVM's
  `nounwind` say it, and one that cannot promises that no unwind leaves it, which the unit's
  verification holds its body to. A fixed-size array and a tuple have an interface too, stated from
  the type alone: the operations on a whole value its parts decide, as a generic impl exists where
  its type arguments meet its bounds, and a fixed-size array's own methods and the items of its
  language traits; a record's methods are its declaration's, each declared the same way. An integral
  type wider than the widest integer a machine operates on at once has one as well: each operator of
  the language over a value of it, a body over the words the value lies in, as compiler-rt and
  Rust's `compiler_builtins` state in the language itself, over the machine's words, what a machine
  lacks for a wide integer. Those words are the one layout every backend gives the type, so such a
  body states them as a view of the value's storage, and nothing else does. An operation takes and
  answers the same at every type that has it, as one trait method's signature is every
  implementation's, and an array's operation is one program over its elements whatever kind of array
  it is, asking the array how many it holds. A type is named by what it is, the same in every unit
  that names it, as clang's canonical type names a type rather than a translation unit's spelling of
  it, so two types are never one. A unit holds no such body, as a call in rustc's THIR holds no
  instance: a body is built when something below this layer finds the unit needs it, as a generic
  impl is instantiated where it is used, from the unit's types and nothing of the source, then
  legalized and verified alone and lowered with the unit. What needs one is a call naming it, a
  record of the library that a value of the type is handed over with, which only the target
  realizing the record can state, or an item of the type's implementation of the library's traits
  that a target's library asks of a value of the type wherever its own code holds one. A type the
  unit only names has none, and a call is well formed without one.
- Which of a type's operations the runtime library asks of it by name: the library declares its
  traits -- a value's life, its change test, its elements, being made a count long, being taken
  over, a net's positions, the questions asked of a whole value, its stream at a width the program
  or its type decides -- each the operations it requires, a library declaration lists the traits its
  types implement, and MIR states a type's implementation from it, the type and the traits, as rustc
  resolves an impl of a trait for a type. A type implements a trait only where its parts allow -- a
  container streams only where its elements stream -- and the implementation states that condition,
  as a Rust impl states its bound. A fixed-size array, which no declaration states, has as items
  each operation of its interface some trait requires, since the library asks each item of a value
  apart from the rest. Each target's library asks the items it needs, however it is told of them,
  and no target decides which items the type has.
- For an array of any kind holding a bounded queue below its outermost dimension, a value of it
  taken into the form of its type with no queue bounded, and a value of that form built as one of
  it, each element built afresh. Every type equivalent to it has that form (a bound is no part of
  equivalence, LRM 6.22.2), so a value is assigned across two such types by passing through it, each
  queue keeping what its own bound admits (LRM 7.6, 7.10.5) -- wherever a value is assigned to a
  type of its own: an assignment, an argument, a returned value, a pattern's item, a value a queue
  method inserts. Each is built where a call reaches it, as a fixed-size array's operations are.
- What a value of a type is before anything is written to it: the code that builds it, stated once
  for the type, because the source's declaration decides it -- a structure member's own initializer
  is part of it (LRM 7.2.2, 7.4.5) -- and nothing below this layer has that declaration. An array's
  type names its element type, so whatever builds, sizes or answers an array of any kind learns how
  its elements start from the type. It is one of the type's operations above, built as they are for
  a type no declaration states it for, and a tuple's is each component's where each has one; an
  untagged union's is its storage holding the first member's. It is what reading an entry an array
  does not have answers (LRM Table 7-1), and for every type but one it is also what a declaration
  that writes no initializer starts at (Table 6-7). The exception is an event: its type's value
  reads as no object, and a declaration -- of the event, of each element of a fixed-size array of
  them, of each member of a structure whose declaration gives it no initializer -- and an array
  growing by `new[N]` (LRM 7.5.1) each state a new object per position on top of the type's value,
  so no copy of one value stands for several objects.
- Capability wrappers: types that represent a storage place instead of being a value -- an
  observable cell; a reference to a cell, which reads and writes through the cell's protocol and is
  either bound once as it is created (a `ref` formal, as C++'s `T&`) or held as a member, bound
  after its object is built and again by a force (a port, as `std::reference_wrapper`); a net's
  resolved value; a net driver's contribution; and a write in progress into what a wrapper or an
  object holds, with the place designated within it. A capability wrapper composes over the value
  type it represents, and the wrapper and that storage are distinct places (invariant 14). The
  capability the wrapper carries -- notifying waiters on change, aliasing another place, resolving
  contributions, reporting a write when it ends -- is a fact of the type alone; naming the wrapper
  type states it, and nothing beside the type restates it.
- Callables: one concept -- callable code (a signature, plus a body where the declaration defines
  it) and a callable value (code plus a bound environment). Which bodies have a receiver is
  [callable.md](callable.md) invariant 2's, and how MIR states it is invariant 11 below. The body
  reaches every value it needs through its parameters and bound environment, never through implicit
  context. [callable.md](callable.md) is the canonical contract.
- How a call names what it runs, in one node whose callee is one of four forms. A **direct** call
  names its target in one of four identity spaces, each resolved by a table of its own: a callable
  the unit declares (invariant 6); an entry of the runtime library; an operation of a type no
  declaration states it for, named by the type, the operation and the types it is instantiated at;
  and a body another unit's namespace is brought up through, which answers to no name. An
  **indirect** call goes through a code address the program computed, as the entry an export
  publishes reaches its implementation (LRM 35.5.4). A **virtual** call names its object and the
  behavior it dispatches on ([object_model.md](object_model.md) invariant 7). A **construction** is
  a library type's one way of coming into existence, which naming the type names. The object a
  direct or virtual call is entered on rides the callee, never the argument list
  ([callable.md](callable.md) invariant 3); so does the part an entry acts on where the call fixes
  it -- a component, a tagged union's member, a property, by its declaring class and slot -- since
  its type is a function of which part it is, as LLVM's `extractvalue` index is no operand; and so
  do the types a library entry generic over a type is called at, as a C++ call names a template's
  arguments. The arguments are then exactly the operands the source wrote, plus what the entry's
  declaration says it takes beyond them.
- What a runtime effect is: an ordinary direct call of a runtime library entry. The entries are one
  closed set every layer names alike, the receiver's type supplying the type-side context, so one of
  LRM 7.12's array methods is one entry whichever container it is called on. Every property of an
  entry -- what the library calls it and how a call reaches it, whether it acts on an object and as
  which class, whether it updates that object, what it answers and what it is told of the type it
  answers at, how it reads each operand, whether it takes the run's handle, the report of what the
  evaluation reached, or where it was called, whether it returns and whether it can be unwound out
  of, whether it answers a wait -- is one row of the entry's declaration, so a consumer reads the
  property it needs and none lists the entries that have it. The run's handle is an ordinary
  operand, the answer of a call of the library's entry for the running run, since the run is reached
  ambiently and never through a receiver or a parameter of the body
  ([runtime_model.md](runtime_model.md)). An effect whose source form omits an optional argument is
  handed that argument's default by the lowering, so the argument list is always complete, and a
  form that differs by more than a default is an entry of its own; an entry answering through
  destinations the call names completes with the values it settled, and the call site writes each
  where the source named it. A construction states which of four things it is, and no consumer
  recovers that from the result type, the operand count or an operand's type: a value that is its
  own parts is the value-build primitive; an operation a library performs -- building a container
  from an element list, sizing one at run time (`new[N]` and `new[N](src)` are two entries, LRM
  7.5.1), reshaping a value into another representation -- names its entry; a value re-typed with
  its bits unchanged is a cast; and a library type's one way of coming into existence is a
  construction. An operation the front end answers outright -- LRM 7.12.4's `item.index`, LRM
  6.19.5's `first` -- is no entry, since no layer could hold a call of it.
- The identity of each object and member within a compilation unit.
- Lightweight structured control flow inside a callable body: `if`, loop, sequence, the region that
  consumes a control effect naming it, the raise that passes one it declines outward, and the body
  paired with a cleanup that runs on every way out of it -- which is how an extent whose exit is an
  effect is stated, rather than by relying on a target language to run code at scope exit. And the
  switch: a machine integer and the values each case is taken at, every value at most once, and what
  runs where none is -- clang's `SwitchStmt` and rustc's `SwitchInt`; a case ends at the end of its
  body, so the order of the cases means nothing. A loop, and a block standing as a statement, may
  carry a label, and every `break` names the one it leaves, as rustc's THIR names the scope every
  `break` leaves; a label is carried only where a `break` names it. No basic blocks at this layer.
- A primitive expression set: literals -- a floating one typed at either floating type, the machine
  float or the real of its width, as clang's `FloatingLiteral` takes its type -- references, unary /
  binary / conditional operators, calls, casts, closures, the block expression, member access
  through an explicit receiver expression, the field step that names a component of a product, a
  member of a union or a member of a sum on the place holding it, the tag a tagged union holds, the
  dereference, the address-of that turns a place of ordinary storage into a borrowed pointer to it,
  the entry of a machine aggregate at a position the program computes, which is a step of the place
  holding the aggregate as rustc's `ProjectionElem::Index` is, the element of a sequence the
  declaration of several objects holds, by position, the read of a place, the move out of one,
  value-build primitives for the literals that spell an aggregate, the borrow, the new-expression,
  and the two waits: awaiting an execution, which yields what it completed with, and stopping where
  a parking call says to, which yields nothing -- each standing only at the top of a statement,
  since SV suspends only there (LRM 13.4).
  - The **block expression** is a sequence of statements and the value it ends with, which is how an
    evaluation of several steps stands where only an expression may. It is sequencing alone: it is
    not a callable boundary, so its steps belong to the enclosing body and capture nothing; it has
    no control-flow effect, so control never leaves from among its steps; and it always yields a
    value, since a sequence that settles none is a statement. Its scope ends when it yields, once
    the value is taken out, so what its steps declare and what its value makes end there and what it
    yields refers to none of it. Any statement form gains a value by standing inside one, so no
    control construct needs a value form of its own.
  - The **new-expression** allocates storage a pointer or a class handle owns and evaluates its
    initializer into that storage rather than handing it over as an argument -- for an object, its
    construction, which first creates the object whole and then enters its constructor on it, so the
    object is of its own class while every constructor runs, or the creation alone, where something
    is evaluated on the object first and the code then enters its constructor by name
    ([object_model.md](object_model.md) invariant 6) -- and in its placement form evaluates its
    initializer into storage an allocation function hands out and answers a pointer owning nothing,
    as C++'s `new (p) T(x)` does.
  - The **borrow** binds a reference to a place, or to a value it holds to the end of the
    full-expression as a C++ reference bound to a temporary does.
  - An element of the source language's arrays is no node. It is a call answering a reference, a
    read form or a write form, because the language gives a read and a write of an element different
    outcomes -- a read through an invalid index answers the element default and a write through one
    is ignored, and an associative entry is allocated by a write and never by a read (LRM 7.4.5,
    7.8.6, 7.8.7) -- and the dereference of what it answers is the place. HIR-to-MIR takes the write
    form where the place is an assignment target, a `ref` actual, a mutating receiver, a
    read-modify-write or a nonblocking target. Bits of a packed value are a step of the place the
    value lies in, as a field is, and a step taken on bits is a step further along the same place,
    so nested selects are read or written once; bits lie at no address of their own, so a reference
    bound to them binds the value a read of them makes, as a C++ reference to a bit-field binds a
    temporary. A window of several elements is no place: it is read as a value of its own and
    written by its array's own operation on the place the array lies in. A string's character is no
    place either: a read is a call on the string read, a write a mutating call on the string's
    place.
  - What an operation takes each operand as is fixed by the operation. A place stands where a value
    is wanted only under a read or a move; a write's target, what a borrow or an address-of takes
    and the object a field is reached on are the storage itself; the object a call is entered on is
    named where it lies, as clang binds the implicit object argument, and an entry taking it by
    value is handed it read by each target's realization of the call. A node's operands run in the
    order it lists them, an assignment's value before its target and a call's arguments before its
    callee and the object it is entered on, and a read takes what its storage holds when it runs.
  - An integral operation whose operands are all constants of the unit is itself a constant of the
    unit: the builder stating the operation asks whether it folds, as LLVM's `IRBuilder` asks its
    `ConstantFolder`, and the answer is computed by the value library the run itself uses, so a
    folded operation and an unfolded one cannot disagree; the unit's verification asks the same
    builder, so it holds an operation to being folded exactly where the builder would fold it. An
    operation that can fail while running is not folded. Nor is one whose answer is too large for a
    frame and wider than its operands together, since folding states as data only what its operands
    already state as data; for the same reason a value of such a type that reaches the lowering
    already a value -- a fill literal, a declared default, a parameter's value -- is stated as the
    extension of its narrowest low part, so what a unit's data holds follows what its source wrote
    rather than the width it was declared at.

  The set is closed under what a generic programming-language AST needs to express; it does not grow
  to model a particular backend's storage realization or runtime library shape.

- Which values are held through a uniquely owning pointer because a value of their type is too large
  for a frame -- a local, a parameter taken by value wherever its signature is stated, a made value
  and a capture -- or because the values a body keeps before them already take what a frame may keep
  of them together -- a local, a variable's cell, a made value -- each written as a box by the
  conversion into the legalized phase, so every backend keeps such a value off the stack by
  translating what it is given. Every type is laid out flat at every size, an integral as its words
  and a structure as its members, and no value type owns storage of its own on account of its size,
  so a frame's size follows no size a design declares. A large value is made in its box, never in
  storage of its own first; no body answers a box, so a caller reading a large call as a value makes
  it in a box and the body answers into its return place. Making the evaluation order explicit asks
  no value's size; deciding where each value lies is the step after it, and asks a type how large a
  value of it is by one query, as rustc's passes ask `layout_of`. A closure kept past the statement
  building it is made where its keeper keeps it: one whose keeper hands out storage for it -- a
  region's deferred effects, an assertion's control, a concurrent assertion's attempts -- is made
  there by a placement new-expression and committed to the keeper in the same expression, as C++'s
  `emplace` builds an element where its container holds it; a closure any other keeper holds in a
  box that keeper takes. Every body MIR holds is concrete: an operation over many types is built per
  site or per type by a builder of the lowering, never stated as a generic function, as clang
  defines a class's implicit members per class and rustc builds drop glue and clone shims per type.
- Who owns each value and until when, stated by the nodes that take values and the scopes that hold
  them, as clang's AST and rustc's THIR state it, and never by a node per copy or per end. A read of
  a place makes a value of its own, independent of the place, as its type copies it: a handle's copy
  names the same object, a queue's holds elements of its own. A move hands the place's value on with
  its ownership, so the place owns nothing after it, keeps no object in use, and is ended with
  nothing to release; it is the last use of a whole local on every way that reaches the local's end.
  A borrow lends without owning, and a value it holds lives for the evaluation that made it. A local
  is owned by the block declaring it and ends on every way out of that block. What the lowering
  makes to carry one evaluation out -- a full-expression, a condition each time it is tested, an
  operand before a suspension -- is a step of that evaluation, and ends when the evaluation
  completes, as a C++ full-expression's temporaries do; what is wanted afterwards is owned by where
  it went, a variable, a callee, or a deferred effect's closure. A closure the runtime keeps past
  the frame that made it -- a deferred effect, a started body -- refers, without owning, only to
  storage that lives for the whole run, and the unit's verification refuses any other where a build
  asks for every check. Every copy, transfer and end follows from these by one rule, which the
  lowering to LIR and the C++ target both apply ([lifetime.md](lifetime.md) owns when each ends).
- Action shapes for constructs that bind behavior to schedule events (always blocks, continuous
  assignments, deferred assertions, concurrent assertions). A value-change wait, a trigger, a
  sampled read and a takeover are calls on runtime types, never nodes
  ([scheduling.md](scheduling.md), its MIR boundary).
- What decides how a definition is linked, which no node states because it is no fact of the
  language: who may name each definition -- what a unit published, and what is reached only by its
  position -- and whether a unit's declarations alone build it or one body's code does. How many
  artifacts define a name is a fact of how a unit is compiled, as rustc settles it after its MIR
  when it splits a crate into codegen units; here a unit compiled to several bodies is several
  artifacts under one unit's names, and the C++ target reads MIR directly, so the answer is one
  query over these facts that the lowering to LIR and that target both ask, as clang's code
  generation asks `ASTContext::GetGVALinkageForFunction`. A definition every body states alike
  stands in for the others where the program is linked, so it names nothing that is one body's own
  -- a closure, a class or a function a body numbers among its own -- and the unit's verification
  refuses one that does, through every constant it reaches. A constant is the value it is and an
  operation of a type is named by the type and the operation, so naming either means the same in
  every body. A constant is data each body holds for its own code and its own definitions, and has
  no linkage of its own.
- Every value settled before the program runs, as the value it is: an integral value's bits, a
  machine integer, a null, a text, an address -- of another constant, of a class's definition, of a
  variable of a namespace or a class, of code -- a structure of the library a part per member, an
  array, and a view of an array. The unit holds each value once with the type it is a value of, and
  a use names the entry, as clang states an evaluated constant as an `APValue` and rustc as a
  `ConstValue`. A constant's identity is its value and its type together, since the type decides how
  many bits are read, whether as signed and whether an unknown plane exists; the bits are canonical
  before they are compared -- one word per 64 bits of the width, those above it clear -- so two
  spellings of one value are one entry, and two tables holding the same entries are one constant
  however many makers asked for it. Nothing in one runs, so no consumer evaluates one, and none is a
  place: nothing writes through a constant. An enumeration's member table, where an operation was
  written, and every table a class's definition points at are such values, and a class's definition
  is one too, held by the class, since every unit names it by the class. The table its objects
  dispatch through is not: the class states which body answers each behavior, and the table is laid
  out below MIR, once for every target ([object_model.md](object_model.md) invariant 8).
- Where in the source each expression and statement came from: the front end's own value for the
  piece of source it was lowered from, or no place where it was lowered from none, as rustc's THIR
  carries a span on every expression. A node is given it as it is built, by whatever lowers that
  piece of source, and a rewrite gives what it builds the span of the node it answers, so a node
  moved by a rewrite keeps it. It changes nothing a node means; each consumer says it to what fails
  while that node is translated, and only the front end opens it.
- Where a call that can fail the design was written, handed to the callee as its last argument: a
  value of the library's location type, one word, either the address of the unit's constant for
  where the call starts, read as that type -- the unit holds one constant per distinct file, line
  and column, each a record of the library's stated with what it says, as clang's AST context keeps
  one per location -- or, inside a body that reports at its caller, the one that body was handed,
  which a closure built there captures. Which callees take one is the callee's own fact -- a runtime
  entry takes one unless its declaration says it takes none, and a body of the tool's own text, or
  one generated for a type's operation that can report, states the parameter it is handed it in --
  and a rewrite of the stage states the argument for every such call, so no site composing a call
  knows which callees take it, and the verifier holds every call to it. How the library carries a
  location past its door is the library's ([runtime_distribution.md](runtime_distribution.md)), and
  MIR says nothing of it. The semantic kept is what LRM 20.10 asks of a report and rustc's
  `track_caller` gives a callee: where a failure is reported is the design's statement, never the
  library's or the tool's own line.
- Opening a class handle, as a call of the library's entry that answers a pointer to the object the
  handle names, which the dereference then reads through, as rustc's `ElaborateBoxDerefs` states a
  `Box`'s dereference through the pointer inside it. A handle naming no object fails the design
  there (LRM 8.4), so it is a call like any other that can fail, handed its location. A unit is
  built with the dereference of the handle and a rewrite of the stage opens it, or, where opening it
  is all a call does, with the opening itself, so no target dereferences a handle itself.
- A textual dumper that serializes MIR for inspection. The dumper is not a backend; its output is
  not executable, and nothing asserts on its wording. It prints no source span, which only the front
  end can read.

## Does Not Own

- Control-flow graphs, basic blocks, branches, or phi nodes.
- Storage offsets or memory layout on a node -- where within storage a value lies -- except an
  integral type's words, which every backend lays out alike and which only the bodies of that type's
  own operations read. Whose storage a value is made in -- a box's, or the storage a placement's
  allocation function hands out -- is MIR's. A rewrite of the stage asks how large a value of a type
  is, to decide which values are held through a box; no node carries the answer, and how the box and
  its target are laid out stays each backend's.
- A source language's coordinate system: a declared range, its direction, which part-select form the
  source wrote. HIR-to-MIR reads it, and a select states where it lands in the selected value's own
  numbering -- a bit from a packed value's least significant end, an element from an unpacked
  array's left -- and how many parts a fixed-size run takes, which the select's result type fixes
  (LRM 11.5.1). An index into a value that numbers its own parts -- a queue, a dynamic array, a
  string -- is stated the same way: the lowering converts it to a position where the source wrote
  it, so no consumer is handed an index beside the type it was written in. Where a declaration does
  not number from the index's own zero, the index is brought to a 64-bit signed 4-state position,
  where no shift a declaration asks for wraps and an unknown index stays unknown, and the origin,
  direction and element width are applied there as ordinary arithmetic; a position with an x or z
  bit names no part (LRM 11.5.1, 7.4.5). A queue's `$` is a leaf HIR keeps, which HIR-to-MIR reads
  as one less than the size of the one evaluation of the queue its select makes (LRM 7.10.1). That
  numbering is the value's, not a storage layout, the way an LLVM `extractvalue` index or a shift
  amount is.
- Scheduling, dirty tracking, or wakeup filtering.
- The frontend's symbol or string identity. MIR carries only MIR's own ids.
- SystemVerilog source-level syntactic sugar. Sugar collapses to MIR's primitives at HIR-to-MIR.
- What a write reports and what storage has an identity of its own ([storage.md](storage.md)); MIR
  owns the nodes that state each.

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose. The body of an
invariant should be derivable from the identity; an invariant that cannot be re-derived is the
suspect, not the analysis ([lowering_organization.md](lowering_organization.md) states this
discipline in general).

1. Every compiled entity is exactly one of: an object, a member of an object, a callable, or an
   action bound to a callable. _Programming-language consequence: the entities a program defines are
   types, members, functions, and registered effects -- nothing else._
2. All hierarchy is expressed as object ownership. A hierarchical reference is navigation on the
   object graph, not a lookup in a side table. _Programming-language consequence: containment is
   expressed in the type system, not by a parallel registry._
3. Generate is lowered to constructor-time construction logic. A module's constructor builds its
   children via `if`, loop, switch and sequence over construction actions. _Programming-language
   consequence: generate is not a meta-language at MIR; it is ordinary constructor code._
4. MIR ids are the single source of identity at this layer. Earlier-layer ids are not carried
   through. _Programming-language consequence: a layer's names are owned by the layer._
5. MIR admits a pure textual dumper that is lossless with respect to MIR identity and structure. It
   is a reading surface, not a compilation path and not an assertion surface. _Programming-language
   consequence: the program is readable as text at the layer where it is meaningful as software._
6. MIR does not reconstruct topology or identity from side tables. A member is owned by exactly one
   object; a callable by exactly one owner. A callable has one identity in a unit, its entry in the
   unit's one table of callable declarations, whoever declares it and whichever unit defines it
   ([callable.md](callable.md) invariant 11): the declaration states its owner and, where the owner
   has places, its place among the owner's callables; what an owner declares is read off those
   declarations and stored nowhere else; and every node naming the callable -- a call, a function
   reference, a dispatch entry, a virtual slot -- names that entry. A symbol is spelled from the
   names the source gave and, for a body given none, from its place among its owner's callables,
   never from the entry, which means nothing past one build. _Programming-language consequence:
   ownership is structural, not name-based, and a function has one identity whoever declares it, as
   a Rust `DefId` names a free function, an inherent method and a closure body alike._
7. A member is a type, and its identity is the position it occupies in its object. Being reachable
   by an identifier is a relation the object holds over that position, stated only where the source
   declared one, so nothing the compiler introduces answers to a name a design could also write. The
   same holds wherever MIR declares: a class, a unit's namespace and a body each hold a list pairing
   an identifier with what it reaches -- a member, a callable, a static, a local -- and a
   declaration the front end or the lowering introduced is absent from it, carrying no name of its
   own. A child -- a module instance or a generate scope -- is a member whose type is a borrowed
   pointer to the child's object type, the handle every route projects through; who owns the child
   is [hierarchy_and_generate.md](hierarchy_and_generate.md) invariant 4's. An array of children is
   a member whose type is a vector of that borrowed pointer; a multidimensional array nests the
   vector wrapper. No member carries a classification flag beside its type; the type is the
   classification. _Programming-language consequence: the type system carries every fact about a
   member; no parallel discriminator exists, and no name has to be invented for a declaration the
   source did not write._
8. A class has one identity in a unit: the registry entry naming it, whichever unit defines it and
   however the name reaching it was written ([object_model.md](object_model.md) invariants 2 and
   17). Whether this unit defines the class or declares another unit's is a fact of the entry, never
   of the reference, and a declared entry holds nothing this unit emits. For a child scope that fact
   is its runtime scope kind -- a generate scope's class is defined here, a module instance's by its
   unit -- and nothing else within MIR encodes it; the records of the scopes a class declares are
   data the class hands the run, which no MIR consumer classifies a member by. _Programming-language
   consequence: a class reference is one kind, as a Rust `DefId` names a local or an extern item
   alike; which crate defines it is a property of the item._
9. Instance multiplicity is the vector wrapper, a property of the member's type, which carries no
   length -- the constructor decides how many elements stand there -- and which element a reference
   names is an index operand of the projection over the one member, a constant the route carries. It
   is orthogonal to which unit defines the held object's class. The same wrapper composes over any
   object type and to any depth. _Programming-language consequence: cardinality and kind compose
   through generic type wrappers._
10. Every semantic decision is explicit in MIR's structure, expressed in MIR's existing primitive
    set. MIR is consumed by more than one backend; each chooses only how to represent MIR's stated
    semantics in its target, by a fixed function of the node kind. A backend reads a semantic fact
    from a MIR node or reference; it never re-derives or re-decides one, and it never expects MIR to
    grow a node kind that carries a backend storage realization, a runtime library's shape, or
    source-language sugar -- a fact of that kind is already expressible by combining existing
    primitives, so if it is missing, HIR-to-MIR is incomplete and should emit that combination
    instead. The primitive set does grow, but only for a genuinely new generic-language concept,
    never to model a backend / library / sugar shape (see Owns). The backend-side consequence is
    owned by [backend_contract.md](backend_contract.md): a backend entry is a fixed function of one
    MIR node and chooses a spelling rather than an operation, and the canonical falsifier for a MIR
    shape is "could a mechanical LLVM IR backend translate this without decisions?" When designing
    or extending a MIR primitive, every binding invariant here cross-checks against
    `backend_contract.md`'s mechanical-translation invariant; the two are halves of the same
    contract. _Programming-language consequence: the program text is the program; consumers do not
    invent vocabulary the language does not have._
11. A body reaches the object it is entered on only through the binding its code names as its
    receiver. Which bodies have one is [callable.md](callable.md) invariant 2's; MIR states it as
    one of the body's own bindings (the callable's binding arena is owned by
    [binding_and_capture.md](binding_and_capture.md)) -- the first parameter where it is a
    parameter, a closure's first local where it is not -- that the code names as the receiver, so no
    consumer infers it from a position or a spelling. It carries no name, since nothing the compiler
    introduces answers to an identifier a design could write (invariant 7). Access to any class
    member -- a member variable, a child instance -- flows through a field access whose receiver
    expression reaches the class from that binding, never through an expression that means "look
    around and figure it out", so a consumer reads the object a body runs on and derives none.

    _Programming-language consequence: a method takes its object explicitly. This is the C++ `this`,
    the Python `self`, the Rust `&self`. Languages that hide it behind keyword sugar at source level
    still expose it explicitly in their IRs (LLVM IR's first parameter, Python's
    `__call__(self, ...)`); MIR does the same. A static method, like a free function, takes none._

12. A nonlocal cross-instance reference is one MIR shape; lexical form does not split it into
    species. A reference whose target lives across an instance boundary is the same MIR concept
    whether the source spelling reached it as a bare name, a downward dotted name, an upward dotted
    name, or an absolute path. _Programming-language consequence: a program has one kind of "name
    that reaches another scope's value"; the syntax that named it is not a structural fact about the
    reference._

13. A nonlocal reference's runtime endpoint is bound in Resolve, before any read observes it, and is
    read directly thereafter. The act of reaching across an instance boundary -- traversing the
    object graph through typed steps -- happens once during elaboration, and no step matches a name;
    the hot path reads the bound endpoint without traversing. _Programming-language consequence: in
    the same way a Rust binding once initialized refers to its target by address rather than
    re-locating it on every use, a MIR reference's runtime cost is paid at the bind, not at access._

14. A capability wrapper's place and the storage it represents are distinct places, told apart by
    structure alone. A bare wrapper place denotes the wrapper: reading it yields the wrapper as a
    value, storing into it rebinds the wrapper, taking its address yields the wrapper's address. A
    dereference of that place denotes the storage the wrapper represents: a descent rooted at it
    reaches part of that storage, and passing it as a by-reference argument lends that storage to
    the callee. Naming storage that way is place formation, not an operation, so it is never a call;
    the protocol that realizes it comes from the wrapper type at each consuming backend, once, for
    both of those consumers.

    Reading that storage, and replacing the whole of it, are operations on the wrapper rather than
    ways of naming it. A read asks the wrapper for what it decides to show, and answers with a
    reference to it, whose dereference is a place the read node then reads; a whole write replaces
    what the storage holds and publishes that change to whatever the wrapper relates to in the
    object graph. Both are ordinary calls against the wrapper's API, alongside the other operations
    that act on the wrapper as an object -- installing its declared representation, attaching it to
    the object graph. Replacing a part is not the same operation at a longer path: it acts on the
    value the storage holds and reaches the wrapper only to name where that value lives, so it is a
    descent through that value ending in an assignment, not a call on the wrapper. What a call
    answers is a value; where it is a reference, only its explicit dereference is a place.

    _Programming-language consequence: this is the distinction every generic language draws between
    `p` and `*p`, applied to every type that represents storage, together with the one every
    language draws between naming a thing and asking it to do something. Rebinding a wrapper and
    writing through it are different programs -- one a store into a place, the other a call -- so
    they are different MIR, and no consumer needs to know which lowering built either to tell them
    apart._

15. No run of a body evaluates a node that computes twice. Every consumer evaluates a node at each
    place that reaches it, which is what serves the requirement that an operand the source wrote
    once is evaluated once: a node reached at two places one run can both take is that operand
    evaluated twice. A value wanted at several such places is bound to a local, by a statement or
    among a block expression's steps, and named at each. Two places under different arms of one
    conditional expression are not such a pair, because a run takes one arm. A node that names a
    declared thing, spells a constant, forms a place, or reads a value where it lies in storage
    nothing but a binding of the body or a constant reaches -- a variable the body declares or a
    closure's body captured, a constant, a field of any of them, or what a box any of them holds
    owns -- computes nothing itself, so it may stand anywhere, and what it stands for is the
    evaluations of what it reaches. Which storage a read reaches is a fact of its place, so deciding
    where a value lies, which holds a large one through a box, changes no node's answer. A read of
    any other storage computes, since a write between two of them makes them two values; so does
    reaching an object through a class handle, which opens the handle and can fail the design, in
    the unit as it is built as much as once a conversion has made the opening a call. Which
    computations would be harmless to repeat is not stated and not asked: a call with no effect is a
    call. The unit's verification refuses a body that breaks this, where the unit is produced and a
    build asks for every check.

    _Programming-language consequence: an expression is a tree, and a value used twice has a name.
    This is the relation a compiler's syntax tree has to a value bound once and referred to from
    several later expressions, and the structured form of a definition dominating its uses._

16. Whether an expression names storage or makes a value is the kind of its node, settled where the
    node is built. A reference to a variable, a dereference, and a field, element or slice step are
    places, and nothing else is. A place states what kind of storage it is -- ordinary, with an
    address of its own; or with none of its own, at or below a member of a tagged union, which
    nothing binds (LRM 13.5.2) -- and each node's kind follows from its operand's. A member of an
    untagged union is a slice step of the union's storage, the positions it takes, so a write to it
    or below it is a write of those positions. Storage is read by a read node, written by an
    assignment whose target is a place, and referred to by a borrow, and each is realized by the
    place's kind. A value where a place is needed is held for the full-expression. _Programming-
    language consequence: this is value category -- a THIR expression's place-or-value category in
    rustc, a value kind and an object kind on every clang expression -- and like theirs it follows
    from the expression's own nodes, read by one query every consumer asks, so no consumer works out
    from a finished tree which expressions name storage by a rule of its own, and no pair of nodes
    or entries differs only by whether it is read or written._

17. A value handed to something that states its type is of that type. An assignment hands its
    target's type, a whole write through a wrapper or a reference to bits the type held or reached,
    a local's initializer the local's, a return what the body answers, an arm of a choice and the
    value a block ends with the node's own, a capture its field's, an argument of a callable the
    unit declares its parameter's, and that callable's call the type it returns -- the relations
    LLVM's verifier holds every operand to, and what LRM 10.8 calls assignment-like contexts. A
    value only alike in representation -- a signed vector's whole select, a reference taken where a
    `const ref` formal binds it -- is held as the other type by a conversion the program states, as
    clang states a qualification conversion and rustc a reborrow. _Programming-language consequence:
    a typed IR checks its operands, so a rewrite that drops a conversion a write needs is a refused
    unit rather than a program whose bits happened to come out the same._

18. A type is interned, and its key is every field its alternative carries. Constructing a type
    answers the one identity for what it is, so within one unit two types are equal exactly where
    their identities are; a nominal type is keyed by its declaration -- a class by its registry
    entry, a record by its declaration's -- because two with the same members are two types, and an
    enumeration by its base and its members, since LRM 6.19 makes its enumerator names unique in the
    scope that declares them. A field that is no part of what the type is -- a spelling, a location,
    a cached answer -- is not carried. A type's identity depends on the order a build constructs
    types in, so it is never a key across two builds, which is a semantic key worked out when asked
    ([incremental_build.md](incremental_build.md)). _Programming-language consequence: a forward
    reference to a class and its definition are one type, since a cycle among types passes through a
    nominal type whose identity exists before its body, as clang's canonical types and rustc's
    interned `Ty` give._

19. Every alternative of a closed set MIR states -- a node kind, a type, a callee form, a runtime
    entry, a classification an operation reads -- is consumed by a dispatch naming each alternative,
    never by a comparison against one or by a catch-all arm, and each of MIR's two consumers names
    every expression and statement alternative. A set gaining an alternative then breaks the build
    at every consumer until each says what the new one means, which is what serves the requirement
    that a fact MIR states reaches every target (invariant 10). Each way around it answers the new
    alternative silently and keeps the suite green: a comparison sends it to whichever side did not
    name it, a catch-all answers it plausibly, and a node one consumer never names is either one
    target's spelling the other has no arm for or a meaning the other lowers around, while the
    consumer that does read it answers rightly. An alternative a consumer cannot carry out is named
    and refused. Where several places ask one question of a set, the question is a named predicate
    defined by one such dispatch, so it is answered once. _Programming-language consequence: a match
    over a sum type is exhaustive, as Rust requires of every match and as a C++ switch over an
    enumeration is under `-Wswitch`; a set that grows breaks its consumers' build rather than their
    behavior._

## Boundary to Adjacent Layers

- Consumes HIR. HIR-to-MIR is the layer where SV-specific concepts get translated into MIR's generic
  programming-language vocabulary. Anything SV-specific that does not appear in MIR's vocabulary is
  either translated at this boundary (an event-control delay becomes a coroutine suspension; a
  non-blocking assignment becomes a deferred closure submission) or rejected as unsupported.
  HIR-to-MIR has the SV knowledge; MIR does not.
- Rewritten into MIR before anything reads it below. Between HIR-to-MIR and every consumer, a unit
  passes through one ordered list of rewrites, each of which keeps what the program does and says
  what it changes in MIR's own vocabulary. The unit is in one of two phases, which it records:
  built, as HIR-to-MIR states it, and legalized, with every operator over an integral value wider
  than a machine operates on at once a call of its type's operation, every class handle opened by
  the library's call, every callee that reports at its caller handed its location, no node leaving
  two of its operands a target could run in the other order to another answer, and no frame holding
  a value too large for one or more than a frame keeps together, which is the only phase a consumer
  takes. A body built after the unit left its rewrites goes through the same rewrites alone before
  anything reads it, as rustc runs its passes over a shim built on demand, so a consumer reads no
  body that is not of the unit's phase. A conversion -- the rewrites that bring the unit from one
  phase into the next -- is what a correct program needs, so no build skips one; every other rewrite
  runs inside a phase, leaves the same program at another cost, and a build may skip it by name,
  though every build runs it unless one does, so the gate tests it. So that no answer depends on an
  optional rewrite is a property a run can show by skipping all of them. The conversions are this
  layer's rather than each consumer's because one consumer writes MIR out as C++, whose own rules
  would otherwise decide the order operands run in and where a value lies; rustc keeps its required
  conversions at the layer below, which has no such consumer. Making the evaluation order explicit
  and deciding where each value lies are two rewrites, the second keeping the order the first left
  by the same rule rather than an order of its own. Which operands are taken ahead is decided by the
  shape of the evaluation, never by what a call does: a node has side effects unless it is a
  constant, a read, a reference formed, an operation over values, a construction or the making of an
  execution, made of nodes without -- a call answering an execution runs none of its body until it
  is driven, a construction joins nothing to the engine, and an allocation touches nothing a program
  reads -- and every other call has them, whatever it calls; a read of a frame's local whose address
  nothing takes can be changed only by what beside it names that local. An operand taken ahead is
  read back as a copy where a copy holds its value entire and moved otherwise, a fact of its type,
  and the verifier refuses a legalized unit with a node still leaving such a pair. Which code a unit
  holds -- a constructor with the nodes it enters its base's with, a class's and the unit's
  callables, a record's methods, a type's operations, a closure's body, an exported entry -- is at
  one named source, and a rewrite local to a body says only how it rewrites one.
- The optional rewrites are those a cost the program's meaning does not decide calls for, taken out
  once for both backends while HIR-to-MIR still states the meaning one way. A whole select of a
  value is replaced by the whole. A loop that moves consecutive bits of packed values one bit a
  pass, a number of passes fixed before the program runs, moves the run ahead of it as one
  assignment of a select as wide as the loop makes passes, where one question asked where the loop
  starts says no position wraps and no part reads what an earlier part wrote, and otherwise runs as
  written, as LLVM's vectorizer asks its runtime checks once ahead of a loop. And a process body
  that stops only at the head of its loop -- a continuous assignment, an `always_comb`, an
  `always_ff` (LRM 4.9.1, 9.2.2, 10.3) -- becomes two plain bodies the run calls, a setup and a
  pass, handed over as a reaction ([scheduling.md](scheduling.md)), which keeps one kind of process
  in every layer above and asks nothing of the language: that a process is a coroutine is this
  compiler's representation. A rewrite that changes a body's signature applies only to a body
  nothing outside the unit names, as LLVM rewrites a signature only under local linkage.
- The verification asks of a unit what its phase requires; it is verified once, as it leaves the
  rewrites, since that is the form every consumer reads, and on reaching each phase and after each
  optional rewrite where a build asks; a dump, which a person reads rather than a consumer, shows it
  unchecked. Every build checks what one node or statement states, at a cost that follows the unit's
  size; what holds only across a whole body or the unit -- the order operands run in, a computation
  reached once, a move as a local's last use, the bytes a frame keeps together, what a block
  expression yields outliving it, a closure kept past its frame -- has to be worked out again over
  all of it, so it is checked where a build asks for every check, as rustc's validator makes its
  costly checks, and its lint pass its dataflow analyses, only where a build asks, and LLVM builds
  its expensive checks only where asked. A rewrite builds a node through the same builders as the
  lowering, folding included, and a consumer cannot tell a node a rewrite built from one the
  lowering built.
- [binding_and_capture.md](binding_and_capture.md) owns the lexical reference contract MIR's value
  references obey: a reference names a binding materialized in its own callable body, and a binding
  crosses a closure boundary only as a captured environment field. MIR carries no navigation
  distance to a declaration, and no reference resolves into another callable body.
- Produces input to LIR. MIR-to-LIR is the layer where the programming language gets translated into
  a machine-execution model. CFG structure, storage placement, and execution-oriented details are
  introduced at this boundary; MIR does not predict them.
- Consumed by backends. [backend_contract.md](backend_contract.md) owns the rules under which a
  MIR-consuming backend realizes per-unit MIR as target-language source: type mapping (one dispatch
  per MIR type variant returning a target-language type representation) and value emission
  (mechanical translation of MIR primitives, with no runtime library names appearing outside the
  type-mapping dispatch, and no entry choosing which operation a node names). A render that needs
  anything beyond the node's structural fields is reading something MIR has not stated; the fix is
  upstream of render, never inside it. **Any MIR design decision is validated against the backend
  contract**: if the proposed shape leaves a backend choosing between arms a reader could tell apart
  by running the program, or fabricating an expression, the MIR design is wrong even if the C++
  backend can express it. Two spellings of one operation are not that. The canonical check is "could
  a mechanical LLVM IR backend translate this without deciding what the node means?".

## Forbidden Shapes

Each entry names the requirement the shape fails; where the failure rests on a premise that could
change, the premise is named too.

- **Basic blocks, successor lists, or phi nodes in MIR nodes.** They fail Purpose's requirement that
  the program read as structured software at this layer, and they decide at MIR what LIR decides
  with the machine model in hand.
- **A `return` anywhere inside a block expression.** It fails the requirement that every consumer
  may run a block's steps and take its value with no question of control leaving from among them,
  which is what lets a target with no such construct spell one as a function faithfully. A `return`
  reaches MIR only from a source return statement, which is a statement of a callable's body and
  never a step of an expression, so permitting one is surface with no producer. Rust's blocks admit
  one only because its function bodies are blocks; MIR's are two constructs.
- **A closure invoked where it is built, to sequence steps in value position.** It fails the block
  expression's purpose: a closure is a value with identity whose result type says whether it
  suspends, so every sequencing site would look like a body that may, and the closure's function
  boundary would reinterpret a source `return` among the steps.
- **Storage offsets, byte layouts, or alignment data in MIR nodes.** They fail invariant 10 on its
  open-backend side: each target lays values out for itself, and a layout stated above it is one
  more answer the two can disagree with. Storage placement belongs to LIR.
- **A select carrying a source language's coordinate system for a consumer to resolve** -- a
  declared range or shape beside the index, a direction, which part-select form was written. It
  fails north star invariant 1 ([north_star.md](north_star.md)): every consumer would work out the
  same position again, on every evaluation where the consumer is the runtime; and the form is
  SystemVerilog syntax reaching past HIR-to-MIR, which Purpose excludes.
- **A secondary hierarchy or identity system that shadows MIR ids** -- coordinate tuples, ordinals,
  symbol paths used as identity. It fails invariant 4: two name systems for one thing can disagree,
  and [identity_and_ownership.md](identity_and_ownership.md) holds identity to one per entity.
- **Global semantic lookup inside MIR, or a side table recovering topology from keys.** It fails
  invariants 2 and 6 and north star invariant 5: a relationship held away from its owner is a
  dependency no unit declares.
- **A parallel list, flag, or side field that classifies members** -- signal versus owned child,
  instance versus generate scope, scalar versus array -- or a member's scope kind, cardinality or
  storage shape re-derived from its name, a naming convention, an enclosing construct or a separate
  enum. It fails invariant 7: the type is the classification, and a second statement of it can
  disagree with the first.
- **Deferred or concurrent assertions represented as flags or lowering-pass side effects instead of
  explicit callables.** It fails invariant 1: an effect is an entity a consumer reads, not a mark
  each consumer interprets.
- **A MIR node that preserves SystemVerilog syntactic sugar as an opaque payload** -- a `CaseStmt` /
  `CaseInsideStmt`, an `InsideExpr` / `CasezExpr` / `CasexExpr`, a `ForeachStmt` -- **or a runtime
  helper call wrapping an operator family to recover the source shape** (`Inside(lhs, items)`,
  `CaseMatch(sel, labels)`). Either fails the requirement that the optimizer reach what a program
  computes (Notes): every backend would re-implement the desugaring or call an opaque helper that
  constant folding, dead-arm elimination and jump-table synthesis cannot see through. The switch is
  not this: it is a dispatch on a machine integer's value, which carries no case equality, no
  wildcard and no order.
- **A list the source writes, stated one level of nesting per item** -- a case's items as an `if`
  whose `else` holds the rest, a join's operands as a chain of two-operand calls. It fails the
  requirement that a program the language accepts be one every target accepts: a target compiler
  bounds how deeply text may nest, and every consumer walking by nesting recurses once per item.
- **A MIR node kind invented to express a backend-side storage realization or runtime library
  wrapper** -- a `ReadCell` / `WriteCell` / `MutateCell` node for one backend's observable cell, an
  `AcquireLock` / `ReleaseLock` node for some scheduling discipline. It fails invariant 10's open
  backend set: an operation on such a wrapper is an ordinary call against the library type's API,
  and reading the storage it represents is a dereference of the wrapper's place (invariant 14), so
  the node would carry one backend's realization into every other.
- **A read through a storage-representing wrapper encoded as a call** (`Call(get, [cell])`), **or
  any call whose result a consumer must recognize to recover a destination it stands for** -- a
  handle opened at one node and written through at another. It fails invariant 14 and
  [backend_contract.md](backend_contract.md) invariant 6: every other consumer would decode the call
  back into the destination, each its own way. A write is not this shape: it names its destination
  among its operands. Nor is a call that answers a typed pointer an ordinary dereference consumes,
  whose type already says what it is.
- **A primitive that stands for more than one operation, leaving a consumer to pick which by reading
  its result type** -- a concatenation node over bits, characters and elements alike. It fails
  invariant 10: every backend would decide what the program means, and the one that decides
  differently is a wrong answer no reader is positioned to see. The test is whether the types answer
  completely: where a type admits several ways to be built, or a pair of types several conversions,
  a node naming none of them has left the decision open; where they admit exactly one, a consumer
  reading them translates rather than decides, and a discriminator added there is a second copy of
  the answer.
- **A write naming several places -- a join of lvalues -- as its target.** It fails the requirement
  that each destination receive its share of one value evaluated once, by one cut every consumer
  agrees on ([lowering_boundaries.md](lowering_boundaries.md) invariant 9): HIR-to-MIR states it as
  a local holding the value, reads of its parts and a write to one place each, so no consumer cuts
  the value, and two that cut it differently are a wrong answer neither can see.
- **A type that names a boundary or a foreign ABI rather than a value's own shape** -- a "DPI
  carrier", a "C ABI type". It fails invariant 10: the ABI classification of a foreign call's formal
  is a property of the signature, already stated on the callable's declaration, and a type standing
  for it forces every backend to re-derive the machine type it means. Missing machine primitives are
  what make such a type look necessary; the fix is to complete them.
- **A cast carrying a kind beside its two types.** It fails the requirement that a node state a fact
  once: the operand's type and `Expr::type` already name the conversion, since SV's overloaded
  conversion syntax is resolved at HIR-to-MIR into one pair, and a kind derived from the pair can
  disagree with it. Which pairs a cast joins is this layer's to state and the verification holds
  every cast to them: a value read as the machine boolean a condition tests (LRM 12.4); an integral
  value at a type laying it out alike, an enumeration's and its base's among them; a machine integer
  at another; a pointer at another pointee and a code address at a signature its target enters
  alike; a class handle as one of another class, the same object whatever class it is seen as (LRM
  8.15, 8.26.5); a real as the machine float of its width and back; a reference owning nothing as
  the borrowed pointer it holds and back; a reference at the same referent made read-only; and a
  reference held as a member read as the reference it holds. They are the pairs every target writes
  in its own conversion syntax, so a backend translates each and refuses none. A conversion that
  reshapes a value -- integral resize, real to and from integral, packed to and from string -- is a
  library call, and so is one only the value answers: whether a handle's object is of a class, which
  `$cast` asks of the object ([object_model.md](object_model.md) invariant 10), whether a value is a
  member of an enumeration, which a cast toward one asks of the type, and a closure made one of no
  known type. One that allocates or hands owned storage over is a node of its own, as a box handed
  on is a move.
- **A backend that recovers a semantic fact MIR does not state by inferring it** from a node's body
  contents or any side signal, instead of reading it from an explicit node or reference. It fails
  invariant 10: if two backends could infer a fact differently, it is not yet in MIR. A node's own
  structural context is not a side signal -- the type of an operand it was handed and the node that
  consumes it are its input; scanning a body to decide what it must be, or matching on what an
  operand happens to be to take a different path, is re-deriving.
- **A node field that no backend's realization reads, or that restates what the node's structural
  context already fixes.** It fails invariant 10's one statement of each fact: a field a realization
  can ignore is dead, and one the surrounding structure already determines is a second copy. The
  check on a new field is whether every backend's realization reads it and whether it states
  something the structure does not.
- **A call handed, as an operand, what an element of the array it builds holds before it is
  written.** It fails the rule that an operand is what can differ between two evaluations
  ([design-process.md](../design-process.md)): the array's element type fixes that value, so each
  site restates one fact and a kind of array needing none has to be told apart. A value a program
  wrote for one array -- the `default:` of an associative array literal (LRM 7.9.11) -- is that
  array's and is an operand.
- **An expression node whose meaning depends on which callable encloses it.** It fails invariant 11
  and [callable.md](callable.md)'s self-contained body: a consumer would walk the surroundings to
  learn what "the current receiver" is.
- **A nonlocal reference split by lexical form into species, or a reference whose mechanism a MIR
  consumer selects by a frontend-supplied lexical-form discriminator.** It fails invariant 12: the
  form that named the target is consumed at AST-to-HIR, and the mechanism follows where the target
  lives relative to the emitting artifact.
- **An IR vocabulary item modeling one runtime library's resolver shape** -- a wrapper type for "the
  cross-tree upward resolver", a named-method family for one runtime API's bind state. It fails
  invariant 10's open backend set: the IR names the reference, its route steps and its bound
  endpoint, and runtime calls appear through the call vocabulary.
- **A route step inside the emitting artifact identified by a textual name.** It fails invariant 13,
  which matches no name at run time. A step whose source and target classes the artifact owns has a
  stable in-artifact identity; a step into a member another unit's scope published names the
  reader's declared entry for it, whose spelling producer and consumer derive independently and the
  referrer's compiler checks.
- **A MIR shape deciding a bound reference's storage placement** -- whether its endpoint is a stored
  member, hoisted into a local, or rematerialized at each use. It fails Does Not Own: placement is
  LIR's, decided with the machine model; MIR states the reference's identity, its route, and the
  protocol the body uses to access it.
- **Two nodes or two entries for one part that differ only in whether the part is then read or
  written, a mutation-op node family beside the expression family, or a visit that works out from a
  finished tree which expressions name storage.** Each fails invariant 16: category is a fact of the
  expression, so a second form restates it, a second family gives "how does this program write" two
  answers, and a recovery re-derives it. Two forms the language gives different outcomes -- an
  element read answering the default and an element written allocating -- are two operations and are
  not this.
- **A node per copy or per end of a value.** It fails the requirement that a fact be stated once, at
  the layer that can state it: where each happens follows from a read, a move, a borrow and each
  value's extent by one rule both consumers apply. Neither clang's AST nor rustc's THIR holds one;
  the layer that states each is LIR, as rustc's MIR does.
- **A position where a value lies in a frame holding a value too large for one, or a backend placing
  a value off the stack on its own account.** The first fails the requirement that a program the
  compiler accepts runs at every size it accepts, since the stack's size is not the design's to
  choose; the second fails invariant 10, two backends answering one question apart.
- **A generic definition, type parameter or bound in MIR.** It fails the requirement that every
  rewrite, the verifier and both backends read concrete bodies, and it rests on a premise: nothing a
  user writes is generic once the front end has specialized it, so a MIR generic would serve only
  the compiler's own operations, each stated as plainly by a builder per site or per type. Where an
  operation appears whose definition, checked against its bounds where it is defined, would catch
  what no check of a concrete body can, or whose concrete forms outnumber what one definition costs,
  a generic comes back whole: a bound is a trait the type implements, a call through one is a method
  of it, the body is checked where it is defined, and making it concrete is the first conversion of
  a unit.
- **Code per member in anything the compiler states for an enumeration** -- a conditional nested one
  level per member for `name` or `next`. It fails north star invariant 1: the work of building it
  follows the member count at every optimization level, where data the library's one routine reads
  costs nothing to build.
- **A field on a type that is no part of its identity.** It fails invariant 18: a key leaving the
  field out lets the first spelling interned be the one every later reader sees, and a key keeping
  it makes one type two.
- **A catch-all arm, a generic visit arm or a comparison against one alternative consuming a closed
  set MIR states, or a node alternative only one consumer names.** Each fails invariant 19: the set
  gains an alternative and the build stays green while some consumer answers it wrongly.

## Notes / Examples

Reviewing MIR via the dumper should feel like reading the compiled program's structure expressed in
a generic programming language. The C++ backend emits MIR to C++ source: it is a backend output, not
a debug view. The C++ surface syntax happens to map naturally onto MIR's vocabulary because the two
share many concepts, but C++ is not MIR's semantics and more than one backend may exist.

A module instance, a generate scope, and an instance array differ only in one member's type:
`borrowed-ptr(object of a class its unit defines)`,
`borrowed-ptr(object of a class this unit defines)`, and `vector(borrowed-ptr(...))` respectively. A
two-dimensional array is `vector(vector(borrowed-ptr(...)))`. Because the classification lives
entirely in the type, a consumer that recurses on the type -- a vector layer recurses on its
element, a pointer to an object reaches the object -- handles every combination, including arrays of
any dimensionality, with no per-form branch. A consumer that switches on whether a member "is an
instance" or "is a generate scope" or "is an array" is reconstructing what the type already states.
The member holding what a loop generate built holds the base every block extends, each block
published under its own class name: which blocks share a body is decided by lowering the bodies, and
what a unit publishes does not depend on its bodies, so an edit to a body changes nothing a referrer
compiles against (north star invariant 4).

The primitive expression set above is shaped by the downstream optimizer. Each backend consumes MIR
as primitives that its optimizer can fold, propagate, and pattern-match across. A sugar node carried
into MIR forces every backend to either re-implement the desugaring or emit a runtime helper call;
in the LLVM path the helper appears as an opaque call through which constant folding, dead-arm
elimination, and jump-table synthesis cannot reach, and which LTO mitigates but does not eliminate.
For the same reason a value fixed before the program runs is a constant the unit states once and a
use reaches by a read, never a construction performed wherever control reaches it: an optimizer
recovers neither a construction through a dynamically initialized descriptor nor one in a body
entered a thousand times. Backend-local readability of generated artifacts is a debug concern and is
recovered through value-type C++ operator overloads in the runtime, not through sugar nodes in MIR.

Constructs in the expression set that look like sugar are not. The rvalue conditional form (`?:`) is
preserved because MIR's `if` is a statement; there is no primitive rvalue branching in MIR's
expression set that decomposes the ternary. It is one primitive, and a condition reaching it has
already been reduced to what a branch tests. LRM 11.4.11 reads a predicate's truth as three-valued
-- a predicate that can be ambiguous selects neither arm, evaluating both and combining them -- and
that third answer is not a second node: the arms are tried by statements one after another, each
adding its value to one answer, and the selection ends at the arm whose predicate is known true. How
two values combine follows from their type and is stated here: one made of parts that agree apart
from each other, an integral or an array that is not associative, through the entry that combines
two results, and any other as the value the two share where an equality says they are equal and the
value its type has where nothing was stored otherwise. The same holds of every operator whose later
operand runs only where the earlier ones leave the answer open: SystemVerilog's `a || b` and
`a && b` (LRM 11.4.7) are a search through the operands' truths, each operand after the first
evaluated by a statement that runs only while the answer is open, and folded in by the logical
operator that combines two values already evaluated. `a -> b` is `!a || b` by that clause's own
words, so it is the search `||` makes over the negated operand and has no shape of its own, and
`a <-> b` evaluates both operands and is an operation over two values like any other. So no operator
of the expression set skips an operand and no backend's own rules decide whether one runs. Where
nothing can be unknown a selection and a search are the conditional alone, the next arm or operand
standing as its third operand, so such a chain is as deep as the source nests it, which the front
end bounds; an `if`-`else if` chain stays nested for the same reason, except under `unique` or
`unique0`, which settles every arm's answer before selecting one (LRM 12.4.2) and so is a search.

What the source writes as a list rather than nested -- the items of a case statement, the labels of
one item, a generate case's items, the bodies a generate loop or an instance array chooses among by
its index -- stands side by side, as deep for thirty thousand items as for two. Where the keyword is
`case`, no qualifier asks about the items together (LRM 12.5.3), and every label is a value of the
subject's type known before the run with no x or z bit, the items are a switch over the subject's
value, entered only where the subject holds no x or z bit (LRM 12.5), each item leaving a block by a
break naming it, the default after the switch, and a value two items name the first item's.
Otherwise -- `casez`, `casex`, `case inside`, a pattern, a label the run computes or one holding x
or z, a qualifier -- the items are a block whose arms are tried one after another, each test a step
that runs while none before it held and each selected arm leaving the block by a break naming it.
Which shape each takes is settled at HIR-to-MIR, so no consumer derives it. The operands an
operation is written over as a list stand side by side the same way, in one of three shapes chosen
by what the operation is. A **join** is what a person writes for it: of bits, a local of the joined
type and one write of each part into the bits it takes, the order the source lists them (each part's
width is its type's, LRM 11.4.12, so where it lands is a constant); of characters, one call over one
list of strings, sized once, a join of one string being that string; of queue elements, a queue the
steps append each part to, the part itself or every element of a part that spreads, which a
destination of another array kind then takes as an assignment between array kinds does (LRM 10.10).
A replication's multiplier reaches its entry as a machine count by a call in the argument list. A
streaming concatenation is no node either (LRM 11.4.14): each operand's bit stream -- a packed
value's own bits, otherwise a call of its type's stream operation -- is joined as bits are, or,
where a part's width is one the program decides, appended one part after another to a run of bits,
the library's type; then, for `<<`, one entry re-orders the joined bits by the slice size (LRM
11.4.14.2), and `>>` re-orders nothing and calls nothing for it. A bit-stream cast is the same two
steps, the stream and then the value read back from it (LRM 6.24.3), and a stream read back into
parts gives the first part whose width the program decides what the parts after it leave (LRM
11.4.14.4). How many bits the stream must hold is the construct's: a cast takes exactly as many as
its type holds (LRM 6.24.3); a stream assigned to a variable whose type fixes its width holds at
most that many, a narrower one widened by zero bits on its right, and one assigned to a variable
holding a part whose width the program decides gives it the fewest elements as wide as the stream
(LRM 11.4.14). Where a width is known only as the program runs, the comparison is a step of the
evaluation, and a stream that fails it is the design's error. A **fold** that evaluates every
operand -- a set's members (LRM 11.4.13), a structure's equality (LRM 11.4.5), the sum of its
members' widths -- is a local holding the operator's identity and one statement per operand folding
it in. A **search** that stops at the first condition settling it -- the tests of a pattern (LRM
12.6), the clauses of `&&&` (LRM 12.6.2), the conjuncts of an assertion's position -- is steps that
each try one condition while the answer is open. A `foreach` is one loop per dimension it iterates,
outermost first: a dynamically sized dimension samples its count once on entry to its level, so a
jagged array needs no case of its own, an associative dimension walks by key, and the outermost loop
carries the label a `break` in the body names (LRM 12.7.3, 12.8).

A write yields nothing, as an assignment does in Rust's MIR: what an assignment yields where the
source reads it (LRM 11.3.6) is what it stored, which the lowering holds in a local and reads, so no
backend's own rule for the value of an assignment -- C++'s is the target itself -- decides it. An
assignment operator and an increment (LRM 11.4.1, 11.4.2) are no write of their own: each is the
operator applied to what the target held and then that write, the target settled once for both, so a
write names no operator. A value-build primitive spells a value rather than operating on values that
already exist -- a structured literal is one, and so is every peer language's array or aggregate
literal. Select expressions are access primitives. Each of these stays in MIR for the same reason:
removing it would require expanding into a statement-form rewrite that does not fit the expression
context. Composing values that already exist is not this, and no peer spells a concatenation as a
node.

A capability wrapper illustrates the boundary between MIR's vocabulary and a backend's storage
realization. A backend may wrap observable storage in a target-side library type; MIR has no "cell
access" node and needs none. The signal's MIR type is the wrapper, so the field access reaching the
signal is the wrapper's place, and a dereference of it is the storage a callee may be lent and a
descent may reach into. Reading what the wrapper holds is not that dereference: a read asks the
wrapper for what it shows, so it is an operation on the wrapper and states itself as a call
answering a reference, whose dereference the read node then reads.

A write is not a way of naming that storage but an operation on the wrapper -- it replaces what the
storage holds and publishes the change to whatever the wrapper relates to -- so it is an ordinary
call whose operands name the destination and the value, and a partial write is a write in progress
opened on the wrapper, assigning into what the descent taken within it reached (below). Rebinding
the wrapper -- pointing a reference at a different place -- is a store into the bare place, with no
dereference and no call, which is what separates it from writing through without either one carrying
a marker. Nothing any of these nodes yields stands for a destination, so no consumer recovers one by
recognizing a shape. What each becomes in a target -- a method call, a runtime-ABI call, a load or
store against an address -- is each backend's own mapping from the wrapper type;
[backend_contract.md](backend_contract.md) owns the rules that keep those spellings out of
value-emission sites.

A part of a value is named by how it is selected, in every layer: an **element** by a coordinate the
program computes, a **component** by its declaration-order position, a **slice** by a run of
consecutive positions; "part" is the word for all three, and "member" stays free for what a scope or
a class declares. An assignment whose target descends into a value -- a container element, a struct
component, a union member, a fixed-width range -- states an abstract update of the owning value:
which owner, which levels the descent reaches through, how many times each evaluates, the value
written and its coercion, and that the owning value is the value updated. The descent is built from
the owner outward, so no consumer walks a target back to find its owner. The owner is a place; the
descent is a run of steps on it -- a field step where the part means the same for reading and
writing, the write form of an element where it does not, a slice step for bits of a packed value,
one for each part they are reached through -- and what it ends in may be no place: a window is
written by its array's own operation or a queue's own slice write between the two positions it
clamps (LRM 7.10.1), each taken on the place the value lies in. Where the owner is a capability
wrapper, the write is opened on it, the whole of the wrapper's contents is designated within it, and
each step into a part a write lands on (an element, or a component, a tagged union's member among
them) is taken within the write in progress rather than on the storage. So the write hears what
forming each part did -- an element made by being written, an index naming none -- and keeps only
what it lands on ([storage.md](storage.md) invariant 13). It lands where what those steps designate
is dereferenced; where the descent ends in bits, they are a slice step of the last designation, so
storing them tells the write which bits moved; and where it ends in a window, the window's write is
taken on the last designation. A union's member is no storage of its own, so the change the write
reports is to the union, whose bits every member reads. The write and a place designated within it
are two types: the write is held by the writer, ended with the full-expression, which is when it
reports, and names no place itself, while a designation borrows the write and ends with nothing to
do. Whether a part is storage of its own is the language's fact about the type it is part of
([storage.md](storage.md)), and the place states it as its kind (invariant 16), so no layer below
works it out. A construct that binds the designated part rather than writing it -- an output pack
component, a nonblocking update -- binds the same statement of owner and descent; it is
owner-relative, so it names no interior pointer and aliases nothing the owner does not already own.
A position a deferred write freezes is one the program computed; a count is its type's and a
constant position has no moment of evaluation, so the deferred body states each again, as a C++
lambda captures no variable usable in constant expressions. A reference actual binds the part for as
long as the callee holds it, and each write through it is a write of the owner at the moment it
lands (LRM 13.5.2, 4.3), which no write ended with the caller's full-expression can be; so a
reference is formed over the owner and each step into a part is taken on the reference, answering a
reference to that part that belongs to the same owner. What such a reference reaches through that
the program can let go of while the callee runs -- the object a property lies in, an element of a
queue or an associative array, a dynamic array's elements -- the binding leases, by a call answering
the reference together with the lease; that answer is a value the call's full-expression makes, so
it is held to that full-expression's end, or by a local of the frame where a closure built before
the call captures the reference, and the callee is handed the reference it carries, which owns
nothing (LRM 13.5.2, 8.29; [lifetime.md](lifetime.md) owns the lease).

Where the owner is a property of an object (LRM 8.4), the object is what hears the write (LRM
9.4.2), and it is reached once, as the source reaches it once. A write is a write-in-progress value
constructed on whatever reaches the object -- a class handle, or the running method's own object --
and dereferenced to the object, as a guard is dereferenced to what it guards, so the write ends with
the full-expression and reports then. A reference is a step taken on the object naming the property,
as a component's is named. An entry acting on an object is handed whatever reaches it, as a method
is handed the pointer to its own object rather than the object.

A field is a member of an object, so a field access is made on the object as a place, and an object
that a handle, a pointer or a write in progress reaches is first dereferenced to it: `p->f` is a
field access over `*p`, as rustc's MIR projects a field from a place that is itself a dereference.
The dereference is stated where it happens rather than implied by the receiver's type, so no
consumer works out from a type whether the receiver still has to be opened. A call entering a member
function is reached the same way: its receiver is the object, so `p->f()` is a call on `*p`, and the
body is handed that object's address, as clang's code generation forms `this` from the object an
arrow call names. A call naming a member function of a library value -- a string, a handle, a
reference -- is entered on that value, which is the object the function belongs to.

Current implementation: the rewrites are named `InstSimplify` and `LoopIdiomRecognize` (optional, on
the built unit), the conversion into the legalized phase `ExpandWideIntegrals`,
`ElaborateHandleDerefs`, `StateCallerLocations`, `OrderEvaluation` and `BoundFrames`, and
`SplitReactions` (optional, on the legalized unit). `--mir-enable-passes=-<name>` skips an optional
one and `--mir-verify-each` asks for every check, each rewrite being a span of `--time-trace`. The
runtime library's entries are the closed enumeration `support::BuiltinFn`, each with one row of
`support::RuntimeEntry`. `tools/policy/check_architecture.py` holds part of invariant 19: A013 that
some `switch` names an enumeration's alternatives, A020 against a `default:` arm, A021 against a
visit arm naming no alternative, and A016 both consumers to every node alternative. No rule finds a
consumer comparing with `==`, and A020's standing list names the switches still carrying a
catch-all.
