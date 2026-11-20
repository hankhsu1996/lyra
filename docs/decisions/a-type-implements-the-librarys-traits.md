# A type implements the library's traits, and both backends reach the same implementation

Date: 2026-11-16 Status: accepted.

## Context

The runtime library's own code holds values of the design's types and asks things of them by name: a
holder of a variable asks whether a write changed it (LRM 9.4.2), randomization asks for an array
made a count long (LRM 18.4), a library array asks its elements whether they are equal. A
run-time-sized container answers each with a body MIR builds for its type
([a-containers-operations-are-built-for-its-type-over-a-core](a-containers-operations-are-built-for-its-type-over-a-core.md)).

Each backend's library is told of those bodies its own way. The execution backend hands its prebuilt
library a record of the type, a table of the bodies' addresses
([a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)).
The C++ backend instantiates the library's class template at a struct holding the bodies as static
functions. Which bodies a type's record holds was the execution backend's list, and which the struct
held was the C++ backend's, worked out by its own collector and written into the struct by its
render. Two lists answered one question, and the C++ one was a declaration no MIR declaration stood
for, which the other backend would have had to assemble again its own way.

Which bodies a unit compiles is a different question, and it stays each backend's. The execution
backend reads what it needs off finished LIR, where a coroutine's frame, a value cell and what
legalization adds first appear; the C++ backend never sees LIR. What both can share is which
operations exist and which of them each requirement of the library is answered by.

## Decision

**The library declares its traits, a library declaration lists the traits its types implement, and
MIR states each type's implementation from that declaration. Each backend's library asks the items
it needs, and neither backend decides which items the type has.**

- A trait is what the library asks of every value of a type implementing it, by name: a value's life
  (its default, a copy, an assignment over a value, its end), its change test, its elements by
  position (at a machine index, to read and to write), where a write through an index lands -- a
  position, or an associative array's index -- being made a count long, being taken over, the
  positions a net reads and writes, the questions asked of a whole value, its stream, at a width the
  running program or its type decides, and its value read back out of a stream. A requirement is
  answered by the type's own operation of the same name, as a Rust trait's required method is by an
  impl's. Several traits rather than one list, because a holder asks for some and not others, and a
  type implements a trait only where it can: a container's stream only where its elements stream, as
  a Rust impl holds only where its type arguments meet a bound.
- Each trait's items are a closed set of their own, and an operation asked of a type is such an
  item, an item of a language trait it implements, one of the type's own methods, a step of its
  value's life, or a conversion into the form of its type with no queue bounded and back, which
  every type holding elements has. rustc tells a trait item from an inherent one the same way, and
  clang a special member from a named one. Whatever declares or builds an item answers every item of
  its trait at once, so a trait gaining an item stops the build until every declaration and every
  body says what it does with it. A call naming the library's entry finds the operation it asks
  among the receiver type's, as rustc finds a method by its name among the receiver's impls, never
  through a list of names kept beside them.
- A kind of array's own methods -- `push_back`, `delete`, `new[]`, a slice -- are its inherent impl:
  what the source reaches through the array itself, by a method's name, by a select of it or a
  stream's window of it, by an assignment into it, or as a step of one of those. Rust's `Vec` has
  `push`, `reverse` and its slicing the same way; Rust puts indexing in a trait only so a type the
  program declares can overload it, which no SystemVerilog type can.
- What a construct asks of the type of a value handed to it as an operand is a language trait: the
  type's side of a protocol whose other side the construct defines, as Rust's `for` calls
  `IntoIterator::into_iter` on what it iterates and `+` asks `Add` of its operand, items rustc knows
  as lang items (`compiler/rustc_attr_ir/src/lang_items.rs`). A memory task's memory loads from and
  dumps to the words the task moves (LRM 21.4, 21.5, 21.3.4.4), an open array formal's actual fills
  the image the foreign side reads and is read back out of it (LRM 35.5.6.1), and a random variable
  is walked part by part for the problem a call of `randomize()` is solving (LRM 18.4). A construct
  whose operand may not implement the trait -- `$fread`, an open array formal, `randomize()` -- asks
  whether its type does, calls the type's items where it does, and answers any other operand its own
  way: `$fread` loads a packed value whole, a single packed value is an open array's one element,
  and a variable whose type does not implement the walk is one part, named to the problem with its
  type's record. `$readmem` and `$writemem` take only a memory, which the front end checks, and call
  its items outright. An array of every kind is a memory, and an array whose elements a position
  names is an open array's actual and implements the walk; a structure and an associative array are
  random variables too (LRM 18.4) and do not yet implement it, and `$fread` into an associative
  array is not yet carried out. No code holding a value asks a language trait, so a type's record
  holds none of its items; each is an operation the call naming it builds where it calls it, as a
  method is.
- Each kind lists its methods and the language traits it implements, and the operations it is called
  by are the two together; MIR states each one's signature once and the lowering builds each kind's
  body once. The two are held to name the same operations when Lyra is compiled, so an operation
  declared with no body, or a body for no declared operation, does not build. An operation two kinds
  share has one signature, and each kind its own body.
- A type's interface is what it implements and nothing more: the items of each trait it implements,
  library or language, and the methods of its inherent impl. Where a type's parts decide whether it
  has an item -- a stream where its elements stream, a net's folds where a net admits every part --
  each kind's implementation states the condition, read off the type's parts, as each Rust impl
  states its own bound (`impl<T: Eq> Eq for Vec<T>`), and no type has a list of its own.
- The library declarations of a dynamic array, a queue and an associative array list the traits
  their types implement. An associative array's lists no element by position, no resize and no read
  back out of a stream: no position names its elements, nothing chooses its size or its indices (LRM
  7.8, 18.4), and it is never a stream's destination (LRM 11.4.14.3). MIR states a type's
  implementation from it -- the type and the traits it implements -- as rustc resolves an impl for a
  type by query rather than holding a copy per crate. A type's interface lists the items among its
  operations. A fixed-size array, which no declaration states, has as items each operation of its
  interface some trait requires, since the library asks each item of a value apart from the rest and
  answers one a type lacks with its failing of it; its class is the library's class over its
  elements in place at the struct holding them,
  `lyra::value::UnpackedArray<T, 4, sv_type_1a2b_impl>`.
- What is unified is the binding of a requirement to an operation, never a target's layout. The
  lowering to LIR carries each type's implemented traits with the unit, as MIR states them, and the
  execution backend's record of the type holds the items its library reads, at offsets its own
  record type fixes. The C++ backend writes the implementation as a struct declaring each item as a
  static function, `struct sv_type_1a2b_impl`, and its type mapping names the type as the library's
  class over the core at that struct,
  `lyra::value::Container<lyra::value::QueueCore, sv_type_1a2b_impl>`. The struct stands for the
  implementation MIR states and declares exactly its items.
- The library's class template realizes C++'s object model once for every such type: its
  constructors, assignments and destructor call the items of the value's life, and its other members
  ask the items of the other traits. It carries out no operation of the language.
- Every item of a type's implementation is defined in the type's code file, which every unit naming
  the type writes alike. Every other operation of a type is a function of the code file calling it,
  on both backends the same body, a fixed-size array's own methods, the items of its language traits
  and a tuple's operations included
  ([a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)).
- No file of either backend, nor of the lowering to LIR, asks for a type's whole interface, since
  doing so is choosing among its operations; each reads the implementation MIR states and the
  operations its code names. A backend asks no operation's declaration either: it is handed each
  with the operation's body. `tools/policy/check_architecture.py` A029 holds them to it.

## Rejected

- **Each backend listing what its library asks of a type**, the shape this replaces. The two lists
  agreed by nothing, and the C++ one was built by the render out of the type's interface, a
  declaration no IR declaration stated.
- **Collecting every body both backends need in one pass over MIR**, as rustc's collector does once
  for all its backends. rustc's collector reads rustc's MIR, which every rustc backend consumes and
  which plays the role of Lyra's LIR; Lyra's C++ backend never sees LIR, and what the execution
  backend needs first appears there. Which operations exist is shared; which bodies a unit compiles
  is not.
- **The implementation as a record of MIR's**, a struct with associated functions. A record is a
  value type the source declared, with members laid out; an implementation has no value and no
  layout. It is an impl, as rustc names it.
- **The type's C++ name as an alias declared beside the implementation.** It shortens a spelling and
  decides nothing, so it would be a declaration no MIR declaration stands for; type mapping spells
  the class at its implementation outright.
- **One flat list of every operation, trait items and methods alike**, each consumer testing it
  against the names it handles. A trait gaining an item then stops nothing, and every consumer's
  chain of comparisons is a place a new name is silently missed.
- **An enumeration of each kind's own methods.** A queue's `size()` and a dynamic array's are the
  same name the source asks, and an enumeration per kind would add a third set of names and a map
  from the request to each. The name and the receiver's kind already find the method, as Rust's
  method lookup finds an inherent method by the receiver type and the name.
- **A class or a file per kind of array for the bodies.** Where a kind differs inside an algorithm
  every kind runs -- where an element lies, how storage is taken and given back -- the algorithm
  asks the kind at that point, exhaustively; a class per kind would turn those questions into calls
  that check nothing more, and the algorithms the kinds share would have to move into a header.
- **Every operation of the type in the implementation.** The library asks the traits' items and
  nothing else; a queue's own `push_back` is an operation a call names, as an inherent method is in
  Rust, and is a function of the file calling it.
- **A construct's protocol among each array kind's own methods**, where it first stood. A method is
  a kind's own and only an array kind has methods, so a structure answering `randomize()`'s walk
  would have had nowhere to state it, and the table of what a kind's own methods are named an
  operation the source never names on an array.
- **A language trait as a library trait.** A library trait's items are in the type's record because
  code holding a value of any type reaches them through it; a construct's call names its operand's
  type, so the record would carry an address nothing reads and every type's code file would define a
  body no unit may call.
- **A check reading the emitted text for where each declaration came from.** No backend can build an
  implementation's membership without asking for a type's interface, so refusing that question is
  the narrower check and is greppable.

## Consequences

- A container type's code file defines every item, the questions and the stream included, wherever
  the type is named; the questions no longer have files of their own that only a unit holding the
  type as an element writes.
- A fixed-size array's and a tuple's operations are rendered by the C++ backend from the bodies the
  execution backend compiles; the library's array class carries out none of them.
- A new trait is a declaration in the library and a line in each library declaration implementing
  it, and reaches both backends at once.
- A new construct asking the same of every type it is handed is a language trait and a line in each
  kind's list, and a new kind answering it is a body and a line in its list; the construct calls its
  items without naming the kind, and reads by kind only what it takes from the operand itself -- a
  memory's addresses, an open array's bounds -- and what it does not yet carry out.

## Cross-references

- LRM 6.24.3, 7.5, 7.8, 7.10, 9.4.2, 11.4.5, 11.4.14.3, 18.4
- [a-containers-operations-are-built-for-its-type-over-a-core](a-containers-operations-are-built-for-its-type-over-a-core.md)
  -- the bodies an implementation's items are.
- [a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)
  -- the execution backend's record of a type.
- [a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)
  -- one definition of an array's operations on both backends.
- `../architecture/backend_contract.md` -- a backend translates declarations and writes none of its
  own.
