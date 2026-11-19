# A container's operations are built for its type, over a core compiled once

Date: 2026-11-05 Status: accepted; the lifecycle (D6, D8) revised by
[a-move-relocates-its-value](a-move-relocates-its-value.md), and which operations the C++ struct
holds (D8) by [a-type-implements-the-librarys-traits](a-type-implements-the-librarys-traits.md).
Supersedes the container half of [jit-aggregate-realization](jit-aggregate-realization.md), revises
D6 of [a-value-is-its-machine-data](a-value-is-its-machine-data.md), and carries out for the
run-time-sized containers what
[an-operation-over-many-types-is-built-by-the-lowering](an-operation-over-many-types-is-built-by-the-lowering.md)
and
[a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)
decide for a fixed-size array.

## Context

A queue, a dynamic array and an associative array (LRM 7.5, 7.8, 7.10) each had one algorithm,
written once over an element interface and compiled twice. The C++ backend's emitted project
compiled it with the element's C++ type, so every element type a unit names instantiated the whole
algorithm, ring arithmetic and bound enforcement included, which depend on nothing but the element's
size. The prebuilt library compiled it once more over a table of the element type's operations, so
every element copy, comparison and end on the execution backend was an indirect call. One source had
two compiled shapes, held in step by nothing but being one text, and the library compiled container
families for both sides: a member a unit compiled by GCC called on one of them carries no
constraint, because the library's clang writes a constraint into the name and GCC 13 does not.

What the language asks of a container does not depend on how it is compiled. Insert, push and delete
outdate references only to what they remove (LRM 7.10.3); an element removed while a call holds a
reference to it goes on existing until the call returns (LRM 13.5.2); a dynamic array's `new[]`
keeps a prefix and outdates every reference (LRM 7.5.1); an associative array orders by its index
type and allocates a missing entry when a write or a reference names it (LRM 7.8, 7.8.7).
[container-element-storage](container-element-storage.md) and
[array-element-storage](array-element-storage.md) decide the storage that answers these: each queue
and associative element is storage of its own that never moves, and a dynamic array's elements are
one block per generation.

## The test each choice answers to

1. Each operation is stated once and every backend compiles that statement.
2. What depends only on a container's structure and runs rarely is compiled once for every element
   type; what runs on every call is compiled with the element's type.
3. What touches an element's value is compiled with the element's type.
4. An element is reached where it lies by code compiled with its caller, never across a boundary the
   optimizer cannot see past.
5. End-to-end iteration time is the primary target (`../architecture/north_star.md`), so what a unit
   compiles per element type is part of the cost; that cost is answered by compiling units in
   parallel and again only when they change, never by erasing the element type from a path that runs
   on every call.
6. Built at `-O2 --lto`, an operation runs as fast as hand-written C++ or Rust keeping the same
   semantics.

Test 6 is what weighs speed against compile time, and test 5 says where compile time comes from
instead. Measured against them, a loop of `push_back` and `pop_front` on a queue took 187.8 million
instructions through a type-erased core, where hand-written C++ keeping the same guarantees --
elements that never move, a lease that outlives removal -- took 19.5 million.

## What the field does

- LLVM's `SmallVectorBase<Size_T>::grow_pod` (`llvm/lib/Support/SmallVector.cpp`) is compiled once
  and told the element's size; `SmallVectorTemplateBase<T>::grow` asks it for storage and moves and
  destroys elements with `T`'s own operations. The split is drawn at the element touch.
- LLVM's `FoldingSetBase::FindNodeOrInsertPos` (`llvm/lib/Support/FoldingSet.cpp`) walks its buckets
  in code compiled once and compares through `NodeEquals`, a function the typed `FoldingSet<T>`
  states once in a record. `StringMapImpl::LookupBucketFor` (`llvm/lib/Support/StringMap.cpp`)
  compares its own key kind itself.
- Rust's `RawVecInner<A>` (`library/alloc/src/raw_vec/mod.rs`) is generic over the allocator and not
  the element; every method takes the element's `Layout`, which its source says is to reduce what is
  monomorphized.
- Rust's `Vec<T>` reaches an element by `Index::index` (`library/alloc/src/vec/mod.rs`), a method
  compiled with `T` that reads the buffer's pointer and length and is inlined into its caller.
- Rust's `VecDeque<T>` (`library/alloc/src/collections/vec_deque/mod.rs`) states its head, length
  and index arithmetic generic over `T`, so they are compiled with the element, and reads an element
  out with `ptr::read` (`buffer_read`); only the buffer's growth is `RawVec`'s shared code.
- LLVM's `RecyclingAllocator` (`llvm/include/llvm/Support/RecyclingAllocator.h`) hands out storage
  of one size from a `Recycler` (`llvm/include/llvm/Support/Recycler.h`), whose free list links each
  freed piece through its first word, and asks its allocator only when that list is empty.
- Go's compiler knows a slice's header -- the data pointer, the length, the capacity -- and indexes
  it inline, while its runtime grows it. The header is a contract between a compiler and the runtime
  of one release, not a stable interface (recalled).
- Swift's code compiled without `T` copies an `Array<T>` through `Array<T>`'s own value witness
  table, which its runtime builds from `T`'s (recalled).

Where Lyra's conditions differ: an element never moves, so growth needs no typed move and a core
needs at most to end an element, handed its type; the typed side is built by the compiler rather
than written by a user; and the C++ backend's units may be compiled by another compiler than the
library, so the typed side is emitted code calling the core's functions and instantiates none of the
library's algorithms.

## Decision

**D1. A container's storage management is a core compiled once in the library, one per container
kind, and what runs on every call of a dynamic array or a queue is built for its type.** A dynamic
array's core is where its block starts, how many elements it holds and the element type's record; it
allocates a generation, adopts it and gives it back, takes the lease a call takes on an element, and
says, when the array lets its elements go, whether the array's own code ends them or a lease keeps
them (LRM 13.5.2). An associative array's core keeps its tree, which finds, places and removes
entries, told, where it is built, the element type's record
([a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md)),
of which it reads the size, the alignment and how to end one. A queue's core is a ring of the
addresses of its elements' storage -- the ring, its mask, its head and its count -- and the run's
recycler that storage comes from, one recycler for each size and alignment of storage, as LLVM's
`RecyclingAllocator` is; it holds nothing that depends on the element type. What runs on every call
of a dynamic array or a queue -- reaching an element to read or to write it (D5), putting an element
at either end and taking one from either end, emptying and ending the container, holding a queue to
its bound, taking storage for an element, and letting an element go by ending it or leaving it to a
lease -- is a body built for the type (D3). The queue's core keeps what is the same for every
element type and runs rarely -- fresh storage where a recycler was given none back, growing the
ring, opening and closing a position in it, reordering and reversing it, giving it back, the lease a
call takes, and building an empty queue, told its type. No core implements anything of an element's
value. Where a dynamic array's or a queue's core ends an element, it is for the last lease on one
the container let go of, and it asks the element type's record, whose function the compiler built.
Every element committed to a container is ended exactly once, after the last lease on it
([a-bound-reference-leases-what-it-reaches-through](a-bound-reference-leases-what-it-reaches-through.md)).
Every entry of a core is an ordinary function of the library, no template, which the execution
backend reaches through the library's C interface and the C++ backend's class for the type calls
directly.

**D2. A core states its fields as a C layout, and both backends read them.** A dynamic array's are
where its block starts, how many elements it holds and the element type's record. A queue's are its
ring, the ring's mask, its head, its count and its recycler, and a recycler's are its free list, the
size and alignment of its storage and the rest of the page it cuts fresh storage from. They are
stated once, as a C struct in a library header, which the execution backend's code generator reads
by `offsetof` and the C++ backend's class for a container type derives from, as a type's record
already is. A dynamic array's core, a queue's core and the queue's recycler are structures of the
library whose members a unit reaches, so a body names each member by its position and every layer
reads the member's type and place off the one declaration. They are a contract between the compiler
and the library of one release, as Go's slice header is; a user's program reads none of them, and a
library shipped with a different compiler is no case this answers. What an associative array's core
does with its storage -- the tree, its leases -- stays behind its entries, and so does what D1
leaves a dynamic array's and a queue's core. The code that reads the fields depends on how elements
are located, so a different data structure changes the header, the bodies the lowering builds and
the library's table of parts together, and neither backend's own code.

**D3. Every operation of a container type is a body the lowering builds for that type, on demand,
from the unit's types.** It is named by the container type and the operation, as a fixed-size
array's operations are. It is made of what MIR already has: a placement new-expression, calls of a
core's entries, element steps, reads, moves, conditions and loops, and, for a dynamic array and a
queue, the core's members and the storage their addresses point at, read and written where they lie.
The user's `q.push_back(x)` is a call of that body. An associative array's body never states a tree
walk; one that would need it is a core entry missing. What a body does with an element -- building
it from a value, comparing it, streaming it, taking it out of its storage -- is the element type's
own operation, compiled with the body. A value is taken out of storage a pointer points at by a call
answering it and leaving the storage holding none, as rustc's `ptr::read` is a call in THIR. A
dynamic array's body reads where its block starts and how many elements it holds off the core's
members, and steps to an element by the element type's size, as `Vec<T>` indexes its buffer in code
compiled with `T`. Its end, and its taking a new generation, let go of the elements it held: the
core says whether the array's own code ends them, which it does not where a call leases one, the
generation then kept to the last lease (LRM 13.5.2); the body takes each element out of its storage
and ends it, and the core gives the storage back. A queue's `push_back` grows the ring through the
core where it is full, takes storage from its recycler's free list or, where that is empty, from the
core, builds the element there by a placement new-expression, puts its address at the ring's end and
holds the queue to its bound. Its `pop_front` reads the address at the head and moves the head on,
then takes the element out and gives its storage back, or, where a call leases it, answers a copy
and leaves the element to the lease (LRM 13.5.2). Its `delete` lets go of every element that way and
keeps the ring.

**D4. Storage for an element is its taker's from the moment it is taken, and the container's,
holding the element, from the moment it is committed.** A body builds the element in storage it took
and commits it as one sequence. Building an element from a value runs no SystemVerilog code and
fails only for storage, which ends the run without the simulation resuming, so no path leaves taken
storage uncommitted while the simulation goes on. Storage handed out -- a queue element's from its
recycler, a dynamic array's new generation -- is the body's until it is committed or adopted, and
the core learns of it only then, so a failure in between leaves the core as it was and the container
ends through it at teardown as any other does; the storage and what was built in it are not
reclaimed, which only the path that ends the run reaches. A C++ container reclaims them because a
C++ program may catch the failure and go on; no handler resumes the design. A value read from the
container is a value of its own before any storage is taken, and no element moves, so
`q.push_back(q[i])` reads a live element. The bound is enforced after the new element is linked. A
queue's free storage holds the link to the next free piece in its first word, over whatever it holds
in use, its lease count included, so the count is started where the storage is taken.

**D5. An element of a dynamic array or a queue is reached by the type's own body, and every write
through a position is built on one body every path reaches.** MIR states an element of any array as
one call answering a reference. For a fixed-size array the callee is the language's element step,
which the lowering to LIR makes a step within the array's layout. For a dynamic array or a queue it
is the type's own body, which reads the core's members and answers where the element lies, or, where
the position names none, the type's default to a read and storage no read reaches to a write (LRM
7.4.5, 7.10.1), as rustc's `Index::index` on a `Vec` is a call of the container's method compiled
with its element. Every write through a position -- the element write, a step within a write in
progress, a step through a reference, the C++ class's element write -- is built on one body of the
type that answers where the write lands, or no address, and whether the write made the element
there, which is what a write in progress must learn (LRM 4.3). A queue's is where a write one past
its last appends the element default and holds the queue to its bound (LRM 7.10.5), and where a
write through any other invalid index gives the run-time warning LRM 7.10.1 requires; it takes the
location the write was written at, which the dynamic array's, reporting nothing, does not. The
library's own steps are handed the array's type, whose record holds that body's address (D6), so
they call the code compiled for the type rather than a second statement of its rule. An associative
step finds or allocates through one function of the library over the core and the element type's
record, reached alike by every path. A value an operation takes out of a container is moved out by
the body taking it where no call holds it and copied where one does (LRM 13.5.2). A reference a call
binds to an element is formed by the step that takes the core's lease, never by the element read.

**D6. A container type is a value laid out by its type**
([a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md) D8). Its bytes are
its core. Its record's table holds its own copy, end, assignment and change test, each a D3 body,
and its relocation, which is its core's bytes, and a table of its parts read off D2's fields, which
the library states once per container kind; a dynamic array's and a queue's record also holds the
address of its body finding where a write through a position lands and of its body making it a count
long (D5, D9). Its default is the body building an empty container at its type. Whatever holds a
value of it -- a variable, a net, a reference, a sampled history, a takeover, a task's answer --
holds it as it holds a structure, and asks the record's table for a whole copy or comparison. The
program's element operations never go through the record; the library's own steps within a write,
and randomization's resize, reach the type's bodies through it. What the library walks by parts --
printing, a foreign call's open array, memory files of a dynamic array or a queue -- reads the parts
table.

**D7. An associative array's core compares keys itself for every index type the language fixes the
order of** -- integral, string, class handle, `chandle`, wildcard -- each in its own walk. For an
index of any other type with equality (LRM 7.8.5), the core is handed the type's equality as a
record and keeps entries in the order they were added, which the clause allows: arbitrary, and
stable while no entry is added or removed. A key holding x or z names no entry (LRM 7.8.5, 7.8.6),
which the body asks of the key type before it reaches the core.

**D8. The C++ type of a container type is the library's one class template for a container whose
value's life is its own, at the core its kind lays it out as and at the struct the backend writes
for the type's implementation of the library's traits.** The type implements, among the library's
traits ([a-type-implements-the-librarys-traits](a-type-implements-the-librarys-traits.md)), a
value's life: its four operations ([a-move-relocates-its-value](a-move-relocates-its-value.md) D2),
its default and a copy, each answering a value, writing a copy over a value, and ending one. The
backend writes the implementation MIR states as a struct declaring each item as an ordinary static
function whose body is the MIR body, and spells the type, in its type mapping, as the library's
template at the core and that struct; the element type is what the struct's operations take and
answer. The template is the class. It derives from the core's C struct, and its special members are
C++'s realization of the four operations and of a relocation, written once for every container type
of either kind: its default and copy constructors build the object from what Default and Copy
answer, its copy assignment and its destructor call Assign and End, its move constructor takes over
the source's core and leaves that core inert, and its move assignment ends the value it holds and
then takes over the source's. A class declaring its own destructor or copy assignment gets no move
constructor ([class.copy.ctor]), and End has to run in the destructor of the complete object, whose
body runs while the object is still in use ([class.dtor]), so the six are the one class's. A default
or copy constructor delegates to the move constructor, handing it the value Default or Copy answers,
which costs a move of the core, a few words, that C++ need not elide (CWG 2403). Every operation is
`noexcept`, as none can be unwound out of, which its declaration states and the unit's verification
holds its body to, so none leaves a value half built. A body takes a value out of a local it built
by moving it, as Rust's `return made;` does; the moved local is ended by nothing, and the core's
inert state is the C++ object's alone. The core is built told the record of the element type, which
a call at that type answers, as Rust's `type_id::<T>()` answers the type it is called at, and
Default's answer is the value its core is, a value made of its parts.

What a library template holding values of any type asks of an array is asked in the shapes MIR's
declarations already have: the element at a machine index by reference, from whose type the template
takes the element type, and where a write through a position lands, an element's address or none and
whether the write made it, which the library turns into what a write in progress is told. The class
template answers each such question by calling the struct's item for it, and the struct declares
exactly the implementation's items: the value's life, the change test, an element by machine index
and where a write through a position lands, the value made a count long, the questions asked of a
whole value, and its stream only where its elements stream. A lease on an element is only the
library's, so the library takes it on the core, as the execution backend's runtime does: a queue's
element keeps the count of its leases and a dynamic array's generation keeps them. Every other
operation is a free function named by the type's struct, the operation and the types it is
instantiated at, declared and defined in each code file that calls it.

The struct is one header per type outside every namespace, named by the type's canonical name, so
every unit naming the type writes the same text ([basic.def.odr]). Its items are reached wherever
C++ copies a value or a library template holds one, which no unit can list for another -- a record
another unit declares, copied here, reaches the copy of the container it holds through that unit's
header -- so they are not defined where each is used: each is defined once, out of line, in the
type's code file, which every unit naming the type writes alike and the project compiles once, as
separate compilation defines a class whose users are not known. The items depend on the type alone,
so they are one code file, as a class's members are its implementation file; a code file per
operation compiled a dozen units for each container type, each reading the library's headers again,
where one serves. A free function is reached only by a call MIR names, so it is the code file's own
that makes the call, as each body is the unit's that needs it on the execution backend. Neither text
names a constant of one unit (`../architecture/backend_contract.md`, how a definition is linked).
The struct is named by the type's canonical name as MIR states it, the same in every unit naming the
type -- a class by the unit defining it and its path there -- never by a C++ spelling, which names a
unit's own class relative to that unit and spells handles of two classes alike. Two types are never
one instantiation: a dynamic array of an enumeration and one of its base type are two, as
`std::vector<E>` and `std::vector<int>` are two instantiations whose equal code is the linker's to
fold, not the front end's.

**D9. Randomization resizes a random dynamic array or queue as a step of its own walk**, keeping the
elements the array reaches and filling the rest with the element default (LRM 7.5.1), in a new
generation of a dynamic array's core, or in a queue's core held to its bound. The solver reaches
every element of a random variable through the variable's type to write what it chose, so the size
it chose is written the same way: through the type's own body making the array a count long, which
the execution backend reaches through the type's record and the C++ backend as an item of the struct
holding the type's implementation of the library's traits; the walk then writes the elements.

## Rejected

- **The whole algorithm a template instantiated per element type**, the C++ backend's shape this
  replaces. Each element type compiled the ring, the bound and the window arithmetic again in a text
  only the C++ backend read, and the execution backend could not share it, so the two backends ran
  one source compiled two ways. A dynamic array's and a queue's per-call work is compiled per type
  again (D1), but as bodies both backends compile from one MIR.
- **The core handed a full table of the element's operations**, the library's shape this replaces.
  Every element copy, comparison and end is then an indirect call, and what touches an element is
  not compiled with the element's type.
- **A call of the library per element access.** Measured on a prototype, a dynamic array read and
  filled through one library call per element took three times the instructions of a template
  without link-time optimization, and the same work over a block the shell indexes took what the
  template took.
- **The core copying and comparing a whole container itself, handed the element's copy and change
  test.** That is the previous bullet's table for every whole-value operation, and it keeps three
  container domains and their holder families whose only job is to hold three types that differ from
  a structure in nothing a holder asks.
- **A library class template per container kind that carries the operations out**, the C++ shape D8
  took first. It located elements and moved and ended the value in the library's text, while the
  execution backend ran the type's bodies for the same operations, so one operation had two
  implementations held in step by nothing. D8's template carries out none: each member calls the
  type's operation or relocates the core, which is no operation of a type's.
- **A library class template per kind that the type's class derives from, answering each question a
  library template asks by calling one of the type's bodies**, the C++ shape D8 took second. It gave
  the class members no MIR declaration stated -- an element type alias, an empty value and a value
  taken out, each spelled as a member the core's entries were declared to name -- and its questions
  had shapes MIR could not state, a formation reported through an out parameter and a lease returned
  by a library class. D8's template asks only in the shapes MIR's declarations have, and holds no
  member but the special members that realize the four operations and one member per question a
  library template asks, each calling the struct's operation for it.
- **A move that keeps its source alive to be ended later**, C++'s `T(std::move(x))` as a node of its
  own. MIR's move is Rust's, which consumes its source; every place a body took a value out was a
  local it built, taken out as its last use, which Rust's own move states.
- **Adapters the C++ backend writes into each class**, each calling one body: which body a special
  member or a question calls, and which library helper and argument it passes, were then the
  backend's own decisions, which no MIR node stated and the execution backend made again. D8's
  adapters are the library's, the same text for every type of a kind.
- **A special member calling a named member that holds the step's body, both written by the
  backend.** Each operation then had two declarations, and the special member's own body was text
  the backend composed, which no node stated.
- **Each special member a MIR body**, the C++ shape D8 took third: six bodies following C++'s
  special members, a move leaving its source an empty value for a later end, and a marker on a
  copy's declaration saying the empty value was built first, since C++ runs another constructor only
  in a ctor-initializer. Each of those is C++'s object model, which MIR then stated for a language
  with no constructors ([a-move-relocates-its-value](a-move-relocates-its-value.md)).
- **A copy built in the storage of the object receiving it**, the C++ entry handed that storage, to
  spare the move of the core D8's delegation costs. MIR states no place an answer lies, so the
  backend would have to find which local a body answers and build it in the receiving object
  instead, which is deciding what the body does, or MIR would state a lower layer's return place.
  What it spares is a few words moved per copy, which also copies every element.
- **Each body the explicit specialization of a member the library's template declares.** C++ and the
  library's templates call a value's members where MIR names no call -- a copy, a change test, an
  array of another kind asking its element -- so every such position needed its own rule to include
  the right definition, and each was found by a failing build of emitted C++. A class whose members
  its type fixes answers every one of them at once.
- **Each member inline, defined in the code file of every unit that uses it**, as C++ defines an
  implicit instantiation in every translation unit odr-using it. That holds only where each such
  unit is known to define it, and a member is reached where no MIR call names it -- a record another
  unit declares, copied here -- so the units using one cannot be listed, and a program missing one
  links only while some other unit happens to define it ([basic.def.odr]).
- **Every operation a member of the type's class.** An operation instantiated at type arguments has
  no bound on its instantiations, and a class is one text in every unit, so its members cannot grow
  with what one unit uses.
- **A comparison handed through a record for every key.** On the prototype an associative lookup
  through a comparison callback cost a third more than a template's, and a core comparing its own
  integral key cost less; the order of every key kind but LRM 7.8.5's is the library's to know.
- **A copy answering its value beside a body building one where none is.** Each container type then
  had two copy bodies, each copying the elements in a loop of its own. A copy answers its value
  alone, and the execution backend builds it in its return place.
- **Building the empty core through a library template at the element type.** Only the C++ backend
  could call it, so the execution backend built the empty value another way, by a constructor entry
  told the container type. The element type's record is an operand both read.

## Consequences

- The dynamic array and the queue are carried out this way; the associative array is not yet.
- A dynamic array's and a queue's per-call work is a body per type, compiled by both backends from
  one MIR, over the library's one core for what is the same for every element type. Neither backend
  instantiates an associative array's storage management per element type, and both call the
  library's one core for it.
- Neither backend works out where an element of a dynamic array or a queue lies, or when a write
  makes one: the type's bodies do, so how elements are located changes the bodies and the core's
  header, and neither the code generator nor the C++ class.
- Built at `-O2 --lto` and counted by callgrind, a loop of 200,000 `push_back` and `pop_front` on a
  queue of `int` took 187.8 million instructions with every operation through the core, and 21.5
  million with the per-call paths as bodies, of which 2.2 million is the run's start-up a
  hand-written program does not have, against 19.5 million for hand-written C++ keeping the same
  guarantees. A read-modify-write of every element of a dynamic array of `int` took 17.0
  instructions an element, against about 13.9 for hand-written C++.
- Storage a queue's elements take stays with the run. A recycler gives nothing back to the system
  and is never destroyed, so storage a lease keeps past its queue's end, or a queue with static
  storage gives back as the program ends, goes back to a recycler that still exists, and a run
  holds, for each size of element storage, the most it ever held at once.
- The library's container classes over an element policy go, with the three container value domains
  and every family and entry stated over them; holders, entries and families of a value laid out by
  its type serve containers.
- A unit compiles a body per container type and operation it uses. On the prototype, a unit using
  forty element types compiled in half the time with clang and three quarters with GCC.
- Where an operation crosses a core, the boundary costs about forty instructions without link-time
  optimization and about half that with it, measured on the prototype; GCC at `-O2` does not
  vectorize a loop whose count is a core's field, which the C++ backend accepts in its role as the
  readable rendering of MIR.

## Cross-references

- LRM 4.3, 7.4.5, 7.5.1, 7.8, 7.8.5, 7.8.6, 7.8.7, 7.10, 7.10.1, 7.10.3, 7.10.5, 13.5.2
- [container-element-storage](container-element-storage.md) and
  [array-element-storage](array-element-storage.md) -- the storage each core keeps.
- [a-bound-reference-leases-what-it-reaches-through](a-bound-reference-leases-what-it-reaches-through.md)
  -- the leases a core honours.
- [a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)
  -- the on-demand bodies this extends.
- [a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md) -- the domain a
  container joins.
- [a-published-operation-is-compiled-once](a-published-operation-is-compiled-once.md) -- why the C++
  backend instantiates none of the library's algorithms.
