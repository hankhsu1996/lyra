# A type is told to the library as a record both sides read

Date: 2026-10-16 Status: accepted. Revises the form of the table in
[a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) and of the type constant in
[a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) D4; where the
table is told and what fills it are unchanged. Revised 2026-10-30: D2 narrows the table to what
every holder of a value asks, and D4 and D5 say where the rest goes.

## Context

On the execution backend the library is compiled once, before any design exists, and is handed for
each type the design declares the type's numbers and the addresses of its operations. That is the
residue of the template argument clang has on the C++ backend, which builds no such object at all.
The execution backend's modules reach the library through the C ABI it publishes, not through its
headers (`../architecture/runtime_distribution.md`).

The type was a C++ polymorphic object of a library class -- a value type, a tuple type, a fixed-size
array type, an integral type -- and generated code fabricated one as constant data, a table pointer
to an Itanium-shaped table it emitted (offset to top, a type-information word, two destructor
entries, then one entry per virtual function in declaration order), then the class's fields. Where
each field lay was read at compiler run time off a probe object, a subclass overriding every
operation so the abstract class could be built; which entry a function took was read by bit-casting
a pointer to member function; the integral type and an array's parts named the library's own tables
by mangled name. Every one of these is how clang compiles a header that publishes none of it, so a
reordered declaration would have moved a call to the wrong function with nothing to say so.
[an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) had already ruled
out generated code posing as the library's own C++ classes.

## Decision

**D1. The type is a record laid out as C lays one out, declared once, which the code generator and
the library both read.** It holds the address of the type's table, the size and the alignment of a
value, and which of the records the library reads a type through it opens. A record adding numbers
of its own -- a tuple type's components, a fixed-size array type's count and element type, an
integral type's shape -- opens with it, so the type's address is the record's. The code generator
places each field at the offset the declaration gives it.

**D2. The table is a record of function pointers, each handed the type first, and it holds what
every holder of a value asks of it: a copy, a move, an end, a write over a value already there, and
whether a write changed the value.** The last is there because the language lets any variable be
waited on (LRM 9.4.2), so a cell, a net or a container's element asks it on every write, whatever
the type. What a value answers about its parts by position is a second such record the table points
to, of the same form. One table serves every type whose functions read the rest off the type, so
every integral type answers through the library's one and the parts of any fixed-size array through
the library's one, each named by a plain symbol.

**D3. The library defines its own kinds' types through the same declaration**, as constant data
built from each kind's own members.

**D4. Every other operation of a type is handed to what asks it, as a record of its own.** Code
compiled once is handed only what its job reads, so no type carries an answer for a holder that
never asks:

- Comparing, testing for unknown bits, and the bit stream's width, count, writing and reading are
  one record per type: the address of a table of those functions, the type, and the element's own
  such record where the type is a container. A call that compares or streams a value is handed it
  after its operands, and nothing keeps it. A container's record names its element's rather than
  being code per container type, so comparing a queue of queues needs no function of its own.
- How a net of an aggregate type resolves its drivers is a record the net is built holding.
- Which positions a force or a procedural assign takes over is a function handed to where it begins.
- How an associative array orders its keys is a record per index kind, which the library defines and
  the array is built holding.

An operation the language does not define for a type is held in its record by the library's entry
failing it, the same failure the library's own kinds give.

**D5. What a value of a type is before anything is written to it is MIR's, handed with the type.** A
holder of storage -- a cell, a net, an array of any kind -- is built from one constant per type
naming the type and the function MIR states for its default
([an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md)
D2), and no type answers its own default. A library container type's own answer was a container with
no element type, which is not the language's value; a tuple's default exists only where each of its
components has one (no declaration holds a wildcard index, LRM 7.8.1).

## Survey

Every system that hands a type to code compiled before it has one owner of the describing object's
layout, and both sides read that one statement.

- rustc writes a trait object's vtable itself -- drop, size, align, then the methods
  (`compiler/rustc_middle/src/ty/vtable.rs`, `COMMON_VTABLE_ENTRIES`; emitted by `get_vtable` in
  `compiler/rustc_codegen_ssa/src/meth.rs`); the library holds it as an opaque type and asks its
  size and alignment through intrinsics (`library/core/src/ptr/metadata.rs`, `DynMetadata`).
- Swift states the value witness table once, in its ABI directory's `ValueWitness.def`; the
  runtime's `TargetValueWitnessTable` and IRGen's enumeration are both generated from it, every
  witness is handed the type's metadata, and the runtime builds its builtin types' tables through
  the same struct.
- Go declares `abi.Type` in its internal `abi` package, which the runtime and the compiler both
  import; a kind's descriptor embeds it first, and the compiler lays the same declaration out for
  its target (`cmd/compile/internal/rttype`).
- clang reads both sides from one class definition, which is the condition the library lacks once it
  is prebuilt.

Lyra's compiler and library are one language in one repository and the execution backend compiles
for the host, so Go's and Swift's answer is available as it stands. The declaration is a header, and
`offsetof` on it is the layout. rustc's answer needs the library to be compiled by the compiler that
writes the table, which a library compiled by clang is not. What is taken from Swift and Go is the
shape of the record alone: every operation is still compiled for its type, so this is not the
description the runtime walks that
[a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) rejects.

What each record holds follows who asks it (D2, D4). Swift's value witness table holds the steps of
a value's life and its layout and no comparison (`ValueWitness.def`); `Equatable` is a witness table
per type and protocol, handed beside the metadata to a generic function that asks it, and an array's
conformance is built from its element's (Swift's type metadata ABI document, the generic argument
vector). rustc's vtable is per type and trait, opening with drop, size and align. Go keeps `Equal`
in `abi.Type` because its runtime compares interface values and map keys whose type it learns only
while running; Lyra keeps the change test in its table for the matching reason, that every holder
may be waited on, and only that.

## Rejected

- **One table per type holding every operation**, the form D2 narrows. Every holder was handed
  comparison, streaming and net resolution it never asks, and a library container kind's own
  default, which was not the language's, sat beside them.
- **Posing as the library's C++ classes and measuring them** (the form this replaces). It rests on a
  layout no header publishes, and kept a probe class, a pointer-to-member reading and mangled table
  names to track it.
- **Generic library code compiled per type by the compiler**, rustc's `core`. It removes most uses
  of a type's table rather than its shape, since what stays prebuilt still needs this record.
  Whether to move a container's algorithms there is a question of run time against code size, not of
  this layout.

## Consequences

- Nothing in the value types is measured at compiler run time, and no value type is named by a
  mangled symbol. Generated classes extending the library's scope and object still follow the C++
  ABI, for the reason
  [an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) gives.
- The layout is the host's, since the backend compiles for the host; compiling for another target
  would lay the same declaration out for it, as Go's compiler does.
