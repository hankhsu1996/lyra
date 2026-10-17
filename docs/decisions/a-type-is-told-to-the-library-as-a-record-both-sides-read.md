# A type is told to the library as a record both sides read

Date: 2026-10-16 Status: accepted. Revises the form of the table in
[a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) and of the type constant in
[a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) D4; where the
table is told and what fills it are unchanged.

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

**D2. The table is a record of function pointers, one per operation, each handed the type first.**
The library calls through it from ordinary member functions of the type, so code asking a type for
an operation is unchanged. What a value answers about its parts by position is a second such record
the table points to, of the same form. One table serves every type whose functions read the rest off
the type, so every integral type answers through the library's one and the parts of any fixed-size
array through the library's one, each named by a plain symbol. An operation the language does not
define for a type is held by the library's entry failing it, the same failure the library's own
kinds give.

**D3. The library defines its own kinds' types through the same declaration**, as constant data
built from each kind's own members.

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

## Rejected

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
