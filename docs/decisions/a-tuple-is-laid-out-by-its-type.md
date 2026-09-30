# A tuple is laid out by its type everywhere, and carries its type's table

Date: 2026-09-26 Status: accepted. Supersedes
[jit-aggregate-realization](jit-aggregate-realization.md) for tuples; unions and containers keep the
realization that entry chose. What the table's operation slots hold, and who states those
operations, is
[a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md).

## Context

A tuple -- the runtime's word for a product, whether MIR states it as a tuple or as a struct -- is a
value built from all of its components at once: an unpacked structure (LRM 7.2) and a callable's
answer, which is its result and then every `output` and `inout` argument (LRM 13.5). The C++ backend
renders a callable's answer as `Tuple<Ts...>` and a structure as a type its declaring unit defines
on it; the host compiler lays either out inline and instantiates its copying and ending per type.
What the execution backend does with a value after MIR is meant to be what clang makes of the C++
backend's rendering of it; the one allowed difference is what its prebuilt runtime causes.

The execution backend held every tuple as one erased runtime object: a heap vector of boxed
components, with every operation written again as a fold over that vector. Building an answer boxed
each component, reading one was a runtime call, and a struct variable's every write rebuilt the
vector. On call-chain (`--release`, callgrind, 20,000 iterations) the tuple entries were about a
fifth of the run.

The runtime is compiled once, before any tuple type exists, and its value families -- a variable's
cell, a net, a driver, a history, a container's element -- are instantiated over one type per
domain. That is a condition of the backend, not a gap: it is what makes a build link a prebuilt
library rather than compile the runtime per design.

## Decision

**A tuple has one form in memory whoever holds it: its type's layout, opening with the address of
its type's operation table.** The table is what a C++ object whose class has virtual functions opens
with, and for the same reason -- code compiled before the type existed reaches the type's operations
through it.

- **The code generator lays a tuple out** the way a C compiler lays out a record: after the table
  word, each component at the first offset its alignment allows after the one before. A component is
  reached at its offset wherever the tuple lies -- a frame slot, a variable's cell, a container
  element -- so a part of one is a place step and never a library call.
- **It compiles each tuple type's lifecycle** -- copy, move, end, assign -- member by member,
  calling the component domain's library entry or, for a nested tuple, that tuple's own step, the
  way clang emits a class's implicit special members. The operations the language defines on the
  whole value are methods the structure's declaration states in MIR, and the table's other slots
  hold them.
- **A structure's table is its declaring unit's**, one in the program, whose operation slots hold
  the structure's methods; every other unit's values of the structure carry that table. A tuple a
  lowering composes has no operations, so a table of one holds the lifecycle alone and each module
  keeps its own.
- **Generated code calls the operations directly**: `==` on two structures is a call of the unit's
  function, and copying one into a local is a call of the lifecycle step.
- **The runtime holds a tuple in that same layout**, in storage of its own, and every family
  operation it performs on one is a call through the table the tuple carries. A tuple crosses the
  boundary as the address of its bytes in both directions, so no entry takes a tuple in any other
  form and none converts one.
- **An address of a tuple is the address of its bytes, wherever it lies.** A pointer, a reference
  and a designation all name the value, so what keeps one inside another object -- a variable's
  cell, a value a closure captured -- is opened where the address is formed, and a dereference
  arrives at the value without asking what it came from.
- **Storage given for a tuple states its type before anything is built there**: the caller writes
  the table's address first. The runtime builds tuples it has no type for -- a scan's or a
  traversal's completion -- from the values it holds, laid out by the table it finds.
- **A closure body answering a tuple names the tuple's table in its declaration**, so the runtime
  builds the answer in storage its type sizes.

## Rejected

- **A description of the type that the runtime walks**, Swift's and Go's answer to a generic runtime
  over types it was not compiled for. It is a second realization of every operation, interpreted at
  run time, where clang compiles each operation for its type; the requirement is clang's.
- **Laying a tuple out in generated code and converting it wherever it crosses to the runtime**,
  which the previous realization did. Every variable's storage is runtime-held, so a struct variable
  paid a conversion on every access, and a read that borrows a value where it lies would have handed
  generated code two forms of one type.
- **Passing a tuple's table beside its bytes instead of inside them.** Entries over a container's
  elements take a value of any domain, so their signatures cannot grow a table argument for one
  domain, and the families that keep a tuple would each need to be told its type again.
- **Opening a variable's cell where a pointer is dereferenced.** A body is lowered once for every
  caller, and a pointer it is handed may name a variable's cell, an element or a component, so the
  dereference would be guessing; a pointer to an element was already the value, and one to a cell
  read as the value was a crash.
- **Keeping the erased tuple and making it cheaper.** Its cost is the boxing and the heap vector,
  which exist only because nothing on the runtime's side knows the tuple's type; the table is what
  gives it that knowledge.

## Consequences

- Measured on `--release`, callgrind, 20,000 iterations, against main: call-chain 1,032.0 M to 851.3
  M instructions, and a loop copying a struct variable through a function and back 278.7 M to 236.3
  M. Ibex's execution trace is identical to main's.
- A tuple costs one word more than its components: the table's address.
- The table is the execution backend's alone. It exists because the runtime's value families hold a
  tuple erased over its domain, so the C++ backend, whose families the host compiler instantiates
  per type, carries none.
- The runtime copies a tuple it is handed into storage of its own, which is an allocation per write
  into a struct variable, as the vector it replaces was. A tuple the runtime only reads for the
  length of a call could be read where it lies.
- A handle to a runtime cell is typed as a pointer to the value it holds, never as that value: once
  a value's type decides how it is laid out, a handle typed as the value would be laid out as one.

## Cross-references

- [a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md) -- what
  fills the table's operation slots.
- [a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md) -- a value is an object
  in its maker's frame; this sizes a tuple's by its type rather than by its domain.
- [jit-aggregate-realization](jit-aggregate-realization.md) -- the erased realization, which unions
  and containers keep.
- [a-part-of-storage-is-reached-where-it-lies](a-part-of-storage-is-reached-where-it-lies.md) -- a
  part is a place step; for a tuple it is an offset.
- [unpacked-struct-representation](unpacked-struct-representation.md) -- a struct is a product
  value, which is what makes it one realization with a callable's answer.
- `../architecture/lir.md` -- physical layout is derived below LIR.
