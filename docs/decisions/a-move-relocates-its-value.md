# A move relocates its value, and each target realizes that with its own objects

Date: 2026-11-16. Status: accepted. Revises the move step of
[a-types-steps-are-stated-in-lir](a-types-steps-are-stated-in-lir.md) D1 and the lifecycle of
[a-containers-operations-are-built-for-its-type-over-a-core](a-containers-operations-are-built-for-its-type-over-a-core.md)
D6 and D8.

## Context

MIR's move was Rust's. A moved place owns nothing afterwards and is ended with nothing to release
(`../architecture/mir.md`). Every layer below stated C++'s instead. The step list a type's table
holds, LIR's move step and the LLVM code generator each built the destination out of a source its
holder still ended afterwards. The runtime's table of a type and the C++ backend did the same,
because C++ destroys a moved-from object. The two models met inside LIR. Its lowering of a move
dropped the source's end, while its move step said the source would still be ended.

The mismatch shaped the run-time-sized containers. Their lifecycle was C++'s six special members:
building empty, building a copy and building by a move in storage holding none, writing a copy over
a value, moving over one, and ending one. A move had to leave its source an empty value, because
something ended it later. A copy ran in storage holding none, so it needed a marker saying that the
empty value was built first. The execution backend then wrapped each of those bodies back into a
function answering in its return place, which is rustc's shape for a clone. The move-assignment body
had no caller on the execution backend at all.

SystemVerilog's values have no constructors of their own, and the language has no move. A move
appears only where the compiler hands a value on instead of copying it.

## Decision

**D1. A move transfers the ownership of an initialized value to its destination, and nothing ends
the source.** The source owns nothing afterwards and is not used again before something initializes
it. This is one meaning, the same in MIR, in LIR, in the LLVM code generator and in what the runtime
library's table of a type calls a relocation. Where it is realized as a copy of bytes, the source's
bytes are left as they are and owed nothing.

**D2. A type that states its own values' life states four operations, and a move is none of them.**
The four are its default (`Default() -> T`), a copy of a value (`Copy(&T) -> T`), writing a copy
over a value (`Assign(&mut T, &T)`) and ending a value (`End(&mut T)`). Default and Copy answer a
value, as Rust's `Default::default` and `Clone::clone` do. Whatever a lower layer makes of where
that answer lies is the lower layer's calling convention, as rustc's MIR answers in its return
place. A move is the same operation for every type and is no body of the type's. Writing a moved
value over another is ending the one there and then the move.

**D3. Each target realizes D1 with its own objects.**

- The LLVM code generator relocates the value's representation and ends nothing of the source.
- The C++ backend realizes a move with a C++ move constructor. That constructor takes over the
  source's representation and leaves the source an inert object. C++ destroys that object later, and
  its destructor does nothing observable. That destruction is how C++'s object model ends the
  storage. It is not a second end of the value.
- The runtime library's table of a type holds only the relocation, which consumes its source. A move
  that leaves the source a live, inert object is C++'s own, made where a C++ holder needs one, and
  never a second way a type's values are moved.

## What the field does

- rustc moves by copying a place's bytes and drops only what drop elaboration finds still
  initialized, inserting drop flags where that depends on the path taken
  (`compiler/rustc_mir_transform/src/elaborate_drops.rs`). Its clone shim builds the copy in the
  function's return place (`build_clone_shim`, `compiler/rustc_mir_transform/src/shim.rs`).
- C++ builds a moved value by a move constructor and destroys the moved-from object afterwards
  ([class.copy.ctor], [class.dtor]). Trivial relocation, which moves an eligible object's
  representation and ends the source without its destructor, was adopted for C++26 ("Trivial
  Relocatability For C++26", P2786R13) and then removed from the C++26 working draft at the November
  2025 meeting over unresolved design issues (P3920R0), its design reopened for C++29 (P4197R0).

Where Lyra differs: standard C++ gives the host compiler no relocation it is bound to provide, so
the C++ backend realizes a relocation as a move constructor followed by the source's destruction.

## Rejected

- **C++'s moved-from model as the meaning of a move in a layer above the C++ realization.** It is
  what the containers' six special members, the marker for building the empty value first and the
  execution backend's wrapping of each body came from. None of those is a fact about SystemVerilog's
  values.
- **A move and a move over a value as bodies of each type.** A move does the same thing for every
  type, so a body per type states nothing a type decides.
- **Leaving each layer to choose.** A moved place whose end one layer drops and the next still runs
  is ended twice, or never, depending on which layer ran last.

## Consequences

- The step a type's table holds for a move is a relocation, and the LLVM code generator's relocation
  ends nothing of its source.
- A value laid out by its type that holds an owning pointer can be moved, as the pointer itself is,
  since a move no longer leaves the pointer for its holder to free.
- The run-time-sized containers state four operations. Building in storage holding none, the marker
  for delegating to the default, and the move bodies go
  ([a-containers-operations-are-built-for-its-type-over-a-core](a-containers-operations-are-built-for-its-type-over-a-core.md)).

## Cross-references

- [../architecture/mir.md](../architecture/mir.md) -- what MIR states of ownership.
- [../architecture/lir.md](../architecture/lir.md) -- what LIR owns of a value's steps.
- [a-types-steps-are-stated-in-lir](a-types-steps-are-stated-in-lir.md) -- who builds each step.
