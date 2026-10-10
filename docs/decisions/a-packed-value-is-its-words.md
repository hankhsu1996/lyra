# A packed value is its words

Date: 2026-10-10 Status: accepted. Carries out D1 to D5 of
[a-value-is-its-machine-data](a-value-is-its-machine-data.md) for packed values and revises its D2
above 64 bits. Supersedes invariant 4 of
[packed-array-representation](packed-array-representation.md), and the rejection of a per-type
realization of a packed value in [jit-value-realization](jit-value-realization.md).

D6 to D8 bind what holds a packed value and what the library is told of one, and the code does not
yet meet them everywhere. The largest exception is what holds an element: a queue, a dynamic array,
an associative array, an unpacked union and a closure's answer are still told the type of what they
hold as one constant per type and act on it through that constant, which D7 does not allow.

## Context

[a-value-is-its-machine-data](a-value-is-its-machine-data.md) decides what a packed value is: its
bits, with its type the compiler's to know. It does not say which layer turns a four-state operation
into machine work, what the layer below MIR calls an integer, what holds a packed value on the
execution backend, or what code compiled once into the runtime library is handed when it must act on
a packed value of a type it was compiled without. This entry settles those.

The requirement, stated without any name the code has: each packed value occupies the bits its type
declares -- twice that when it can hold unknowns -- every operation on it costs the bits it touches,
and nothing at run time reads a description of its type. The library exists before any design does,
so the second half is a question of its own: a program fixes types that did not exist when the
library was compiled, and for each thing done to a value of such a type the code that does it is
either produced with the design or is earlier code told something.

What the answer rests on:

- A packed array is a contiguous set of bits (LRM 7.4.1), and `int` is `bit signed [31:0]`, one type
  with it (LRM 6.11.1).
- An x or z bit in an arithmetic operand makes the whole result x (LRM 11.4.3), and the bitwise
  operators act per bit by table (LRM 11.4.8). A four-state operation is therefore a short formula
  over two planes.
- Every expression's width and signedness come from its type, and the front end hands each operator
  operands of its own type (LRM 11.6, 11.8).
- The runtime library is compiled by the host compiler that builds Lyra, and Lyra links its own LLVM
  release. LLVM bitcode is read only by the same or a newer LLVM, so no bitcode of the runtime
  exists for the execution backend to inline.
- Emitted C++ is held to a small multiple of the SystemVerilog it came from, so text per operation
  and type in every unit is not affordable; a header template costs a unit no lines.
- A design's own code is compiled unoptimized by default, because the time from an edit to a running
  simulation is the first objective.
- A net is cut at positions known only when the design is constructed (LRM 23.3.3.7), so how many
  positions a net has is a count the program computes.
- Measured before this change, Ibex made about 10,000 packed operation calls per cycle, a 32-bit add
  costing about 190 instructions against one.

### Who decides the representation

|                 | types the expression | picks the machine shape                             | up to 64 bits            | above                                                |
| --------------- | -------------------- | --------------------------------------------------- | ------------------------ | ---------------------------------------------------- |
| clang `_BitInt` | Sema                 | CodeGen (`iN`, a storage unit in memory)            | native                   | the type legalizer per word; an IR pass for division |
| Verilator       | `V3Width`            | emit, from the width                                | C operators              | `V3Expand` per word; calls for add, multiply, divide |
| Lyra            | HIR-to-MIR           | the library header (C++); the code generator (LLVM) | native or plane formulas | header loops (C++); a call taking the words (LLVM)   |

Sources, read in each project's tree: clang `lib/CodeGen/CodeGenTypes.cpp` (`ConvertTypeForMem`) and
LLVM `lib/CodeGen/SelectionDAG/LegalizeIntegerTypes.cpp`, `lib/CodeGen/ExpandIRInsts.cpp`;
Verilator's `V3AstNodes.cpp` (`cTypeRecurse`), `V3Expand.cpp` and `verilated_funcs.h`. NVC (two
planes for four-state, inline up to 64 bits, a call above) and CXXRTL (`value<Bits>` with
always-inline templates) were consulted from summaries only, their sources not opened, and nothing
below rests on either.

### What code compiled once is told of a type it was compiled without

| System    | Where a generic algorithm over a user's type is produced                             | What its compiled-once code is told                                                                      |
| --------- | ------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------- |
| clang     | instantiated in the user's object, deduplicated at link                              | a size as a number, the address of a function generated in the user's object, a type's identity          |
| rustc     | generic MIR shipped with the library, monomorphized into the user's crate            | nothing, except where the library chose one body: then a size and alignment, or a pointer and a function |
| Verilator | header templates the host compiler instantiates; the compiled library holds none     | a word count or a bit count as an `int`, and a pointer                                                   |
| Go        | indexing emitted inline; growth, map access and copying are runtime functions        | a per-type constant: a size, a pointer extent, an equality function, for a map a hash function           |
| Swift     | one body for unspecialized generics, specialized where the optimizer sees the callee | a value witness table: size, stride, and functions that copy and destroy; field records serve reflection |

Sources. Clang and its libraries, read in tree: `compiler-rt/lib/builtins/atomic.c`
(`__atomic_load_c(size, ...)`), `libcxxabi/include/cxxabi.h` (`__cxa_throw` taking a destructor
address, `__cxa_vec_new` taking an element size and two function addresses),
`libcxx/include/__format/format_arg.h` (a handle of one pointer and one function),
`libcxx/include/__algorithm/copy_move_common.h` (a trivially copyable element is a `memmove` of a
multiplied size). Rustc, read in tree: `library/alloc/src/raw_vec/mod.rs` (a vector's allocation and
growth written once over the element's `Layout`, to cut monomorphization cost by its own comment).
Verilator, read in tree: `verilated_types.h` (`WDataInP`, so that runtime functions need not be
templates) and `verilated_funcs.h` (`VL_ADD_W(words, ...)`). Go and Swift were read from their
published sources and not from a checkout: Go's `internal/abi/type.go` and its runtime's `slice.go`;
Swift's ABI document `TypeMetadata.rst`.

Two things every one of them agrees on. A value that can be copied as bytes has no per-type code and
no table at all. And nothing compiled once interprets a description of a type's fields to act on a
value: where behaviour crosses, it crosses as the address of a function produced on the user's side.

Lyra's conditions differ in three ways: two backends consume one MIR, the execution backend links a
runtime compiled once, and a value may hold x and z, which Verilator drops. None changes what the
survey selects.

## Decision

**D1. One layout for one type, on both backends, and storage is clean.**

```
unit(N)             N <= 8: 1 byte; <= 16: 2; <= 32: 4; <= 64: 8; else ceil(N / 64) words of 8
two-state  T(N)     unit(N)
four-state T(N)     { value: unit(N); unknown: unit(N) }
encoding (v, u)     0 = (0, 0)   1 = (1, 0)   z = (0, 1)   x = (1, 1)
```

Every position above the width is clear in both planes, whatever the signedness. Clang extends a
`_BitInt` per its signedness on a store and truncates on a load. Lyra does not, because two things
need one encoding per bit pattern: a conversion between a signed and an unsigned type of one width
changes how the bits are read and none of the bits, and a write decides whether it was a change by
comparing storage. A two-state value has no unknown plane at all, so `int` occupies four bytes,
`logic [7:0]` two and `logic [99:0]` thirty-two, and a value is trivially copyable.

**D2. MIR states an integral type as exactly its width, signedness and state domain, and an
operation as the language's operator on that type.** It states no words, planes, masks or storage
units, and no description of a type as a run-time value. Packed dimensions do not reach it:
HIR-to-MIR reduces a packed type to those three facts and every select to a position, reading the
declared ranges from the source type where the select is lowered. An index into anything that
numbers its parts -- a queue, a dynamic array, a string -- is converted to a position the same way,
where the index is written. A constant an expression names is its bits at its type, held once by the
unit and named at each use, and an operation or a conversion over constants -- including reading one
out as the machine integer a runtime entry takes -- is folded where it is built, by the one builder
the lowering makes its operators, its casts and its own calls on an integral entry through.

**D3. Every operation is stated once, as one function per operator of the language over planes and a
width, and that one statement is used three ways.** Each settles what the operator does with an x or
a z and answers as the language says the operator answers. The C++ value type of a packed type calls
them on arrays whose size its type fixes. Called with widths given at run time they are the prebuilt
entries the execution backend calls above 64 bits. And they are the compiler's own constant folding.
A folded operation, an inlined one and a called one therefore cannot disagree.

Whether such a function is folded into its caller is decided by the build that compiles the caller.
An optimized build inlines the short ones, and the host compiler reduces an operation on one word to
the instructions its width leaves. Measured with clang at `-O2` on x86-64: `int + int` is one
instruction, `int * int` one multiply, `int / int` the machine's divide behind a test for zero, a
four-state 32-bit add seven instructions, `==` fifteen and `&` seventeen. An unoptimized build folds
nothing, so there the short functions are called and a unit holds one copy of each it uses. An
operation whose work grows with the value -- a product or a quotient of many words, a power, a run
of digits -- is never forced inline in either build.

**D4. On the execution backend the four-state formula lives in the code generator, as a fixed
function of the operand type.** The layer below MIR keeps the integral type MIR states and names the
operator; the code generator translates each operator on it to inline arithmetic on one or two words
up to 64 bits and to a call above. An operator's semantics come from its operand type: rustc's MIR
states `BinOp::Div` once and `codegen_scalar_binop` picks `sdiv` or `udiv` from the type. The
translation decides nothing beyond the type, so it stays mechanical, and no intermediate level of
exact-width integers and pairs exists for one consumer to read. The single-word formulas are thus
written twice in total -- the functions of D3 and the code generator -- which is how both backends
already realize every primitive operator. What holds the second statement to the first is a test
that evaluates each single-word form and the function of D3 on the same operands and compares both
planes of their answers. The conformance corpus, which runs every case on both backends, stands
beside it.

**D5. The line is 64 bits.** Up to a word an operation is a handful of instructions and inlines;
above it the execution backend calls. LLVM's own legalization of a wide `iN` is not used: it emits
straight-line code proportional to the word count at every site, and code size per operation would
follow the width instead of being constant.

**D6. What holds a packed value reads only its layout.** A cell, a net, a sampled history, a
reference and a shared cell copy, compare and store a value's bytes and never interpret them. Up to
64 bits what a holder needs of a type is one of eight layouts -- one, two, four or eight bytes, with
or without an unknown plane -- so the runtime library compiles each holder family once per layout,
and the execution backend lays out values, cells and frame slots of such a type from the type. No
holder of them is told a type.

A value wider than a word is held as the words of its planes at a width the holder is told once, by
whatever installs it: a declaration for a variable's cell, the default a history is filled with, the
count of positions a net's declared type has, and, for a procedural local, which no declaration
installs, whatever builds its storage. Every value the holder takes afterwards is the bytes of a
value that wide, and a write is a change exactly where the words differ. A whole write compares
those bytes with the words the holder has and copies them there; a level of a procedural continuous
assignment holds no words until the first value it is driven with, so that one value is built for
it. A reference, and a place designated within a write, hold nothing and name bytes lying in
something else, so neither can be told at an install; an access through one is told the width, as
the size of what a thin pointer names is the accessing code's and never the pointer's.

**D7. Code compiled before a design exists is told three things about the design's types and no
others: a number of bytes, a number of bits, and the address of a function the design's own code
contains.** It is never handed anything it must interpret to learn what a value is. This is the
survey's common ground, and it follows from what such code can do at all. Without knowing a type it
can allocate, free, move and copy a number of bytes, which is everything a holder does. Without
knowing a type it can do integer arithmetic over a number of bits, with or without a second plane,
which is every integral operation and every net's resolution. Anything else means something only for
the type, and is reached by address.

For a packed value the third kind is never needed: it owns nothing, so copying it is copying its
bytes, and every operation on it is arithmetic over its bits. Prebuilt code that reads a packed
value's meaning -- formatting, conversion to and from text, power, file and memory-image parsing, a
delay's amount, the wide operations -- therefore takes its bytes and the numbers it reads of them.

**D8. How an entry reads each operand is declared with the entry, and a call is arranged from that
declaration and from nothing about the value in hand.** The set of readings is closed, an operand
has one, and a reading names one way of passing. An operand whose planes are read is passed with its
width and whether it has an unknown plane, and one read as a quantity with its signedness as well.
One put into, or taken out of, something that already knows its layout is passed with nothing. An
ordinal is passed as a position, which the lowering converted it to. A value the entry keeps, and
the key an associative array is reached by, are passed with their type. A count, a flag, a
descriptor and the storage the entry acts on are passed as the machine values they are. This is how
clang arranges a call from the callee's signature and never from the argument
(`clang/include/clang/CodeGen/CGFunctionInfo.h`, one `ABIArgInfo` per formal) and rustc from the
callee's `FnAbi` (`compiler/rustc_target/src/callconv/mod.rs`).

What a realization for one layout is told beyond its operands is stated with that realization. The
library realizes an entry acting on storage once per layout of what the storage holds. Storage
holding a value's bytes is told nothing, storage holding the words of a wider value is told how wide
it is, a memory reached by keys is told the type its keys are built at, and a union is told the type
of the member it is to hold.

An entry is over one kind of value. An operation the source states over values of several kinds is
an entry of its own over integral values, and that entry is the operation of D3. An associative
array is reached by a key and every other container by a position, so each access into one is an
entry of its own. MIR's own check holds a call to its entry's one declaration.

Arranging how arguments are passed is a step apart from emitting the call, as clang keeps
`CodeGenTypes::arrange*` apart from `CodeGenFunction::EmitCall`. A test holds every prototype the
library publishes to its one declaration.

**D9. A comparison answers a scalar.** Equality, a relation and a wildcard match answer 0, 1 or x as
one four-valued bit, encoded as its two plane bits, whatever was compared and wherever the
comparison was compiled. That encoding is stated once, with the scalar, and every producer and
reader of one uses that statement. A case equality is never unknown (LRM 11.4.5), so code compiled
without the type of what it compares answers whether it holds. Generated code, which knows the type
the language gives the answer, stores it at that type; the C++ library's own typed operators answer
a value of that type directly. A stream of bits is written into, and read out of, planes whose width
the caller states.

**D10. A net's positions are words and a count.** A net is resolved over two planes and the number
of positions the design's construction gave it, by the same functions that fold an integral value,
with the count as their width. The fold is instantiated for no type. Reading a driver's contribution
into positions, and showing resolved positions in the value a name holds, are compiled once per
layout, because each reads or writes a value where it lies. A net of an integral type has one
install on both backends, told the count of positions its declared type has and the scalar its net
type contributes where nothing drives it; a net of an unpacked aggregate is installed with a value
of its type instead, which is an entry of its own.

## Rejected

Where the execution backend's four-state formula could live:

| Alternative                                                                                    | The requirement it fails                                                                                                                                 |
| ---------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| The runtime's own implementation compiled to bitcode and inlined into each module              | The host compiler and Lyra's LLVM are different compilers by construction and bitcode reads only forward, so the build cannot produce it.                |
| HIR-to-MIR synthesizes one MIR function per operation and type, compiled by both backends      | Emitted text per unit must not grow with the operations and types a unit uses.                                                                           |
| Prebuilt word routines called for every operation                                              | An operation costs the bits it touches: a call per 32-bit add is the cost being removed.                                                                 |
| The layer below MIR restates each four-state operation on exact-width integers and their pairs | Correct, and the first design. It adds a level only the code generator reads, where the operand type already decides the operation as it does for rustc. |

And beside them:

- **Extending a value per its signedness in storage**, as clang does. Fails the requirement that a
  write is a change exactly where storage differs: two encodings of one bit pattern make a byte
  comparison answer wrongly and make a reinterpreting conversion do work.
- **Leaving wide integers to LLVM's legalization.** Fails constant code size per operation; see D5.
- **Keeping packed dimensions on the MIR type.** Fails MIR's rule that a source language's
  coordinate system does not reach it. Their readers were a position computation, a memory-image
  range and a cast deciding it was a no-op, and each reads the source type where it is lowered.
- **A holder family per type on the execution backend, or one holder handed a type.** The first
  fails the rule that artifacts follow what one unit states: the runtime's size would follow the
  design's widths. The second fails D7.
- **A value wider than a word held with a record of its type.** Fails D7: every read of the value
  first reads the record, and a record for a width the library never named has to be made while the
  program runs.
- **A net's positions held as a value of a type made from the count.** Fails D7 the same way, with a
  lookup under a lock on every construction.
- **A comparison answering a one-bit value of a type the library looks up.** Fails the requirement
  that an operation costs the bits it touches: an element comparison inside a container paid a
  lookup for an answer that is two bits.
- **A table of functions per integral type**, as Swift's value witness table would be. Fails nothing
  in correctness, and is unnecessary: a packed value owns nothing, so nothing about it crosses as a
  function.

## Consequences

- The runtime value class that carried its own width, signedness and state domain, the record of an
  integral type made while the program runs, the type and range descriptions for packed types, and
  the runtime entries that performed a packed operation up to a word are gone on both backends.
- Measured on Ibex, whole run under callgrind, `--release`, both programs ending at `$finish` at
  26548: the execution backend 7.16 G instructions, from 13.13 G before this entry, against
  Verilator's 0.122 G. The C++ backend was last measured on 2026-10-08, before D7 to D10 were
  carried out, at 6.04 G from 10.87 G, and has not been measured since.
- The C++ backend's compile time under one instantiation per distinct type, which
  [a-value-is-its-machine-data](a-value-is-its-machine-data.md) says is measured on the way, has not
  been measured; this entry was accepted without it.
- The earlier entries that describe a packed value as one runtime type carrying its shape --
  [integral-representation](integral-representation.md),
  [runtime-shape-and-default-value](runtime-shape-and-default-value.md),
  [packed-shape-belongs-to-the-type](packed-shape-belongs-to-the-type.md),
  [enum-representation](enum-representation.md), [value-type-concepts](value-type-concepts.md) --
  keep their arguments and describe a shape this one replaces: an enumeration's value is still its
  base integral and the operator surface is still a lattice of concepts, over a value type per
  packed type instead of one for all.
