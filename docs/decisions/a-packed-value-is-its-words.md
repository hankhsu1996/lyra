# A packed value is its words

Date: 2026-10-10 Status: accepted. Carries out D1 to D5 of
[a-value-is-its-machine-data](a-value-is-its-machine-data.md) for packed values and revises its D2
above 64 bits. Supersedes invariant 4 of
[packed-array-representation](packed-array-representation.md), and the rejection of a per-type
realization of a packed value in [jit-value-realization](jit-value-realization.md). D2 narrows
[a-constant-is-stated-not-computed](a-constant-is-stated-not-computed.md) and P3 of
[a-select-names-a-position](a-select-names-a-position.md) for a value too large to lie in what holds
it. Such a value is stated as data only where the source wrote that much data.

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
nothing at run time reads a description of its type, and a program the compiler accepts runs at
every width it accepts. The library exists before any design does, so what it reads of a type is a
question of its own: a program fixes types that did not exist when the library was compiled, and for
each thing done to a value of such a type the code that does it is either produced with the design
or is earlier code told something.

What the answer rests on:

- A packed array is a contiguous set of bits (LRM 7.4.1), and `int` is `bit signed [31:0]`, one type
  with it (LRM 6.11.1).
- A tool may limit how long a vector or a packed array is, to no fewer than 65,536 bits (LRM 6.9.1,
  7.4.1). The front end's limit is 2^24 - 1 bits.
- A design's author states a width and has no way to say where a value of it lives. The value an
  expression makes lies in the frame of the function evaluating it, and a frame is taken from a
  stack whose size whoever started the program chose.
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
- Measured before this entry, Ibex made about 10,000 packed operation calls per cycle, a 32-bit add
  costing about 190 instructions against one.

### Who decides the representation

|                 | types the expression | picks the machine shape                             | up to 64 bits            | above                                                |
| --------------- | -------------------- | --------------------------------------------------- | ------------------------ | ---------------------------------------------------- |
| clang `_BitInt` | Sema                 | CodeGen (`iN`, a storage unit in memory)            | native                   | the type legalizer per word; an IR pass for division |
| Verilator       | `V3Width`            | emit, from the width                                | C operators              | `V3Expand` per word; calls for add, multiply, divide |
| Lyra            | HIR-to-MIR           | the library header (C++); the code generator (LLVM) | native or plane formulas | header loops (C++); plane formulas in place (LLVM)   |

Sources, read in each project's tree: clang `lib/CodeGen/CodeGenTypes.cpp` (`ConvertTypeForMem`) and
LLVM `lib/CodeGen/SelectionDAG/LegalizeIntegerTypes.cpp`, `lib/CodeGen/ExpandIRInsts.cpp`;
Verilator's `V3AstNodes.cpp` (`cTypeRecurse`), `V3Expand.cpp` and `verilated_funcs.h`. Verilator
states an operation per word only up to a size: `V3Expand.cpp` (`doExpandWide`) against
`--expand-limit`, 256 words of 32 bits by default, which is 1024 bytes and also the default of its
`--localize-max-size`. NVC (two planes for four-state, inline up to 64 bits, a call above) and
CXXRTL (`value<Bits>` with always-inline templates) were consulted from summaries only, their
sources not opened, and nothing below rests on either.

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

### Where the words of a wide value live

| Family                                                 | Members                                                                         | Words of a temporary       | Widest value accepted                                                                |
| ------------------------------------------------------ | ------------------------------------------------------------------------------- | -------------------------- | ------------------------------------------------------------------------------------ |
| Width in the type, words in the value                  | clang `_BitInt`, rustc, CXXRTL `value<Bits>`, Verilator `VlWide<N>`             | the host frame             | clang 128 bits by default and 2^23 on x86 and RISC-V; Verilator 65,536 for a literal |
| Width at run time, one word in the value, a heap above | LLVM `APInt`, slang `SVInt`, GMP `mpz_t`                                        | the heap                   | millions of bits; slang 2^24 - 1                                                     |
| Placed by size, decided where the type is compiled     | Go: a variable past 128 KiB declared or 64 KiB implicit is not put on the stack | the frame below, heap past | unbounded                                                                            |

Sources, read in each project's tree: clang `lib/CodeGen/CGExpr.cpp` (`CreateTempAlloca`, which
tests no size) and `TargetInfo::getMaxBitIntWidth`; rustc `rustc_codegen_ssa/src/mir/mod.rs`
(`allocate_local`); Go `cmd/compile/internal/ir/cfg.go` (`MaxStackVarSize`,
`MaxImplicitStackVarSize`) and `escape/utils.go` (`HeapAllocReason`); Verilator `V3Premit.cpp`,
`V3EmitCSyms.cpp` (`stackCheck`) and its runtime's `verilated.cpp`, where a generated model states a
predicted stack need and the runtime raises the process limit to meet it.

Nobody in the first family accepts millions of bits. Where the frame holds the value the type is
capped low, and where millions of bits are accepted the words are on the heap. Clang and rustc can
leave a large value in the frame because their author wrote the type and can move it, and a
SystemVerilog author cannot. Go's conditions are the ones that match, which are sizes known where a
type is compiled, an author with no say, and a compiler that places by size.

## Decision

**D1. One layout for one type on each backend, storage is clean, and a value is its words at every
size.**

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
`logic [7:0]` two and `logic [99:0]` thirty-two.

A value is exactly its words at every width, so it is trivially copyable and code compiled without
its type reads and writes it where it lies. What a frame may not keep is a value whose size follows
a declared width: a value past 1024 bytes -- a four-state type above 4,096 bits, a two-state one
above 8,192 -- is held through a box MIR states before either backend reads it, and both backends
translate that box
([a-new-expression-allocates-then-initializes](a-new-expression-allocates-then-initializes.md)); a
holder keeps one apart likewise
([a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md)). **Amended
2026-10-13:** the C++ value type no longer owns its words past the bound. Owning them put where a
value lies in the library, decided by size, beside the box MIR states, so one fact was stated twice
and every temporary of such a type allocated; with the box stating it once, the library lays every
value out flat.

Keeping every value in the frame that uses it fails the requirement that a program the compiler
accepts runs at every width it accepts. What one call takes from the stack would follow the widths a
design declares, and the stack's size is not the design's to choose. A value of 2^24 - 1 four-state
bits is 4 MiB, and an expression holds several at once. Below the size nothing is added to a value
of a word or a few, which is where a design spends its time.

On the execution backend a holder of a wide value obtains its run of words where it is built, and a
design builds and ends such holders many times, at the few sizes its types fix. So a run of words
given back is kept for the next holder that long instead of being returned to the system, per
thread, and how much is kept beyond what is in use is bounded by two constants and the design's own
widths.

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

Folding states as data what its operands already state as data, and makes none. An operation over
constants is folded when its answer is no larger than the size D1 bounds, or is no wider than the
operands it was computed from together. Otherwise it stays the operation it is, and MIR's own check
asks the same builder, so it holds an operation to being folded exactly where the builder would fold
it. A value of a type too large for a frame can also reach the lowering already a value: a fill
literal, a declared default, a parameter. It is stated as the extension of its narrowest low part,
which is an operation MIR already has, wherever that part takes fewer words than the type. A table
the source writes out in full is that many distinct words and stays a constant of that size. The
requirement is that an artifact follows what a unit states. A fill, a small number widened and a
default of all x say a few bits whatever the type's width, and stated as data each would cost the
unit, its emitted text and its build the width of the type. MIR's integral constant is unchanged by
this, and both backends write the same operations.

**D3. Every operation is stated once, as one function per operator of the language over the values
it takes, and that one statement is used three ways.** Each settles what the operator does with an x
or a z and answers as the language says the operator answers. The C++ value type of a packed type
applies them to its own words. Applied to values whose width is given at run time they are the
prebuilt entries the execution backend calls where it does not state the operation itself (D5). And
they are the compiler's own constant folding. A folded operation, an inlined one and a called one
therefore cannot disagree.

An operation takes a value in one of two ways. A value read is the words of its planes where they
lie, how many bits it is, and the signedness its bits are read as. A value written is the words of
its planes where they lie and how many bits it is. An operation is handed nothing a value already
says, so no width, signedness or plane travels beside one, and a width that is not the value's own
is stated by making the value at that width. Whatever holds a value answers the same two questions
the same way, the value read and the value written: the C++ value type, the words a holder keeps for
a value wider than a word, and bytes with a shape in code compiled without the type. A write that
reaches part of a value wider than a word therefore touches the words that part lies in and no
others. One thing in several shapes fails the requirement that an operation is stated once. Each
further way of handing a value over is a further spelling of every operator's plumbing, and a holder
answering differently puts a branch in every adapter over two holders.

A plane is held in storage units: a value wider than 64 bits in 64-bit words, and a narrower one in
one integer per plane of 1, 2, 4 or 8 bytes (D1). Which integer a plane's unit is, and how many of
them a plane has, are part of a view's type where the code holding the value knows the value's type,
so a view always refers to the value where it lies and nothing is copied in before an operation or
written back after one. An operation works in 64 bits whatever the unit is, widening each unit as it
reads it and narrowing as it writes. That is what reduces an operation on a value of one unit to the
instructions its width leaves. The operation is compiled for the unit and the count a type fixes,
its loops over units are a single step there, and an optimizing host compiler folds the short ones
into their callers on its own account. No function is marked to force it in. Marking was the
alternative, and it fails two things. It decides in the library what the build that compiles the
caller is there to decide, and it needs an attribute each host compiler spells its own way, which is
a macro over every operation.

Code compiled once for every type works over 64-bit words alone, because it cannot be one body and
also follow the integer a plane is held in. So a value held in a narrower unit reaches it as words
made of its bytes, and that is done in one place for the whole library: the words of a value laid
out at some bytes, read or written, which are the bytes themselves where the value is wider than a
word. Fixing the unit at 64 bits everywhere was the alternative, and it fails the requirement that a
value is handed to an operation the same way at every width. A value held in narrower integers then
needs a second form for the length of each operation, copied in before and written back after, with
a lifetime and a type of its own that a wider value's view does not have. Storing a narrow value as
a whole word, as LLVM's `APInt` does, removes the question and fails D1's sizes.

A view is handed to an operation by value, the written one as the read ones. A view handed by
reference leaves a host compiler unable to tell that a write through it does not change the view, so
it reloads the view after every word written and the operation outgrows what it will fold into a
caller. Measured on Ibex with the written view by reference, a select at a position the program
computes went from 40 instructions in place to a call on 116, and the C++ backend ran 6% more
instructions.

Measured on x86-64 over 28 one-word operations of the C++ value type -- the arithmetic, bitwise,
shift, comparison, reduction and logical operators at a two-state and a four-state 32 bits, a part
read and a part write, a concatenation, a conversion and the readings out as a machine value -- and
counted as instructions in the compiled functions, each with its return, they come to 227 under
clang 20 at `-O2` and 297 under gcc 13 at `-O2`, with no call in any of them. The same file compiled
against the word functions marked to be inlined always was 270 and 323. Under clang `int + int` is
one instruction and the return, `int * int` a multiply behind one move, a four-state 32-bit add
seven instructions, `==` fifteen and `&` seventeen. An unoptimized build folds nothing, so there the
short functions are called and a compilation unit holds one copy of each it uses per storage unit,
count and state domain. Measured on two benchmark designs built unoptimized on the C++ backend,
against the storage unit fixed at 64 bits: the program's code is 5% larger and the build takes as
long, within what two runs of one build differ by. Measured on Ibex built optimized, whole run under
callgrind, the C++ backend runs 2.47 G instructions where it ran 2.53 G with the unit fixed at 64
bits, and the execution backend 3.18 G where it ran 3.17 G; that backend states its own arithmetic
up to 64 bits and reaches the library only through entries. An operation whose work grows with the
value -- a product or a quotient of many words, a power, a run of digits -- takes its values with
the count open, so it is one body for every type and nothing reduces it per width.

**D4. On the execution backend the four-state formula lives in the code generator, as a fixed
function of the operand type.** The layer below MIR keeps the integral type MIR states and names the
operator; the code generator translates each operator on it to arithmetic on the value's planes,
each held as an integer of the value's own width, or to a call (D5). An operator's semantics come
from its operand type: rustc's MIR states `BinOp::Div` once and `codegen_scalar_binop` picks `sdiv`
or `udiv` from the type. The translation decides nothing beyond the type, so it stays mechanical,
and no intermediate level of exact-width integers and pairs exists for one consumer to read. The
formulas are thus written twice in total -- the functions of D3 and the code generator -- which is
how both backends already realize every primitive operator. What holds the second statement to the
first is a test that evaluates each generated form and the function of D3 on the same operands, at
widths on both sides of a machine word, and compares both planes of their answers. The conformance
corpus, which runs every case on both backends, stands beside it.

**D5. The execution backend states an operation itself wherever every value it takes and answers is
within the size a frame keeps (D1) and its work is linear in the width, and calls otherwise.** A
value's planes are bytes an integer of its width is loaded from, so each formula of D4 is written
once for every width, on two such integers, and LLVM's type legalization turns it into operations on
the words that width has. A product, a quotient and a remainder are stated that way up to 64 bits
and are calls past it, because their work grows with the square of the width. A power, a count of
bits, a logarithm, and whatever reads text or a foreign buffer are calls at every width. An
operation over a value past the size a frame keeps (D1) is a call.

The line is drawn by two requirements together. An operation costs the bits it touches: measured on
a concatenation of 32 bytes into 256 bits, optimized, a call into the library for each step ran
411,000 times a second and the operation stated in place 22,000,000, against 20,000,000 on the C++
backend. And what a unit's code holds for one operation is bounded: a formula stated in place is
straight-line code that follows the width, about a byte a bit a plane unoptimized, so it is stated
only up to a fixed size. That size is the size a frame keeps, so one figure bounds both things a
declared width could otherwise be followed by, a frame and the code of one site; Verilator uses one
figure for the same two questions, whether a wide operation is expanded per word and whether a value
may be a function's local. Measured with clang 20 on one plane at 8,192 bits, unoptimized: an and,
an add and an equality are each about 8.5 KB and compile in under 40 ms, where a product is 717 KB
and takes 7.4 s and a quotient is 145 KB. A dozen operations on a four-state value of 4,096 bits add
about 130 KB to an unoptimized program and a quarter of a second to its build.

This entry first drew the line at 64 bits, on the ground that legalization's code follows the width
at every site. The premise holds and is the bound above; the line it drew left every operation
between 65 bits and the size a frame keeps paying a call of some hundreds of instructions for work
of a few words.

**D6. What holds a packed value reads only its layout.** A cell, a net, a sampled history, a
reference and a shared cell copy, compare and store a value's bytes and never interpret them. Up to
64 bits what a holder needs of a type is one of eight layouts -- one, two, four or eight bytes, with
or without an unknown plane -- so the runtime library compiles each holder family once per layout,
and the execution backend lays out values, cells and frame slots of such a type from the type. No
holder of them is told a type.

A value wider than a word is held as the words of its planes at a width the holder is told once. A
variable's cell, a net and a procedural local are told it where they are built, and hold the words
in their own storage, which the code generator lays out at the size the type gives, as clang lays
out a member of a class it compiles; so the value has one address for as long as its holder lives,
and a reference bound to it before the declaration's value arrives names that address already. A
history is told it by the default it is filled with. Every value the holder takes afterwards is the
bytes of a value that wide, and a write is a change exactly where the words differ. A whole write
compares those bytes with the words the holder has and copies them there; a level of a procedural
continuous assignment holds no words until the first value it is driven with, so that one value is
built for it. A reference, and a place designated within a write, hold nothing and name bytes lying
in something else, so neither can be told at an install; an access through one is told the width, as
the size of what a thin pointer names is the accessing code's and never the pointer's.

**D7. Code compiled before a design exists is told three things about the design's types and no
others: a number of bytes, a number of bits, and the address of a function the design's own code
contains.** It is never handed anything it must interpret to learn what a value is. This is the
survey's common ground, and it follows from what such code can do at all. Without knowing a type it
can allocate, free, move and copy a number of bytes, which is everything a holder does. Without
knowing a type it can do integer arithmetic over a number of bits, with or without a second plane,
which is every integral operation and every net's resolution. Anything else means something only for
the type, and is reached by address.

For a packed value the third kind is never needed: its words are all it is, so copying it is copying
their bytes wherever they lie, and every operation on it is arithmetic over its bits. Prebuilt code
that reads a packed value's meaning -- formatting, conversion to and from text, power, file and
memory-image parsing, a delay's amount, the wide operations -- therefore takes its bytes and the
numbers it reads of them.

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
layout, because each reads or writes a value where it lies. A net of an integral type counts its
positions by its declared type: a type that states its width gives it, and a layout serving several
widths is built told the count, as a holder of words is told its width. Its one install on both
backends is told only the scalar its net type contributes where nothing drives it; a net of an
unpacked aggregate is installed with a value of its type instead, which is an entry of its own.

**D11. The widest packed type is the front end's, 2^24 - 1 bits.** The standard's floor is 65,536
(LRM 6.9.1, 7.4.1), and a design is refused only where the front end refuses it
(`architecture/language_conformance.md`). A limit of Lyra's own below the front end's would refuse a
design the front end accepts. The generous limit is affordable because no cost follows it. Under D1
what a call takes from the stack does not follow a width, under D2 a unit's data does not, and under
D3 an operation costs the bits it touches.

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
- **Leaving wide integers to LLVM's legalization at every width and for every operation.** Fails the
  bound on what a unit's code holds for one operation; see D5.
- **A call for every operation past 64 bits.** Fails the requirement that an operation costs the
  bits it touches; see D5.
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
  in correctness, and is unnecessary: a packed value is its words and nothing else, so nothing about
  it crosses as a function.
- **Every value held in place at any width**, as clang holds a `_BitInt` and Verilator a wide
  temporary. Fails the requirement that a program the compiler accepts runs at every width it
  accepts. A frame is then the sum of what a body's expressions hold, each as large as its type, on
  a stack the design did not size.
- **Giving the program a larger stack, or predicting what a design needs of one**, as Verilator's
  generated model does. The need is a sum over the whole design, a recursive subroutine makes it
  unbounded, and memory then follows the temporaries a body writes instead of the values alive at
  once.
- **A width limit of Lyra's own below the front end's.** Fails D11's requirement, and buys nothing
  once no cost follows the limit.
- **Folding every operation over constants whatever the answer's size.** Fails the requirement that
  an artifact follows what a unit states. A fill of a type of millions of bits became megabytes of
  data and of emitted text for a value the source wrote in two characters.
- **A constant stated in MIR as its low words and the scalar above them.** It makes the same data
  short, by giving a generic language's constant a shape made of words and planes. The extension is
  an operation MIR already has, and stating it leaves the constant, both backends and the folder as
  they are.
- **Marking the word functions to be inlined always.** Fails as D3 says, and the word and the count
  carried in the type make it unnecessary.

## Consequences

- The runtime value class that carried its own width, signedness and state domain, the record of an
  integral type made while the program runs, the type and range descriptions for packed types, and
  the runtime entries that performed a packed operation up to a word are gone on both backends.
- Measured on Ibex, whole run under callgrind, `--release`, both programs ending at `$finish` at
  26548: the execution backend 7.16 G instructions, from 13.13 G before this entry, against
  Verilator's 0.122 G. The C++ backend 3.03 G from 10.87 G, measured on the merged tree, so what
  else merged in between is in that figure.
- The C++ backend's compile time under one instantiation per distinct type, which
  [a-value-is-its-machine-data](a-value-is-its-machine-data.md) says is measured on the way, has not
  been measured; this entry was accepted without it.
- A design declaring a vector of 2^24 - 1 bits, adding to it, shifting it, writing a part of it and
  passing it through a function builds and runs on both backends, started directly under an 8 MiB
  stack. The merge gate's growth check builds one design at 4,194,304 bits and at twice that.
  Measured on that design: the execution backend builds either width in under two seconds and the
  C++ backend in about six, and the two programs differ in size by under a hundred bytes.
- Writing one 8-bit element of a packed array costs the element wherever the array lies. Measured as
  20,000 such writes at 1,024, 16,384 and 262,144 elements, `--release`, process start included,
  writes into a variable take 3, 3 and 4 ms on the execution backend and 2, 3 and 3 ms on the C++
  backend, and writes into a subroutine's own local 2, 2 and 3 ms on the execution backend and 2, 2
  and 2 ms on the C++ backend. While the execution backend rebuilt a local to change part of it, the
  same writes into a local took 7, 42 and 638 ms there.
- A fill or an extension of a type too large for a frame is carried out each time it is evaluated, a
  pass over the value, where naming a constant was a copy of the same length.
- The size past which a value is kept out of a frame, 1024 bytes, is the figure Verilator uses for
  whether a value may be a function's local (`--localize-max-size`). No measurement moves it:
  counted under clang at `-O2`, while a value past it owned its words, a temporary that owned them
  cost 60 to 70 instructions more than one lying in place at every width, so the bound on a frame is
  what decides the size. Neither it nor the number of word runs kept has been measured against the
  benchmark corpus; this entry was accepted without that.
- The earlier entries that describe a packed value as one runtime type carrying its shape --
  [integral-representation](integral-representation.md),
  [runtime-shape-and-default-value](runtime-shape-and-default-value.md),
  [packed-shape-belongs-to-the-type](packed-shape-belongs-to-the-type.md),
  [enum-representation](enum-representation.md), [value-type-concepts](value-type-concepts.md) --
  keep their arguments and describe a shape this one replaces: an enumeration's value is still its
  base integral and the operator surface is still a lattice of concepts, over a value type per
  packed type instead of one for all.
