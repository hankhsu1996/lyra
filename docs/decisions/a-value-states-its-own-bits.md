# A value states its own bits, as it already states how many

Date: 2026-09-11 Status: accepted. D2 is superseded 2026-10-11 by D6 of
[an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md):
the entry that builds is told the type it answers and handed the stream alone, because the premise
of D2's second paragraph -- a shape the runtime holds as a value -- was withdrawn when a value's
shape became its type's. D4 and D6 are revised 2026-10-22: the operator has no type in HIR, and a
stream whose width is not fixed is no longer refused but is a run of bits whose length is its own.

## Why this decision matters

IEEE 1800 defines, for every bit-stream type, one sequence of bits a value of it makes: the first
item most significant, an array's elements in `foreach` order, a structure's members in declaration
order, a class's base members before its own (LRM 6.24.3, 11.4.14.1). Three source constructs are
written over that sequence and nothing else -- a bit-stream cast (LRM 6.24.3) and a streaming
operator in either direction (LRM 11.4.14) -- so what has to be decided is not what they mean but
**which component produces the sequence, and at which stage**.

Both constructs refused, one at AST-to-HIR and one at HIR-to-MIR, and the two refusals read as
neighbours. They are not: 11.4.14's opening sentence defines the streaming operators as packing of
the bit-stream types 6.24.3 describes, so they are one operation with two spellings.

## What established systems do about it

- **slang** splits it by stage. The legality question and the size formula are answered from the
  _type_ while the expression is bound: `Bitstream::dynamicSizesMatch` models a type's width as
  `aR + b` with `a` and `b` compile-time constants and `R` runtime-determined, and `canBeSource` /
  `canBeTarget` refuse a program from that alone. The bit movement is answered from the _value_,
  during constant evaluation: `packBitstream` walks a `ConstantValue` and flattens it,
  `unpackBitstream` walks the destination _type_ with a cursor over the packed leaves, and `reOrder`
  is a third step over the flattened stream.
- **Verilator** keeps the operator as an AST node (`AstStreamL` / `AstStreamR`) into its middle end
  and rewrites an assignment carrying one into ordinary selects and concatenations where the widths
  are static (`V3Const.cpp`), with `V3Number::opStreamL` performing the block reversal on constants.
  That covers the sub-language in which every width is static, which is the one it accepts.
- **C++** faces the same "read a value as its bits and read it back" question in `std::bit_cast`,
  and the language requires both types to be the same size and trivially copyable -- so the
  operation has a pure layout answer and codegen emits a copy. (Recalled rather than read against
  the standard text here; the size and triviality requirements are the load-bearing half.)

**Where our conditions differ, and it is the sentence the design turns on.** C++ can answer from
layout because a type's size is a property of the type. A SystemVerilog bit-stream type's width is
not: LRM 6.24.3 admits dynamic arrays, queues, associative arrays and strings, recursively, and
their bit counts exist only while the program runs. So neither a copy over a layout nor Verilator's
static rewrite reaches the language, and what is left is slang's division -- the type answers what
is statically knowable, the value answers about itself.

## The decision

### D1. The bits a value makes are the value's own answer, beside the count

Two runtime entries sit beside the one that already answers `$bits` over a value whose width is not
fixed. One reads a value out as the sequence LRM 6.24.3 fixes; one builds a value from such a
sequence. Both recurse the way the width query does -- a part reports its own bits, and no caller
inspects the part's shape.

The count and the content are one question asked two ways, and Lyra had already answered the count
this way. Answering the content anywhere else means a second traversal stating the same order, which
agrees with the first only by inspection: where they disagree, `$bits` and a pack report different
widths for one value, silently.

### D2. Building takes a prototype, because a sequence of bits carries no shape

The entry that builds is a static factory over the stream and a prototype value. The result is the
prototype's type, held at the prototype's representation, and the sequence is consumed from its most
significant end. This is the operand role the entry declaration already names for a value an entry
has no other way to shape, and the prototype is the destination's default value, which HIR-to-MIR
synthesizes for any type.

A type reference would not serve: the runtime holds a shape as a value, so an aggregate's element
widths and state domains are reachable only through an instance of it. _(Superseded 2026-10-11: a
value's shape is its type's, so the entry is told the type it answers and takes no prototype.)_

### D3. Re-ordering is a third entry over the bit vector, and `>>` calls nothing

LRM 11.4.14.2's re-ordering divides a vector into slice-sized blocks from its least significant bit
up and reverses the block order, leaving the bits inside each block alone and the short final block
unpadded. That is a generic bit operation -- LLVM's `llvm.bswap` is it at a block size of eight --
so it is an entry over the vector rather than a mode of the pack.

`>>` performs no re-ordering and ignores any slice size beside it, so it emits no call at all. Which
of the two a program asked for is settled where the source is read; nothing below reads the
operator.

### D4. HIR carries the operator; MIR carries the calls

HIR gains a streaming concatenation -- its operands and its block size, with zero standing for `>>`
because the front end has already folded a written slice type or constant to a bit count. Which
direction an occurrence is used in follows from where it stands, so it carries no discriminator. It
has no type of its own, as the front end gives it none: the stream between packing and unpacking is
never visible (LRM 11.4.14), and where the operator is a source or a cast's operand, the conversion
around it carries the type the stream becomes. What the stream is held as is a representation, which
HIR-to-MIR decides from the operator's parts.

MIR carries no streaming node. A source-level `{...}` is an operation over values that already
exist, which every one of MIR's peer languages reaches through a library call and none of them
spells as a node; the same holds one operator over. So the lowering states a pack as a call per
operand, joins them with the entry that already joins bit planes, and re-orders with D3's entry.

### D5. A bit-stream cast is those two steps and costs no vocabulary of its own

LRM 6.24.3 states the conversion in two steps -- source to a generic packed value of the same width,
then that value to the destination -- and the lowering is those two steps against D1's entries. A
cast reaching MIR would be the shape `mir.md` already forbids: a conversion that reshapes a value is
a library call, never a cast node.

### D6. A stream whose width is not fixed is a run of bits whose length is its own

A stream is a value. Where every part of it has a width its type fixes, its type is an integral one
of that width, and nothing about it changes. Where some part's width is the running program's -- a
dynamic array, a queue, an associative array, a string, or an aggregate holding one -- its type is a
run of bits whose length is the value's own, which the source never names, laid end to end from its
parts, re-ordered in blocks, cut at a position, and read back a fixed-width part at a time. Which of
the two a stream is follows from its parts' types -- integral exactly where every part's stream is
-- decided where the stream is built, so the fixed form costs nothing it did not before. This is
slang's division carried to run time -- the type answers what is statically knowable, the value
answers about itself -- in LLVM's shape for a value whose width is per value (`APInt`, `BitVector`)
and Rust's `bitvec` for the operations. The run is a type the library declares and MIR only names:
every one of those operations is a call, no MIR operation or pass looks inside a run, and a
different representation in the library would change nothing in MIR -- which is what makes a type
MIR's own or the library's, as `std::string` is the library's to a C++ compiler. It is still a
value, made, copied, handed on and ended as any value is; how a type is held and whether it is a
value are separate questions.

Reading a run back is where the two directions differ, and the type decides it alone: a part whose
width the program decides takes what the parts after it leave when they are empty, in whole
elements, so the first such part takes all that is left and any later one stays empty (LRM 6.24.3,
11.4.14.4). A cast takes exactly the bits its operand holds or is a run-time error (LRM 6.24.3); an
assignment is lenient, giving a dynamically sized destination the fewest elements as wide as the
stream and a fixed-size one the stream left-aligned, and erring only where the stream is too wide
for it (LRM 11.4.14).

## Rejected alternatives

- **Unroll the traversal at HIR-to-MIR into descent and join calls.** It needs no new entry, no
  runtime surface and no ABI symbol, and it is what Verilator does. Rejected on three counts, the
  first decisive: it is a second statement of the bit order, beside the one the width query already
  makes, and a disagreement between them is silent. It also cannot reach the dynamic case at all, so
  it guarantees a second mechanism later -- two shapes for one operation. And it needs special cases
  the uniform shape does not: a join carries two or more runs, so an aggregate with one part, and
  one with none, each need an arm.

- **One entry read two ways, with the direction as an operand.** The direction is not data the
  program computes; it is which operation the source wrote. An operand would leave every consumer
  branching on it, which is the decision-in-render shape the backend contract exists to remove.

- **Give the build entry a type reference instead of a prototype.** A packed value's shape does
  travel as a type reference today, but an aggregate's does not: nothing below MIR holds a
  description of an unpacked structure's members. One operand role for every family is what keeps
  the entry's declaration from splitting per family. _(Taken 2026-10-11: every family's type now
  reaches the library, a structure's through the functions the unit states for it, so a type
  reference is the one role for every family.)_

- **Let a dynamically sized stream take the statically known part as its type.** It types the
  expression with a width the value will not have: every consumer downstream reads a width that is a
  lie, and the first one to allocate against it is wrong.

- **Take the other side's fixed width, as Verilator does.** It converts against whichever side has a
  width its type fixes, so it cannot express a stream whose both sides are the program's -- a queue
  of strings into a string, which UVM writes -- and that is a condition of the language, not a gap
  to fill later.

- **Make every stream a run of bits, fixed ones included.** One shape, but a reversal of the bits of
  a fixed-width vector would then cost an allocation and a call each, where today it is the vector's
  own operation.

## Consequences

- The participation set for reading a value's bits is narrower than the set that counts them, and
  says so as a concept of its own, so a value family that gains a fixed-size stream gains both
  entries or fails to build.
- The execution backend needs no new mechanism: both entries dispatch on a value's domain the way
  every other value entry does, and the build entry is named by what it answers with, as every other
  factory is.
- Both backends carry every form; the one both refuse is a class handle in a stream, which their
  path records name.
- A streaming target list is distributed by the assignment that consumes it rather than by a place:
  a stream stands for a sequence of destinations but not for storage laid out like them, so what
  each target takes is a share of a sequence rather than a share of a value.

## Cross-references

- `../architecture/mir.md` -- a conversion that reshapes a value is a library call; MIR's vocabulary
  is the generic-language one and the source language does not shape it.
- [concatenation-realization](concatenation-realization.md) -- a join is stated as what it is over
  its operand family, never as a node of its own; the same reasoning one operator over.
- [cast-is-a-pair-of-types](cast-is-a-pair-of-types.md) -- what a cast node is for, and why a
  reshape is not one.
- [builtin-call-identity](builtin-call-identity.md) -- one flat entry per runtime function, with
  every property of an entry stated once beside its identity.
- [an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md)
  -- D6: a conversion out of a stream of bits is told its answer's type, and no prototype crosses.
- LRM 6.24.1 (cast operator), 6.24.3 (bit-stream casting), 11.4.14 (streaming operators), 11.4.14.1
  (concatenation of stream expressions), 11.4.14.2 (re-ordering), 11.4.14.3 (streaming concatenation
  as an assignment target), 11.4.14.4 (streaming dynamically sized data), 20.6.2 (`$bits`).
