# A fixed-size array is its elements, laid out by its type

Date: 2026-10-11 Status: accepted. Carries out the last sentence of D6 of
[a-value-is-its-machine-data](a-value-is-its-machine-data.md) on the execution backend, and narrows
that record's D5 for a fixed-size unpacked array: the type the prebuilt library is handed for one is
constant data over its element's type. Supersedes
[jit-aggregate-realization](jit-aggregate-realization.md) for a fixed-size unpacked array, the
element default a construction carried under
[runtime-shape-and-default-value](runtime-shape-and-default-value.md) decision 6, and the count and
the element default among a fixed-size array's construction arguments in
[value-construction-forms](value-construction-forms.md) D1. Two alternatives
[a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) rejects for a tuple are taken
here for an array; Rejected quotes both and says what differs, and the first is taken back by D4's
revision, under which an array's whole-value operations are code compiled for its type
([a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)).
Who states an element's default, and that no array of any kind is handed one, is
[an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md).
D6 is superseded by [a-holder-lays-out-the-value-it-keeps](a-holder-lays-out-the-value-it-keeps.md):
a variable, a net and a local lay an array out after themselves from the type they are told where
they are built. The type constant's form -- a record both sides read, not an object of a library
class -- is
[a-type-is-told-to-the-library-as-a-record-both-sides-read](a-type-is-told-to-the-library-as-a-record-both-sides-read.md).

## Context

A fixed-size unpacked array (LRM 7.4.2) has a count its type fixes and elements of one type. On the
C++ backend it is `UnpackedArray<T, N>`: the elements in place at every size, every whole-array
algorithm instantiated by the host compiler for the element type and the count
([unpacked-array-representation](unpacked-array-representation.md)).

The execution backend held the same array as an object of the prebuilt library's: a run of elements
on the heap, each held beside a witness of its type, with the element type and the element's default
kept inside the object. Reading one element was a call per dimension. Every construction was handed
a count and an element default, which the object stored, and one type object answered for every
array of a design because the array described itself.

What the execution backend does with a value after MIR is what clang makes of the C++ backend's
rendering of it, and the one allowed difference is what its prebuilt runtime causes: a variable's
cell, a net, a driver, a sampled history and a container's algorithm are compiled once, before any
design exists, and have to copy, end, compare and stream an array of a type they were compiled
without.

## The test each choice answers to

The five of [a-value-is-its-machine-data](a-value-is-its-machine-data.md), of which three select
here, and one of the north star's:

1. An operation costs the bits it touches: reading one element does not enter the library.
2. Memory follows the declared bits by at most a constant factor, for every element type and every
   nesting.
3. Compiled artifacts follow what one unit states.
4. End-to-end iteration time is the primary target, so what is generated per type is weighed against
   what it buys while the program runs.

Two more are the language's. A reference to an element goes on naming it across every assignment to
the array (LRM 13.5.2 lists the operations that outdate a reference, and all of them are on
variable-size arrays). A read through an index naming no element answers the element type's value of
LRM Table 7-1, and a write through one does nothing (LRM 7.4.5).

## What the field does

- clang decides an array's layout where it converts the type: the memory type of a constant array is
  an LLVM array of the element's memory type and the count (`clang/lib/CodeGen/CodeGenTypes.cpp`,
  `case Type::ConstantArray`). An element is address arithmetic (`CGExpr.cpp`,
  `EmitArraySubscriptExpr`). Ending an array is a loop over the elements calling the element type's
  destroyer, emitted where the array is ended (`CGDecl.cpp`, `emitArrayDestroy`), and beginning one
  is the same loop over the constructor (`CGClass.cpp`, `EmitCXXAggrConstructorCall`). No object
  stands for the array.
- rustc computes an array's layout in the layout query from the element's layout and the count
  (`compiler/rustc_ty_utils/src/layout.rs`, `ty::Array`), and drop glue for one is a generated loop
  over the elements (`compiler/rustc_mir_transform/src/elaborate_drop.rs`, `open_drop_for_array`).
- A class's implicit default constructor is declared on demand and defined only where something uses
  it (`clang/lib/Sema/SemaDeclCXX.cpp`, `DeclareImplicitDefaultConstructor` and
  `DefineImplicitDefaultConstructor`): how a type begins at its default is a function of the type's,
  stated once, and not an argument of whatever builds a value.

Both compilers answer layout in the layout step, from the type, and generate the whole-array
lifecycle as loops over the element's. The condition that differs here is the one above: the loops
that run in a cell or a net were compiled before the type existed.

## Decision

**D1. An array is its elements one after another, and nothing else.** Neighbouring elements lie the
element's size apart, rounded up to its alignment. The array holds no count, no element type, no
default and no header. Its layout is one rule over the element's size, the element's alignment and
the count, stated once in the library and read by the code generator, so the two cannot disagree.

**D2. An array is its elements one after another where it lies, at every size.** The rule is the C++
type's, so the two backends lay one array out alike; a frame's size never follows a declared size
because an array too large for a frame is held through a box MIR states
([a-packed-value-is-its-words](a-packed-value-is-its-words.md) holds why a frame may not). An
assignment writes each element where it lies, which is what keeps an element's address. **Amended
2026-10-13:** an array past 1,024 bytes was one address of a run it owned; the box now states that
once, in MIR.

**D3. An element is reached by address arithmetic in generated code.** The position is an ordinal
and whether it names an element, so a read or a write that names one is a bounds test, a multiply
and an add. A position naming no element is the one path that enters the library: a read is answered
with the element type's default and a write is handed storage no read reaches (LRM 7.4.5), each
asked of the element type. LIR states the element step and the type as element and count; the
stride, the bound and where the elements lie are decided below it (`../architecture/lir.md`).

**D4. The prebuilt library is handed the array's type as constant data** (revised 2026-10-15). The
code generator writes one constant per array type per unit: the count and the element's type, which
is itself a constant stating how an element starts (D7), which is what reaching an element, a run of
them and their order by position need. The constant's table holds the array's whole-value operations
-- copy, move, end, assign, the equalities, the bit stream, the net folds -- as code compiled for
the array type
([a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)).
This revises the earlier rule that the library carried each of them out over the count and the
element's type, which had the library interpret the type. Every unit naming an array type states its
constant under a name the count and the element settle, and the linker keeps one of them, as it
keeps one of a template's static data member however many translation units instantiate it; one
array type is therefore one object in the program, and the library tells two types apart by which
object each is (revised 2026-10-16: two units' constants were two objects, compared by count and
element).

**D5. An array crossing to the library is its bytes, and its type crosses once** (revised
2026-10-15). An entry realized for arrays of one type takes that type once, after its operands, as a
generic function takes its type argument once however many arguments are of that type; every array
of that type it takes, and the one it builds, are its bytes alone. An array of another type -- the
run of elements a slice write puts -- is read with its own type, which its declaration says. This
revises the earlier rule that each array crossed with its type, which told an operation over two
arrays of one type the type twice and had the library check or drop the second copy. An entry of
another kind that holds an array as an element -- a queue taking one, a container's algorithm
visiting one -- takes the bytes alone, since it was built knowing its element's type. Where an entry
of another kind reads an array as an array, its declaration says so of that operand. A reference to
an array and a part designated within a write name bytes lying inside something else, and an access
through one is told the array's type the same way.

**D6. Whatever holds an array for the library holds its bytes with its type.** A variable, a net, a
driver and a sampled history keep the array in storage of their own beside the type they were first
handed one at, and are told nothing more afterwards.

**D7. The array type's constant names its element type's constant, and states no default of its
own.** How an element starts, and the two answers of LRM 7.4.5 built from it, are the element
type's, for a fixed-size array as for every other kind; the element type's constant carries them
([an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md)
D2 and D4).

**D8. A fixed-size array's construction carries no count and no default.** It carries a unit of
elements, laid down again and again until the array is full, so an element list and a replicated
pattern are one form. A count the type fixes is the same at every evaluation, so it is never an
operand. What an array method answering an array is handed of that answer is settled for every array
kind in
[an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md).

**D9. Generated code copies and ends an array itself.** An array lying in place whose elements end
with nothing to do is copied as its bytes and ended by nothing. Any other array is copied, moved,
assigned and ended by a function generated for its type, each element in turn.

## Rejected

One of these is a rejection another record makes, reversed here for an array.

- **"A description of the type that the runtime walks ... It is a second realization of every
  operation, interpreted at run time, where clang compiles each operation for its type; the
  requirement is clang's"** ([a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md)),
  and **"Nothing reads a description of a type while the program runs"**
  ([a-value-is-its-machine-data](a-value-is-its-machine-data.md) D5). Both stand for an array too.
  This record first held that an array was the case their reason did not reach, since its elements
  are one type and a whole-array operation is the element's operation and a count; that left the
  library carrying out every operation by reading the type, and the execution backend doing other
  work than the C++ backend's instantiated template. D4's revision takes it back. What the library
  is still told -- a count and the element's table, for reaching an element, building an array from
  elements and merging two -- is what [a-packed-value-is-its-words](a-packed-value-is-its-words.md)
  admits: numbers and the addresses of functions the design contains.
- **"Passing a tuple's table beside its bytes instead of inside them. Entries over a container's
  elements take a value of any domain, so their signatures cannot grow a table argument for one
  domain, and the families that keep a tuple would each need to be told its type again"**
  ([a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md)). Inside the bytes is a
  word per array, and an array is an element of arrays: `byte m[1000][2]` would be eight thousand
  bytes of type addresses around two thousand of data, the inner arrays would stop being what the
  C++ backend lays out, and test 2 fails for the nesting that is most common. Each of the two
  objections has an answer that record did not have. An entry over a container's elements takes the
  bytes alone and its signature does not grow, because a container is built knowing its element's
  type (D5); only an entry whose subject is an array takes the type, and that is every entry of that
  kind alike. And a holder is not told again: it keeps the type from the first array it is handed
  (D6). What is told at each access is a reference and a designated part, which is what a wide
  integral's width already is.
- **An element default handed to every construction**, which
  [runtime-shape-and-default-value](runtime-shape-and-default-value.md) decision 6 chose. Rejected
  for every array kind, with that record's own reasons quoted, in
  [an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md).
- **The library building an element's default from the type it is handed.** It cannot: a structure's
  default holds its members' declared initializers and a union's is its first member's (LRM Table
  7-1), and only code compiled for the design knows either.
- **Every array in place, at any size.** A frame holding one would follow a declared size, and a
  design declares memories of megabytes.
- **Every array behind an address.** Element access gains a load and every small array an
  allocation, where the C++ backend pays neither.
- **Two units' constants for one array type kept as two objects, compared by count and element**
  (taken back 2026-10-16). It was chosen over one constant for the program on the ground that one
  "needs a unit to know what other units state". It does not: each unit states the constant from its
  own declarations alone, and the linker keeps one where the names agree. Kept as two, the library
  compared every pair by what they state, which is a description of a type read at run time.

## Consequences

- An element read is inline arithmetic on both backends. A write into an element of a variable is
  still a call for each level of the select, because that is where the variable is told which part a
  write reached
  ([a-part-of-a-variable-is-named-by-its-steps](a-part-of-a-variable-is-named-by-its-steps.md)).
- A whole-array operation is a loop compiled for the array type, calling the element's operation
  directly at each position.
- An array inside an array, a structure or a frame slot costs its elements and nothing more. A
  variable, a net and a history holding one hold one allocation for it.
- The library's own array object, its type object, and the entry that read one element go. A cell of
  an array is installed with the array's type where a cell of a wide integral is installed with its
  width.
- No speed or memory figure for the new layout is recorded here; none was taken.
- A tuple's bytes opened with its type's table when this was decided. Nothing here needed them to,
  and nothing here changed when they stopped.

## Cross-references

- [a-value-is-its-machine-data](a-value-is-its-machine-data.md) -- D6 asks for the layout; D5 is
  what D4 narrows.
- [a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md) -- the product's answer, and
  the rejection reversed here for an array.
- [a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md)
  -- the bodies an array type's table holds.
- [a-packed-value-is-its-words](a-packed-value-is-its-words.md) -- what code compiled once may be
  told of a type, and why a frame's size does not follow a declared size.
- [unpacked-array-representation](unpacked-array-representation.md) -- the C++ backend's array, and
  the type-level shape both backends lay out.
- [array-element-storage](array-element-storage.md) -- an index is a persistent slot a reference
  binds.
- [value-construction-forms](value-construction-forms.md) -- a container built from an element list
  is a call; D8 says what a fixed-size array's call takes.
- [an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md)
  -- an element's default is its type's for every array kind, and no call carries one.
- `../architecture/lir.md` -- physical layout is derived below LIR.
- `../architecture/backend_contract.md` -- a count a type fixes is never an operand.
