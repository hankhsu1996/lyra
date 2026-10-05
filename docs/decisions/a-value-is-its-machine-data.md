# A value is its machine data

Date: 2026-10-04 Status: accepted. Supersedes decisions 1 and 4 of
[integral-representation](integral-representation.md), decision 1 of
[runtime-shape-and-default-value](runtime-shape-and-default-value.md), the self-describing-value
rule of [packed-shape-belongs-to-the-type](packed-shape-belongs-to-the-type.md), invariant 5 of
[jit-value-realization](jit-value-realization.md), and the one-runtime-type-per-domain rule of
[a-value-lives-in-its-makers-frame](a-value-lives-in-its-makers-frame.md).

## Context

Every packed integral is one runtime type that carries its width, signedness and state domain as
data, and every operation on it is a call into the prebuilt library that reads them back. On Ibex,
C++ backend `--release`, a 32-bit add costs about 190 instructions, `&` about 415, `++` about 340,
and a run makes about 10,000 such calls per simulated cycle. The execution backend realizes every
value as a pointer to opaque runtime storage and every operation as a runtime entry, and its
containers hold each element as an erased `RuntimeValue`.

Nothing in the language asks for that. The width, signedness and state domain of every expression
are fixed by its type (LRM 11.6, 11.8), the front end hands each operator operands of exactly its
own type, and a unit is shared across instances only where no width depends on the parameter that
differs -- so within one compiled unit every expression has one type for all its instances. The
choice was made for the C++ backend's text to have one shape and no bridges between shapes, with
speed explicitly left unmeasured.

Every fast compiled simulator fixes width where it compiles: Verilator's `CData` to `QData` and
`VlWide<N>` (its `verilated.h`), CXXRTL's `value<Bits>` with always-inlined operations (its runtime
`cxxrtl.h`), Arcilator's LLVM `iN`, ESSENT's `UInt<w>`. NVC's Verilog support shows the four-state
cost at a fixed width: `vec2` and `vec4` types with compile-time width and sign, two words inline up
to 64 bits, a four-state add about four or five instructions over a two-state one, a runtime call
only above 64 bits.

## The test each choice answers to

A compiler meets designs larger than any it was measured on, so a choice is justified by what each
cost scales with, for any input; measured designs only check the arithmetic.

1. An operation costs the bits it touches.
2. A change propagates at the cost of the part that changed.
3. Compiled artifacts follow what one unit states, never the whole design or its instances.
4. Memory follows the declared bits, by at most a constant factor.
5. Every decision reads one unit and the published declarations it uses.

## Decision

**D1. A value is held as the machine holds data of its size, and its type is the compiler's to
know.** A two-state packed value is its value bits in words; a four-state one is the same words
followed by an equal run of unknown words, the encoding DPI's `aval`/`bval` already uses. Up to 64
bits that is one word or two; above, a number of words fixed by the type. No value carries its
width, signedness or state domain.

**D2. An operation is the machine's work on those words, generated where the operation is
compiled.** Up to 64 bits it is inline instructions; above, a loop over a word count the type fixes,
or a call taking the words and that count for the long algorithms (multiply, divide). Signedness is
part of the type, which decides the operation, as the front end has already made both operands of
every operator one type.

**D3. A concrete type is identified by its exact width, signedness and state domain**, and a packed
collection's operations work on the types of its elements and members. A multidimensional packed
array's element and a packed struct's member are ordinary values of their own type; what is wide is
the storage, not the arithmetic. Counted over nine open designs, taking packed collections apart
leaves 32 to 140 distinct shapes per design however large it is -- XiangShan grows from 121 to 123
between its small and its default configuration -- against 87 to 598 counted whole, which is the
bound principle 3 asks for.

**D4. A packed value is laid out as the standard describes it: one contiguous run of bits.** An
element or member at a run-time index is reached by bit addressing -- a word index and an offset
computed from the index, one or two words read, shifted and masked -- which costs the element, not
the collection. LRM 7.4.1 makes a packed array "a contiguous set of bits" and 7.2.1 a packed
structure "packed together in memory without gaps"; no clause lets a user observe a layout, and
every way out of the language (DPI, VPI, `$readmem`) copies through a fixed format. Laying each
element in a slot of its own was considered and rejected below.

**D5. Nothing reads a description of a type while the program runs.** Every type is known where it
is used, so what is specific to a type is code the compiler generates there: `$display` calls the
formatting of the argument's type, a DPI call converts at the call site, a wait compares through a
function generated for that wait. Where code written once in the prebuilt runtime must act on a
value of a type it was compiled without -- a container's algorithm, the scheduler's question of
whether a change is an event -- it is handed the functions the compiler generated for that type,
never a description to interpret.

**D6. On the execution backend, a container whose size is a run-time quantity -- a queue, a dynamic
array, an associative array -- holds its elements as raw storage and acts on them through the
functions the compiler generated for the element type.** That is the one difference the prebuilt
runtime causes, and it is the shape `TupleOperations` already has. An element at an index is
addressed by the compiler (base plus index times element size) without a call. A fixed-size unpacked
array is no container: its size is a constant, so the compiler lays it out in place.

**D7. A write reports the range it reached, and a wait decided at the write tests that range against
the bits it watches in place**, on the words, without materializing either side. A wait's watched
part stays the contiguous bit range it is today, because under D4 a bit's position is its place in
storage. Which waits are decided at the write is unchanged
([the-waiting-process-evaluates-its-wait](the-waiting-process-evaluates-its-wait.md)).

## Rejected

- **A runtime type per value, with fast paths inside it.** The structure this replaces. Three rounds
  made the object cheaper inside the same boundary -- the shape out of the value, a 48-byte object
  with its planes in one run, positions settled at compile time -- and the operations still cost
  hundreds of instructions, because what remains is the boundary itself.
- **Identifying a type by its storage size, the exact width an argument of each operation**
  (Verilator's helpers). It keeps the count of types down when that count is the whole design's
  distinct widths; under D3 the count is bounded already, and the exact type keeps the emitted text
  readable and makes a missing conversion a compile error instead of a wrong value.
- **A slot per element, each padded to 8, 16, 32 or 64 bits or a whole number of words.** Element
  access is one load instead of a few instructions, and both are constant. It breaks principle 4: a
  two-bit element grows fourfold and a packed struct of one-bit fields eightfold. It also gives one
  storage two layouts where the standard makes them one: a `ref` between equivalent packed types of
  different shape (LRM 13.5.2, "both the caller and the subroutine share the same representation"),
  a packed union read through another member (7.3.1), and a net alias (10.11).
- **The execution backend generating each container's algorithms per element type**, as clang
  instantiates the C++ backend's templates. Correct and the closest to clang, at the price of a
  second implementation of every container algorithm, for containers that live in testbenches rather
  than on the paths a design spends its time in.
- **A description of a type the runtime interprets** (a self-describing value, or a type record the
  runtime walks). The type is known where every operation is compiled, so interpreting it later is
  work the compiler had already done.

## Consequences

- `PackedArray` and the erased `RuntimeValue` go, with the runtime entries that perform an operation
  the compiler now generates; runtime scalars that were packed values (a file descriptor, a delay, a
  seed) are machine integers.
- The execution backend lays out values, cells and frames from the type, as the cross-unit record
  layout already does, so a cell's layout is a pure function of its type on both sides of a unit
  boundary.
- Constants are compile-time constants on both backends.
- The C++ backend compiles one instantiation per distinct type a unit uses. Under D3 that number is
  bounded per unit; its compile cost is measured on the way, and edits pay it only for the units
  whose emitted text changed.
