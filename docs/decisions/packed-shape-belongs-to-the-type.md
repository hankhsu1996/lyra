# A packed value's shape belongs to its type, not to the value

Date: 2026-09-17 Status: accepted

Supersedes the packed clause of [selector-coordinate-resolution](selector-coordinate-resolution.md)
decision 1 and the packed carve-out of
[unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md). Decisions 2, 3 and 4 of the
first stand, and the whole of the second's unpacked treatment stands.

## Why this decision matters

An integral value was 120 bytes, of which 48 were a descriptor holding the declared dimension stack,
signedness, state domain and the width derived from the stack. Generated code constructs one of
these at every operation, because every result of every operator is a fresh value.

Measured on a fixture that is nothing but 32-bit arithmetic, self-cost: building a value from word
planes 9.7%, checking a view's range 9.2%, masking a value's top bits 7.2%, building the descriptor
7.1% + 4.5%, wrapping storage in a view 5.1% + 3.4% -- and the addition itself 4.4%. On a RISC-V
core running to its own `$finish`, making and bounds-checking a view over a bit vector was near a
quarter of all instructions, and it survives any fix to the write path because every bit access in
every design pays it.

That is what prompted the question, but it is not what decides it. What decides it is that a
declaration's shape is one fact per declaration and the value was carrying one copy per value.

## The tension this addresses

Two things were true at once and only one of them is load-bearing.

**A value must describe itself.** It is stored in containers whose element type is erased at the
runtime boundary, held in cells, and crossed through an ABI where generated code holds only a
handle. Nothing beside it can say how many bits it has. That is
[runtime-shape-and-default-value](runtime-shape-and-default-value.md) decision 1 and it stands.

**A value must be reached by a coordinate somebody wrote in the source.** `y[3]` on a
`bit [3:0][1:0]` names a pair of bits; the same position on a `bit [7:0]` names one. Something has
to say which division applies.

The first needs three scalars -- width, signedness, state domain. The second needs the declared
division, and it is the second that the dimension stack serves. Conflating them put a
per-declaration fact inside a per-value object.

## What established systems do about it

- **slang** (`slang/numeric/SVInt.h`, read 2026-09-17). `SVIntStorage` is 16 bytes: a union of one
  inline word and a pointer, a 4-byte `bitWidth`, and two flags. It implements the same language,
  and **there is no dimension stack in it** -- SystemVerilog's packed dimensions live in
  `IntegralType`, the type system, and the value carries only what an operation on the bits needs.
- **Verilator** (`verilator/include/verilated_types.h`, `verilator/include/verilated_funcs.h`, read
  2026-09-17). A wide packed value is `struct VlWide { EData m_storage[N_Words]; }`, whose own
  comment says that array "should be the only data member". The word count is a call argument:
  `VL_ADD_W(int words, WDataOutP owp, WDataInP lwp, WDataInP rwp)`. Nothing at run time knows a
  width; it is a C++ template parameter chosen while generating code.
- **LLVM `APInt`** (`llvm/ADT/APInt.h`, `llvm/lib/Support/APInt.cpp`, read 2026-09-17). 16 bytes: a
  union of one word and a pointer, plus a 4-byte `BitWidth`. Operand widths agreeing is an `assert`,
  compiled out of a release build; masking above the width is `clearUnusedBits()`, done in place on
  the destination.

Three systems, three shapes, one agreement: **the width is a scalar in the value and the structure
is not in the value at all.**

**Where our conditions differ, and it is the sentence this turns on.** Verilator's answer -- width
as a C++ type parameter -- is closed to us, because `north_star.md` invariant 2 makes compile-time
work scale with the count of distinct specializations rather than with instances, and a
width-parameterized unit that compiles once needs a width that arrives at construction. That is a
structural condition and not a gap: it stays true however much gets built. But it rules out only
Verilator's answer, and `APInt` and `SVInt` already answer the same condition with a run-time width
field. So the field's agreement reaches us intact.

## The decision

### D1. A packed value carries its width, its signedness, its state domain, and its bits

Nothing else. The value is self-describing for every operation determined by the bits and those
three facts -- arithmetic, comparison, bitwise, reduction, shift, concatenation, conversion,
formatting, change detection -- which is every operation except the ones that name a position inside
it.

### D2. An access that names a position takes the receiver's declared shape as an operand

An element select and a part-select receive the shape of the value being selected from, materialized
at HIR-to-MIR from the receiver's static type. This is the shape-operand pattern the unpacked family
already uses for its declared range, applied to the one family that was the exception; after this,
every selectable family takes its coordinate system from the same place and none carries one.

A consequence worth stating, because it is what the change means rather than a side effect: one run
of bits answers to both declarations. The same value reached with a one-dimensional shape yields a
bit and with a two-dimensional one yields a pair, which is exactly what the two declarations say and
was not expressible while the value held one of them.

### D3. A part-select states the receiver's shape, not the result's

The result of a packed part-select is unsigned, as wide as the selected range, and in the receiver's
state domain (LRM 11.5.1, 11.8.1). All three follow from the receiver and the bounds, so the result
shape the select used to carry stated nothing the receiver did not. A packed aggregate's member
access is the same step over the aggregate's flat run, and its bounds are the member's bit offset
and width, so it needs no separate treatment either.

### D4. A relation the compiler established is not re-established while the program runs

The error policy defines `InternalError` as an invariant the compiler itself established. Such a
check does not belong on a path the simulation runs per bit access; it belongs where the compiler
can run it, or it belongs to a construction that cannot produce the bad state.

Two were of the second kind and are gone. A view over a value's words was bounds-checked against
those same words on every construction, and the relation holds by construction of the value; the
check could not fail. A view also carried a bit offset that every construction in the tree set to
zero and that 44 call sites checked was zero -- the field's only reader was the check that it was
zero. Operand-shape checks between two values stay, because a mismatch there is a missing conversion
the lowering owes and is not excluded by construction.

### D5. An operation writes its result's bits into the result

A result value is built at its shape with its bits clear and the operation fills them, rather than
the operation filling a scratch buffer that is then validated and copied into a fresh value. The
declared default of a four-state type is all-x (LRM Table 6-7), which belongs to a declaration
rather than to an operation, so a result does not pay for it and then overwrite it.

## Rejected alternatives

- **Keep the stack in the value and make the descriptor cheaper.** This is what the two previous
  attempts did -- inlining the stack's allocation, then stopping the sequence type holding two
  storages at once -- and each returned a few percent while leaving the structure intact. The
  structure's cost is that it exists at every operation at all, which is why a no-op source change
  in the descriptor's constructor could move a run by 5%.

- **Give the value a pointer to a shared descriptor.** The shape already exists once per unit as a
  named constant, so a pointer would be sound and cheap. It is rejected because it answers the wrong
  question: the value still would not be the thing that knows how a declaration divides its bits,
  and the site that does know would still be passing a coordinate without the system it belongs to.
  The pointer buys bytes and settles nothing.

- **Keep the result-shape operand beside the receiver's.** One decision in two places. Where they
  agreed the second stated nothing, and the code that reconciled them threw if they disagreed -- a
  check whose existence says the two were never independent.

- **Take Verilator's answer and make the width a compile-time type parameter.** It is the fastest
  answer available and it is closed by `north_star.md` invariant 2: distinct types per width is
  compile-time work scaling with the design rather than with its distinct specializations, and it
  forecloses the parameterized unit that compiles once.

## Consequences

- An integral value is **64 bytes**, from 120. The width was stored twice in a two-state value and
  three times in a four-state one, and the state domain was stored twice -- once as a flag and once
  as a variant's discriminant, kept in step by a runtime check. Each is now stored once.
- Measured by interleaving both builds inside each round, five rounds per case: **1.71x on scalar
  arithmetic, 1.95x on wide bitwise work over 256-bit values, 2.01x on a tight call-and-loop case,
  and 2.09x on the representative compute block.** Each column's own spread runs from 1.2% to 15.4%
  depending on how quiet the machine was, and every gap clears its own round's spread several times
  over.
- An element select's runtime entry takes one more operand on both backends. A part-select's takes
  the same number as before, and a different one.
- Resolving a selection no longer allocates: the three helpers that built a result dimension stack
  per access are gone, and a selection is now an offset and a width.
- Building an integral literal no longer round-trips its width. Naming a declared type used to
  rebuild that type from its dimension stack and multiply the stack back out, twice over on the
  width-shorthand path, to recover the width the caller already held.
- A packed designation into a value carries where it starts and how long it runs, and nothing about
  structure; a further step down the chain is named by the site that writes it, as the first was.
- Nothing about the language's answers changes. The conformance corpus is unchanged and no path
  record gains an entry.

## Cross-references

- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- the same move for the
  unpacked family, whose reasoning this adopts and whose packed carve-out it supersedes.
- [selector-coordinate-resolution](selector-coordinate-resolution.md) -- decisions 2, 3 and 4 stand:
  a coordinate still resolves in the value's wide X/Z-aware domain, the resolution still happens
  inside the value, and what changes is only where the coordinate system arrives from.
- [integral-representation](integral-representation.md) -- one class for every integral, and the
  dispatch axis of width, signedness and state domain, which is now exactly what a value holds.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- why a value describes
  itself at all, which this keeps.
- [a-value-states-its-own-bits](a-value-states-its-own-bits.md) -- the same division one question
  over: the type answers what is statically knowable, the value answers about its own bits.
- LRM 7.4.1 (packed arrays), 11.5.1 (vector bit- and part-select), 11.8.1 (result signedness), Table
  6-7 (default values), 7.2.1 / 7.3.1 (packed structure and union member access).
