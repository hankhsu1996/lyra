# A value is its machine data, and a representation is judged by what its cost scales with

Status: Accepted, 2026-10-04.

## Requirement

A compiler meets designs larger than any it was measured on, so how a value is represented is
justified by what each cost scales with for any input, and measured designs only check the
arithmetic. Five bounds are the test:

1. An operation costs the bits it touches.
2. A change propagates at the cost of the part that changed.
3. Compiled artifacts follow what one unit states, never the whole design or its instances.
4. Memory follows the declared bits, by at most a constant factor.
5. Every decision reads one unit and the published declarations it uses.

Nothing in the language asks a value to carry its type. The width, signedness and state domain of
every expression are fixed by its type (LRM 11.6, 11.8), and the front end hands each operator
operands of one type. Every fast compiled simulator fixes width where it compiles -- Verilator's
`VlWide<N>`, CXXRTL's `value<Bits>`, Arcilator's `iN`, NVC's `vec2` and `vec4`.

## Choice

- **A value is held as the machine holds data of its size, and its type is the compiler's to know.**
  A two-state packed value is its bits in words; a four-state one is the same words followed by an
  equal run of unknown words, the encoding DPI's `aval` / `bval` uses. No value carries its width,
  signedness or state domain.
- **An operation is the machine's work on those words, generated where it is compiled**, so its cost
  follows the bits it touches (bound 1). A type is its exact width, signedness and state domain.
- **A packed value is laid out as the standard describes it, its bits contiguous** (LRM 7.4.1,
  7.2.1), and an element at a run-time index is reached by bit addressing, whose cost is the
  element's (bounds 1 and 4).
- **Nothing reads a description of a type while the program runs.** What is specific to a type is
  code the compiler generates where the type is used; code compiled once in the runtime is told only
  sizes, widths and the addresses of functions the design's own code contains.
- **A write reports the part it reached**, so a wait is decided on what changed (bound 2).

## Alternatives turned down

- **A runtime type per value, with fast paths inside it.** It fails bound 1, since an operation
  reading its operands' shape back costs hundreds of instructions whatever its width. The failure
  rests on the boundary between generated code and a precompiled library, which three rounds of
  making the object cheaper inside it did not remove.
- **A type identified by its storage size, with the exact width an argument of each operation**, as
  Verilator's helpers have it. It fails a missing conversion being a compile error rather than a
  wrong value. What it buys rests on the count of exact types growing with the design, which does
  not hold once packed collections are taken apart into their elements' types (bound 3).
- **A slot per element, padded to a machine size.** It fails bound 4, since a two-bit element grows
  fourfold, a packed struct of one-bit fields eightfold. It also gives one storage two layouts where
  the standard makes it one (LRM 13.5.2, 7.3.1, 10.11).
- **A description of a type the runtime interprets** -- a self-describing value, or a record the
  runtime walks. It fails bound 1, doing again at run time what the compiler had already done where
  every operation is compiled.

## Consequences across layers

- How a value lies below LIR, what code compiled once is told of a type, and where a value too large
  for a frame is held are [`lir.md`](../architecture/lir.md)'s and
  [`mir.md`](../architecture/mir.md)'s; what a write tells a wait is
  [`scheduling.md`](../architecture/scheduling.md)'s; how a container's operations divide between
  its core and the bodies generated for its type is [`mir.md`](../architecture/mir.md)'s.
- The library never chooses by a type's kind
  ([the-library-is-parametric-in-the-types-a-design-composes](the-library-is-parametric-in-the-types-a-design-composes.md)).
- Whether a declared width belongs in a type is answered here, and
  [an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) follows that
  answer.
