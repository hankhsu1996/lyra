# A loop moving bits one at a time moves the run

Date: 2026-11-19. Status: accepted; revised 2026-11-20: a select of bits is a step of a place,
whether the run is exact is asked once, ahead of the loop, and the run is one assignment of a select
as wide as the loop makes passes, so only a loop whose passes are counted before it runs is one run.
An optional rewrite of MIR under
[a-rewrite-of-mir-says-whether-a-program-needs-it](a-rewrite-of-mir-says-whether-a-program-needs-it.md)
D1 and D2.

## The requirement

> A loop that moves consecutive bits of packed values one bit a pass, a number of passes fixed
> before the program runs, costs what moving that run costs, in every build, and leaves every bit,
> the loop variable and what anything waiting sees as the loop would.

A packed value is one vector (LRM 7.4.1), and the loops a design writes to convert between two
layouts of it -- a hash state as a box of lanes and as a flat vector, a bus as fields and as bits --
move one bit a pass at positions that step by one, across a width the types fix. Each pass is a
select of one bit and a write of one bit, each valid or not by itself (LRM 11.5.1). Written that way
in C++ or Rust the loop stays one bit a pass under clang and rustc at `-O2`, and LLVM does not merge
single-bit moves into a word move even with every position a constant.

## Decisions

**L1. The loop is recognized in MIR, where a part of a packed value is still one select.** Below MIR
a bit move is shifts and masks over words, and the run it belongs to is gone; and MIR is where the
two backends part, so it is the last place one decision reaches both. So an optional rewrite of the
unit as built finds a counted loop whose body moves one bit, and moves the run ahead of it. It runs
in every build, so a default build, which compiles the design unoptimized, gets the run too.

**L2. What is recognized is stated in the loop's own terms.** The loop runs while its variable is
below a constant bound, from a constant the statement just before the loop gives it, and each pass
ends by adding one to it, so how many passes it makes is fixed before it runs. The body declares
what it computes and makes one write: one bit of a place assigned one bit read, each a one-bit
select whose position is the loop variable plus what every pass computes alike. Nothing else in the
loop writes anything, calls anything that could, or waits, and nothing it reads is what the write
writes, except the bits it moves. A position that steps by other than one, a body doing more, a loop
counting down, and a loop whose passes are counted only while it runs are not one run, and stay a
loop.

**L3. The run is one assignment of a select as wide as the loop makes passes, ahead of the loop,
which then makes no pass.** Where the loop starts, the run starts where its first pass would read
and write. Where it is exact (L4), the select of the run read is assigned to the select of the run
written, each of a type as wide as the loop makes passes, and the variable is left at the bound, so
the loop as written finds its condition false and makes no pass. Where it is not, the loop runs as
written. The assignment is the language's own: a select of several bits is valid or not bit by bit
as a one-bit select is (LRM 11.5.1), so the run reads x and writes nothing at the positions the
passes would, a select taken within an element stops at that element's edge as each pass's does, and
an index holding x names nothing for the run as for every pass. Every position the run uses is what
the first pass computes, stated again ahead of the loop from what each local the loop declares is
initialized with.

**L4. Whether the run is exact is asked once, of the values the program computes where the loop
starts.** A position summed in integral arithmetic at a declared width wraps at that width (LRM
11.6.1): the run goes on past where the passes start again, so the run is taken only where no
position wraps over it. And a pass reads what the earlier passes wrote (LRM 12.7.1): where the bits
are read from the value they are written into, the run is taken only where it reads ahead of what it
writes, or far enough behind that no part reads bits an earlier part wrote. Both are properties of
where the run starts, so one question ahead of the loop answers them for every pass, as LLVM's
vectorizer asks its runtime checks once ahead of a loop and keeps the original loop for when they
fail.

## What the field does

- **LLVM** recognizes a loop that stores what it loads at addresses stepping by the element size,
  and replaces it with a `memcpy`, or a `memmove` where the two may overlap
  (`processLoopStoreOfLoopLoad` and `mayLoopAccessLocation` in
  `llvm/lib/Transforms/Scalar/LoopIdiomRecognize.cpp`). It runs from `-O1`, on bytes.
- **LLVM's vectorizer** takes a loop whose accesses may overlap by asking how far apart they are
  (`MemoryDepChecker` in `llvm/include/llvm/Analysis/LoopAccessAnalysis.h`, whose maximum safe width
  is the distance that makes moving several elements at once read what one at a time would), and
  where it cannot settle a question before the program runs, checks it while running and keeps the
  original scalar loop as the fallback for when a check fails and for the iterations left over
  (`GeneratedRTChecks` in `llvm/lib/Transforms/Vectorize/LoopVectorize.cpp`, with a no-wrap question
  stated as `SCEVWrapPredicate` in `llvm/include/llvm/Analysis/ScalarEvolution.h`).

Where Lyra's conditions differ: a bit exists as a unit only above LLVM, so the recognition is MIR's,
and the default build compiles a design unoptimized, so it runs in every build. LLVM's idiom is a
memory operation of a byte count, while the run here is a value of a packed type, so its result is
the assignment of one select to another that MIR already states, with the width in its type, as
every other operation in Lyra has its width fixed before the program runs. The rewrite reads the
loop with the analyses LLVM's names for them: the object a place is part of (`getUnderlyingObject`),
whether a value is the same on every pass (`isLoopInvariant`), how a position moves with the loop
variable (an add recurrence), the trip count from the start entering the loop and the bound, and the
loop's first pass stated again ahead of it (`SCEVExpander`).

## Rejected

- **An operation copying a run of bits whose length the program computes while it runs.** It is the
  one operation in Lyra whose width would not be fixed before the program runs, and a library entry
  would have to read where each run lies from the chain of selects naming it while the program runs,
  the work the rest of Lyra moves to compile time. Nothing measured needs it: the designs that
  convert layouts bit by bit do so across widths their types fix. The earlier revision of this
  record took the operation for a loop bounded by a variable, which it said a design writes as often
  as one bounded by a parameter; that was never measured, and such a loop now stays a loop.
- **A run whose passes read what earlier passes wrote, as one operation that reads and writes in
  order.** Moving bits toward the ones not yet read, `v[z + 4] = v[z]`, repeats the first bits along
  the vector; an operation stating that order is the loop under another name. Such a loop fails L4
  and runs as written.
- **Composing an element select's index into the position of the bit selected within it.** It
  composes an element's validity into a test on the loop variable, so moving the run reads the next
  element where the passes read x. The bit-select stays a step within the element the select before
  it reaches, bounded by that element, so its validity is the element's own, the same for a run as
  for a bit.
- **Asking on each pass whether the run is exact, and moving one bit where it is not.** Each
  question is a property of the run's start rather than of a pass, so asking it on every pass asks
  the one question again a pass, and it spreads the run across the loop's body, which a later reader
  has to follow to see the loop is otherwise unchanged. Moving the run ahead of the loop and leaving
  the loop as written keeps the body once, as the vectorizer's fallback loop is the original one.
- **Moving the run a word at a time, then what is left in halves, each a select as wide as the part
  moved.** The run is one value of the loop's width; splitting it by a machine word's size puts the
  machine into MIR, where how a value's words are moved is the backends'.

## Consequences

- Measured on the benchmark moving the 1,600 bits of a 5x5 array of 64-bit lanes into a flat vector,
  `-O2 --lto`, under callgrind, instructions a call of the whole benchmark step: the execution
  backend 958 and the C++ backend 3,245, from 3,119,512 and 962,240 with the loop moving a bit a
  pass. What is left of the step is the benchmark's own write of one bit into a variable and of the
  flat result into another.
- `--mir-enable-passes=-LoopIdiomRecognize` leaves every such loop a bit a pass, with the same
  answers.
