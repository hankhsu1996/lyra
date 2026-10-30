# What Lyra writes is the same whichever compiler built it

Date: 2026-10-30. Status: accepted.

## The requirement

> For a fixed design, Lyra revision, set of options, target and LLVM release, everything Lyra writes
> -- each intermediate form, the emitted C++, the program it links -- is the same whichever C++
> compiler built Lyra.

A build Lyra keeps is found again by what it read, which assumes one input writes one output. A
failure seen on one build of Lyra reproduces on another only if the two write alike, and a test that
compares what Lyra writes for one design at two sizes gives one answer only if every build of Lyra
writes alike.
[an-evaluation-runs-its-operands-first-to-last](an-evaluation-runs-its-operands-first-to-last.md)
holds the programs Lyra builds to the same rule; this holds Lyra itself to it.

Lyra builds every intermediate form by calls that append to an arena or a builder and answer what
they appended: a node and its id, a local, an LLVM instruction at the builder's insertion point. The
order of those calls is the order of the ids, the locals and the instructions, and from there of the
machine code. C++ leaves the order of a call's arguments to the compiler ([expr.call]/8), and of
most operators' operands ([intro.execution]/10), and clang and GCC choose opposite ones. So two
appends written as two arguments of one call landed in whichever order the host compiler chose: over
the conformance corpus, Lyra built by GCC and Lyra built by clang wrote different MIR, LLVM IR and
C++ for nearly every case, all of it the same tree with its nodes numbered or its instructions
placed in the other order. The programs answered alike; what Lyra wrote did not.

## Decisions

**D1. Two operands of one evaluation whose order C++ leaves open never both build.** An operand that
appends, or calls something that does, is bound to a local in a statement of its own, in the order
the operands are written; a braced initializer, whose clauses C++ runs first to last
([dcl.init.list]/4), may build its parts in place. Statements run in the order they are written, so
the IR is built in the order the lowering reads its operands. Nothing about how nodes are allocated
or numbered changes.

**D2. The source is read for it, on every pull request and over every source each night.**
`tools/policy/check_sequencing.py` reads the syntax tree with clang-query for an evaluation C++
leaves unordered -- a call's arguments, a constructor's in parentheses, an operator's operands --
with two operands that each reach a call which builds. What builds is known by type: a method of a
type that holds or appends to an IR, a function handed one, and any lambda, whose body the call does
not show. The reading is conservative on purpose, as a statement costs a line and states the order,
where a reading that cleared an operand it could not see into would let the one shape it exists for
through. It is held to cases it must and must not report before it reads anything else. A pull
request reads the sources it changes, inside the merge-time budget; the nightly reads all of them,
which is what reaches a header's inline code and a change to the reading itself.

**D3. Two builds are compared over the corpus each night.** Lyra built by clang and by GCC are
handed every conformance case, and each dump, the emitted project and the linked program are
compared byte for byte, reporting the earliest form that differs. It sees what the reading does not
model -- an order a container's iteration gives, an option, a library -- and only what the corpus
reaches; its time grows with the corpus, which is why it is not on a pull request.

## What the field does

- **LLVM and flang** fix each such site as it is found, by moving the builder calls into statements
  of their own (flang's "make IR generation for ieee_next deterministic", llvm-project #125055).
  Neither has a rule or a check for it; each case was found by comparing outputs.
- **Solidity** emitted different bytecode from its GCC-built Linux release and its clang-built macOS
  one for the same reason, and fixed it the same way (ethereum/solidity#14361).
- **Rust** fixes operands left to right in the language, so rustc cannot write this shape.

Where Lyra differs is that it checks: the shape is visible in the syntax tree, so the first site a
change adds fails its pull request instead of the night after it merges.

## Rejected

- **Numbering every IR's nodes again by a canonical walk**, so the order they were built in never
  shows. It answers ids and none of what else the order decides -- an LLVM block is a sequence of
  instructions, not a set of numbered nodes -- and it needs a canonical order for nodes that
  structure alone does not tell apart. A collection whose order means nothing is sorted where it is
  written out; a sequence the lowering builds is built in a stated order.
- **A construction framework that sequences for the caller** -- a deferred-expression builder, a
  helper taking lambdas. Each states an order that a statement already states, at the cost of a
  concept every lowering has to learn.
- **The comparison alone**, as the field has it. It reaches only what the corpus runs and fails the
  night after a site merges; the reading reaches every source and fails the pull request that adds
  one.
- **Building Lyra with one compiler only.** Lyra builds under both and the merge gate holds it to
  that, and a host compiler's freedom in an order is not something to depend on even where one
  compiler is chosen: its next release may choose otherwise.
