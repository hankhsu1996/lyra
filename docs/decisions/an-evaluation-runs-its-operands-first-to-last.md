# An evaluation runs its operands first to last

Status: Accepted, 2026-10-28.

## Requirement

The order in which one evaluation's operands run is decided once, above every target, and every
target runs that order whatever its own language leaves unordered. The standard does not ask this: a
call's arguments run in an undefined order (LRM 13.5), an assignment's target and value likewise
where no timing control separates them (LRM 10.4.1), and "the ordering of assignment operations
relative to any other operation within an expression is undefined" (LRM 11.4.2). It is Lyra's own
requirement, chosen as part of the product, so that a design keeps its answer when the backend or
the host compiler changes and a failure seen on one reproduces on the other. Before it, the
execution backend ran a write's target before its value, and the C++ backend left the order to the
host compiler, where GCC and clang chose opposite orders and some of the emitted C++ was undefined.

## Choice

- **A node's operands run first to last, as MIR lists them; an assignment's value runs before its
  target, and a call's arguments before its callee and the object it is entered on.** That is Rust's
  order for operands and for an assignment, and the one a write with an intra-assignment timing
  control takes anyway (LRM 10.4.1), so every write is one order. A read takes what its storage
  holds when it runs.
- **MIR's conversion into its legalized phase leaves no node with two operands a target could run in
  the other order to another answer.** The test is the shape of the evaluation, never whether two
  operands conflict. An operand is taken ahead into a temporary where what runs after it could
  change what it evaluates to, or where its side effects could change a later one. A target is then
  free to run what remains in any order its language gives.
- **Every call has side effects, whatever it calls.** No fact about what a library entry reads or
  changes decides an order.

## Alternatives turned down

- **Each backend running the order itself.** It fails the order being decided once. The failure
  rests on two backends each realizing it being an agreement nothing holds in step.
- **Any order the standard allows, with only undefined behaviour removed from the emitted C++.** It
  fails a design answering the same on every backend and host compiler.
- **Stating on each library entry what it reads and changes**, as LLVM's `memory(none)` does. It
  fails an order nothing can silently get wrong. The failure rests on that being a contract per
  entry, over a thousand of them, that nothing checks.
- **Asking whether two operands conflict.** It fails one account of each node's effects, being a
  second account, recomputed by the verification, and took ahead no fewer operands than the shape.
- **Taking every operand into a temporary.** It fails keeping the source's shape where nothing could
  change an operand, for no gain.
- **The receiver first, as Rust runs it.** It fails storage staying valid while it is held. The
  failure rests on MIR having no borrow checker to keep a receiver formed first alive while the
  arguments run, which may end what it designates.
- **A write's target before its value, as Java runs it.** It fails the same, since a value may
  resize what the target lies in, and reverses the order a timing control imposes.

## Consequences across layers

- `x + f()` where `f` writes `x` reads `x` before the call on both backends, and a nonblocking
  assignment's value is frozen before its target's coordinates.
- The rewrite after this one, deciding where each value lies, keeps the order it is handed.
- How the conversion decides which operands to take ahead, and what the verification checks, is
  [`mir.md`](../architecture/mir.md)'s.
- What Lyra writes for a given order is the same on every run of one build
  ([the-pinned-clang-builds-the-lyra-that-is-run](the-pinned-clang-builds-the-lyra-that-is-run.md)),
  which is a different requirement about Lyra's own C++.
