# A process stopping only at the head of its loop is called, not resumed

Date: 2026-10-12. Status: accepted. Adds a stage between HIR-to-MIR and every consumer of MIR.
Agrees with `a-wait-is-storage-of-its-activation.md` and its rejection of two kinds of process.

## Context

What a process costs to compile should be what it computes. A continuous assignment, the one a port
implies, an `always_comb` and an `always_ff` each compute one statement, and wait at one place, the
head of the loop they run (LRM 4.9.1, 9.2.2, 10.3). HIR-to-MIR states every process as one
coroutine, which is right for what the source means, and every backend compiles each one as a
coroutine: the C++ backend writes one for clang, and the execution backend splits one into its
resumable parts, which is where a coroutine's compile cost lies. OpenTitan's `pinmux_reg_top` holds
12,195 of them, one per drive, and its code generation took 164 s and 4.4 GB.

The human's rule for this (2026-10-09): one process shape upstream, and an optimization below it
that turns a coroutine stopping only at its loop head into a call, never two process kinds in HIR
and MIR. And it has to reach both backends, which only something both of them read can do; a render
may not decide it, since a render is a fixed function of one node.

## Decision

**A stage rewrites MIR into MIR between HIR-to-MIR and every consumer, and its first rewrite turns a
process body that stops only at the head of its loop into two plain bodies the runtime's one
coroutine runs.**

- **The stage.** Each rewrite keeps what the program does and states its result in MIR's own
  vocabulary. The unit is verified once, as it leaves the stage rather than also as it leaves
  HIR-to-MIR, since what leaves the stage is what every consumer reads. A consumer cannot tell what
  a rewrite built from what the lowering built. Every build runs it, so no answer depends on an
  optimization setting. Arenas are never edited, so a rewrite builds the body that replaces one: it
  walks the old body from its statements, adds each node it reaches to the new body's pools, which
  name it, and states its own nodes the same way.
- **What fits.** A coroutine method entered on its object alone whose statements before the loop
  never stop, whose loop runs until left, whose loop body stops once -- at its first statement or
  its last -- on a wait the statements before the loop declared, never leaves the loop, and names
  nothing else they declared. Anything else stays a coroutine: the same program, dearer to compile.
- **What it becomes.** Two methods taking the object and the wait, both erased, and answering a
  machine boolean. The setup runs the statements before the loop, writing the wait into the storage
  it is handed. The pass runs the loop body once and answers whether its stop has still to end. A
  body that runs its statements and then stops has a setup answering whether a pass follows at once
  -- a return before the loop answers no (a net shown through its source). A body that stops and
  then runs has a setup going on to the first stop and answering as a pass does, and its pass runs
  the statements and then that stop. Every call that started the process enters the runtime instead,
  on the object's address and the two methods' addresses, through one of two entries named by that
  order.
- **What the runtime does.** One coroutine runs any split process: it holds the wait, as an
  activation holds every wait it stops at, calls the setup and then a pass each time the wait ends,
  and gives up control only between them. A departure out of either part unwinds it as it would the
  body.

The engine still schedules one kind of thing, a coroutine resumed where it stopped; what changed is
which code is compiled as one.

## Survey

- **rustc** turns coroutines into state machines in a MIR-to-MIR pass, `StateTransform`
  (`compiler/rustc_mir_transform/src/coroutine/mod.rs`), run by the pass manager
  (`rustc_mir_transform/src/pass_manager.rs`, `run_passes`, validating between phases) before code
  generation, so every backend receives the result. A pass there edits a body in place through
  `MirPatch` (`patch.rs`); ours builds the body anew because a MIR node is never edited once added.
- **clang** leaves coroutines whole in its AST and splits them in LLVM's `CoroSplit`, inside code
  generation. Lyra's second backend writes source rather than LLVM IR, so a rewrite left inside one
  backend's code generation would never reach the other. That is a condition, not a gap: it stays
  true however much either backend gains.
- **SystemC** (IEEE 1666) runs a method process as a plain function the kernel calls on its static
  sensitivity, and the kernel owns the waiting. Whether it runs once before it first waits is set
  when it is registered (`dont_initialize`), not answered by each call -- which is why the order
  here picks the runtime entry rather than widening what a part answers.

## Rejected

- **Splitting in one backend's lowering.** Built first: the C++ backend's render had to scan a body
  to decide, which a render may not do, and the other backend had to repeat it.
- **Two kinds of process in HIR and MIR.** Every layer would carry the split
  (`a-wait-is-storage-of-its-activation.md`, "two kinds of process"); here only the stage knows.
- **A state class keeping the setup's locals for every pass.** A wait reads its leaves when it is
  built and keeps nothing of them, so of what the setup declares only the wait outlives it.
- **One runtime entry with a three-way answer** (ended, run a pass, stop). A pass can never end, so
  that answer type would let a pass state an impossible outcome; two entries keep each answer to the
  two outcomes that can happen where it is given.
- **Predicting where an added node lands in a block.** A pool gives an identity; a rewrite that
  names a node by its future position assumes the pool numbers by position and nothing else adds.

## Consequences

- On the execution backend a body naming another body's address is lowered as an operand of its own,
  where before only a constant could name one; and a library entry may answer with an execution it
  built, which every target then builds in its caller's storage as entering a body's frame does.
- A body whose wait is built inside its loop, or that waits anywhere but the loop's head, stays a
  coroutine.
- `pinmux_reg_top` on the execution backend, one build from an empty store: lowering its units went
  from 219 s to 100 s, of which generating the object from 170 s to 49 s, and the peak while
  lowering from 4.41 GB to 2.49 GB; the whole build from 3 min 44 s to 1 min 44 s. The stage itself
  took 4 s. No growth design's coroutine count follows its size any more on either backend.

## Cross-references

- `a-wait-is-storage-of-its-activation.md` -- the wait is storage of the activation, which here is
  the runtime's coroutine.
- `waiting-is-an-operation.md` -- a stop is a wait on a call's answer, which a pass returns instead.
