# A process stopping only at the head of its loop is called, not resumed

Date: 2026-10-12. Status: accepted; the stage revised 2026-10-18 by
`a-rewrite-of-mir-says-whether-a-program-needs-it.md`; what the run makes of it revised 2026-11-18
(section "At run time"). Adds a stage between HIR-to-MIR and every consumer of MIR. Agrees with
`a-wait-is-storage-of-its-activation.md` that every layer above the run keeps one kind of process;
the run carries it in two ways (section "At run time").

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
process body that stops only at the head of its loop into two plain bodies the run calls.**

- **The stage.** Each rewrite keeps what the program does and states its result in MIR's own
  vocabulary. The unit is verified as it leaves the stage, since what leaves the stage is what every
  consumer reads, and after each optional rewrite and on reaching each phase where a build asks. A
  consumer cannot tell what a rewrite built from what the lowering built. This rewrite is optional
  -- skipping it leaves the same program, dearer to compile -- so no answer depends on whether it
  runs, and every build runs it unless a developer skips it by name. Arenas are never edited, so a
  rewrite builds the body that replaces one: it walks the old body from its statements, adds each
  node it reaches to the new body's pools, which name it, and states its own nodes the same way.
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
  the statements and then that stop. The registration that handed the process to the run hands it
  over as a reaction instead: the object's address, and a constant of the unit naming the two
  methods and the form, which says in which order the run calls them.
- **A body that never stops** is an ordinary function too: one entered on its object alone becomes
  its own setup, taking the object and a wait it never builds, both erased; its form says it runs
  once and ends, so its answer is not read. In SystemVerilog a process is an execution whatever it
  does, so that it is a coroutine is this compiler's representation and not the language's, as it is
  in C++, which is why this is a rewrite.
- **Only a body nothing outside the unit names.** Both forms change a body's signature, so a class
  another unit can name keeps its bodies, as LLVM rewrites a signature only under local linkage.
- **What the runtime does** is the section below.

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
  here is stated once in the reaction's constant record rather than widening what a part answers.

## Rejected

- **Splitting in one backend's lowering.** Built first: the C++ backend's render had to scan a body
  to decide, which a render may not do, and the other backend had to repeat it.
- **Two kinds of process in HIR and MIR.** Every layer would carry the split
  (`a-wait-is-storage-of-its-activation.md`, "two kinds of process"); here only the stage knows.
- **A state class keeping the setup's locals for every pass.** A wait reads its leaves when it is
  built and keeps nothing of them, so of what the setup declares only the wait outlives it.
- **A part answering three ways** (ended, run a pass, stop). A pass can never end, so that answer
  type would let a pass state an impossible outcome; the form the reaction's record states keeps
  each part's answer to the two outcomes that can happen where it is given, and a body that runs
  once is a form rather than an answer.
- **Predicting where an added node lands in a block.** A pool gives an identity; a rewrite that
  names a node by its future position assumes the pool numbers by position and nothing else adds.

## At run time

Date: 2026-11-18. As first decided, the run wrapped the two parts in a coroutine of its own and that
coroutine in a whole process object, so a continuous assignment still cost a frame, a process and a
resume path per instance while its body cost tens of instructions. Measured on a manycore design's
two-core configuration, the LLVM path at `-O2`: 56,406 continuous drivers for two cores, about 1.6
KB of memory each, about 1,690 instructions a resume; its 128-core configuration held 3.4 GB of such
processes beside 0.3 GB of declared storage.

**What the language asks of such a process is an identity, a generator and a schedule, not a
frame.** A continuous assignment is a static process (LRM 4.9.1, Annex P): it has a generator seeded
from its instance's as it is created (LRM 18.14.1), `process::self()` names it from a function its
expression calls (LRM 9.7), and an `always` one may be controlled and may fork. None of that needs a
computation kept part-way between triggers, which a body that never stops mid-way does not have.

- **Registration is one operation over the two ways a body is compiled.** `register_coroutine` hands
  the run a coroutine; `register_reaction` hands it the object and the unit's constant record of the
  setup, the pass and the form. Both take the process's own constant record: its kind, which says
  when it starts and what created its thread, and where the source writes it. Which one a
  registration is, the rewrite knows where it builds the call, so nothing is erased to be asked
  again at run time.
- **A reaction is an activation that is no frame.** The engine queues activations and runs each
  through the activation's own entry: a frame is resumed, a reaction's setup or pass is called. The
  engine tells them apart nowhere, and no construct reaches it.
- **What runs is one thing at a time** -- a thread of frames, a reaction, or code on no thread --
  and the code running reads its state, its context and its process from that one, and a stop parks
  its leaf. A run started inside another puts the outer one back as it returns.
- **The process is the object the language names, and what executes it is not part of it.** Process
  control is written once on the process and reaches what executes it -- its leaf, its release, a
  foreign call still holding it, what of its state outlives a stop -- through one interface a thread
  of frames and a reaction both answer. A reaction's process is made the first time the language
  asks for it -- `self()`, a fork, a deferred report's pass, a program's root -- and holds its
  generator from then on, which `srandom` then seeds. Making it creates no process the language can
  see. The reaction draws its generator as it is created whether it ever draws or not, so every
  process created after it is seeded as before.
- **Reactions live in pages the run holds for its length**, so millions of them make no allocation
  each, and what runs a part keeps nothing of it once the part returns.

Rejected at run time:

- **The parts run inside a coroutine of the run's own, under a whole process object.** The first
  form; it is what the measurement above is of.
- **An erased execution type every start site converts its body into**, so that one registration
  takes either. The realization is known where the call is built, so the erasure carries a tag
  nothing needs to ask, at every start site.
- **A process object that fronts a reaction as a second mode of the one that owns frames**, with the
  reaction and the process each published as running. Two answers to what runs had to be kept in
  step by two guards, which a frame started inside a pass showed they were not.
- **A frame's promise deriving from the activation.** With the activation run through virtual
  functions, every promise type would be a dynamic class, with a table and type information of its
  own in every unit stating a body of that result; the promise holds its activation instead, whose
  code is the library's alone.
- **Process control declared on the process and carried out by each realization deriving from it**,
  as SystemC's method and thread processes override its kill, suspend and resume
  (`sc_method_process.h`). A process is an object of a class here, reached from generated code and
  from a handle at the address of the part every object starts with, so the process can have no
  virtual function of its own; what executes it is reached through the interface instead.

Survey: SystemC's evaluate phase pops a method process off an intrusive list and calls it on the
scheduler's stack (`sc_simcontext::crunch`, `sc_method_process::run_process`); its process object
exists for every method, where SystemVerilog's continuous assignments are counted in millions and
its object is asked for only through `self()`. tokio keeps a task's hot header -- its state, its
run-queue link and a table of run entries -- apart from what is read only at creation or at its end
(its task core's `Header` and `Trailer`); a reaction is that header, and its process the trailer,
made on demand.

Measured on a generate loop of continuous assignments, before and after: 1.56 to 0.86 KB per
continuous assignment onto a variable, 1.88 to 1.21 KB onto a net element. On the manycore design's
two-core configuration, its steady state went from 3.95 million to 3.55 million instructions a
clock, and its peak memory from 197 MB to 166 MB.

## Consequences

- A reaction's parts are named by address in a constant of the unit, as a class's definition names
  its methods; no body names another's address as an operand, and no library entry answers with an
  execution.
- A body whose wait is built inside its loop, or that waits anywhere but the loop's head, stays a
  coroutine, and is registered and run as one.
- `pinmux_reg_top` on the execution backend, one build from an empty store: lowering its units went
  from 219 s to 100 s, of which generating the object from 170 s to 49 s, and the peak while
  lowering from 4.41 GB to 2.49 GB; the whole build from 3 min 44 s to 1 min 44 s. The stage itself
  took 4 s. No growth design's coroutine count follows its size any more on either backend.

## Cross-references

- `a-wait-is-storage-of-its-activation.md` -- the wait is storage of the activation, which here is
  the reaction.
- `waiting-is-an-operation.md` -- a stop is a wait on a call's answer, which a pass returns instead.
- `a-coroutines-storage-states-its-lifetime.md` -- what a body that stays a coroutine hands LLVM.
