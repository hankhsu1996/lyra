# Combinational updates are coalesced where a build asks

Status: Accepted, 2026-11-15; revised 2026-11-18 and 2026-11-19 to the form below.

## Requirement

A design keeps the standard's reading unless its author asks for another, and a design written for a
schedule the standard allows but does not promise can still be run. Two `always_comb` procedures
that each write a default for every output and then its value, and each read an output the other
defaults, wake each other for ever under a schedule that runs each procedure to its stop, which is
how Lyra runs every process; under a schedule that interleaves them (LRM 4.7) they settle. Both
schedules are legal. The style is ordinary RTL, and CVA6's write buffer and OpenTitan's SRAM byte
adapter have it.

## Choice

- **By default every update is an event, as the standard reads.** A design whose procedures wake
  each other for ever ends with its time slot reported unsettled, naming them, and where one is an
  `always_comb` or `always_latch` a note names the option below.
- **A build may ask for combinational updates to be coalesced**, by `--coalesce-comb-updates` or by
  `coalesce_comb_updates` in a library's `lyra.toml`. Then an `always_comb` or `always_latch` is
  triggered by the net change another such procedure's execution leaves of what it reads, not by
  each write. A write by any other process triggers every wait as it lands.
- **It is a dialect of the language, chosen where a library is compiled**, as `-fwrapv` is chosen
  per translation unit. HIR-to-MIR builds each such procedure's wait as the one of its library's
  dialect, so MIR states which and every layer below translates it. A library stating nothing is
  written for the standard whatever uses it, and the command line's answer is every library's.
- **Only these two procedures.** They are the standard's constructs for combinational logic and
  latches, whose outputs are a function of what they read (LRM 9.2.2.2, 9.2.2.3). An event control,
  `@*` included, keeps the standard's reading in either dialect.

## Alternatives turned down

- **Coalescing by default.** It fails keeping the standard's reading where the author did not ask
  otherwise. Wherever a procedure's side effects show it, the design would depart from the standard
  without a word. Its cost to designs that do not need it was measured as small, so the failure
  rests on meaning alone, and how many designs need the dialect bears on neither side.
- **Coalescing a slot only once it fails to settle.** It fails one meaning per design, since the
  design would mean two things depending on whether it loops.
- **A run-time option of the simulation.** It fails a program's meaning being the compiled program,
  and has every wait ask the switch at every write.
- **Holding every value-change wait until the event ends.** It fails events every legal schedule
  has. An armed `@(a)` would count nothing for `a = 1; a = 0;`, and a zero-width pulse would be no
  edge.
- **Judging the execution of every process, whoever writes.** It fails runs every legal schedule
  makes where the reader is idle (LRM 4.3, 4.5), for no design that needs it.
- **A library stating nothing takes the root's answer.** It fails a library meaning one thing
  whatever uses it, which no other choice of how a library is compiled breaks.
- **Refusing a library whose dialect differs from the root's.** It fails a build mixing libraries
  written in each dialect, and protects nothing, since each unit's waits are built in its own
  dialect, so a mixed build needs no rule.

## Consequences across layers

- The option reaches the command line, the manifest, the lowering of the two procedures and the
  runtime's change detection.
- How a write announces what it modifies, what an interval keeps and when it ends, and the dialect's
  stated limit are [`scheduling.md`](../architecture/scheduling.md)'s;
  [`project_model.md`](../architecture/project_model.md) holds the manifest key.
