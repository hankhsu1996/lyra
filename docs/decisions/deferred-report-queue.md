# The deferred report queue

Date: 2026-09-08. Status: accepted, and realized on both backends for the deferred `assert` and
`assume` forms the compiler carries. What the compiler does not yet carry is stated at the end: an
assertion whose action block is a subroutine call, and the deferred `cover` form, are refused rather
than lowered, so R9's argument evaluation has no realization to be right or wrong about.

## Why this decision matters

Two constructs already place reports on one per-process queue: deferred immediate assertions (LRM
16.4) and the violation reports of `unique` / `unique0` / `priority` (LRM 12.4.2.1).
[qualified-statement-violation-check](qualified-statement-violation-check.md) settles what a
violation report says and what the statement evaluates, and deliberately leaves where the report is
issued to 12.4.2.1. Nothing records how the queue itself behaves.

That gap is not cosmetic. The queue carries a cancellation model with more than one granularity, a
one-way maturity latch, and a report identity that is neither per-assertion nor per-process. A
mechanism built from the most visible half of those requirements satisfies the common case and
silently fails the rest, and the failure is invisible: a report that should have been withdrawn is
indistinguishable, in the output, from one the design earned.

## What the standard requires

**R1. Evaluation and action are separated in time.** A deferred assertion's expression is evaluated
where the statement is processed; the reporting or action block is scheduled at a later point in the
same time step (16.4).

**R2. There are two maturity placements.** An observed (`#0`) report matures in the Observed region
and its subroutine is scheduled in the Reactive region; a final report matures and is scheduled in
the Postponed region (16.4.1).

**R3. Maturity is a one-way latch.** "Once a report matures, it may no longer be flushed" (16.4.1).
Pending and matured are two states, and the transition removes every cancellation right described
below.

**R4. There are three flush points, and the two consumers do not share one set.** For deferred
assertions (16.4.2): a process resuming after an event control or wait statement; an `always_comb` /
`always_latch` resumed by a transition on a dependent signal; and the outermost scope of the process
being disabled. For violation reports (12.4.2.1) only the first two are listed. One queue, two
clauses, two flush-point sets.

**R5. Cancellation has two granularities.** Disabling the outermost scope of a procedure flushes the
whole queue; disabling a task or a non-outermost scope flushes nothing. Separately, "a specific
deferred assertion may be disabled. Any pending assertion reports for that assertion are cancelled"
(16.4.4). The standard presents the selective form as a user idiom, not an edge case:

```systemverilog
a1: assert #0 (bad_val) else $fatal(1, "Sorry");
if (bad_val_ok) begin
  disable a1;
end
```

**R6. A report's identity is an assertion instance paired with the process that executed it.** A
deferred assertion inside a function belongs to whichever process calls it, and each such execution
is independent, so one assertion can hold pending reports in several processes at once and each is
flushed on its own process's terms (16.4.5). Identity is therefore neither per-assertion nor
per-process.

**R7. Every deferred assertion has an owning process.** One outside procedural code is a static
deferred assertion and is treated as if contained in an `always_comb` procedure (16.4.3).

**R8. The observed form is not glitch-free, and the standard says so.** Because the Observed region
is iterative, a matured report can be followed by a further execution of the same assertion in the
same time step reaching the opposite result; the standard's own example displays both a pass and a
fail for one assertion in one time step. The observed form prevents glitches arising from the order
of procedural execution and not those arising from loops between regions; the final form is the one
that is glitch-free, because the Postponed region does not iterate (16.4.1, 16.4.2).

**R9. Only the action call is deferred, not the evaluation of its arguments.** The subroutine
arguments of an action block are evaluated on each failure even though the action block is not
executed, so their side effects survive a flush that discards the report they were evaluated for
(16.4.1). The standard attaches a caution to this rather than treating it as incidental.

## The tension

A pending report has four axes, owned by four different things:

| Axis                 | Owned by                 | Content                                               |
| -------------------- | ------------------------ | ----------------------------------------------------- |
| When it runs         | the engine               | matures and is scheduled per R2; Observed iterates    |
| What it is           | the language             | assertion instance paired with executing process (R6) |
| Bulk revocation      | the process              | any of the three flush points (R4)                    |
| Selective revocation | the assertion's own name | `disable` of that assertion alone (R5)                |

and a fifth fact that ends the last two: maturity (R3).

The difficulty is not that the item has two owners. It is that **its placement in time belongs to a
global, ordered, append-only structure, while its validity is revocable at two different
granularities by events the structure knows nothing about, until a commit point after which neither
revocation applies.** Any shape that satisfies the placement requirement tends to lose the
revocation requirements, and any shape that satisfies the revocation requirements tends to owe the
engine a way to find the reports again when the region runs.

The standard itself contains both families of answer to this, which is the strongest evidence that
the tension is real rather than an artifact of one implementation:

- **Run when settled.** The final form waits for a region that does not iterate, so nothing it
  reports can be contradicted later in the time step. VHDL's postponed process is the same strategy
  under another name: it executes only in the last delta cycle, so it observes settled signals and
  needs no retraction.
- **Run eagerly and retract.** The observed form reports from a region that may run again, and pays
  for it with the flush machinery -- and, per R8, still does not fully succeed.

## Prior art

The same pair appears outside hardware simulation as the two implementations of a trailing-edge
debounce: delay the action until the input has settled, or act at once and take it back.

Where the second is chosen, the retraction itself splits into two families, and they differ in what
the canceller has to be able to find.

**Notify the holders.** A cancellation source keeps the set of things it can cancel and tells them.
This is `std::stop_token` with its `stop_source`, the `cancellation_signal` / `cancellation_slot`
pair in Asio, and the cancellation contexts of Go and of Rust's async runtimes. Two properties make
the family a poor fit for this subject. It requires the source to hold, and therefore to find, its
dependents -- which is the capability the placement side of the tension takes away. And a stop token
is one-shot: once stopped it stays stopped, whereas a scope a `disable` cancelled can be entered
again and has to be live again when it is.

**Validate at the commit point.** Nothing is notified and no set is kept. The dependent records a
version when it is created and re-reads it when it is about to act; a mismatch means something
invalidated it in between. This is the read-set validation of optimistic concurrency control, and
the fencing token of distributed systems -- a number that only increases, used to reject an
operation issued under an older one. R3's maturity is exactly such a commit point, and R3's rule
that a matured report can no longer be flushed is what a commit means in that family.

The distinction is worth stating precisely, because the vocabulary invites the wrong one: the first
family is what "cancellation token" now means to a C++ reader, so naming a mechanism of the second
family that way promises a notification and a re-arm it does not have.

## What is already settled nearby

[disable-scope-invalidation](disable-scope-invalidation.md) decides how `disable` cancels: a
cancellation source carries a monotonic generation, an execution captures it on entry, and a uniform
validity gate compares the captured generation against the current one before the execution runs its
next statement. Membership is carried by the running process, spans a call, and is captured at a
spawn.

Two consequences bear directly on this subject. First, LRM 16.3 makes a statement label create a
named block around the statement, so `disable a1` naming an assertion is an ordinary disable of a
named block, and R5's selective cancellation names a target that the existing mechanism already
gives a cancellation source and a generation. Second, R4's third flush point is a `disable`, so the
deferred-report flush and the disable model are not two independent cancellation systems that happen
to meet -- one is reached through the other.

The layer this mechanism may not occupy is settled as well. `../architecture/mir.md` does not own
scheduling, and forbids a node kind invented to express a runtime library's shape; an operation on
such a library appears as an ordinary call. It does own the action shape for a construct that binds
behaviour to a schedule event, and forbids a deferred assertion represented as a flag or a lowering
side effect rather than an explicit callable. So MIR states which action is deferred and where it
matures, as a callable and a call; no version, generation, or queue appears at that layer, and every
question below is a runtime question.

## The decision

**D1. The report stays where the engine put it, and withdrawal is a refusal to act rather than a
removal.** It is submitted into the region's own deferred-effect list where the assertion is
reached, and nothing takes it out again. This is what makes the placement side of the tension free:
the report needs no owner that can find it later, and no registry of the processes that hold one.

**D2. A report records the validity it was created under and acts only if all of it still holds.**
Three sources can withdraw it, and each is recorded as it stands at creation: the assertion's own
label block, where it has one; the outermost scope of the enclosing procedure; and the execution
pass of the process that created it. The first is R5's selective form, the second is R5's bulk form
and R4's third flush point -- one event stated by two clauses -- and the third is R4's first two.

**D3. The two scope sources are recorded the way the accepted `disable` model records them**: the
target's cancellation source and the generation captured from it. LRM 16.3 makes a statement label a
named block, so an assertion's label is already such a target and needs nothing built for it. No
second cancellation mechanism appears beside that one, and a `disable` withdraws a report through
the single act it already performs -- advancing its target's generation.

**D4. The process source is recorded differently, and the reason is lifetime, not taste.** A
cancellation source is per-instance storage and outlives every report that captures it, so a report
may hold it and read it later. A process is not: a report can still be pending when its process is
released. The process source is therefore a token whose liveness is the validity, held strongly by
the process and weakly by the report, so a report never reaches for a process that is gone. Two
representations, one rule -- a report acts when every source it recorded still stands.

**D5. Maturity is the commit point and it discards what it validated.** In the maturity region the
report tests its recorded sources once; if they all hold, it is committed to its action region and
the record is dropped, after which neither a `disable` nor a re-execution reaches it. R3 is not a
second flag but the absence of anything left to invalidate.

**D6. What a report records is per construct, which is how one queue serves two clauses.** A
deferred assertion report records all three sources; a violation report records the process
execution pass alone, which is the set LRM 12.4.2.1 lists. R4's mismatch between the clauses is
expressed as what each report captures, never as a second queue or a second flush path.

**D7. MIR states the deferred action and where it matures, and nothing else.** No source, version,
or queue appears at that layer; every part of this decision is the runtime's.

## Consequences

- Selective and bulk cancellation stop being two mechanisms. Both are "a target's generation moved",
  differing only in which target, so neither needs code the other does not.
- Nothing enumerates processes, and no registry of the processes holding a pending report exists to
  keep in step with queuing, flushing, and termination.
- A pending report cannot be inspected. There is no way to ask a process what it currently holds,
  because the set is implicit in which records still validate. Should assertion control (LRM 20.11)
  ever need that enumeration, this is the decision it reopens.
- The observed form's residual glitch (R8) is reproduced rather than removed: a matured report is
  committed, and a later execution in the same time step creates its own. That is what the standard
  describes, and removing it would be divergence.
- R9 is untouched by any of this. The action's arguments are evaluated where the assertion is
  reached, whether or not the report they were evaluated for survives; deferral applies to the call.

## Rejected alternatives

**The process holds its reports, and the engine finds them at maturity.** A container per process:
queuing appends, a flush point clears it, selective cancellation removes the entries naming one
assertion. Both granularities are direct and a process can be asked what it holds. Rejected because
the engine must then find the reports -- a walk over every process, most holding none, or a registry
of those that do, which needs its own upkeep at every queue, flush, and termination. It buys
inspectability that nothing currently requires with a second structure that has to be kept true.

**One validity token per process, and nothing else.** The shape that satisfies R4's bulk flush with
a single act and no per-report state. Rejected because one token per process cannot tell the reports
of one assertion from its neighbours', so R5's selective form has no expression in it at all. This
is what the code did before this decision, and the reachable failure is the standard's own idiom:
`disable a1` after a failing `a1: assert #0 (...)` leaves the report standing and it is issued.

**A notify-style cancellation token.** A source that holds its dependents and tells them, as
`std::stop_token` and Asio's slots do. Rejected on both counts named under Prior art: it requires
the source to find its dependents, which is the capability D1 gives up on purpose, and it is
one-shot, whereas a scope that a `disable` cancelled has to be live again when it is next entered.

## What this does not settle

- The action block (LRM 16.4's single subroutine call) and, with it, where R9's argument evaluation
  is realized. Until it exists a deferred assertion carrying one is refused.
- The deferred `cover` form, likewise refused.
- Whether the region fixes any order between the reports of different processes. D1 leaves the
  question moot, since every report sits in the region's own list and takes whatever order it gives.

## Cross-references

- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- the other
  construct that queues a deferred report; settles what such a report says, and leaves its placement
  to LRM 12.4.2.1.
- [disable-scope-invalidation](disable-scope-invalidation.md) -- the generation-and-gate
  cancellation model that R4's third flush point and R5's selective form both reach.
- `../architecture/scheduling.md` -- the region order the maturity placements of R2 are stated in.
