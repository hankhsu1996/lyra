# What evaluates a concurrent assertion

Date: 2026-09-09. Status: accepted, for concurrent assertions (LRM 16.5 through 16.14) and the
expect statement (LRM 16.17) evaluated against a single clocking event.

## Why this decision matters

Every construct the compiler carries so far answers where it stands. A concurrent assertion does
not: one statement starts a new evaluation at every tick of its clock, each of those evaluations
spans an unbounded number of later ticks, and any number of them are in flight at once. So the
question is not how to test an expression -- it is what an evaluation in progress _is_, what holds
it between ticks, and what advances it.

That shape has two well-established answers with a real trade-off between them, and picking the
wrong one is expensive in the way an execution model is expensive: the operator set, the reporting,
and the per-tick cost of every design that carries assertions all inherit it.

## Findings that shaped the decision

### F1. One formula covers both placements, and an enabling condition is the whole difference

The standard gives concurrent assertions a formal semantics in Annex F, and its top-level definition
takes an assertion statement, a trace `w`, and a Boolean `b`:

```text
w, b |= always @(c) assert property T  iff  for every i so that w_i |= c and w_i |= b,
        either w_i.. |= @(c) T or w_i.. |=d @(c) T
```

`b` is named there as the **enabling condition** of the assertion statement: "b is derived from the
conditions causing a queued evaluation attempt of a procedural assertion statement (see 16.14.6),
while b is 1 for a declarative assertion statement."

So an attempt begins where the clock ticks **and** the enabling condition holds, and that is the
only respect in which the two placements differ. The queue LRM 16.14.6 describes is not a second way
to start an attempt: it is how a procedural assertion's `b` is observed, because `b` there is "did
control reach this statement", which is no function of the trace.

Attempts therefore overlap, and each carries its own result -- LRM 16.14.3 counts a cover
statement's attempts and its successes separately, at most one success per attempt. Two attempts
that begin at different ticks can resolve at the same tick, and each runs its own action block.

### F2. A sequence is a regular expression, and a property is a recursive structure over sequences

LRM 16.7 defines a sequence as "a regular expression over the SystemVerilog Boolean expressions"
that concisely specifies a set of linear sequences, and an attempted evaluation as "a search for a
match of the sequence beginning at a particular clock tick". LRM 16.12 defines each property
operator by what its operands evaluate to, and states that the result of property evaluation is
either true or false.

The alphabet is not a character but the valuation of the design's Boolean expressions at a tick,
which LRM 16.5.1 fixes as their sampled values. Nothing else about the shape differs from a regular
language.

### F3. The two quantifiers in the formal semantics decide what may be pooled

Two questions look alike and are not, and Annex F writes them with different quantifiers.

A sequence's own matching is existential -- `w |= (R1 ##1 R2)` holds if there **exist** x and y with
`w = xy` matching the two operands, and `strong(R)` holds if there **exists** a prefix matching `R`.
So the paths through a sequence contribute to one disjunction, and two of them that reach the same
point have the same future. Taking their union loses nothing.

An implication is universal, and each of the quantified cases is a satisfaction question about a
**different suffix** of the trace:

```text
w |= (R |-> P)  iff  for every j so that w_0,j |= R,  w_j.. |= P
```

A conjunction is not preserved by union. An instance that dies makes the implication false, and once
its positions are pooled with a sibling's there is nothing left to say which of them died.

That is decisive rather than academic. In `a ##[1:2] d |-> b` one attempt can match its antecedent
at two ticks and so run two evaluations of `b`, one a tick behind the other. If `b` is low at the
first and high at the second, the attempt is false; pooled into one position set, the surviving
evaluation's match is indistinguishable from the dead one's and the attempt reads as true.

Across attempts the join is neither: from F1 each attempt reports on its own, so merging two of them
loses a report the standard requires.

### F3b. A finite trace has four answers, not two

Annex F evaluates a property against an infinite word, so an attempt does not "finish" there. What a
tool checking a **finite** word is told to answer is one of four: "holds strongly", "holds (but does
not hold strongly)", "fails", and "pending". Disabling is a separate relation beside all of them,
and the top-level definition in F1 is satisfied by either passing or being disabled.

Weak and strong are not two ways of evaluating: a sequence still able to match holds where it is
weak and is pending where it is strong, and that is the whole difference. Pending and holds are the
two answers the rest of the trace can still change, so an attempt answering either is still in
flight, and one the run ends in the middle of answers one of them.

### F4. Evaluation is in Observed and the action is in Reactive

LRM 16.5 evaluates concurrent assertions in the Observed region. LRM 16.14.1 executes the pass and
fail statements of an `assert` in the Reactive region, and LRM 16.14.3 does the same for a cover
statement's pass statement. The two regions are already in place and already carry work of this
shape: a deferred immediate assertion's report matures in Observed and acts in Reactive
([deferred-report-queue](deferred-report-queue.md) D5).

### F5. What observes a procedural enabling condition already exists

F1 leaves one thing to build: how `b` is observed where it is "did control reach this statement".
LRM 16.14.6 answers it. Reaching the statement places a pending instance on a queue belonging to the
executing process, saving the values of the constant and automatic expressions in its arguments; the
instance matures in the Observed region of each time step, and the attempt begins at the tick of the
leading clock. LRM 16.14.6.2 gives three flush points that clear the queue -- reaching one means
control did not stay in the state that enabled the assertion -- and they are the three LRM 16.4.2
gives for a deferred immediate assertion.

That is the mechanism [deferred-report-queue](deferred-report-queue.md) already decides, down to the
sources that withdraw an entry and the maturity that discards what it validated. What differs is
what maturity does with the entry.

### F6. The clock and the sampled values are already answered

The clocking event a concurrent assertion counts ticks of is the one its property leads with, and
where the source writes none, the one in force where the statement stands -- the clock the procedure
settles (LRM 16.14.6), and otherwise the scope's default clocking (LRM 14.12). A property or
sequence declared in a clocking block leads with that block's event (LRM 16.16 b). The front end
reports the procedure's clock rather than the inputs to it
([sampled-value-and-its-clock](sampled-value-and-its-clock.md) D2, F3). What an assertion's Boolean
expressions read is the sampled value of LRM 16.5.1, which the same decision retains on the cell.

The exception is the disable condition: LRM 16.12 evaluates it over current values, not sampled
ones, and tests it from the start of the attempt in the Observed region through the end of the
evaluation attempt inclusive. A disabled evaluation is a third outcome -- neither success nor
failure -- and runs no action block statement (LRM 16.14.1).

## The decision

### D1. Three levels, and F3 decides which of them may be pooled

The property is a finite automaton over positions, and what is in flight has three levels, each
carrying the one below because the join between them differs:

- **An evaluation is a set of positions.** It asks whether some path matches, so the union of paths
  is exactly the question, and two paths reaching one position are one thing from then on.
- **An attempt is a tree of evaluations.** It starts with one, answering for the whole property, and
  every match of an implication's antecedent adds another -- an instance of the consequent, seeded
  at its start positions at the tick of the match, and counted in that implication of the evaluation
  whose antecedent matched. F.3.4.3 writes `|=>`, the followed-by operators, `if` and `case` in
  terms of `|->`, so each of those is counted as an implication too. `R |=> P` is `(R ##1 1) |-> P`
  (F.3.4.3.3), so the tick after a match is a position of the antecedent, and every instance starts
  at the tick that seeded it. An implication quantifies universally over its instances, so no two of
  them are ever pooled. The operands of a negation, a conjunction, a disjunction, an `implies` or an
  `iff` start together and live as long as the property they compose, so they are positions of one
  evaluation rather than evaluations of their own. A selection -- `if` or `case` -- reads what it
  selects by once, at its start, and is then the one arm it seeds.
- **An assertion holds its attempts.** One begins at every tick, each resolves on its own, and each
  runs its own action.

Advancing is one function at every level: an evaluation's next position set is computed from the
sampled values of the Boolean expressions its outgoing positions read, and the same function serves
every evaluation of every attempt, because they differ only in which positions are live.

**What an evaluation answers is one of F3b's four answers, and they are ordered.** The two ends are
settled: no continuation of the trace can satisfy a property that fails, or fail one that holds
strongly. The two between are what the trace as it stands says, read as F.5.3.1 reads a finished
trace: pending where it does not satisfy the property, holds where it does. Negation reverses the
order; a conjunction is the least of two answers and a disjunction the greatest; `implies`, `iff`,
`if` and the followed-by operators are what F.3.4.3 derives them as. These are not F.5.3.2's
verdicts on the trace extended by letters satisfying everything or nothing: extended by letters
satisfying nothing, an implication whose antecedent may still match holds strongly, yet a later
match can still fail it, so an answer reported on that reading could be withdrawn. A sequence
answers holds strongly once it matched and fails once no path is left; still live, it holds where it
is weak and is pending where it is strong (LRM 16.12.2). An implication answers the least of its
instances' answers and its antecedent's own: holds while the antecedent may still match, holds
strongly once it cannot -- so a run ending before the tick after a nonoverlapped antecedent's match
leaves that antecedent still able to match and the implication holding. Its instances' answer is the
least any of them reached, those that settled and left as well as those still live. The code
computing each answer is fixed with the property, so it is generated, per part of the property an
evaluation can answer for, as D4's transition is.

An attempt reports once its answer is settled and not before: every way of combining answers keeps
the two ends of the order, so an answer of fails or holds strongly can no longer change. It fails at
the first tick its answer is fails and holds at the first tick it holds strongly, and until then it
is in flight. Combining the operands' answers rather than the whole property's can settle an attempt
a tick later than the earliest tick no continuation could change it --
`strong(##1 a) and strong(##1 !a)` fails at the tick `a` is read, not the one before -- and never
earlier.

A run that ends with an attempt still in flight is the one case no tick resolves, and its answer as
it stands is the neutral verdict on the finished trace (F.5.3.1), which is what the clause's own
example reads: `assert property (@clk not a ##1 b)` fails when the clock stops after `a` (LRM
16.12.3). So the attempt succeeded where its answer is at least holds and failed otherwise, for
every statement: a weak obligation still able to match holds, a coverage goal's strong sequence
still able to match is not covered, and a `strong` sequence written in an assertion fails.

That is what the attempt came to; what is run for it is a second question, and the standard answers
it only through the first. LRM 16.14.1 runs the pass statement of an attempt that succeeds and the
fail statement of one that fails, and the clause above calls the assertion failed, so the statements
the answer selects are run, as they are mid-trace. What does not carry over is the region: no time
step is left to hold a Reactive region, so they are started once the run has stopped and before the
`final` procedures, which read what they wrote. An attempt whose clock tick shares a time step with
the `$finish` that ends the run is not evaluated at that tick, since the run never reaches that
step's Observed region (LRM 16.5, 20.2); it is judged by its answer at the tick before, and one that
would have begun there never begins.

### D2. The automaton is built at HIR-to-MIR, and HIR carries the standard's own structure

HIR holds the sequence and property tree as LRM 16.7 and 16.12 write it, because that is what the
user wrote and HIR's whole identity is that "what did the user write" has one answer
(`../architecture/hir.md`). Turning the tree into an automaton is exactly the work a lowering exists
to do: after HIR-to-MIR no layer needs to know that a regular expression was ever there.

The automaton is a compile-time artifact. Every range bound and repetition count in a sequence is an
elaboration-time constant (LRM 16.7), and a parameter that changes one changes the specialization,
so nothing about the automaton's shape waits for run time.

### D3. What advances attempts is a synthesized process, one per assertion and clocking event

The process waits on the clocking event and, at each tick, submits one closure to the Observed
region of that time step. The closure starts a new attempt, advances every live one, and for each
attempt that resolved submits its action to the Reactive region, where it starts as an execution of
its own: an action is any procedural statement (LRM 16.3, 16.14.1), so it may wait, and two attempts
resolving together owe two actions that run side by side. This is the shape a sampler already has
([sampled-value-and-its-clock](sampled-value-and-its-clock.md) D3): an action bound to a schedule
event, waiting on it as any `always` does.

Waking is not evaluating. The process wakes where any value-change wait wakes, and the tick's work
goes to Observed as a deferred effect, which is what places evaluation in the region F4 requires
without the engine learning what a concurrent assertion is.

### D4. The transition is generated code and the attempts are runtime storage

The part that is fixed at compile time is the transition and the answer: code taking a position set
to its successor, starting the instances its matches seed, and computing what each part of the
property answers -- built from the design's own Boolean expressions read as of Preponed. The part
that varies at run time is how many attempts are live and how many evaluations each carries, which
is member storage holding one position set per evaluation.

Nothing interprets a table. A backend renders the transition the way it renders any other callable,
and the storage the way it renders any other member -- which is what keeps the whole of this
mechanical (`../architecture/backend_contract.md`).

### D5. An attempt begins where the clock ticks and the enabling condition holds

There is one start rule, F1's, and where an assertion is written decides only where its `b` comes
from. A declarative assertion's `b` is 1, so every tick starts an attempt and nothing else is
needed. A procedural assertion's `b` is whether control reached the statement, which is no function
of the trace, so it is observed rather than evaluated -- through the queue F5 names, which is the
queue [deferred-report-queue](deferred-report-queue.md) D1 through D6 already decide. The entry is
submitted into the region's own list where the statement is reached, records the sources that can
withdraw it -- the assertion's own label block, the outermost scope of the enclosing procedure, and
the creating process's execution pass -- and acts only if all of them still stand. Its withdrawal is
`b` turning out not to have held.

What a matured entry does is start an attempt rather than issue a report. The values of the constant
and automatic expressions LRM 16.14.6.1 requires saved are the entry's own by-value captures, which
is what a deferred effect's closure already is.

**Where the assertion sits in HIR follows from this and not from the grammar.** An enabling
condition that is a position in the control flow can only be recorded by being at that position, so
a procedural assertion is a statement; one whose `b` is 1 has no position to record, so a
declarative assertion is a declaration of the scope. Neither has an evaluation path the other does
not.

### D6. Strength belongs to each sequence, and the statement only gives the default

Revised 2026-10-23. LRM 16.12.2 reads a bare sequence in an `assert` or `assume` as `weak`, and in a
`cover` as `strong`; `strong(s)` and `weak(s)` say it outright, and `not` keeps each sequence's own
strength while exchanging what that strength demands (LRM 16.12.3). So strength is a fact of each
sequence, stated where the property is translated into HIR, and what it decides is what that
sequence answers while still live: holds where weak, pending where strong (D1). The statement
decides nothing else: an attempt is reported by its answer, mid-trace at the answer's two ends and
at the end of a run by whether it reaches holds (`../architecture/scheduling.md`).

This replaces a reading in which the statement's demand was one comparison made at the end of a run
for every pending attempt. That reading was derived while every sequence carried its statement's
strength and implication was the only operator combining evaluations, and under it a `strong`
sequence written in an `assert` that a run ended in the middle of passed, where the finished trace
fails it.

### D7. No layer gains a node kind

The automaton is code; the attempts are a member; the process is a callable plus an action
registration; the two region submissions are the placements a deferred effect already takes. The
semantic IR gains one member type and a few runtime entries, and the execution-model IR gains
nothing -- the same result [sampled-value-and-its-clock](sampled-value-and-its-clock.md) D6 reached,
for the same reason: a mechanical backend can translate a call and a member without deciding
anything, and would have to invent an expansion for a node that states neither.

### D8. An expect's one attempt is held and advanced by the wait its process stops at

Added 2026-10-17. LRM 16.17 makes `expect` a wait: the process reaching it starts a single attempt
at the next tick of the clock, blocks until that attempt succeeds or fails, and runs the statement
after it once the Observed region in which the property completed is over. Three facts decide the
shape. The property may read the process's automatic variables and `ref` formals, at their values as
of the Observed region (LRM 16.5.1), so the evaluation reads that process's frame. Every occurrence
of the clock is a tick, and each is evaluated in the Observed region of the time step it happened in
(LRM 16.5) -- for a program's process too, which is woken only in Reactive, after that region. And
when the process stops waiting -- answered, or stopped from outside -- "the property stops being
evaluated", so the evaluation lives exactly as long as the wait.

So the wait is what watches the clock and the disable condition while the process is parked, and an
occurrence wakes nobody: a tick is handed to the Observed region of now, and a change to what the
disable condition reads to the Active region after it, each through a queue node the wait holds.
What runs there is code of the waiting body, bound to its frame, handed to the wait once the wait
exists; the wait is a local of that frame, so ending the scope ends the wait and takes the queued
work off its queue with it. The tick is the same body a declared assertion's tick runs over D1's
attempt. The process is woken once, by the answer, into the region its own set runs woken work in,
which is after that Observed region; a disabled attempt answers nothing, so its process is not woken
at all. Nothing is registered for the end of the run, since a process still waiting then never goes
on. This is D3's division -- waking is not evaluating -- with the wait in the synthesized process's
place, which a scope's member cannot take: it reaches no frame, and would be shared by every call of
an automatic task holding the statement. Waking the process at each tick to step the attempt itself
was built first and rejected: a program's process is woken after the Observed region it should have
been evaluated in, and a second tick in that time step comes while the process is parked for the
first. A clocking event only the waiting process could decide (one that calls or reads through a
handle) has nothing to watch it and is refused. A sequence standing as the property is read strongly
(Annex F.3.4.3.1).

## Consequences

- A design carrying no concurrent assertion pays nothing: no process exists, no member is declared,
  and no region gains an entry.
- The per-tick cost is proportional to the evaluations actually in flight, and nothing is allocated
  for an attempt that fails at the tick it starts -- which is what an antecedent that does not match
  looks like, and is the common case in a real design.
- Where an assertion's Boolean expressions are the whole property, an attempt starts with one
  evaluation and resolves in one tick, so the general path answers the degenerate one with no case
  of its own.
- **A local variable (LRM 16.10), and a subroutine call attached to a match (LRM 16.11), are outside
  this model rather than unimplemented within it.** Both make two paths that reach the same position
  distinguishable, which is exactly the premise the innermost of D1's three levels rests on;
  carrying them means an evaluation holds a set of environments rather than a set of positions. Each
  is refused by name.
- Multiclock sequences and properties (LRM 16.13) are outside it for a different reason, and a
  weaker one. Annex F defines a clocked sequence by rewriting it into an unclocked one over time
  steps, so more than one clock is a property of that rewrite rather than of the model underneath.
  What one clocking event per assertion buys is that a tick is a single advance, which is the whole
  of why the restriction is here; lifting it is a rewrite this lowering does not yet perform.
- The Reactive region stops being unreachable for the assertion family, and the assertion checking
  policy gains an answer for the concurrent form, which is what ends a design carrying both forms
  having no setting that checks either.
- Assertion control (LRM 20.11) will need to reach an attempt in flight and an assertion's counters.
  Neither exists here, and the storage D4 declares is where both would live.

## Rejected alternatives

- **An attempt is a coroutine.** The engine already has coroutines, spawning, and event suspension,
  so an attempt could be a spawned process that awaits ticks and walks the property, and it is the
  shape a checker written by hand has. Rejected on two counts. It allocates a coroutine frame per
  assertion per tick, where the overwhelmingly common attempt does one Boolean test and ends. And a
  sequence's non-determinism -- a delay range, a repetition range, a disjunction -- has no
  expression in a single coroutine, so each becomes a sub-thread that has to be joined by "any
  succeeded": that is D1's automaton rebuilt out of scheduler objects, at a cost per tick rather
  than per compile.

- **One merged position set for the whole assertion, advanced like a state register.** The shape an
  independent implementation reaches, and the cheapest possible per tick: one bit per position, no
  per-attempt storage at all. Rejected by F3 twice over. It pools across attempts, so two attempts
  that fail at one tick report once and an attempt that has drained cannot be told from one that
  never started. And it pools the evaluations inside an attempt, which is where a conjunction is
  being asked and a union cannot answer it.

- **A transition table the runtime interprets.** It makes the automaton data, which is smaller to
  emit and lets one runtime entry serve every assertion. Rejected because the table's entries have
  to name the design's Boolean expressions, so either the table holds closures -- which is the
  generated code of D4 with a dispatch in front of it -- or the runtime learns to evaluate the
  design's expressions, which is an interpreter inside a compiler.

- **Building the automaton at AST-to-HIR.** It would save carrying the sequence and property tree at
  all. Rejected: HIR would stop being the answer to what the user wrote, and every diagnostic about
  a sequence would then have to be recovered from a graph.

- **A MIR node kind for a sequence, a property, or an attempt.** It reads as the honest way to say
  the program has an assertion. Rejected by `../architecture/mir.md`: MIR's vocabulary is a generic
  programming language's, a node preserving source-language structure as an opaque payload is
  forbidden outright, and the falsifier applies directly -- a mechanical backend can translate the
  call and the member D4 produces, and would have to invent an expansion for a node that carries a
  regular expression.

- **Evaluating where the process wakes.** The process is resumed in the Active region and could
  simply do the work there, saving a submission per tick. Rejected: LRM 16.5 places evaluation in
  Observed, and the difference is observable -- the values a design writes in Active and NBA are
  exactly what the region order exists to settle before an assertion looks.

- **Deriving the clocking event here.** Rejected for the reason
  [sampled-value-and-its-clock](sampled-value-and-its-clock.md) already gives: the front end answers
  it, and a second implementation of a name-resolution rule is a second thing to keep correct, with
  nobody positioned to see the two disagree.

## Cross-references

- LRM anchors: 16.5 (evaluation at a clock tick, in Observed), 16.5.1 (sampled value), 16.6 (Boolean
  expressions and what they may not read), 16.7 (a sequence is a regular expression; an attempt is a
  search for a match), 16.9 (sequence operations), 16.12 (declaring properties, `disable iff`, weak
  and strong, the operator semantics), 16.12.3-16.12.6, 16.12.8, 16.12.9 and 16.12.16 (the operators
  composing properties), 16.12.7 (implication, and the separate evaluation per antecedent match),
  16.14 (the assertion statements), 16.14.1 (the action block, and the Reactive region), 16.14.3
  (the cover counters), 16.14.5 (`always` semantics outside procedural code), 16.14.6 (the
  procedural assertion queue, maturity, and clock inference), 16.14.6.2 (the flush points).
- Annex F is the formal semantics this decision is derived from, and outranks a reading of the
  clauses above where the two seem to differ: F.4.1 (a named sequence or property instance is its
  body with the actuals substituted), F.5.1 (a clocked sequence is defined by rewriting it into an
  unclocked one), F.5.2 (tight satisfaction, where a sequence's operators are existential), F.5.3.1
  (the top-level definition, its enabling condition `b`, and implication's universal quantifier),
  F.5.3.2 (the four answers a finite trace admits), F.3.4.3 (the derived property operators),
  F.5.3.3 (vacuity).
- [sampled-value-and-its-clock](sampled-value-and-its-clock.md) -- the values an assertion reads,
  the clocking event it counts ticks of, and the synthesized-process shape D3 reuses.
- [deferred-report-queue](deferred-report-queue.md) -- the pending-entry mechanism D5 is an instance
  of, and the Observed-then-Reactive placement pair.
- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- the immediate
  family's report, and the precedent that a construct whose whole observable is a message is still
  decided by what its statement evaluates.
- `../architecture/scheduling.md` -- the region order, deferred effects as closure submissions, and
  where a run ends.
- `../architecture/backend_contract.md` -- the mechanical-translation contract D4 is written
  against.
