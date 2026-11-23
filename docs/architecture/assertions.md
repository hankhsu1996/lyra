# Assertions

The compiler does not yet carry out the whole of the concurrent assertion model below. Its automaton
gives every position exactly one tick and has no cycle, so lowering refuses by name what needs more:
a property with more than one clocking event, which it does not rewrite as F.5.1 defines; an
assertion local variable (LRM 16.10) and a subroutine call attached to a match (LRM 16.11); the
sequence operators `and`, `or`, `intersect`, `within`, `throughout` and `first_match`; a `$` bound
on a delay or a repetition, goto and nonconsecutive repetition, and a repetition that can match
empty; the property operators over time and the abort operators; and `cover sequence`. The rules
below bind each of them regardless.

Every construct the compiler carries answers where it stands, except the ones this doc is about. A
deferred assertion acts later in the time step it was reached in, and may be withdrawn before it
does. A concurrent assertion starts a new evaluation at every tick of its clock, each spanning an
unbounded number of later ticks, any number of them in flight at once, all reading values as of a
moment no reader chose. And the assertion control tasks reach statements and executions anywhere in
the hierarchy while they run. So the questions here are what an evaluation in progress is, what
holds it between ticks, what advances it, what withdraws it, and what a control reaches.

This doc covers the immediate assertions, simple and deferred (LRM 16.3, 16.4); the violation
reports of `unique`, `unique0` and `priority` (LRM 12.4.2.1), which share the deferred report's
path; the concurrent assertions, declarative and procedural (LRM 16.5 through 16.16), and the
`expect` statement (LRM 16.17); the sampled values they and the sampled value functions read (LRM
16.5.1, 16.9.3); and the assertion control tasks (LRM 20.11). The regions all of this lands in, and
the executions of no lineage an action runs as, are [scheduling.md](scheduling.md)'s. A clocking
block reads the same sampled values for its `#1step` inputs and supplies a default clock (LRM 14.12,
14.13), and is otherwise not this doc's subject.

## Sampled values, and the clock they are sampled against

A sampled value function and a concurrent assertion read a value as of the Preponed region of a
clocking event (LRM 16.5.1), before anything in that time step ran, so the value has to have been
taken before its reader existed and kept until it asks; `$past` keeps several.

**A sampled value belongs to the variable or the net.** LRM 16.5.1 defines the sampled value of a
variable as its value in the Preponed region and that of an expression recursively over its
operands, a call being made on the sampled values of its arguments. A net is sampled the same way,
its default sampled value being its type's default (LRM 6.8, Table 6-7). Three things answer with
their current value instead -- an automatic variable, an assertion local variable and a `const'`
cast -- and the exceptions are stated on the variable or the cast, so an expression mixing them is
composed over each part's own answer: `$past(b[i])` is the past `b` at the current `i`.

**It is taken in the Postponed region of the slot the place changed in.** Sampling in Preponed is
equivalent to sampling in the previous Postponed region (LRM 4.4.2.1), and nothing changes once
Postponed is reached (LRM 4.4.2.9), so the value taken there answers every Preponed read until the
place changes again. An armed variable or net is enrolled on its place as a wait is -- a net's
through the member naming where its value lies, so it follows the net onto the object one driver
carries and back -- and told of the slot's first change, it submits the taking into that slot's
Postponed region. It wakes nothing. Arming installs the default sampled value -- what a variable
holds when armed, the type's default for a net -- and happens in Activate, after Initialize has run
the declaration assignments the default is defined by (`elaboration_lifecycle.md`). A place nothing
samples, and a slot a sampled place does not change in, cost nothing. A pass over Preponed that
snapshots every armed place fails the bound the engine is built on, that work follows the changes
made rather than the slots that pass. A hook before every write fails the net: what it shows changes
through resolution, forces and the object its driver carries, none of which writes the net, and the
wait enrolment already follows all of them.

**A clocking event is an event control.** What a sampled value function or an assertion samples
against is the trigger set any value-change wait builds, translated as an `@(...)` is. Where the
source writes no clock, LRM 16.9.3 orders the rules that infer one. The procedure's rule (LRM
16.14.6) is answered by the front end's analysis of the procedure, which this compiler reads and
does not restate; the rest -- clock flow through a property (LRM 16.13.3), an action block's leading
clock, the default clocking outside a procedure (LRM 14.12), a property declared in a clocking block
(LRM 16.16) -- the front end checks and does not record, so they are applied where the assertion is
lowered, over clocking events the front end built. Where no rule yields a clock the construct is
refused with the diagnostic the standard requires, never sampled against a guess.

**A history is kept by a sampler, a synthesized process.** A clocking block is specified like an
always procedure (LRM 14.7), and a history has to record every tick whether or not anything was
waiting when it happened, which an observation armed by a reader does not. So each sampled value
function that keeps anything has a process of the instance its body belongs to, waiting on the
function's clock through its own loop as any `always` does, which takes the sampled value of each
piece it keeps and commits it in the Postponed region of the tick's time step. `$past(e, k)` reads
the kth strictly prior tick (LRM 16.9.3), so committing at the end of the step means the history
cannot hold the current step's tick, and every reader agrees whichever order it ran in beside the
sampler. Committing at the tick fails that: whether a reader skips the newest entry would depend on
an order within one region that nothing fixes (LRM 4.7).

**What is kept is each piece of the operand that has a past.** `$past(e)` is `e` evaluated where the
call stands as of the tick it names: every static variable it reads as of that tick, every
automatic, constant cast and value a procedural assertion saved as it is now. So a value per tick is
kept for each largest piece of `e` that reads a sampled variable and nothing read as now, and makes
no call: operators and selects compute the same over a tick's values whenever they run, while a call
may answer from the state of the run, so it is made where `e` is read, on its arguments' past values
(LRM 16.5.1). `$past(mem[addr])` with a static `addr` keeps one element per tick, `$past(mem[i])`
with an automatic `i` keeps `mem`, `$past(const'(x))` keeps nothing. A piece read inside another
sampled value function of the operand is kept as that function's history holds it, at each tick of
the outer function's clock, so functions nest across clocks with every history of a fixed depth.

Each history is static storage of the body the function is written in, or of the scope where it
stands among the scope's own expressions, so it lives where that body's statics live and its sampler
reaches what they reach. Its depth is the tick count, an elaboration-time constant, and it is
installed full of each piece's default sampled value, which LRM 16.9.3 requires before the kth prior
tick exists, so it has no empty case and no counter. A read names the entry where the history keeps
it, so `$past(mem[i])` reaches one element of the kept `mem`. A clocking block input is read, by
every sampled read, as the value its block samples at the event (LRM 16.5.1, 14.13), computed as the
block computes it rather than read from the input's variable, which the block writes later in the
same step. Where the value is a handle the history keeps references, so sampling extends an object's
life by at most the declared depth per history: `$past` has to answer with a handle a member can be
reached through (LRM 8.4).

A design that samples nothing pays nothing: no place keeps a sampled value, no sampler exists, and
no write path grows a test.

## Concurrent assertions

### An attempt begins where the clock ticks and the enabling condition holds

Annex F.5.3.1 defines a concurrent assertion over a trace `w` and an enabling condition `b`: it
holds iff for every tick `i` where `b` holds, the property holds or is disabled on the suffix from
`i`. `b` is 1 for a declarative assertion, and for a procedural one it is "derived from the
conditions causing a queued evaluation attempt" (LRM 16.14.6). That is the only respect in which the
two placements differ, and so there is one start rule.

A declarative assertion is the same statement standing in an `always` procedure (LRM 16.14.5),
reached at every tick, so its clock's process is where its attempt matures. A procedural assertion's
`b` is whether control reached the statement, which is no function of the trace, so it is observed
rather than evaluated, through the queue LRM 16.14.6 describes. That queue is the deferred report's
(below): the entry is submitted into the region's own list where the statement is reached, records
the sources that can withdraw it -- the assertion's own label block, the outermost scope of the
enclosing procedure, and the creating process's execution pass, whose flush points LRM 16.14.6.2
names -- and acts only if all of them still stand. What a matured entry does is start an attempt
rather than issue a report. **Where an assertion sits in HIR follows from this, not from the
grammar:** an enabling condition that is a position in the control flow is recorded by being at that
position, so a procedural assertion is a statement; one whose `b` is 1 is a declaration of its
scope. Neither has an evaluation path the other lacks.

The values LRM 16.14.6.1 requires saved belong to the attempt: it reads them for the whole of its
evaluation, later ticks and its action block included. So the entry captures them where the
statement is reached, as a lambda's by-value captures are taken, and its attempt holds them: an
automatic variable of the procedure as the value it holds -- a read and a `const ref` argument both
name that value, and a write to one is refused by the front end -- and a constant cast as the value
it makes. A static variable is read as its sampled value, and the sampled value of a constant cast
is its argument's current value (LRM 16.5.1). A static assertion and an `expect` save nothing.

Whether a matured attempt begins now or waits is answered against the time step, by the storage that
holds the attempts, which alone knows whether the step has ticked. At a tick the storage opens every
matured attempt and steps each; an attempt maturing after the tick in the same step is stepped then,
alone; one matured before the tick, or in a step with none, waits on the matured assertion queue
(LRM 16.14.6). A matured instance is past every flush point and every `disable` and is no attempt
yet, so a disable condition coming true between ticks preempts only the attempts in flight (LRM
16.12, 16.14.6.4). It begins only where checking is on when its tick comes (LRM 20.11), and a tick
where the disable condition holds discards it.

### Three levels, and what may be pooled

A sequence is a regular expression over the sampled valuations of the design's Boolean expressions
(LRM 16.7), so a property is a finite automaton over positions. What is in flight has three levels,
each carrying the one below, because Annex F writes the joins between them with different
quantifiers:

- **An evaluation is a set of positions.** A sequence's match is existential (F.5.2): two paths
  reaching one position have one future, so their union loses nothing.
- **An attempt is a tree of evaluations.** It starts with one, answering for the whole property, and
  every match of an implication's antecedent adds another, an instance of the consequent seeded at
  the tick of the match. Implication is universal over its instances (F.5.3.1), and a conjunction is
  not preserved by a union: in `a ##[1:2] d |-> b` one attempt may run two evaluations of `b` a tick
  apart, and pooled, the survivor's match would hide the dead one's failure. So no two instances are
  pooled. `|=>`, the followed-by operators, `if` and `case` are written in terms of `|->` (F.3.4.3)
  and counted as implications; `R |=> P` is `(R ##1 1) |-> P`. The operands of a negation,
  conjunction, disjunction, `implies` or `iff` start together and live as long as what they compose,
  so they are positions of one evaluation. A selection reads what it selects by once, at its start,
  and is then the one arm it seeds.
- **An assertion holds its attempts.** One begins at every tick, each resolves on its own and runs
  its own action (LRM 16.14.3 counts attempts and successes apart), so merging two loses a report
  the standard requires.

Advancing is one function at every level: an evaluation's next positions are computed from the
sampled values of the Boolean expressions its live positions read, the same for every evaluation of
every attempt. A single merged position set per assertion, advanced like a state register, fails
both joins above.

The three levels rest on two paths reaching one position being indistinguishable. A local variable
(LRM 16.10) or a subroutine call attached to a match (LRM 16.11) makes them distinguishable, and an
evaluation then holds a set of environments rather than a set of positions. One clocking event per
assertion is what makes a tick a single advance; a multiclock property (LRM 16.13) is defined by
rewriting it into an unclocked one over time steps (F.5.1), which lowering performs before this
model is reached.

### What an evaluation answers, and when an attempt reports

A finite trace admits four answers (F.5.3.2): holds strongly, holds, pending, fails. They are
ordered, and the two ends are settled: no continuation can satisfy what fails or fail what holds
strongly. The two between are what the trace as it stands says, read as F.5.3.1 reads a finished
trace. Negation reverses the order; a conjunction is the least of two answers and a disjunction the
greatest; `implies`, `iff`, `if` and the followed-by operators are what F.3.4.3 derives them as. A
sequence answers holds strongly once it matched and fails once no path is left; still live, it holds
where it is weak and is pending where it is strong. An implication answers the least of its
instances' answers -- those settled and gone included -- and its antecedent's own: holds while the
antecedent may still match, holds strongly once it cannot. Reading F.5.3.2's extensions by letters
satisfying everything or nothing instead would report answers a later match can withdraw.

**Strength belongs to each sequence; the statement only gives the default.** LRM 16.12.2 reads a
bare sequence as `weak` in an `assert` or `assume` and `strong` in a `cover`; `strong(s)` and
`weak(s)` say it outright, and `not` keeps each sequence's strength while exchanging what it demands
(LRM 16.12.3). Strength is stated per sequence where the property is translated into HIR, and
decides only what that sequence answers while live.

An attempt reports once its answer is settled and not before, since every combination keeps the two
ends of the order: it fails at the first tick its answer is fails and holds at the first tick it
holds strongly (LRM 16.14.3). Combining operands' answers can settle an attempt a tick later than
the earliest tick no continuation could change it, never earlier. A run that ends with an attempt in
flight is the one case no tick resolves: its answer as it stands is the neutral verdict on the
finished trace (F.5.3.1), which is the clause's own reading of `not a ##1 b` when the clock stops
after `a` (LRM 16.12.3). So it succeeded where its answer is at least holds and failed otherwise,
for every statement, and the statements its answer selects run once the run has stopped and before
the `final` procedures, which read what they wrote, since no time step is left to hold a Reactive
region. An attempt whose tick shares a time step with the `$finish` that ends the run is not
evaluated at that tick, since the run never reaches that step's Observed region (LRM 16.5, 20.2).

Each answer carries whether the evaluation was nonvacuous, computed beside it operator by operator
by LRM 16.14.8's rule: a sequence is, a negation is where its operand is, a conjunction, a
disjunction and `iff` are where either operand is, `implies` is where its left operand holds and
both are, and an implication is where one of its instances is. So `implies` and `iff`, which answer
as other operators do and differ in vacuity, are terms of their own. Whether an operand holds is
read on the trace so far, as every answer is: a weak left operand of `implies` still able to match
holds where a strong one does not. An immediate assertion has no vacuous success.

### Where it runs

**What advances attempts is a synthesized process, one per assertion and clocking event.** It waits
on the clocking event and, at each tick, submits one closure to the Observed region of that time
step (LRM 16.5), which opens the matured attempts, advances every live one, and for each that
resolved submits its action to the Reactive region (LRM 16.14.1), where it starts as an execution of
no lineage, since an action may wait and two attempts resolving together owe two actions side by
side. Waking is not evaluating: the process wakes where any value-change wait wakes, and the tick's
work goes to Observed as a deferred effect, which places evaluation where the standard does without
the engine learning what an assertion is. Evaluating where the process wakes fails LRM 16.5: the
values a design writes in Active and NBA are what the region order settles before an assertion
looks. An attempt as a coroutine fails the same per-tick bound the three levels serve: it allocates
a frame per assertion per tick where the common attempt does one test and ends, and a sequence's
nondeterminism becomes sub-threads joined by "any succeeded", the automaton rebuilt from scheduler
objects.

**The automaton is built at HIR-to-MIR.** HIR holds the sequence and property tree as LRM 16.7 and
16.12 write it, which is what the user wrote (`hir.md`); turning it into an automaton is what a
lowering exists to do, and every range bound and repetition count is an elaboration-time constant
(LRM 16.7), so nothing about the automaton's shape waits for run time. A named sequence or property
instance is its body with the actuals substituted (F.4.1).

**The transition is generated code, and the attempts are runtime storage.** What is fixed at compile
time -- the code taking a position set to its successor, seeding the instances its matches start,
computing each part's answer and vacuity, built from the design's Boolean expressions read as of
Preponed -- is code, rendered as any callable is. What varies at run time -- how many attempts are
live and how many evaluations each carries -- is storage belonging to whatever replicates the
statement, as its control does. Each attempt is a closure, the attempt's code bound to the values it
was made with, one type per statement and any number of values of it, and whatever reads what an
attempt saved -- a tick's step, a test of the disable condition, the statement its answer selects --
is a call of it. A tick reads the Booleans no saved value bears on once for every attempt and hands
them to each. A transition table the runtime interprets fails one of two things: its entries name
the design's Booleans, so it holds closures -- this code with a dispatch in front -- or the runtime
evaluates the design's expressions, an interpreter inside a compiler. A MIR node for a sequence, a
property or an attempt fails `mir.md`'s mechanical translation. MIR gains one member type, the type
of the attempt its closure is handed, and LIR nothing.

**The disable condition reads current values, not sampled ones** (LRM 16.12), tested from the start
of the attempt in the Observed region through its end inclusive. A disabled evaluation is a third
outcome, neither success nor failure, and runs no action statement (LRM 16.14.1).

### The expect statement

`expect` is a wait (LRM 16.17): the process reaching it starts a single attempt at the next tick,
blocks until the attempt succeeds or fails, and goes on once the Observed region the property
completed in is over. The property may read the process's automatics and `ref` formals as of the
Observed region, every occurrence of the clock is a tick evaluated in the Observed region of its
time step -- for a program's process too, which is woken only in Reactive -- and the property stops
being evaluated when the process stops waiting. So the wait is what watches the clock and the
disable condition while the process is parked: a tick is handed to the Observed region of now and a
change the disable condition reads to the Active region, each through a queue node the wait holds,
running code of the waiting body bound to its frame. The wait is a local of that frame, so ending
the scope takes the queued work with it. The process is woken once, by the answer, into the region
its own set runs woken work in; a disabled attempt answers nothing and wakes nothing. Waking the
process at each tick to step the attempt fails LRM 16.5 for a program's process, which would run
after the Observed region it is evaluated in. A clocking event only the waiting process could decide
has nothing to watch it and is refused. A sequence standing as the property is read strongly
(F.3.4.3.1).

## Deferred reports

A deferred immediate assertion (LRM 16.4), a violation report of `unique`, `unique0` or `priority`
(LRM 12.4.2.1), and a procedural concurrent assertion's pending entry (LRM 16.14.6) are each placed
where the statement is reached and act later in the time step. What the standard requires of them:

- Evaluation is where the statement is processed; the report or action is later in the same step. An
  observed (`#0`) report matures in Observed and acts in Reactive; a final one matures and acts in
  Postponed (LRM 16.4.1).
- Maturity is one-way: a matured report can no longer be flushed (LRM 16.4.1).
- Flush points differ by clause: a process resuming after an event control or wait, an `always_comb`
  or `always_latch` resumed by a dependency, and a `disable` of the process's outermost scope for a
  deferred assertion (LRM 16.4.2); the first two alone for a violation report (LRM 12.4.2.1).
  Disabling a task or a non-outermost scope flushes nothing.
- `disable` of an assertion's label cancels that assertion's pending reports (LRM 16.4.4), a label
  making a named block (LRM 16.3).
- A report's identity is an assertion paired with the process that executed it, so one assertion in
  a function holds pending reports in several processes, each flushed on its own process's terms
  (LRM 16.4.5); a deferred assertion outside procedural code is treated as in an `always_comb` (LRM
  16.4.3).
- The observed form is not glitch-free: a matured report can be followed in the same step by a
  further execution reaching the opposite result. The final form is, because no earlier region of
  the slot runs after Postponed (LRM 16.4.1, 16.4.2, 4.4.2.9).
- Only the action call is deferred: the subroutine's arguments are evaluated on each failure, so
  their side effects survive a flush (LRM 16.4.1).

**A qualifier is lowered as the assertions it states, never as a qualifier kind.** `unique` asserts
uniqueness and totality, `unique0` uniqueness, `priority` totality, and a catch-all -- an `else` or
a `default` -- discharges totality ([language_conformance.md](language_conformance.md) holds the
`default` ruling). Each live assertion is decided by itself and makes its own report, naming the
assertion in the standard's words. Totality is the statement reaching its fall-through, so it needs
no arm's predicate; uniqueness needs every arm's, and LRM 12.4.2 and 12.5.3 evaluate every arm for
`unique` and `unique0` regardless. A statement left with no live assertion is lowered as the
unqualified statement is and stops at the first match, so a qualifier never changes what its
statement evaluates beyond what the standard asks. A qualifier kind carried past HIR-to-MIR fails
`mir.md`'s mechanical translation: each consumer would rederive which assertions it makes, and the
`default` ruling would be answered once per consumer.

So a pending report's placement belongs to an ordered, append-only structure, while its validity is
revocable at two granularities by events that structure knows nothing of, until a commit point after
which neither applies. That is the second of the two answers the field has to such a tension --
validate at the commit point, as optimistic concurrency control and a fencing token do -- rather
than notifying holders, as `std::stop_token` does.

**A report stays where the engine put it, and withdrawal is a refusal to act.** It is submitted into
its region's own deferred-effect list where the statement is reached, and nothing takes it out, so
no registry of processes holding reports exists to keep in step with queuing, flushing and
termination. A container per process, searched at maturity, fails that: the engine would walk every
process or keep a registry, for an inspectability nothing requires.

**A report records each source that can withdraw it, as it stands when the report is made, and acts
only if every one still stands.** The assertion's own label block and the outermost scope of the
enclosing procedure are recorded as a `disable` target's cancellation source and the generation
captured from it, so a `disable` withdraws a report through the one act it already performs,
advancing its target's generation, and no second cancellation mechanism stands beside it. The
process's execution pass is a token the reports hold and a flush point marks withdrawn; the process
going away withdraws nothing, since termination is no flush point and a report queued in a body's
last pass still matures. The generation of the statement's assertion control is a fourth source,
which a Kill advances (below). What a report records is per construct, which is how one queue serves
two clauses: a deferred assertion records all of them, a violation report its process pass and its
control. One validity token per process alone fails LRM 16.4.4, since it cannot tell one assertion's
reports from another's. A cancellation source that notifies its dependents fails twice: it has to
find them, and it is one-shot, where a scope a `disable` ended is live again when next entered.

**Maturity is the commit point.** In the maturing region the report tests its sources once; if all
hold it is committed to its acting region and the record dropped, after which neither a `disable`
nor a re-execution reaches it. The observed form's residual glitch is reproduced, not removed:
removing it would answer differently from the standard.

**The action is a body of its own, built where the statement is and started once it is due.** The
action is one subroutine call, and the subroutine may be a task, so what a report carries is a body
started at maturity as an execution of no lineage in the region it acts in (`scheduling.md`). It
takes the scope of what reached the statement and a generator that one hands over. Each argument the
call reads as a value is evaluated at the statement and carried in, as a C++ lambda's init-capture
is, and one it designates -- a `ref` or `const ref` actual -- is designated in the body when the
call runs; arguments are evaluated on every outcome that selects the action, so a flush discards the
call and keeps their effects. The default report is the same shape with no call, and a deferred
`cover` holds back the hit it records with its statement, so a withdrawn report credits nothing. MIR
states the deferred action and where it matures, as a callable and a call, and nothing of sources,
generations or queues.

## Assertion control

The control tasks select what they act on by hierarchy, name, type and directive (LRM 20.11, Tables
20-6 and 20-7): a list of scopes or assertions, `levels` below each named instance counted as
`$dumpvars` counts them (LRM 21.7.1.2), a statement label naming its assertion. `directive_type` is
checked only for assertions; `$asserton`, `$assertoff` and `$assertkill` are `$assertcontrol` with a
fixed mask, so `$assertoff` reaches no violation report and `$assertcontrol(4)` reaches every one.
Off stops new attempts and new reports and leaves those executing, their actions and queued reports
alone; Kill also aborts the attempts executing and flushes reports not yet matured, then stops
checking until On; On, Off and Kill do not reach an `expect`. The action controls choose what an
outcome runs, the default `$error` included, for assertions and `expect` and not for violation
reports, and an assertion already executing is unaffected (LRM 16.14.8 tells vacuous and nonvacuous
success apart). Lock keeps what it locks from every change until Unlock.

**Every statement the tasks select owns a control, as static storage of the statement.** It is
declared by the scope the statement stands in, as a static variable is, and lives where that
statement's other static storage lives: one per instance of the declaring scope, one for a class or
the program where a class method or a package subroutine declares it. A statement a loop or a
reentrant task runs many times is one control, as LRM clause 39 gives each assertion one handle with
the tasks' own enable, disable, reset and lock callbacks. One state per type and directive for the
whole run fails LRM 20.11's naming of an instance, a block or an assertion; a control per scope
inherited by its statements fails Lock, which would have to say which of an inherited state and an
override it protects.

**A control is registered where its scope's storage is brought up, at every scope above it.** It is
recorded at the instance holding it and at each scope above, with how many instance levels below
each it stands, and within each named block and task it stands in; one no instance holds -- a
class's or a package's -- is recorded with none. A task naming an instance or a generate block takes
what was recorded there fewer than `levels` below; naming nothing, the same at each top and every
control no instance holds; naming a block or a task, what was recorded within it. A name standing
for several scopes -- an array of instances, a loop generate construct's blocks found as the task
runs (LRM 23.3.2, 27.4) -- is each of them. A scope where no controlled statement can stand selects
nothing, and a list whose every name selects nothing is not a call naming no list. Registration
precedes every process, so a task at time zero reaches a statement no process has reached; applying
a control twice leaves what once does. So a task costs the controls under the scopes it names, and
registering costs each control the depth it stands at. Walking the hierarchy from the named scopes
on each call fails that bound for a testbench toggling assertions around every test, over scopes
that mostly hold none. A statement can run before the design's storage is brought up -- a function a
parameter is computed with runs while the design is built -- and a control is usable from the moment
its storage exists.

**What an execution may do is read once, where it begins, and kept.** An immediate assertion reads
it as the statement is reached, a deferred one decides there which action it queues, a concurrent
attempt records it as it opens, an `expect` as its wait begins. This one rule is LRM 20.11's
"executing is not affected" for every statement alike. A deferred assertion reached while checking
is off queues nothing, and a violation report raised then is dropped as it is made.

**Kill reaches what has begun by two means, chosen by who holds the work.** The control holds its
assertion's attempts, so a Kill discards them where they are. A report the region holds is not the
control's to find: it records the control's generation as it is queued, a Kill advances it, and the
report acts at maturity only while it still stands, as every other withdrawal of one does. A Kill
that searched the region's lists for its reports fails the deferred report's placement above, since
those lists know nothing of assertions. On does not bring either back.

**Every layer above the runtime holds what it holds for any other static cell and call.** HIR
declares each controlled statement in its scope with the type and directive a task selects it by,
and writes each name of a task's list as what it names -- an object, the objects of an array, the
blocks of a loop, the extent of a block or subroutine. HIR-to-MIR gives the control a cell,
registers it at bring-up, reads it where an execution begins, and lowers a task to one runtime call
per scope it names. The tables' meanings, Lock, which controls a task reaches and the generation are
the runtime's. A design that calls no task pays one cell and one registration per controlled
statement, and one read where each execution begins. A control type Table 20-5 does not give selects
nothing, and the call is ignored with a warning.
