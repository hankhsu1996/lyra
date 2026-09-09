# A sampled value, and the clock it is sampled against

Date: 2026-09-08. Status: accepted, for the sampled value functions used as ordinary logic (LRM
16.9.3, 20.12) and for the clocking declaration only as far as it supplies a sampling event.

## Why this decision matters

Every construct the compiler carries reads a value where the statement stands. A sampled value
function reads one **as of a moment its reader did not choose**: the Preponed region of a clocking
event, which is before anything in that time step ran. So the value has to have been taken before
the reader existed and kept until it asks, and `$past` keeps several. This is the first state in the
engine whose lifetime is a history rather than a current value, and getting its shape wrong is
expensive in the way a storage model is expensive -- every later consumer inherits it.

Three questions have real answers with real alternatives: what holds the retained value, where the
clock comes from, and what makes "the previous tick" unambiguous when the reader and the sampler run
in the same time step.

## Findings that shaped the decision

### F1. What is sampled is a variable; an expression's sampled value is composed

LRM 16.5.1 defines the sampled value of a _variable_ as its value in the Preponed region of the time
slot, then defines the sampled value of an _expression_ recursively: the sampled value of `e1 & e2`
is the bitwise AND of the sampled values of `e1` and `e2`, and a function call is called on the
sampled values of its arguments.

So retention belongs to the variable. An expression has no state of its own; it is evaluated over
whatever its leaves retained.

The clause carves out three exceptions, and each is a _current_ value rather than a sampled one: an
automatic variable, an assertion local variable, and a `const'(...)` cast. A past value of any of
them is also the current value, not a retained one.

### F2. Preponed is not a moment that needs a pass

LRM 4.4.2.1: "Sampling in the Preponed region is equivalent to sampling in the previous Postponed
region." LRM 4.4.2.9 forbids any value change once Postponed is reached. Together these say the
Preponed value of a time slot is the value the variable held before that slot's first change -- a
fact recoverable at the first change, with no region pass and no moment to be at.

Time zero is not an exception to this but a consequence of it. LRM 16.5.1 makes the sampled value at
time 0 the _default sampled value_ -- for a static variable, the value its declaration assigns. A
retained value initialized to exactly that, stamped with time 0, answers every read during time 0
without any of that slot's writes disturbing it.

### F3. The clock is not in the syntax, and the rules for finding it are already implemented

`$rose(b)` written with no clocking argument carries none in the elaborated AST. LRM 16.9.3 gives
five ordered rules for inferring it, of which two reach ordinary procedural code: the clock inferred
from the procedural context (LRM 16.14.6), and otherwise the default clocking (LRM 14.12). LRM
16.14.6 states the inference as three conditions over the whole procedure -- no blocking timing
control, exactly one event control, and exactly one event expression of the right form none of whose
terms the body reads elsewhere.

The front end implements all of it: it detects that a procedure contains sampled value calls, runs
the inference, falls back to the enclosing scope's default clocking, and reports the error the
standard requires when neither yields a clock. What it produces is a timing control -- the same kind
of object an ordinary event control is.

### F4. A clocking block is already specified as a process

LRM 14.7 makes a clocking block "both a declaration and an instance of that declaration ... one copy
is created for each instance of the block containing the declaration (like an always procedure)",
with static lifetime and scope local to its enclosing module, interface or program. LRM 14.10 has it
trigger its own event upon processing its clocking event.

So the standard already describes the sampling agent as an always-procedure-shaped thing, not as a
subscription some reader arms.

### F5. Only strictly prior ticks are ever read

LRM 16.9.3 defines `$past(e, k)` against "the kth strictly prior time step in which the event
occurred", and the value change functions against "the most recent strictly prior time step in which
the clocking event occurred". A tick in the reader's own time step is never among them.

This is what makes the reader and the sampler order-independent, and it is a property of the
standard rather than of any schedule we choose.

## The decision

### D1. A sampled value is retained by the cell, at its transition boundary, when armed

An observable cell may carry a retained value and the time slot that value belongs to. The one
boundary every mutation already reports through
([owner-transition-and-observation](owner-transition-and-observation.md) D1) is where it is
maintained: when the owner transitions and the retained value does not already belong to the current
slot, the outgoing value becomes the retained one and is stamped with the current slot. Reading the
sampled value answers with the retained value when its stamp is the current slot, and with the
current value otherwise.

A write that changes nothing needs no case of its own: the retained value and the current value are
then equal, so keying on the transition rather than on the write is not an optimization but the
whole rule.

**Only an armed cell carries any of this**, and arming is what installs the retained value: the cell
takes its current contents, stamped with time 0. A write during time 0 then finds the stamp already
current and leaves the retained value alone, so every read through the whole of time 0 answers with
the value the cell held when it was armed. F2's time-zero requirement is the general path rather
than a branch.

**Arming therefore belongs to Activate, not to Resolve.** LRM 16.5.1 makes a static variable's
default sampled value the value its _declaration_ assigns, and a declaration initializer runs in
Initialize (LRM 10.5) -- so a cell armed any earlier would retain the type's default instead, and
answer time 0 wrongly for every variable that declares an initial value. Activate is where reactive
behaviour is installed and event controls are armed, and it is after Initialize, so the cell holds
the declared value by then. Nothing about reaching the cell needs the earlier phase: the endpoint a
clocking block samples through has been sealed since Seal, which is what lets it sample a signal in
another unit (LRM 14.5) without either unit knowing at compile time that the other sampled it. A
cell nothing samples pays nothing.

### D2. A clocking event is an event control, and the compiler does not re-derive which one it is

What a sampled value function samples against is the same per-leaf trigger set every value-change
wait already registers ([value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md)).
The compiler reads the resolved clocking event from the front end's procedure analysis -- the same
analysis it already runs for read-set inference -- and translates it exactly as it translates the
event control of an `@(...)`.

By F3 the rules for finding it are the front end's, so no layer here restates LRM 16.9.3's ordering
or LRM 16.14.6's three conditions. Where the front end resolves no clock, the construct is refused
with the diagnostic the standard requires, rather than sampled against a guess.

### D3. A sampler is a synthesized process, one per scope and clocking event

Per F4 the sampling agent is an always procedure, and it is lowered as one: a body that waits on the
clocking event, evaluates each sampled expression over its leaves' retained values, and repeats. It
is an action bound to a schedule event, which is a category that already exists, so it needs no
scheduling concept of its own and re-arms through its own loop like every other `always`.

This is deliberately not an observation armed by a reader. An armed observation exists only while
someone waits at it, which is exactly the LRM 9.7 resensitization property
([owner-transition-and-observation](owner-transition-and-observation.md) D6) -- and it is the wrong
property here, because a history has to record every tick whether or not any process was watching
when it happened.

### D4. The history is committed in the Postponed region, which is what makes "strictly prior"

structural

The sampler evaluates the sampled value at the tick and commits it into the history in the Postponed
region of that time step. F5 says a reader never wants the current step's tick; committing at the
end of the step means the history _cannot_ contain it, so every reader in that step agrees with
every other and with the sampler, whatever order they ran in.

The alternative -- committing at the tick and having readers count around it -- makes correctness
depend on whether a given reader ran before or after the sampler in the same region, which nothing
in the engine orders and nothing in the standard licenses.

### D5. A history is member storage of a fixed depth, prefilled with the default sampled value

Each `(sampled expression, clocking event)` a unit needs has a history of the depth its deepest
`$past` asks for; the count is an elaboration-time constant (LRM 16.9.3), so the depth is known
where the storage is declared. It is member storage reached only through its address, the shape a
named event already has.

It is installed already full of the expression's default sampled value. LRM 16.9.3 requires exactly
that answer before the kth prior tick exists, so a history that starts full has no empty case and no
tick counter to compare against.

### D6. No layer gains a node kind

Every piece above is expressible in vocabulary that already exists. Reading a cell's sampled value
is an operation on the wrapper, so it is an ordinary call beside the read
([owner-transition-and-observation](owner-transition-and-observation.md) D5). Arming a cell and
advancing a history are calls. The sampler is a callable plus an action registration. The clocking
event is the trigger set a value-change wait already builds. The Postponed commit is the placement a
deferred effect already submits to.

So the semantic IR gains one member type and a few runtime entries, and the execution-model IR gains
nothing at all -- which is what `lir.md` invariant 7 requires, since a clocking event reaching it
would be an upstream leak.

## Consequences

- A design that samples nothing pays nothing: no cell carries a retained value, no sampler process
  exists, and no write path grows a test.
- The sampled value functions become available in ordinary procedural code, which is the use the
  standard explicitly admits ("not limited to assertion features") and the one no assertion-eliding
  policy can stand in for, because `b <= $past(a)` with `$past` removed has no defensible answer.
- What a concurrent assertion will later need from sampling is this and nothing more: it evaluates
  against sampled values on a clock tick, which is D1 through D4 with a different consumer.
- The front end's analysis diagnostics have to reach the user for the refusal in D2 to be the
  standard's error rather than silence. They are currently computed and dropped.
- A clocking block is carried only as far as D2 needs it -- a name and an event that a scope may
  select as its default clocking. Its clockvars and skews, the cycle delay operator, synchronous
  drives, and the global clocking family are refused, each by name.

## Rejected alternatives

- **A pass over the Preponed region that snapshots every armed cell.** The region exists and is
  empty, so this reads as filling it in as designed. Rejected on cost shape: it does work
  proportional to the number of time slots times the number of sampled signals, where D1 does work
  proportional to the changes that actually happen -- and proportionality to changes rather than to
  time is the premise the whole engine rests on. It also puts the cost on slots in which nothing was
  sampled and nothing changed, which is most of them.

- **Per-leaf history, with the expression re-evaluated at read time.** Sharing history across
  expressions that read the same leaves is real, and this shape gets it. Rejected because LRM 16.5.1
  has a function call in a sampled expression called on the sampled values of its arguments at the
  time of evaluation; replaying the expression later calls it at a different time, with the same
  arguments but not necessarily the same answer. D5 stores what the clause defines rather than the
  inputs from which it could be recomputed.

- **Implementing the clock inference rules here.** LRM 16.9.3's five ordered rules and LRM 16.14.6's
  three conditions are exactly specifiable and would be a self-contained piece of work. Rejected
  because the front end already answers them and the answer is a timing control this compiler
  already knows how to translate; a second implementation of a name-resolution rule is a second
  thing to keep correct, and the two would be free to disagree with nobody positioned to see it.

- **A node kind for the sampled read, or for `$past`.** It reads as the honest way to say the
  program samples. Rejected by `mir.md`: a node invented for a runtime library's shape or a
  scheduling discipline is forbidden, and the falsifier applies directly -- a mechanical backend can
  translate a call and a member without deciding anything, and would have to invent an expansion for
  a node that states neither.

- **An armed observation rather than a process.** It reuses the machinery a reader already uses to
  wait on a clock, and it would make the sampler exist only while something reads it. Rejected: a
  history must record every tick of its clock, and an armed observation by construction records only
  the ticks something was waiting for -- which is the right property for `@(...)` and the wrong one
  here.

- **Committing the history at the tick, with readers indexing around it.** Cheaper by one deferred
  submission per tick. Rejected: whether a reader must skip the newest entry then depends on whether
  it ran before or after the sampler within one region, which no rule orders.

## Cross-references

- LRM anchors: 4.4.2.1 (Preponed equals the previous Postponed), 4.4.2.9 (Postponed), 14.7 (a
  clocking block is instanced like an always procedure), 14.10 (the clocking block event), 14.12
  (default clocking), 14.13 (input sampling), 16.5.1 (sampled value, default sampled value, and the
  three current-value exceptions), 16.9.3 (the sampled value functions and the clock inference
  ordering), 16.14.6 (the procedural-context inference conditions), 20.12 (the function list).
- [owner-transition-and-observation](owner-transition-and-observation.md) -- D1 is the boundary D1
  here maintains the retained value at; its D5 is why a sampled read is a call; its D6 is the
  observation lifetime D3 deliberately does not use.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- the trigger set a
  clocking event is expressed as.
- [read-set-inference](read-set-inference.md) -- the analysis that yields both a sampled
  expression's leaves and the resolved clocking event.
- `architecture/scheduling.md` -- the region structure, the placement a deferred effect is submitted
  to, and the Preponed region this decision declines to fill.
- `architecture/elaboration_lifecycle.md` -- why arming is an Activate step: the endpoint reaching a
  cross-unit cell is sealed by Seal, but the declared initial value a static variable's default
  sampled value is defined as does not exist until Initialize has run.
