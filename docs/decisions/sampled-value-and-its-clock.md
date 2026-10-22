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

Three questions have real answers with real alternatives: what keeps the sampled value, where the
clock comes from, and what makes "the previous tick" unambiguous when the reader and the sampler run
in the same time step.

## Findings that shaped the decision

### F1. What is sampled is a variable or a net; an expression's sampled value is composed

LRM 16.5.1 defines the sampled value of a _variable_ as its value in the Preponed region of the time
slot, then defines the sampled value of an _expression_ recursively: the sampled value of `e1 & e2`
is the bitwise AND of the sampled values of `e1` and `e2`, and a function call is called on the
sampled values of its arguments. A net is sampled the same way, its default sampled value being the
default of its type (LRM 6.8 Table 6-7), since a net declares no value.

So the sampled value belongs to the variable or the net. An expression has no state of its own; it
is evaluated over whatever its leaves kept.

The clause carves out three exceptions, and each is a _current_ value rather than a sampled one: an
automatic variable, an assertion local variable, and a `const'(...)` cast. A past value of any of
them is also the current value, not a retained one.

### F2. Preponed is not a moment that needs a pass

LRM 4.4.2.1: "Sampling in the Preponed region is equivalent to sampling in the previous Postponed
region." LRM 4.4.2.9 forbids any value change once Postponed is reached. Together these say the
Preponed value of a time slot is the value the place held at the end of the last slot it changed in
-- a value taken in that slot's Postponed region, and in no other, with no pass over every place at
every slot.

Time zero is not an exception to this but a consequence of it. LRM 16.5.1 makes the sampled value at
time 0 the _default sampled value_ -- for a static variable, the value its declaration assigns. A
kept value starting at exactly that answers every read during time 0, since the first value taken is
taken at the end of it.

### F2b. What a net shows changes where the net is not told

A variable's value changes only through its own writes. A net's comes from wherever it shows: a
resolution, its one driver, a force and its release, and, for a net whose one driver carries the
whole of another object, that object's own writes, which never pass through the net. What the net
shows is already followed by one thing: a wait on the net enrols through the member naming where the
net's value lies, and is moved with the member as it is bound, forced and released. Anything else
tracking that would say it a second time.

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

### D1. A sampled value is kept by the variable or the net, learning of changes as a wait does

An armed variable or net keeps a value of its own type, which is what a sampled read answers with.
It is enrolled on the place the way a wait on it is -- a net's through the member naming where its
value lies -- and is told of every change there, so by F2b it follows a net onto the object its
driver carries and back without a word of its own about what the net shows. Told of the first change
of a slot, it submits a taking of the value into the Postponed region of that slot; F2 makes what it
takes there the Preponed value of every slot until the place changes again. A slot the place does
not change in costs it nothing, and a write that changes nothing tells it nothing.

What it is enrolled as is an observer of the place, as an awaiter is: an awaiter is told so that the
execution parked on it may be woken, and this is told so that it may take a value, waking nothing.

**Arming installs the default sampled value.** For a variable it is what the variable holds when
armed; for a net it is the default of the net's type, whatever the net shows before time zero.

**Arming therefore belongs to Activate, not to Resolve.** LRM 16.5.1 makes a static variable's
default sampled value the value its _declaration_ assigns, and a declaration initializer runs in
Initialize (LRM 10.5) -- so a cell armed any earlier would keep the type's default instead, and
answer time 0 wrongly for every variable that declares an initial value. Activate is where reactive
behaviour is installed and event controls are armed, and it is after Initialize, so the cell holds
the declared value by then. Nothing about reaching the cell needs the earlier phase: the endpoint a
clocking block samples through has been sealed since Seal, which is what lets it sample a signal in
another unit (LRM 14.5) without either unit knowing at compile time that the other sampled it. A
place nothing samples pays nothing.

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
clocking event, evaluates each sampled expression over its leaves' sampled values, and repeats. It
is an action bound to a schedule event, which is a category that already exists, so it needs no
scheduling concept of its own and waits on its clock through its own loop as any `always` does
([a-wait-is-storage-of-its-activation](a-wait-is-storage-of-its-activation.md)).

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
where the storage is declared. It is member storage reached only through its address.

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

- A design that samples nothing pays nothing: no variable or net keeps a sampled value, no sampler
  process exists, and no write path grows a test.
- The sampled value functions become available in ordinary procedural code, which is the use the
  standard explicitly admits ("not limited to assertion features") and the one no assertion-eliding
  policy can stand in for, because `b <= $past(a)` with `$past` removed has no defensible answer.
- What a concurrent assertion will later need from sampling is this and nothing more: it evaluates
  against sampled values on a clock tick, which is D1 through D4 with a different consumer.

- **Where the value is a handle, what D1 and D5 keep are references, so sampling extends an object's
  life.** An object the program no longer names anywhere is still named by the entries of every
  history over it and by the sampled value of every armed variable that held it, and it is reclaimed
  only once those move on. That is not a cost to be avoided but the requirement: LRM 8.4 reclaims an
  object when nothing references it, and `$past` has to answer with a handle whoever reads it may
  reach a member through, so an answer that did not hold the object would name a reclaimed one. The
  extension is bounded by the declared depth, which D5 fixes where the storage is declared -- so a
  design that samples a handle holds at most that many objects it would otherwise have dropped, per
  history, and never a growing number.
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

- **Keeping the value at the first write of each slot, before the write lands.** It takes no
  deferred submission, and it is what a variable did first. Rejected by F2b: it needs a hook before
  every way the value can change, which a variable has one of and a net has many, one of them in
  another object's writes, which never pass through the net; a hook there would be a second account
  of what the net shows beside the one its waits already follow.

## Cross-references

- LRM anchors: 4.4.2.1 (Preponed equals the previous Postponed), 4.4.2.9 (Postponed), 14.7 (a
  clocking block is instanced like an always procedure), 14.10 (the clocking block event), 14.12
  (default clocking), 14.13 (input sampling), 16.5.1 (sampled value, default sampled value, and the
  three current-value exceptions), 16.9.3 (the sampled value functions and the clock inference
  ordering), 16.14.6 (the procedural-context inference conditions), 20.12 (the function list).
- [owner-transition-and-observation](owner-transition-and-observation.md) -- D1 is the boundary D1
  here learns of a change at; its D5 is why a sampled read is a call; its D6 is the observation
  lifetime D3 deliberately does not use.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- the trigger set a
  clocking event is expressed as.
- [read-set-inference](read-set-inference.md) -- the analysis that yields both a sampled
  expression's leaves and the resolved clocking event.
- `architecture/scheduling.md` -- the region structure, the placement a deferred effect is submitted
  to, and the Preponed region this decision declines to fill.
- `architecture/elaboration_lifecycle.md` -- why arming is an Activate step: the endpoint reaching a
  cross-unit cell is sealed by Seal, but the declared initial value a static variable's default
  sampled value is defined as does not exist until Initialize has run.
