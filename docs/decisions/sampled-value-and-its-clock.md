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
them is also the current value, not a retained one. The exceptions are stated on the variable or the
cast, never on an expression around it, so an expression mixing them is the sampled one composed
over each part's own answer: `$past(b[i])` is the past `b` at the current `i`.

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

By F3 the procedure's rule is the front end's, so no layer here restates LRM 16.14.6's three
conditions. The rest of LRM 16.9.3's ordering -- the clock flow inside an assertion (LRM 16.13.3),
an action block's leading clock, and the default clocking outside a procedure -- the front end
checks and does not record, so it is applied where the assertion is lowered, over clocking events
the front end built. Where no rule yields a clock, the construct is refused with the diagnostic the
standard requires, rather than sampled against a guess.

Revised 2026-10-23. This section first said the front end answers all of LRM 16.9.3's ordering. It
answers only the procedure's rule: its assertion analysis follows clock flow to check a design and
rewrites nothing. So a sampled value function inside an assertion with no default clocking was
refused, and with one it counted the default clocking's ticks instead of the assertion's.

### D3. A sampler is a synthesized process, one per sampled value function that keeps a piece

Per F4 the sampling agent is an always procedure, and it is lowered as one: a body that waits on the
function's clocking event, takes the sampled value of each piece D5 keeps, and repeats. It is an
action bound to a schedule event, which is a category that already exists, so it needs no scheduling
concept of its own and waits on its clock through its own loop as any `always` does
([a-wait-is-storage-of-its-activation](a-wait-is-storage-of-its-activation.md)). It is a process of
the instance that replicates the body the function is written in, and it reads what that body's
statics hold as the body does. A function whose operand keeps no piece reads only what answers as it
is now, and has none.

Revised 2026-11-09. This section said one sampler per scope and clocking event; the lowering has
built one per history since it was written, because telling two clocking events apart is a
comparison of expressions, and sharing a wait is what one per event would save.

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

### D5. A sampled value function keeps the pieces of its operand that have a past

`$past(e)` is `e` evaluated where the call stands, as the sampled value of the tick the call names
(LRM 16.9.3): every static variable `e` reads as of that tick, and every automatic variable,
constant cast and value a procedural assertion saved as it is now (F1). So what has to have been
kept is a value per tick for each **piece** of `e` that reads a sampled variable and nothing read as
now, taken as large as it can be while it makes no call: operators and selects compute the same over
the values of a tick whenever they run, so their result is kept, while a call -- a library method, a
system function, a subroutine of the program -- may answer from the state of the run, an object's
state or what its arguments do not name, so it is made where `e` is read, on its arguments' past
values (LRM 16.5.1), which is the moment `architecture/language_conformance.md` rules the standard
leaves open. A piece is also one its sampler can evaluate by itself: a range of values bounds only
the membership or case item it stands in, and a part reading a queue's `$` counts from the select
around it, so neither is kept whole and their own parts are kept instead. `$past(mem[addr])` with a
static `addr` keeps one element per tick; `$past(mem[i])` with an automatic `i` keeps `mem`, since
any `i` may be asked for (LRM 16.6 anticipates that copy); `$past(const'(x))` keeps nothing. A piece
read inside another sampled value function of the operand is kept as that function's history holds
it, at each tick of the outer function's clock, so functions nest across clocks with every history
of a fixed depth.

Each history is storage of the body the function is written in, or of the scope where it stands
among the scope's own expressions, of static lifetime, so it lives where that body's statics live
and its sampler reaches what they reach, a static local of the body among them (LRM 6.21). Its depth
is the tick count, an elaboration-time constant (LRM 16.9.3), known where the storage is declared.
It is installed full of each piece's default sampled value, which LRM 16.9.3 requires before the kth
prior tick exists, so it has no empty case and no tick counter; reading an automatic as now before
that tick too is the reading `architecture/language_conformance.md` rules on. A read names the entry
where the history keeps it, as a sampled read names the value where the variable keeps it, so
`$past(mem[i])` reaches one element of the kept `mem` rather than a copy of all of it. A clocking
block input is read, there and by every other sampled read, as the value its block samples at the
event (LRM 16.5.1, 14.13), computed as the block computes it rather than read from the input's cell,
which the block writes later in the same time step. The triggered state of an event or a sequence is
an operand whose past value is sampled elsewhere -- in the Postponed region of the past tick (LRM
16.5.1) -- and a function reading one is not yet carried out.

Revised 2026-11-09. This section kept a history of the whole operand, lowered a second time into the
scope's own arena. That copy could read neither an automatic variable nor a static local of the body
that wrote the call, so `$past(b[i])` in a loop was refused and `$past` of a static local ended the
compiler; it also called a function in the operand at the tick rather than at the read.

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
- A clocking block's own sampling (LRM 14.13) reads a `#1step` input over its leaves' sampled
  values, as D1 keeps them; what samples it is the block's own execution, woken inside each
  occurrence of its clocking event rather than started as D3's sampler is
  ([a-clocking-block-is-storage-of-its-scope](a-clocking-block-is-storage-of-its-scope.md)). Revised
  2026-11-05: this line said the block was carried only as far as D2 needs it, with its clockvars,
  skews, cycle delays and drives refused. The global clocking family still is.

## Rejected alternatives

- **A pass over the Preponed region that snapshots every armed cell.** The region exists and is
  empty, so this reads as filling it in as designed. Rejected on cost shape: it does work
  proportional to the number of time slots times the number of sampled signals, where D1 does work
  proportional to the changes that actually happen -- and proportionality to changes rather than to
  time is the premise the whole engine rests on. It also puts the cost on slots in which nothing was
  sampled and nothing changed, which is most of them.

- **A history of the whole operand, evaluated at each tick.** It is what D5 first said, and it is
  what a delay of an expression is in a language whose every variable is delayed (Lustre's `fby`).
  Rejected because SystemVerilog has operands whose past is their present (F1): the copy cannot read
  them, and it calls a function at the tick where LRM 16.5.1 has it "called on the sampled values of
  its arguments at the time of the expression evaluation". Until 2026-11-09 this entry rejected the
  opposite shape -- a history per leaf, the expression re-evaluated at the read -- for the same
  clause read the other way, as calling the function at the wrong time; the read is the call's
  evaluation, and `$sampled` already calls it there. D5 keeps more than leaves only where nothing is
  called.
- **A history per variable, matched to the read by the variable's cell.** It keeps the whole of
  `mem` for `$past(mem[3])`, which copies every element at every tick, and a read reaches its cell
  by a route of its own while the arming names the same cell by another, so the two meet only at run
  time, through a table. Keyed by a piece of the operand, the read knows its history where it is
  lowered.

- **Implementing LRM 16.14.6's three conditions here.** They are exactly specifiable and would be a
  self-contained piece of work. Rejected because the front end already answers them and the answer
  is a timing control this compiler already knows how to translate; a second implementation of the
  rule is a second thing to keep correct, and the two would be free to disagree with nobody
  positioned to see it. The other rules of LRM 16.9.3 are not in this position, because the front
  end records no answer to them (D2).

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
