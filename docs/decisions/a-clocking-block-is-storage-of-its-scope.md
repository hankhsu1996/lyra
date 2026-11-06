# A clocking block is storage of its scope and an observer of its clock

Date: 2026-11-05. Status: accepted.

## Why this decision matters

A clocking block (LRM 14) is used in four ways -- an input is read and waited on, the block's event
is waited on, a cycle delay counts the block's events, an output is driven -- and from three places:
the scope declaring it, a unit holding an interface port to it, and a class holding a virtual
interface to it (LRM 14.9, 25.9). Each use has to mean the same wherever it is written, while what
the block is (its signals, its skews) is known only to the unit declaring it.

## Findings

F1. LRM 14.7 makes the block "both a declaration and an instance of that declaration ... one copy is
created for each instance of the block containing the declaration (like an always procedure)", with
static lifetime and scope local to its module, interface or program. Nothing names the block as a
value: the front end refuses `.triggered` on one and admits it only as an event control's operand.

F2. A clockvar is a variable of the block to the front end (`ClockVarSymbol` is a `VariableSymbol`),
whose initializer is the signal it names. Reading an input reads "the value that the clocking block
sampled at the most recent clocking event" (LRM 14.13); `@(cb.x)` waits on that value (LRM 14.15).

F3. LRM 14.13 places each input's sample by its skew: `#1step` at the end of the previous time step,
an explicit `#0` in Observed, a delay at the end of the time step that long before the event. A skew
"can be specified as a parameter" (LRM 14.4), so its value is the instance's. An output with no skew
"or explicit #0 skew" is driven in Re-NBA, and "an explicit #0 skew does not suspend any process,
nor does it execute or sample values in the Inactive region" (LRM 14.4).

F4. LRM 14.16: a drive "shall not cause execution of the statement to block", is evaluated where it
is reached, and lands in Re-NBA after the output skew, counted from the event of its own time step
or, made between events, from the next. Of drives of one output maturing in one Re-NBA region "the
last value is the only value driven onto the output signal", and Re-NBA may run more than once in a
time step, so an output can change more than once in it; a variable output from several blocks
"shall take on the value most recently assigned by either clocking block" (LRM 14.16.2). For a net
output "a driver on that net shall be created ... (strong1, strong0) ... updated as if by a
continuous assignment from a variable inside the clocking block", 'z until the first drive.

F6. The engine runs a region in passes: what it holds as a pass begins, and what arrives meanwhile
in the next pass (LRM 9.3.2). A write tells the waits on the written place at once, with what that
write changed; nothing keeps an earlier value to tell against later.

F5. A unit compiles against what another publishes, and the signature is computed from the declaring
unit alone, before any body of it is lowered; a referrer lays a published class out from that
signature and calls a published callable through the virtual slot it states. What the declaring unit
adds while lowering its bodies is placed after the published fields and named by nothing, so no
referrer depends on it.

## Decision

**D1. The block is what the source wrote, held by its scope.** The scope declares it as its clocking
event and its clockvars, each with its signal, its skew per direction as written or as LRM 14.3
defaults it, and the variables the standard gives the block: each input's sample (F2), the event the
block's name stands for (LRM 14.10), and for each output the variable inside the block holding what
the block drives onto its signal, which every drive of it writes (F4). The signal is one expression
however many halves read or write it. Nothing a lowering invents to carry the block out -- what
notes its occurrences, what keeps a skewed input's copy, the driver a net output's variable drives
the net by, what puts a variable output's value onto its variable, what lands a drive -- is part of
the declaration. The one thing beyond the variables a net output states is what the standard says of
it: that the net is driven "as if by a continuous assignment from" its variable (LRM 14.16), so the
declaration names that variable's read and what the read watches, as a continuous assignment states
its own, and the driver built from it is the lowering's. The variables are declarations rather than
storage a lowering adds behind an identity the publication reserves, because a use in any unit -- an
event control on the block, a drive of an output -- names them as it names a variable, and the
standard defines each.

**D2. Sampling is an execution of the scope woken inside each occurrence of the clocking event.** It
counts the occurrence, then places into that time step's Observed region the sampling of every input
and the trigger of the block's event (LRM 14.10, 14.13). Running inside the occurrence means
anything the occurrence wakes sees the occurrence counted, which is what makes "the event of its own
time step" (F4) a question with one answer. It belongs to no lineage (LRM 9.6.1).

**D3. Each skew reads from the earliest moment that knows the answer.** `#1step` is the signal over
its leaves' sampled values ([sampled-value-and-its-clock](sampled-value-and-its-clock.md)). A delay
-- `#0` among them, the delay of none -- is a copy of the signal kept that far behind it: an
execution of the scope following every change of what the signal reads lands each value in the NBA
region of the slot the delay names, ahead of that slot's Observed region, whichever region set the
scope's code runs in. No reader at the event can know what the signal was before it, and the skew's
value is the instance's (F3), so no compile-time test of it is made.

**D4. A drive is landed by the scope declaring its block, on the instance its target is reached
on.** The drive writes the output's driving variable under the selects it wrote. It reads its value,
its target's selects, its count of cycles and the instance it reaches where it is reached, and hands
an execution of no lineage the write. Through a virtual interface the drive names the member access
of its target whose instance the landing is made on, so the IR states that one evaluation serves
both, and the lowering evaluates that access once. The execution calls the declaring scope's landing
of the output, a task taking the count: it waits for the occurrence the drive matures at -- `n` on,
or one where `n` is none and its time step has had no occurrence yet -- and from that occurrence for
the skew, returning in the Re-NBA region of the slot the skew names; a skew of none, written or not,
is that occurrence's own slot (F3). The region is named outright, so a drive made by a program lands
where a module's does. The execution runs inside each occurrence it waits for: executions waiting on
one event are woken at an occurrence in the order they began to wait, and one waiting again waits
behind those already waiting, so by induction drives of one block maturing together go on, and land,
in the order they were made (F4). `##n` is the same count over the block's event (LRM 14.11).

**D5. An output onto nets drives them from its variable, and an output onto a variable takes the
last value of each pass.** A net output's driver is the continuous assignment LRM 14.16 names; it
runs once after the pass, so it reads only what the pass's drives left. Every output onto one
variable -- of one block or of several, a variable output's signal being a variable of the scope
declaring them all -- shares one driving variable, and that variable is put onto the signal once
each Re-NBA pass that drives it has run: the first drive of the pass to land keeps an effect for the
end of the pass, and every drive of the pass waits there for it. That effect sets the driving
variable to what the signal holds then, lets the waiting drives write it in the order D4 lands them,
and writes the signal once from it. The staging holds nothing between passes; it exists to compute
one pass's update. So only the last value each part was given reaches the variable, a value the pass
took back is no change, and a part no drive wrote keeps what the signal had, whoever else wrote it
in the pass (F4). Each pass is committed by itself, so an output still changes once per pass. Drives
of several blocks landing in one pass are applied in the order they land whichever block made each,
part by part, so the drive landing last sets each part: LRM 14.16.2 asks for the value "most
recently assigned by either clocking block". The waiting drives are woken inside the occurrence of
the effect's trigger and each finishes its write before the trigger returns, so every write of the
pass is in the driving variable before the signal is written; and what the effect places lands in a
later pass.

**D6. What another unit reaches is published, and what carries it out is not.** A block publishes
its event, its inputs' samples and its net outputs' driving variables as members under the block,
the driving variable the outputs onto one variable share as a member of the scope named after that
variable, and each output's landing as a callable whose body the declaring unit's lowering writes,
overriding the slot the signature reserved. The names of what the standard keeps invisible to user
code -- the block's event, the driving variables, the landings -- each carry a space, which no
identifier can, so name lookup in the language finds none of them. The count of occurrences, the
copies and what a variable output commits by are fields the declaring unit adds and nothing names
(F5), so how a skew or a commit is carried out moves nothing a referrer compiled against.

**D7. The runtime learns no clocking block.** It gains placements naming a region outright -- a
region of a slot a delay from now, the region an operand -- because LRM 14.13 and 14.16 name those
regions whatever region set the thread runs in; the effects run once the pass of the region running
now has run all it held, because LRM 14.16.2 speaks of what one pass drove (F6); and a start kind
for an execution of a scope woken at the occurrence. Everything else is what MIR already states:
variables, events, closures, waits and executions.

## Rejected

- **A runtime clocking type holding the count and the pending drives.** It would restate what an
  event object already is (its count and when it last fired), and its pending list would decide the
  order drives land in, which the order executions are woken in at one occurrence already decides.
- **A drive as a nonblocking assignment with `repeat (n) @(clock)`.** Its carrier would be woken
  into Active, after processes the same occurrence woke, so a drive made later in the same cycle
  could land before one made earlier (F4).
- **The machinery as declarations of the block** -- the count, the copy and the process keeping it,
  a task waiting out the skew. Each is how the block is carried out rather than what the source
  wrote, and a task the referrer called to wait out the skew read the instance the drive's target
  named a second time, at maturity.
- **A stamp on each drive ordering what lands together.** Within one block the wake order already is
  the order made (D4), so the stamp would restate it.
- **Collapsing a pass's writes in the engine, by telling the waits once the pass ends.** A write
  tells what it changed as it is made, and the engine keeps no earlier value to tell against (F6);
  and an ordinary nonblocking update is told update by update, so the engine would have to learn
  which writes are a clocking block's.
- **Copying the signal into the driving variable at the first drive of the pass.** A part another
  block's commit wrote later in the same pass would be put back, so the copy is taken where the
  commit runs (D5).
- **Committing in the next pass of Re-NBA.** What the pass woke in the Reactive region would run
  before the commit and read the variable as the drives had not landed (F6).

## Limits

- Global clocking (LRM 14.14) is not carried.
- An input skew written as an edge, an output skew of `#1step`, an output skew written as an edge
  where the clocking event is not an edge of one signal, an output whose signal mixes nets and
  variables or names anything but nets or one variable of the declaring scope, and an output driving
  a net of a type other than a 4-state packed vector are not carried: the block is declared without
  that half, and a use reaching it is refused by name.
- An output skew written as an edge is read as the next such edge of the clocking event's signal
  after the occurrence the drive matures at, including where the clocking event is that same edge,
  and the delay it may write as counted from that edge; LRM 14.3 does not say which edge it is.
- A drive made in a time step whose clocking event has not occurred yet when the drive is made but
  does later in the step -- a clock changed in the NBA region -- is counted as made between events.
- Drives of different blocks on one edge maturing at one occurrence land in the order the blocks'
  clocking events wake them, not in the order the drives were made; LRM 14.16.2 does not order them.
- A drive of a member of a clocking output, a drive of a clocking block that another scope of the
  same design element declares, and a clocking block whose clocking event names another clocking
  block, are refused.

## Cross-references

- LRM 14.3, 14.4, 14.7, 14.10, 14.11, 14.12, 14.13, 14.15, 14.16, 25.9.
- [sampled-value-and-its-clock](sampled-value-and-its-clock.md) -- the sampled values a `#1step`
  input reads, and D3 there, the sampler this generalizes.
- [a-unit-takes-what-another-holds-from-what-it-publishes](a-unit-takes-what-another-holds-from-what-it-publishes.md)
  D4 -- what crosses to another unit.
- [a-covergroup-is-a-class-over-a-coverage-library](a-covergroup-is-a-class-over-a-coverage-library.md)
  D4 -- the execution woken inside an occurrence.
- `architecture/scheduling.md` -- the envelope a drive's write reuses, and the placements naming a
  region outright.
