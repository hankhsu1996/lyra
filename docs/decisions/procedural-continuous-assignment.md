# A procedural continuous assignment is a process that takes a target over

Date: 2026-09-10 Status: accepted

## Why this decision matters

LRM 10.6 gives a procedural statement the power to take over a variable's or a net's value with a
continuously evaluated expression, and to give it back. Four spellings share one clause and one pair
of syntax nodes -- `assign` / `deassign` on a variable, `force` / `release` on a variable or a net.
The shape they land in decides whether the net model, the variable cell, and the scheduler each gain
a special case, or whether the construct is assembled from what they already state. It also decides
what a read of an ordinary signal costs, because a takeover that has to be consulted on every read
is a tax on every design that never uses one.

## What the standard requires

Read from IEEE 1800-2023 10.6 rather than recalled, because two of its rules are the ones the
tempting shapes get wrong:

- A `force` on a net **overrides all drivers** -- gate outputs, module outputs, continuous
  assignments -- until released. On release the net immediately takes the value its drivers resolve
  to.
- A `force` or `assign` on a variable overrides procedural assignments. After a `deassign` or a
  `release` the variable **keeps the value it was last given** and does not revert; but where a
  continuous assignment or an active `assign` sits underneath a released `force`, that source is
  reestablished and a reevaluation is scheduled.
- The right-hand side is treated as a continuous assignment: while the takeover is in effect, a
  change to anything it reads reevaluates it.
- The target may be a range rather than a whole signal: `force` admits a constant bit-select or
  part-select of a vector net.

## What compiling per unit forces, before any lower contract is consulted

This is the load-bearing derivation, and it comes from the top: **the compile-time scope is the
compilation unit, and per-instance facts arrive at runtime construction.**

A `force` names one instance's target, and the unit that declares that target is compiled once for
every instance of it. So nothing a `force` needs may be a compile-time artifact of the target's own
unit -- and in particular the target cannot be given a compile-time twin, a shadow signal plus an
enable, materialized beside it because something forces it. Whether anything forces a given target
is a whole-design question: the code that forces it usually lives in another unit, a testbench that
reaches in by hierarchical name. Answering it at compile time would mean a unit could not be
compiled until every unit that might reach into it had been seen, which is a cross-unit dependency
nobody declared and which serializes work the architecture is shaped to keep independent.

**So the state has to live at runtime, on the thing being taken over. That is not a preference; it
is what is left once the compile-time shapes are ruled out.**

The second constraint from the top decides what that state may cost. The optimization target is the
whole edit-compile-run-inspect loop, simulation time included, so state carried for this construct
is paid by every design that never uses it. It therefore has to be absent until a takeover actually
happens, and it must not appear on the read path at all -- reads are what a simulation spends its
time on, and almost no cell in any design is ever taken over.

Everything below is downstream of those two.

## Findings that shaped the design

### F1. A forced value is not a driver

The cheapest-looking shape gives a forced value a drive strength above every other driver and lets
the existing net resolver pick it, which adds no concept at all. One argument defeats it, from the
standard alone:

**A wired-logic resolver combines rather than selects.** `wand` and `wor` fold their inputs through
a truth table (LRM 6.6.3), so a contribution at any strength is combined with the other drivers
rather than replacing them, and 10.6.2's "override all drivers" is not delivered at any strength.
The language committee reaches the same conclusion in its own discussion of force strength: while a
force is active the drivers are ignored rather than out-competed, and force carries no strength.

That this also leaves [net-driver-resolution](net-driver-resolution.md)'s post-Seal driver topology
undisturbed is a consistency check and not a reason. An invariant agreeing with a derivation is
worth noticing and is never evidence for it.

### F2. The operation acts on the wrapper, not on the storage it represents

MIR exists so that nothing downstream needs to know the source language was SystemVerilog, and a
node kind spelled after a SystemVerilog keyword fails that on its face. The sharper test is whether
a mechanical backend could translate such a node without deciding anything: a node meaning "force"
would leave each backend to invent what installing one does, and two backends are then free to
invent it differently.

What the four spellings actually do is change what determines a target's value -- not write a value
through it. That is an operation on the target's capability as an object, and the vocabulary for
that already exists, because attaching a driver to a net is the same kind of act. So they are
ordinary calls on the capability type and MIR gains no node kind.
[storage-access-as-place-formation](storage-access-as-place-formation.md) D3 draws the same line,
which is a confirmation reached afterwards rather than the reason.

### F3. The evaluation is the loop a continuous assignment already runs, and it must end when it can

Re-evaluating an expression whenever anything it reads changes is not a new requirement: it is what
a continuous assignment is, and evaluating a SystemVerilog expression is generated code by
definition. So the takeover's evaluation is that same evaluate-then-await loop. Reaching for it is
not reuse for its own sake -- the requirement is identical, and a second mechanism for it would be a
second thing to keep correct.

**A takeover whose source can never change must end rather than park.** The tempting reading is that
an empty trigger set is the N=0 case of the loop, "wait for nothing" being a legal wait that never
wakes -- and there is a precedent for that in an `always_comb` with a constant body. The precedent
does not carry: an `always_comb` is one per declaration and its count is fixed at elaboration, while
a takeover is one per _execution_ of a statement, so a testbench forcing inside a loop would leave a
parked evaluation behind on every pass. Unbounded growth in simulation is a cost the top-level
objective counts, so this is a defect rather than a tidiness question.

What the standard says gives the answer without a special case. An empty read set means nothing can
ever change the source, so the evaluation has no further work: the loop's continuation condition is
that the drive was accepted **and** there is something whose change could matter. With no
sensitivity the second half is false where the code is generated, so what is emitted is a single
drive and a return -- no run-time branch, and the zero case falling out of the general form at
compile time rather than being written twice.

### F4. The standard removes the need for shadow storage

A takeover looks like it must shadow the target: the displaced value has to come back when the
takeover ends, so a read has to ask which of the two is current. Two sentences of 10.6.1 remove it.
An `assign` overrides _all_ procedural assignments, and after a `deassign` the variable keeps its
current value rather than reverting. A procedural write made while a takeover is in effect is
therefore never observable, at any later time, so there is nothing to preserve and nothing for a
read to choose between.

## The decision

1. **A takeover is state on the target's capability type, at one of two precedence levels** -- the
   level an `assign` occupies and the level a `force` occupies, the latter outranking the former and
   both outranking the target's ordinary value source. Installing at an occupied level replaces what
   is there, which is what the standard requires of a second `assign` and of a second `force`.

2. **What a target shows is what its storage holds, so reads are untouched and the cost sits on the
   write path.** What a level records under D4 is the value that level computed, never a copy of
   anything it displaced.

   **What happens to a displaced write differs between the two targets, because the standard makes
   it differ, and getting this backwards is the easy mistake.** A procedural write to a variable
   under a takeover is discarded outright and is never observable, at any later time -- which is
   what lets a released variable keep the value the takeover gave it instead of reverting. A net's
   drivers, by contrast, go on updating their contributions the whole time it is forced; only the
   resolved value the net shows is overridden. That is forced by 10.6.2's rule that a released net
   is immediately assigned the value its drivers determine: a driver update discarded while the
   force was in effect would leave the net unable to answer with anything current when the force
   ends.

3. **The evaluation is an execution of no lineage, started where the statement runs, and it stops
   itself.** Its body is the evaluate-then-await loop a continuous assignment already uses. Nothing
   reaches in to end it: it carries the generation it started under, and stops as soon as the target
   says that generation is no longer the one driving its level. Two things follow. The ending
   statement needs no handle on it, so `deassign` and `release` are ordinary calls on the target
   like the rest. And the language makes a takeover no process of its own -- `disable fork` does not
   end a force, and `wait fork` does not wait for one -- so the execution belongs to no lineage and
   nothing that names processes finds it.

   **An evaluation whose source can never change ends after driving once**, because there is nothing
   left for it to do; F3 has why the alternative is a leak rather than a resting state.

4. **Each level in effect records what it last evaluated to, including while a higher level covers
   it.** So ending a level hands the target to the value the level beneath it already holds, and the
   standard's "reestablish that assignment and schedule a reevaluation" is delivered with nothing
   recomputed and nobody woken. A level goes on recording while covered because its own operands go
   on changing, and what it would show has to be current at the instant the cover is removed.

5. **Removal restores nothing on a variable and re-resolves a net.** The two targets differ here
   because the standard makes them differ, and they are already distinct capability types whose
   write capability is what separates them; neither borrows the other's ending.

## Consequences

- No MIR node kind is added, and no backend gains a construct: the four spellings are calls on a
  capability type plus the process and await primitives that already exist.
- A design that never forces anything pays nothing on any read, and pays one comparison on a write.
- The net model keeps its post-Seal immutability, because nothing here attaches or detaches a
  driver.
- A constant right-hand side needs no special path, being the empty-trigger case of the general
  loop.
- Drive strength on a forced value has nowhere to live, which is correct: the standard gives a force
  no strength.

## Rejected

- **A compile-time twin beside each target -- a shadow signal and an enable.** The shape Verilator
  used until 2026, and the one to reject first, because it is cheap where a whole design is compiled
  at once and impossible here. Materializing it requires knowing at the target's compile time
  whether anything forces that target, and what forces it is usually a testbench in another unit
  reaching in by hierarchical name. Asking the question is a cross-unit dependency nobody declared;
  answering it serializes compilation the architecture is shaped to keep independent. Verilator's
  own move away from it was driven by expressiveness rather than by this -- a compile-time twin
  cannot carry a conditional force or two forces on one target -- which is worth knowing, because
  the shape fails twice for unrelated reasons.

- **A forced value as a maximum-strength driver.** F1: no strength delivers "override all drivers"
  on a wired-logic net, because that resolver combines rather than selects.

- **Shadow storage under every takeover-capable target.** F4 shows nothing needs restoring, so the
  shadow would be write-only state, and consulting it would put a branch on the read path of every
  signal in the design to serve a construct almost none of them ever meet.

- **A dedicated MIR statement for each spelling.** The dividing question in F2 already classifies
  these as operations on a wrapper, and a node kind invented for one would have to be given a
  realization by each backend separately -- the shape
  [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) removed for the wait.

- **Evaluating the right-hand side in the runtime rather than in a process.** The runtime cannot
  evaluate a SystemVerilog expression; only generated code can. A callback holding the expression
  would be a process with its lifetime hidden.

## Cross-references

- [net-driver-resolution](net-driver-resolution.md) -- the driver topology this must not disturb.
- [storage-access-as-place-formation](storage-access-as-place-formation.md) -- the dividing question
  that makes these calls rather than stores.
- [value-change-wait-as-runtime-call](value-change-wait-as-runtime-call.md) -- the
  evaluate-and-await loop and its empty case.
