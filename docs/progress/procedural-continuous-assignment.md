# Procedural continuous assignments

Tracks LRM 10.6: a procedural statement that takes over a target's value with a continuously
evaluated expression until the takeover is explicitly ended. Two features share the clause and one
pair of syntax forms -- `assign` / `deassign` on a variable (10.6.1), and `force` / `release` on a
variable or a net (10.6.2).

Done when a design may take over a variable or a net from procedural code, in both features, and
every path that reads or drives that target sees the taken-over value while the takeover is in
effect and the correct value after it ends -- including the two different endings the standard gives
for a released variable.

## Contracts

This workstream reasons from these and does not restate them:

- `../architecture/net_resolution.md` -- a net is a resolved observable value plus independently
  attached driver contributions under the net type's resolver.
- `../decisions/net-driver-resolution.md` -- the settled net model: a resolution node with
  node-owned contributions, attachment during Resolve, seeding in Initialize.
- `../decisions/value-store-discipline.md` -- how a write reaches a cell.
- `../architecture/elaboration_lifecycle.md` -- the phase protocol an attachment rides on.

## What the standard requires

Read from IEEE 1800-2023 10.6 rather than summarized from memory, because the two features differ in
ways their shared syntax hides:

- **Targets differ.** An `assign` names a singular variable or a concatenation of variables, and
  never a bit-select or part-select. A `force` additionally names a net, a constant bit-select of a
  vector net, or a constant part-select of one -- so a takeover covers a **range of a target**, not
  necessarily a whole one.
- **What each overrides differs.** An `assign` overrides procedural assignments to the variable. A
  `force` overrides procedural assignments, continuous assignments, and an active `assign`; on a net
  it overrides every driver.
- **Re-applying is defined.** A second `assign` to a variable that already has one deassigns it
  first.
- **The two endings differ, and this is the subtle part.** Releasing a **net** makes it immediately
  take the value its drivers resolve to. Releasing a **variable** retains the current value until
  the next procedural assignment -- unless the variable is driven by a continuous assignment or has
  an active `assign`, in which case that assignment is reestablished and a reevaluation is
  scheduled.
- **The source expression is continuous.** While a takeover is in effect, a change to any variable
  the right-hand side reads reevaluates it, exactly as a continuous assignment does.

## The order the work falls in

**The variable half introduces the concept and the net half consumes it**, which is the reverse of
the order the clause is written in. A net is already a runtime node whose value has one funnel, so
taking it over is a state on an object that exists and one place to apply it. A variable is a cell
written directly, with nothing to attach a state to and nobody positioned to refuse a write while a
takeover is in effect -- so what the variable half establishes is the mechanism itself.

`../decisions/procedural-continuous-assignment.md` holds why the state is runtime state on the
target and why a forced value is not a driver; this file does not restate it.

## Sub-steps

- [x] PCA1 -- A variable carries a takeover: a source expression installed at a stated precedence,
      evaluated continuously while installed, with a procedural write discarded while one is in
      effect. The evaluation is the loop a continuous assignment already runs, and a source nothing
      can change ends it after one pass rather than leaving it parked -- a takeover is created per
      execution, so anything it leaves behind accumulates.

- [ ] PCA2 -- `assign` and `deassign` on a variable (LRM 10.6.1), including a second `assign` to an
      already-assigned variable ending the first, and a concatenation of variables as the target.
      The plain form of both keywords is in; a concatenation target is refused by name, and the
      supersede case is carried by the mechanism but is not covered by a case.

- [ ] PCA3 -- `force` and `release` on a variable (LRM 10.6.2), at a precedence above PCA2's, with
      both endings the standard gives: the value retained where nothing else drives the variable,
      and the continuous assignment or active `assign` reestablished and rescheduled where one does.
      Both keywords are in, and a release hands the variable back to an `assign` underneath it with
      that assignment's current value. **What is not in is a release over a continuous assignment**
      (LRM 10.3 on a variable): that assignment is not one of the takeover levels, so its writes are
      discarded while the force is in effect and nothing reestablishes it on release -- it waits for
      its own sensitivity to fire again, which the standard does not allow.

- [x] PCA4 -- `force` and `release` on a net, overriding the resolution rather than contributing to
      it, and on release resolving the drivers immediately so the net takes their value in the same
      step. The drivers go on updating their contributions while the force is in effect, which is
      what makes that release answer with a current value.

- [ ] PCA5 -- A constant bit-select or part-select of a vector net as a force target, which is what
      makes the range the unit of a takeover rather than the signal.

## What a read costs, settled

**Nothing, and the standard is what makes that available.** The question looked like it reached the
value model: if a takeover shadows a target, a read has to consult the shadow, and almost no target
ever carries one. Reading 10.6.1 closes it instead. An `assign` overrides _all_ procedural
assignments to the variable, and after a `deassign` the variable keeps the value it was last given
-- so a procedural write made while a takeover is in effect is never observable, at any later time.
There is nothing to shadow.

So a takeover's value lives in the target's own storage, a procedural write arriving while one is in
effect is discarded, and removing a takeover restores nothing because nothing was displaced. Reads
are untouched, and the whole cost sits on the write path, where the check is one comparison against
state the target already has to carry.

**A release still owes one thing that is not removal.** Where a lower source is active underneath --
a continuous assignment, or an `assign` beneath a `force` -- the standard requires that source to be
reestablished and a reevaluation scheduled (LRM 10.6.2). That source is parked on its own trigger
set and nothing it watches has moved, so removing the takeover above it cannot wake it. The removal
has to wake it explicitly, which is why a target tracks the identity of what installed each level
rather than only that the level is occupied.

## Open questions

- **Whether `assign` is worth carrying at all.** The standard notes that the procedural `assign` and
  `deassign` constructs are under consideration for deprecation (LRM 10.6.1, C.4.2). That is not a
  reason to skip them, since existing code uses them, but it does argue against paying any
  structural cost for them beyond one precedence level on the mechanism PCA1 establishes.
