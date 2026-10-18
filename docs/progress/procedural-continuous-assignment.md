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
      evaluated continuously while installed, with a procedural write discarded at the positions it
      holds while one is in effect. The evaluation is the loop a continuous assignment already runs,
      and a source nothing can change ends it after one pass rather than leaving it parked -- a
      takeover is created per execution, so anything it leaves behind accumulates.

- [ ] PCA2 -- `assign` and `deassign` on a variable (LRM 10.6.1), including a second `assign` to an
      already-assigned variable ending the first, and a concatenation of variables as the target.
      Both keywords are in for a variable and for a concatenation of variables, each member held to
      its share of one evaluation of the source; the supersede case is carried by the mechanism but
      is not covered by a case.

- [x] PCA3 -- `force` and `release` on a variable (LRM 10.6.2), at a precedence above PCA2's, with
      both endings the standard gives: the value retained where nothing else drives the variable,
      and the continuous assignment or active `assign` reestablished and rescheduled where one does.
      A release hands the variable back to an `assign` underneath it, or to the continuous
      assignment driving it, with the value that source holds once the reevaluation the release
      schedules runs. A variable that stands for what its port connection drives it with is forced
      without what drives it being touched: it, every port it is handed on to, and every wait on
      either follow the forced value until the release, and show what drives it again from then.
      Such a port is forced as any variable is, alone or as a member of a concatenation. A variable
      of every type a takeover may name is taken over on both backends -- integral, `real`,
      `shortreal`, `string` and a class handle -- and the same holds for `assign` and `deassign`. A
      `chandle` may not be named (LRM 6.14), and the front end refuses it.

- [x] PCA4 -- `force` and `release` on a net, overriding the resolution rather than contributing to
      it, and on release resolving the drivers immediately so the net takes their value in the same
      step. The drivers go on updating their contributions while the force is in effect, which is
      what makes that release answer with a current value.

- [x] PCA5 -- A constant bit-select or part-select of a vector net as a force target, on both
      backends: the bits nobody forced go on showing what the net's drivers resolve to, its one
      driver gives, or the object that driver carries, and a released bit takes that value at once.

- [ ] PCA6 -- A `force` or an `assign` naming a whole variable of an unpacked structure, union or
      array type -- fixed-size, dynamic, queue or associative -- is refused. The variable either may
      name is a singular one (LRM 10.6.1, 10.6.2), which none of those is (LRM 6.4), and no open
      simulator takes the text, so nothing argues for running it on request. The front end accepts
      it today, which is its defect to correct; until then the C++ backend carries it out, and the
      execution backend does so for a structure, a union and a fixed-size array and ends the build
      as a failure of the compiler's own for the rest.

- [x] PCA7 -- A force on part of a variable, when the caller asks for it. The standard forbids it,
      and Lyra runs it as an extension whose meaning is the rules for a whole variable applied to
      each forced position: a procedural write leaves the forced part as it is while the force is in
      effect, and the release keeps the forced value of that part until the next procedural
      assignment to it, or hands it back to a continuous assignment that drives it. Bits of a packed
      variable, a member of an unpacked structure, an element or a slice of a fixed-size unpacked
      array, and bits within any of them are carried on both backends: a write of any part of the
      variable -- whole, through a member, through a `ref` argument -- keeps the forced positions
      and stores the rest, a later force of overlapping positions takes them from the earlier one,
      and a release of some positions ends the force on those alone. Part of a port that stands for
      what its connection drives goes on following that source where nothing is forced. Refused
      still: a member of an unpacked union, which is one storage its members read differently (LRM
      7.3), and part of a string, a queue, a dynamic array or an associative array, whose parts lie
      at no fixed place in the value (LRM 7.10.3).

- [ ] PCA8 -- A takeover of an `event` variable (LRM 10.6, 15.5.5). Refused where it is written, as
      giving an event a value is.

- [x] PCA9 -- A force on a module's `ref` port, from inside the module or by a hierarchical name and
      through any number of `ref` ports, is a force on what the port is connected to (LRM 23.3.3.2),
      on both backends: a variable, which every reader of it then sees, or an input port, which the
      `ref` port follows and whose source a force through it leaves alone.

## What a read costs, settled

**Nothing, and the standard is what makes that available.** The question looked like it reached the
value model: if a takeover shadows a target, a read has to consult the shadow, and almost no target
ever carries one. Reading 10.6.1 closes it instead. An `assign` overrides _all_ procedural
assignments to the variable, and after a `deassign` the variable keeps the value it was last given
-- so a procedural write made while a takeover is in effect is never observable at the positions it
holds, at any later time. There is nothing to shadow.

So a takeover's value lives in the target's own storage, a procedural write arriving while one is in
effect is discarded at the positions it holds and lands everywhere else, and removing a takeover
restores nothing because nothing was displaced. Reads are untouched, and the whole cost sits on the
write path, where the check is one comparison against state the target already has to carry.

**A release still owes one thing that is not removal.** Where a lower source is active underneath --
a continuous assignment, or an `assign` beneath a `force` -- the standard requires that source to be
reestablished and a reevaluation scheduled (LRM 10.6.2). An `assign` goes on recording what it
evaluates to while a force covers it, and what a continuous assignment writes while one does is kept
beneath it rather than discarded, so in both cases nothing is recomputed. The release leaves the
variable as it is and schedules the reevaluation, which shows that value when it runs in the Active
region; until then the process that released still reads the forced value. That value is part of the
takeover's own state, so it exists only while something covers the variable or the reevaluation is
still to run, and no read consults it.

## Open questions

- **Whether `assign` is worth carrying at all.** The standard notes that the procedural `assign` and
  `deassign` constructs are under consideration for deprecation (LRM 10.6.1, C.4.2). That is not a
  reason to skip them, since existing code uses them, but it does argue against paying any
  structural cost for them beyond one precedence level on the mechanism PCA1 establishes.
