# Every body the front end does not share is lowered

Status: Accepted, 2026-10-31. Replaces the choice, stated in
[`specialization_model.md`](../architecture/specialization_model.md) before this, that a key worked
out before lowering decides which instances share an artifact.

## Requirement

Every instance runs the code its own elaboration states, and two instances share compiled code only
where what they state comes out equal; nothing worked out before lowering decides that one instance
may run code lowered from another's body.

The key it replaces was meant to name every fact that can make two bodies elaborate differently. It
was kept right one field at a time, missed four in one week, and each miss had an instance run code
lowered for another with no error. A SystemVerilog body reaches out of its instance (LRM 23.8,
9.6.2, 23.10.1), so what it states follows where it stands, a question rustc's
`Instance { def, args }` never has.

## Choice

- **Which elaborations are alike is the front end's word, read as given.** The front end leaves an
  instance it finds alike an earlier one pointing at that one's body, and shares none a hierarchical
  name leaves upward, nor any a defparam, a bind or a configuration reaches. A wrong share is the
  front end's defect, corrected there; Lyra keeps no second authority on it.
- **Every body nothing points at is lowered, once, for its own instance under its own environment.**
  No key worked out before lowering says two instances may share a lowering.
- **Equal code is found by comparing what was lowered.** The lowerings of one unit whose HIR is
  equal are one body, compared whole after a digest narrows the candidates, as lld folds identical
  code and LLVM's `MergeFunctions` merges equal functions.
- **A lowering is named by its instance's hierarchical path, inside a run only**, and a body by its
  unit and its order among the unit's bodies, so that adding an instance elsewhere or editing a body
  renames no unit.

## Alternatives turned down

- **A key worked out before lowering, naming every fact that can make two elaborations differ.** It
  fails every instance running its own code. The failure rests on the facts that can reach a body
  having no list anyone can keep complete.
- **Checking the front end's sharing by lowering each pointed-at instance through its own body.** It
  fails one authority on one meaning. The failure rests on the front end, which has every scope open
  at once, being the one place the answer is computed.
- **A body named by a digest of its HIR.** It fails an edit rebuilding only what it changed, since
  every edit of a body renamed it, so the root's tables and the emitted file were written again on
  the common edit.
- **A body named by the least key among its lowerings, or by the path of its first.** It fails the
  same, renaming a body when an edit hands an instance a smaller key or adds one earlier in the
  design.
- **A switch turning the sharing of equal HIR off, for a test.** It fails having no product option
  only a test reads. The count of bodies asserted both ways states the same property.

## Consequences across layers

- Lowering every body the front end did not share costs little beside everything after HIR, which is
  most of a build. The premise is that ratio; a front end or a lowering that changed it would reopen
  the choice.
- N instances a hierarchical name leaves upward are N lowerings before they are found equal.
- A query-based build is left room to key a lowering on its instance's hierarchical path, reading
  its body and environment, as rustc memoizes a query per `Instance`
  ([`specialization_model.md`](../architecture/specialization_model.md),
  [`incremental_build.md`](../architecture/incremental_build.md)).
- The scheme as it holds is [`compiling_a_design.md`](../architecture/compiling_a_design.md)'s and
  [`specialization_model.md`](../architecture/specialization_model.md)'s.
