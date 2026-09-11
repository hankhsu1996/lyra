# A run reports every gap it met, not the first

Date: 2026-09-10 Status: accepted

## Why this decision matters

Lowering returned on its first failure, so a run over a design yielded exactly one refusal however
many stood behind it. That made the developer loop one gap long -- fix, rebuild, run, find the next
-- and it made measuring a design's readiness a sequence of runs rather than a run. The shape a
compiler lands in here decides whether "what does this tool not support yet" is a question with an
answer or a question with a first answer.

## What the objective requires, before any lower contract is consulted

`north_star.md`'s first invariant is that **end-to-end iteration time is the primary optimization
target -- the whole edit-compile-run-inspect loop**. A compiler that reports one error per run makes
the loop's length the number of errors, and no stage of it can be made fast enough to compensate.
That is the derivation; everything below is how far it can be taken honestly.

The honest limit comes from the other direction. A refusal means a construct has no lowered form, so
there is no IR to carry on with, and anything built after it is not the design. Reporting more
therefore must not mean producing more: a run that met a gap reports every gap it can and produces
nothing.

## What makes one gap's neighbours still reachable

A pass could report everything and be wrong about most of it, if a failure left its neighbours
unlowerable. What rules that out is already stated.

`declarations-before-bodies.md` D1 puts lowering in three stages -- take identities, settle
declarations, lower bodies -- and says there is no fourth **because nothing ever needs another
entity's body in order to be built**. D5 says the same from the other side: a body attaches to a
declaration that already exists and grows no shape a peer reads.

That is exactly the property a recovery point needs, and it is a property of the stage rather than
of any construct in it. So:

- **A body that will not lower is reported, and its siblings lower anyway.** Nothing reads it.
- **A declaration that will not settle ends its stage.** Every later stage reads declarations, so
  continuing past one produces failures for want of a shape nobody stated -- follow-on errors that
  bury the account this exists to give. The driver already applies that reading to the front end: a
  compilation slang rejected is not lowered, for the same reason.

## The decision

1. **Collect within a stage; stop between stages.** Every unit's declarations are attempted, then
   every unit's bodies, then the per-unit vertical below HIR, then the one whole-design step. A
   failure inside a stage is reported and the stage continues; a stage that reported anything is the
   last one that runs.

2. **A scope's member walk cannot fail.** It reports and goes on, so it answers with the scope it
   managed to build and nothing else. Whether anything was reported is the sink's answer and the
   only one; the walk carries no second one, which is what keeps a caller from branching on a
   question already answered elsewhere.

3. **What a run that reported anything produces is discarded, and no stage is asked to be
   complete.** The artifact is short of the design by exactly what was reported, so the checks that
   hold a unit to what its consumers require do not run on it -- holding it to that contract would
   answer a stated gap with a bug report.

4. **This is not a mode.** There is no flag that asks for the whole account, because there is no
   reason to ask for less: the run fails either way, and the work of continuing is work the next run
   would otherwise repeat.

## Consequences

- A design's readiness is measured in one run. The per-module workaround that existed only to widen
  a pass -- compile each module as its own top so one module's first blocker does not hide the rest
  -- is no longer needed.
- A gap standing behind another **inside the same body** is still unseen, because the body is
  abandoned at its first refusal. That boundary is the same one that makes the rest sound, and
  moving it would need a statement about what a half-lowered body is.
- The diagnostic subsystem needed nothing: the sink collects, the renderer already counts and
  pluralizes, and `diag::Make` already built a diagnostic for the report-and-continue path. Only the
  lowering was answering one at a time.

## Rejected

- **A flag that asks for every gap.** Two shapes for one question, and the cheap one is wrong: a run
  that stops early has done work the next run repeats, and the user who wanted one error can read
  the first line.
- **Continuing past a declaration failure.** Every later stage resolves names against declarations,
  so what follows is failures for want of a shape nobody stated. The count would grow and the
  account would get worse.
- **Continuing inside a body after a refusal.** A statement that will not lower leaves the
  statements after it with no defined relationship to it -- a `disable` naming a block that was
  never built, a read of a variable whose initializer was abandoned. Recovering there needs a
  statement about what a half-lowered body means, which nothing needs yet.
- **A per-member error limit.** slang already caps its own diagnostics, and a cap on this side would
  answer a question nobody has asked; the count a real design produces is the measurement, not a
  problem to bound in advance.

## Cross-references

- `../architecture/north_star.md` -- the iteration-loop objective this serves.
- `declarations-before-bodies.md` -- the staging, and why bodies are last.
- `diagnostic-construction.md` -- the two construction surfaces, split by control flow.
