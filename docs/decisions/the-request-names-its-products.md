# A request names the products it is answered with

Date: 2026-09-12 Status: accepted

## Context

A compile took how far to run as a value and answered with one slot per stage down to that depth.
Which slots were filled followed from the depth and from nothing any type stated, so every reader
had to know the depth its caller had asked for and trust that it matched.

Three things came out of that, and only the first is the one anybody notices.

**A product had to be read through a check that turned an empty slot into a compiler bug.** Two such
helpers stood in the tree, one at the command layer and one at the front end's own, and eight reads
went through them. Each was correct and each was a bug report waiting for a driver that asked for
less than it read. A third accessor of that kind had no caller at all, which is what a slot nobody
can predict the contents of eventually costs: surface added in case somebody needed it.

**An absence stood for more than one outcome.** The front end's design was absent for a request that
stopped at elaboration, and for a lowering that failed; the elaborated syntax beside it was absent
for those and for a third reason -- it is released on purpose once its last reader finishes, which
is a lifetime rather than a failure. A unit's executable body was absent for a request that stopped
one layer short, and, according to the three consumers that guarded against it, for a unit that has
no body at all.

**The depth was stated twice.** One switch over the command kinds answered how far the front end had
to run; a second switch ran the command and read what it liked. Nothing held the two together, so a
command that asked for less than it read compiled and failed at run time, in the check above.

## What the survey says about where a depth lives

Three systems, and none of them carries a depth past the driver.

- **clang** names an ordered `phases::ID` -- its own comment calls it "ordered values for successive
  stages in the compilation process which interact with user options" -- and pairs it with the
  artifact by `types::getCompilationPhases(Id, LastPhase)`, "the list of compilation phases to be
  done for type Id up until including LastPhase". The driver computes that list before any work runs
  and builds a graph of actions, each node carrying the type it produces. A phase's product is the
  next phase's typed input, never a slot somebody asks about afterwards.
- **rustc** has no depth value at all. Its `Callbacks` hands each stage's product to the caller at
  that stage -- the parsed crate, then the type context -- and each call answers whether to go on.
  Where you stopped is what "how far" means, and there is nothing left over to describe.
- **Swift's frontend** keeps one flat action enum covering both depth and what to print, and defines
  the questions over it as named total functions -- `doesActionGenerateSIL`, `doesActionGenerateIR`,
  `doesActionProduceOutput`. The invocation fixes the action; nothing recomputes it downstream.

**Where our conditions differ, and it is the sentence the rest of this rests on.** clang and rustc
both put a process or a job boundary between a stage and its reader: clang hands a file to the next
job, rustc hands a borrowed reference and then tears the compilation down. Neither ever needs a
value describing what a finished run produced. A command here **is** the last stage -- it writes the
project, or builds and runs it -- so the products cross back to the caller and what a run produced
has to be a type in this program. Being a type, it is the request's to name.

## Decision

**What a compilation step answers with is decided by what was asked of it, so no caller asks whether
a product it requested is there.** Asking is calling the entry for what is wanted: one that answers
with the elaborated source, one with the design's HIR, one with every unit modelled semantically,
one with every unit in the form something runs. A depth is not a value, not a parameter, and not
carried anywhere.

**The only absence left means the run failed, and the account is in the sink.** That is one meaning
at one boundary, and it is what the collect-and-continue rule already requires: a run that reported
a gap produces nothing at all, because what is short of the design is not the design
(`reporting-every-gap-in-one-run.md`).

**A product a successful run still lacks is not an absence.** It is either a different request or a
different kind of thing, and the answer's type says which. Where a kind of thing genuinely has no
such product, the party that already knows which kind it is says so -- never a slot the consumer
tests.

## Consequences

- Six slots, two refusing helpers and one ordered scale left the compiler driver, and with them the
  only way to spell a request whose answer does not fit it.
- A command states its own depth once, by calling for what it reads. The switch that answered how
  far the front end must run is gone, so a new command cannot disagree with itself.
- The elaborated syntax is taken by the step that lowers it rather than released by a member reset,
  so its lifetime is in the signature instead of in a comment.
- The design root stops being a third kind of artifact. It is synthesized as a unit and taken the
  rest of the way by the same entry every other unit uses.
- A compiled unit's two halves -- the body and the metadata defining it -- travel as one value, so
  whatever composes a program takes a sequence of units rather than two sequences it has to index in
  step. That coupling had been stated in prose on the entry it crossed, which is where a type
  belongs instead.

## Rejected

- **A depth fixed at compile time instead of at run time** -- a tag type per stage, with the answer
  deduced from it. It expresses with a tag family and a trait what two named entries express with no
  mechanism at all, and the survey's own answer is that a driver which has already fixed the depth
  has nothing left to carry.
- **One entry answering with an alternative per depth**, consumed by a visit. Every call site knows
  which depth it asked for, so each visit would have one live arm and one unreachable one --
  alternatives no consumer tells apart are not alternatives.
- **Keeping the refusing accessor and making it the rule.** It turns a driver's mistake into a
  report rather than undefined behaviour, which is worth having exactly where the mistake remains
  possible. rustc's `ErrorGuaranteed` is the shape for an absence that must travel and be trusted;
  here none travels.
- **Lowering only the units that root objects into an executable form.** This was the reading three
  consumers stated in their own comments, and it is false: every unit has a body, a package
  included, because a package's variable initializers and its subroutines are code like any other.
  Acting on it would have dropped every package's initializers from what a session loads. The three
  guards were always taken, and dumping the executable form of a design with a package shows the
  package's own unit in it.

## Cross-references

- `reporting-every-gap-in-one-run.md` -- why a run that reported anything produces nothing, which is
  what leaves exactly one absence to spell.
- `../architecture/compiler_overview.md` -- the pipeline whose stages these requests name.
- `../architecture/emission_model.md` -- the unit boundary the per-unit walk keeps.
