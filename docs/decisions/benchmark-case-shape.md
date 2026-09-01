# A benchmark case fixes what work is done and lets the harness choose how much

Date: 2026-08-31. Status: accepted.

## Why this decision matters

A benchmark's numbers are worth having only if a number taken today can be read against one taken a
year ago. Everything else about a benchmark suite is arrangement; this is the property that decides
whether it was worth running at all.

It is also the property that a simulator makes hard, because the tool under measurement is expected
to get faster by orders of magnitude. A workload sized to take a second on today's engine takes two
milliseconds on an engine a few hundred times quicker, which is process startup and nothing else.
Sized instead for the engine one hopes to have, it does not finish today.

Established suites have settled this, and none of them settles it by writing a number down. Go's
`testing.B` raises the iteration count until a run reaches a target duration and reports the cost of
one operation. Google Benchmark does the same. SPEC CPU reports a ratio against a reference machine
rather than a time. LLVM's test-suite -- the closest analogue, being whole programs compiled and run
-- leaves the numbers to a separate service that holds their history.

## The axis that decides everything: what is measured, and how much

Every benchmark carries two quantities that look alike and behave oppositely.

```text
the shape of the work   -- array length, process count, pipeline depth
                           fixed; it is what the case measures, and its identity

the amount of the work  -- iterations, cycles
                           free; it only decides how long the measurement takes
```

Per-unit cost depends on the first and not on the second. So two runs at different shapes are not
comparable at all, while two runs at different amounts are comparable exactly, once the amount is
divided out. Confusing the two is what makes a suite's numbers rot: a fixture shrunk to fit a budget
looks like a rescaling and is really a different measurement.

That distinction generates every decision below.

## Two things can be timed, and only one of them is a simulation

A case times either running a design or building one, and which it is decides how its amount of work
reaches it. A simulation's amount is a runtime argument, so one build serves every amount. A build's
amount **is** the design -- more modules, wider signals -- so it has to be fixed before the build
starts, which makes it a compile-time constant of necessity rather than by choice.

That is the one place the two diverge. Everything else below holds for both, because both are a
duration divided by an amount.

## The decisions

```text
D1. A case is a SystemVerilog program that performs one kind of work a chosen number of times. One
    that times a simulation also consumes its own result observably, because its translation unit is
    compiled optimized and a loop whose result nothing reads is free to be deleted -- the case would
    measure the empty program. One that times a build is compiled the way an edit loop compiles,
    since that is the cost it exists to protect, and it runs once only to prove the artifact is
    real.

D2. A simulation's amount of work is a runtime argument, never a compile-time constant. The program
    reads it from a plusarg (LRM 21.6) and falls back to a default when none is given, so one build
    serves every amount and the harness may choose the amount after compiling. A build's amount is
    a top-level parameter it is given (LRM 23.10), which is how a value reaches a design before
    elaboration, and every probe is a fresh build in a directory nothing has built in.

D3. The harness chooses the amount, by measuring probes and scaling from them to reach a target
    duration. No amount the measurement uses is written down anywhere, so none can go stale, and an
    engine that gets a thousand times faster is followed automatically rather than by editing files.
    A case still carries a default for the amount, which is what it runs at when someone runs it by
    hand; nothing measured ever reads it.

D4. The reported quantity is work per second, never elapsed time, and what a measurement cost before
    any of the work is taken out of it first. A time means nothing without the amount beside it; a
    rate is the same number whatever the amount was, which is what lets D3 pick amounts freely. The
    fixed part is a rounding error against a simulation and most of a small build, so leaving it in
    would make a build case's number move whenever the compiler's prelude cache did. Two probes
    separate it, which is why at least two are always taken.

D5. The shape of the work is fixed in the source and is the case's identity. It is never a
    parameter and never adjusted to fit a budget. Shrinking an array to make a suite quick shrinks
    whatever the array's length was costing, which is frequently the thing the case exists to show.

D6. A reference simulator is measured in the same run, on the same machine, scaled independently to
    the same target duration. The comparison is a ratio of rates, so it survives a slow machine, a
    loaded machine, and a change of machine, none of which an absolute time survives.

D7. There is no per-case manifest. What the source cannot state is a `// @key: value` directive at
    the top of the file it describes, and prose describing what the case isolates is an ordinary
    comment that is never parsed. This is the conformance corpus's rule and it is adopted for the
    same reason: a sidecar restates what the path already says and can disagree with it.

D8. The outer directory is the cost family the case isolates; the inner one is the case. A suite
    with one case per family cannot say which family a regression landed in, and that question is
    the only reason to have more than one case.

D9. A tool that cannot measure a case says so and does not fail the run: a reference simulator that
    is not installed, or a construct Lyra does not yet carry out, is a fact about the tool and not
    about the case. A build or run that fails for any other reason does fail it.

D10. The benchmark is not a Bazel test and not a merge gate. A timing needs a machine that is not
     running anything else, and a build system deliberately runs its tests in parallel. Its
     scheduled run gates on a case building and running, never on a duration, since a duration is a
     claim about the machine that produced it.
```

## Rejected alternatives

- **A per-case manifest carrying named scale profiles.** This is the shape being replaced. Its
  numbers are maintained by hand, and because they encode how fast the engine was when someone chose
  them, they are wrong after any large change in either direction -- too slow to finish before a
  fix, too small to measure after one. It also splits a case across two files that can disagree, and
  in practice did: the manifest named the category the directory already stated, so the harness
  carried a check that the two matched.

- **Reporting elapsed time.** It ties every number to the amount it was taken at, which means the
  amount can never be changed without invalidating the history. That is the constraint that makes
  the manifest's numbers unmaintainable in the first place.

- **Exposing the work's shape as a scale parameter.** It reads like more configurability and
  destroys comparability, because per-unit cost is a function of it. The two look identical in a
  manifest, which is exactly why the distinction has to be structural: amount is a runtime argument,
  shape is a constant in the source.

- **A single amount chosen once, carefully, for all time.** This is what the suite had before, and
  the amounts were sound when they were chosen. Nothing is wrong with them except that an engine
  that changes by three orders of magnitude leaves no single amount usable at both ends.

- **Detecting regressions inside the runner.** Every surveyed suite keeps this separate -- Go has
  `benchstat`, LLVM has a results service, Criterion keeps a stored baseline. Comparison needs
  history and a statistical model; a runner needs neither, and a runner that owns both cannot be
  used to answer a one-off question.

- **Running the suite as Bazel tests.** It would give the corpus discovery, sharding and caching for
  free, and each of those is actively wrong here: sharding runs cases concurrently, and caching
  returns a duration measured on some earlier machine state.

## Where D3 came from

The first attempt at repairing this suite kept the manifest and re-chose every number in it, sizing
each case to about a second on the current engine. Every objection to that came back to the same
place. The numbers now tracked engine speed, so they would need choosing again after any real
improvement. They no longer matched the amounts recorded alongside the pre-reset measurements, so
the comparison those measurements existed for was gone. And one case had its array shortened from
32768 elements to 2048 to make it finish, which moved it from 41,000x the reference tool to 1,000x
-- in line with every other case, and therefore hiding the single largest defect the suite had
found.

Each of those is a symptom of a human choosing the amount. D3 removes the choice rather than making
it better, which is why it is the decision the others hang from.

## Consequences

- Every scale profile, tier, and hand-chosen iteration count leaves the corpus, along with the
  configuration file that held the tiers and the per-case project files.
- A case is a directory holding SystemVerilog, entered through one file, on the same terms as a
  conformance case -- so the two corpora share their naming rules, their directive syntax, and the
  way a case declares companion sources.
- The suite no longer depends on project-file lookup, since a case names its own sources by sitting
  beside them.
- A case too slow to reach the target duration still reports: it takes its two readings at one and
  two units and reports what they give. The suite therefore has no notion of a case being too slow
  to measure, only of one being slow.
- A case whose work is dwarfed by what it has to do before any of it can still report the wrong
  thing, and the suite cannot tell. Separating a fixed part from a marginal one needs the marginal
  part to be above the noise between two readings, and where it is not -- an array that has to be
  filled by the very path that is slow -- the whole duration is charged to the work. Such a rate is
  a lower bound, and the case becomes truthful again when the path underneath it does.
- Comparing two revisions is a separate program that reads two result files. Nothing in the runner
  knows what "before" was.

## Cross-references

- `conformance-case-shape.md` -- the corpus this one borrows its directory, directive and naming
  rules from, and the reasoning behind each.
