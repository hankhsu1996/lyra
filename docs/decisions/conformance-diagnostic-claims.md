# A requirement whose observable is a report is stated as a directive, in both directions

Date: 2026-09-01. Status: accepted. Revised 2026-10-27: a claim names the kind of report and what
the standard fixes of it, never its wording, and an absence is of a kind of report rather than of
all output.

## Why this decision matters

A conformance case checks itself in SystemVerilog, and the judgement reads whether the sentinel was
printed and how the run ended. That shape is what makes a case portable and its oracle independent,
and it is settled in [conformance-case-shape](conformance-case-shape.md). It also has a boundary:
IEEE 1800 states requirements whose entire content is that a tool issues -- or does not issue -- a
report, and a program cannot read a message about itself. Such a requirement moves neither signal,
so a case for it passes whatever the implementation does.

The boundary is not hypothetical and it is not narrow. LRM 12.4.2 and 12.5.3 require a violation
report when a `unique` or `priority` statement matches nothing, and forbid one when an explicit
catch-all covers the rest. Lyra had that requirement wrong in four of its twelve forms for as long
as the forms existed, with a case for each statement kind passing the whole time. What surfaced it
was a production design flooding its own output, which is the most expensive instrument available
and the only one that was looking.

## The axis that decides everything: a claim about a report has two directions

An implementation that never reports anything satisfies every "no report is issued" claim, and one
that reports on everything satisfies every "a report is issued" claim. Either direction alone is a
checker whose passing state is its default. The two are not alternatives to pick between; a
requirement of this kind is only covered when both are stated, by two cases that differ in the one
thing the standard says decides it.

## What the standard fixes of a report

A report has two properties the standard states, and they are independent.

- **What issues it.** A severity task, called directly or as the default action of a failing
  assertion, which calls `$error` (LRM 16.14.1); a run-time error the clause of an operation names;
  a violation report of a unique or priority statement, issued through "a tool-specific violation
  report mechanism" (LRM 12.4.2.1, 12.5.3.1); the message `$finish` and `$stop` print at their level
  (LRM 20.2, Table 20-1).
- **Its severity**, which only the first two have: info, warning, error, or fatal (LRM 20.10).

Beyond that, a severity task's report "shall include" the file and line of the call, the scope it is
made in and the simulation time (LRM 20.10) -- as information, never as words -- and "the
user-defined message if specified". That message is the only text the standard puts in any report.
Everything else a tool writes, the report's own wording included, is the tool's.

The standard also says which reports may end the run. `$fatal` "terminates the simulation" and
`$finish` ends it through the final procedures; `$stop` suspends it, and what a tool does then is
its own; a violation report goes through the tool's own mechanism. `$error` is stated to "generate a
run-time error" and no more, and nothing anywhere promises the run goes on after one; `$warning` and
`$info` describe reports an operation makes on its way, as a bounded queue "discarded and a warning
shall be issued" (LRM 7.10.5) does. So an error is read as able to end the run, and a warning or a
message of no severity as not.

## The decisions

```text
E1. A case may state what the run reports, as a directive. `@reports:` names the kind of report,
    by what issues it -- info, warning, error, fatal, violation, finish, stop -- and may name the
    line, the time, the scope and the program's own message that the standard fixes of it.
    `@reports-none:` names the kinds of report the standard forbids there. A case carrying neither
    is held to nothing about its reports.

E2. Both directions are written, as separate cases, whenever a requirement of this kind is covered.
    The pair is the unit of coverage; either half alone passes against an implementation that is
    uniformly silent or uniformly noisy.

E3. `@reports-none:` is the absence of the kinds it names, and nothing else. A conforming tool may
    write a banner, a run summary, or a remark the standard does not mention; what a clause can
    forbid is one kind of report under one condition, so that is what the claim states. A line in
    the shape of a report that the tool's reader cannot place leaves the claim unresolved rather
    than met.

E4. A case still checks itself. A report directive is stated beside the checks, never instead of
    them. Where the standard lets the tool end the run at the report -- error, fatal, violation,
    stop -- the case prints that it is ready for the report before it, and the sentinel in the
    same procedure after the last report, never in a final procedure, which runs whether or not
    the run went on past it. The run is held to being ready and to the report; a check after the
    report holds where the run reaches it, and a report after the first that the run never
    reached leaves the verdict unresolved (E7).

E5. No wording of a tool is ever named. The only text a claim names is the message the program
    itself supplied, which the standard requires the report to include, and it names it whole: a
    message that only begins with it is another value.

E6. A claim is met only by the report of the statement that should issue it. Each required report
    is answered by one report of its own, of the severity it names: a case's own failing check is a
    $fatal, so a fatal never answers a claim of an error. Where the standard puts the source line in
    the report, the claim names the line and its count is exact, since one more report from that
    line is the statement reporting an execution the standard does not report; where it does not,
    the case lets only the statement under test issue a report of that kind, and a run issuing more
    of them than asked for is unresolved. A count is exact either way, so a report lacking
    something the claim names, or a line the reader could not place, leaves it unresolved even
    when every report asked for was read.

E7. A verdict is about the tool, and the case's own validity is a separate question. Evidence that
    only happens to fit the claim is never "conforms" -- it is unresolved -- and a case written
    wrong is never the tool's "does not conform". A run observes a prefix of what the program would
    do, and what lies past its end is not shown: a run the tool ended at the first of two reports
    asked for leaves the second unresolved, and a claim of absence holds only for a run that reached
    its sentinel. Where a case's mistake cannot show in a run that takes one of the paths the
    standard allows -- readiness printed after a report the tool went on past -- it is found by a
    tool taking another of those paths, not by the judge.
```

## What the revision found

The first version of E1 matched text the diagnostics contained, and E3 required the run to write no
diagnostic at all, on the ground that "a conforming tool has nothing to say about a correct
program". Both were derived with one tool in view. Run under a second one, the corpus showed what
they had pinned: every case claiming a report named Lyra's wording, which the standard leaves to the
tool, and a run summary the second tool prints on every run -- which nothing in the standard forbids
-- failed every claim of silence. A requirement for one cannot be read as a requirement for all.

The same run showed a violation report issued at once and the run stopped there, where LRM 12.4.2.1
defers it to the Observed region; checks the case had placed in a final procedure never ran, and the
case read as wrong for the wrong reason. That is what E4 answers. Its first form moved the sentinel
before the report, which left one marker meaning "every check passed" in most cases and "the
reporting statement was reached" in these, and made any report of the claimed kind meet a claim
naming the kind alone; the readiness marker and E6 replace it, so each marker means one thing and a
report is tied to the statement that issues it. A second form then printed the sentinel from a final
procedure, which a tool that ends the run at an error runs as well, so a check meant for a run that
went on read as failed in one that had legitimately ended; the sentinel now comes from the procedure
that issues the reports.

Every case the revision rewrote was then made to fail on purpose, one injection at a time, and two
of the injections passed. A claim of three reports from one line was met by three and also by two,
because only a claim naming the kind alone looked at reports beyond what it asked for; and a message
claim was met by any report whose message contained it, so `final saw 2` would have accepted the
`final saw 23` that the case exists to tell apart. clang's `-verify` and rustc's compiletest both
match a message by containment, because what they match is the compiler's own wording, which a test
abbreviates; what a case names is program text it wrote in full, so containment only lets wrong
values through. Both also hold a count exactly and fail on reports nobody asked for, which E6 now
does wherever the line ties the report to its statement.

## What this does not become

The directive states a claim about a report; it is not a golden-output mechanism and does not grow
into one. `conformance-case-shape` D6 reserves recorded output for the cases whose subject is the
output channel itself -- how stdout and stderr divide, and the order output interleaves -- and that
stays the only place a case is compared against text a tool wrote.

## Rejected alternatives

- **Golden stderr for these cases.** It reaches further than the claim: any rewording of any message
  in the run breaks cases whose subject is a statement qualifier. The wording is not the
  requirement, and a mechanism that pins it makes every message change a corpus edit.

- **Checking the requirement in SystemVerilog.** There is nothing to check against. LRM 12.4.2.1
  lets assertion control tasks turn violation reporting on and off, and nothing in the standard lets
  a program observe that a report was issued. The check would have to invent an observable the
  language does not have, which is what makes this the boundary rather than an oversight.

- **A required phrase per report**, the first version's E5. It is the one place a case named a tool,
  and it decided the verdict for every other tool by how close its words came to Lyra's.

- **Requiring silence outright**, the first version's E3. It was preferred over a per-message "no
  report of this text" because an implementation that stops issuing the report, or rewords it,
  satisfies the latter. A claim of absence by kind has neither weakness: E2's other case fails the
  implementation that stops reporting, and no wording enters it.

- **Leaving these requirements uncovered and noting the gap.** What was in place already, in the
  form of prose. The gap was written down accurately and read by nobody in a position to act on it,
  and the requirement stayed wrong for as long as it took a real design to run.

- **Recording the wrong answers in the defect record instead.** The defect record holds text a
  failing run produces, and these cases do not fail: they pass, with a wrong report beside a correct
  answer. Nothing distinguishes them from a covered case, which is the property this decision is
  for.

## Consequences

- A tool's reader turns what it wrote into reports by kind, and the judgement reads those; a claim
  never reads a tool's text.
- A requirement of this kind has a home, so the answer to "is this covered" stops being "there is no
  way to say".
- A report a correct program's own work causes -- a failed `$cast`, an `$error` the run outlives --
  fails no case that does not forbid it.

## Cross-references

- [conformance-case-shape](conformance-case-shape.md) -- the shape this extends, and the judgement
  every claim is read by.
- [qualified-statement-violation-check](qualified-statement-violation-check.md) -- the requirement
  that forced this, and what the corpus could not see about it.
- `../../tests/conformance/README.md` -- the contract cases are written against.
