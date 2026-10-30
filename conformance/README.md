# The conformance suite

Every case here answers one question: does a tool implement IEEE 1800 correctly? A case states what
the standard requires of a program and checks itself against it, so it is valid under any conforming
simulator and says nothing about any one tool. Any tool someone writes a reader for is held to it,
Lyra among them. This directory is no part of Lyra, and builds none of it, so a simulator is held to
the suite without building Lyra:

```
cases/<clause>/<subject>/   what IEEE 1800 requires, one case a directory
harness/                    SystemVerilog helpers a case includes by name
selftest/                   cases that check those helpers, run by every tool, counted toward none
framework/                  a case, running a child, what a tool was seen to do, the verdict, results
adapters/<tool>/            a reader per tool: how to ask it to run a case, how to read what it wrote
runner/                     a program that holds a tool to the whole suite
docs/decisions/             why the suite has this shape
```

How Lyra records what it answers is Lyra's, in `../tests/paths/README.md`.

## A case is a directory, entered through `main.sv`

```
cases/09_processes/always_comb_combinational_value/main.sv
```

```systemverilog
// An always_comb procedure runs once at time zero and again whenever a
// variable it reads changes, so its target holds the combinational value of
// its inputs (LRM 9.2.2.2).
module Top;
  int a, b, c;

  initial begin
    a = 1;
    b = 2;
  end

  always_comb c = a + b;

  final begin
    if (c !== 3) $fatal(1, "c was %0d, expected 3", c);
    $display("All checks passed");
  end
endmodule
```

There is no manifest beside it. A run is judged by whether it printed `All checks passed`, how it
ended, and -- for the cases that claim one -- the reports it issued. `$fatal` ends the run with an
error code (LRM 20.10) and a `final` procedure runs once at the end of simulation (LRM 9.2.3), so
the sentinel and the end together separate "a check failed" from "the checks never ran".

Six rules follow from that shape:

- **The expected value is written by hand and is never one a tool produced.** A comparison whose two
  sides both come from the implementation proves only that it agrees with itself.
- **Every legal answer passes.** Where the standard allows more than one result -- an order it does
  not fix, a value it leaves open within a set -- the case accepts each of them: it sorts what a
  locator returns before reading it, accepts either neighbour a narrower format rounds to, keeps
  only checks that every order of its processes passes. A case that pins one is testing a tool's
  choice, and that is not a case here (a tool that wants its own choice held keeps such a case
  itself, as Lyra does in `../tests/choices/`). Where two clauses give one text different meanings,
  the case keeps what both require and its prose names the clauses that disagree.
- **The program is one the standard allows.** A case built on a forbidden text claims nothing of a
  tool that rightly refuses it, so a forbidden text is covered by a case that claims its refusal
  (`@rejected:`), never by being used. Static initializers have no order among themselves (LRM 6.8),
  so no initializer reads another's value unless every order gives the answer it checks; a check
  over the whole corpus holds every case to that, and the few that pass every order are named in it
  with why.
- **Every target is set to something the check would reject before the case writes the value it
  checks.** Otherwise "the operation never ran" passes as "the operation answered correctly", and a
  whole class of requirements walks into this by construction: `.name()` of a non-member is the
  empty string, `.len()` of an empty string is zero, an unequal comparison is zero, an out-of-range
  read is the element default. For all of those the right answer _is_ the target's default, so the
  check can only discriminate if the target held something else first.
- **Every check has run by the time the sentinel is printed.** A case that reports success without
  having run its checks is the one failure the judgement cannot catch, so the sentinel goes after
  the last check. A case whose subject is that something does _not_ run prints the sentinel and then
  places a `$fatal` where nothing should reach.
- **Checks use `if`, case equality, and `$fatal`, and nothing else.** Whatever the corpus leans on
  is something whose defects fail every case, so the foundation stays as small as it can be.
  Immediate assertions are a subject the corpus tests, not a tool it uses.

**A case names no tool, no option, no generated file, and no message wording.** `$system` (LRM
20.17.1) is the one exception the standard itself creates: its meaning is a command the host runs,
so a case for it writes a command line, keeps to what a command processor answers for on its own,
and asserts only the distinction the standard fixes rather than the encoding a particular host gives
it.

## What else the directory may hold

Most cases are `main.sv` alone. A case that needs more puts it in the same directory, and that is
the whole of how it is declared -- what belongs to a case is where it sits, not something a
directive has to say:

```
26_packages/import_makes_names_directly_visible/
  main.sv
  width_pkg.sv
35_dpi/imported_function_returns_result/
  main.sv
  foreign.c
```

- Every other `.sv` in the directory is compiled **before** `main.sv`, in name order. A reference
  reaches only the part of a compilation-unit scope declared before it (LRM 3.12.1), so a case is
  written against what its companions declare rather than the other way round.
- Every `.c` and `.cpp` is built and linked as a source of foreign symbols (LRM 35.4). It includes
  `svdpi.h`, the one header the standard defines (Annex H), and declares the exported subroutines it
  calls itself, by the standard's mapping (LRM 35.5.6). A header a tool generates for a design is
  that tool's own, and a case never includes one.
- A `lib.map` is the case's library map (LRM 33.3.1), saying which library each companion belongs
  to. The standard leaves the file's name to the tool and writes its own examples against this one.
  A path in it is relative to the map, so it names a companion by its file name.
- An `expected/` directory holds, under the name the case writes it by, each value change dump the
  run is held to (LRM 21.7). A dump is compared as the standard reads one -- its declarations and
  its values at each time, in the time unit it states -- and not as text, so what the standard
  leaves to the tool, its identifier codes, its spacing, the order of changes within one time, does
  not decide anything. The expected dump is written by hand like any expected value.

Name a companion for what it is within its case (`width_pkg.sv`, `foreign.c`), not for the case --
the directory already says which case it belongs to.

## Where a case lives

The outer directory is the LRM clause the case tests and the inner one is the subject:

```
cases/09_processes/always_comb_combinational_value/
cases/27_generate_constructs/conditional_scope_elaborates/
```

The standard's structure is the only enumeration of its requirements that exists, so it is the only
basis on which coverage can be measured. Two clauses that separately require the same behaviour each
get a case; those are two requirements, not a duplicate.

**The clause a case belongs to is where its normative text sits, not what its subject is made of.**
A case about `+` on real operands states a rule from clause 11 and belongs there, even though every
value in it is a `real`; a case about how a `real` converts to an integer states a rule from 6.12.1
and belongs in clause 6.

Letters, digits and underscores only, in both parts. A case is run by name as often as it is opened
by path, and holding the two to one alphabet keeps converting between them a matter of swapping the
separator. A name outside it is rejected rather than quietly folded into one that is.

## Directives

What a case needs that its sources cannot state is a `// @key: value` line at the top of `main.sv`.
Directives come first, then a blank `//`, then the prose that states the claim. Prose is never
parsed, and an unrecognized key is an error rather than a line quietly ignored. Most cases carry
none.

### `@top`

The instances to elaborate, whitespace-separated. A case that omits this elaborates `Top`.

```systemverilog
// @top: Producer Consumer
//
// Two top-level modules elaborate independently, and each one's initial
// procedure runs (LRM 23.3.1).
```

### `@argv`

The simulated program's own arguments, which is where LRM 21.6 plusargs reach a design.

```systemverilog
// @argv: +width=3
//
// $value$plusargs matches a plusarg given on the simulation command line and
// assigns the matched text to its variable (LRM 21.6.3).
```

### `@compilation-units`

Which of the two use models a case's files are compiled under: `single`, where every file is one
compilation unit, or `separate`, where each file is one of its own. The standard requires every tool
to offer both and leaves which it takes by default to the tool (LRM 3.12.1), so a case whose meaning
depends on it says which, and one whose meaning does not says nothing.

```systemverilog
// @compilation-units: single
//
// Declarations in one file's compilation-unit scope are visible in another
// when both are compiled as a single unit (LRM 3.12.1).
```

### `@includes`

Files of the corpus's own harness, in `harness/`, compiled before the case's sources. The harness is
SystemVerilog a case leans on where checking in the language would otherwise mean writing the same
recognizer in many cases: today, a recognizer of the assignment patterns `%p` writes, which takes
every spelling LRM 21.2.1.6 allows of a value. It is held to a case of its own under `selftest/`,
which every tool runs and none counts toward what it conforms to, so a tool failing it shows the
harness wrong, not the tool. A runner names where the harness is; nothing finds it by where the
cases are.

```systemverilog
// @includes: assignment_pattern.sv
```

### What the run is held to

A case states at most one of these. One that states none claims that it runs: it reaches its
sentinel, having passed every check, and ends with a status of success.

`@rejected:` takes no value and is a program the standard requires a tool to refuse before it runs.
Such a case makes no checks and prints no sentinel. Refusing it is the whole of what it is held to,
so a tool that crashes, or one the harness could not hand the program to, has not refused it.

```systemverilog
// @rejected:
//
// It shall be an error to connect any bit of a uwire net to more than one
// driver, so a conforming tool rejects this program (LRM 6.6.2).
```

`@reports:` is a requirement whose observable is a report the run issues, since a program cannot
read a message about itself. It names the kind of report, by what the standard says issues it --
`info`, `warning`, `error` and `fatal` for a severity (LRM 20.10, and the `$error` a failing
assertion calls by default, LRM 16.14.1), `violation` for a unique or priority statement's report
(LRM 12.4.2.1, 12.5.3.1), and `finish` and `stop` for the message those tasks print (LRM 20.2). It
may then name what the standard fixes of that report: the `line`, the `time` and the `scope` a
severity task's report carries (LRM 20.10), and the `message` the program itself supplied, which is
the only text the standard puts in a report and so the only text a case may name. The message is
named whole, since one that only begins with it is another value. A claim is met by a report of the
severity it names and no other: a case's own failing check is a `$fatal`, and it never stands in for
the error a case asks for.

```systemverilog
// @reports: info line=21 time=37 scope=Top.probe message="a probe report"
```

**A claim is met by the report of the statement that should issue it, and no other.** Each report a
case requires is a `@reports:` line of its own, or a `count=` on one, and each is answered by a
report of its own, so two required reports are not met by one. Where the standard puts the source
line in the report -- a severity task, a failing assertion's default action -- the claim names that
line. The line is what ties a report to its statement, and nothing else in a report does: one more
report from that line, whatever it carries, is the statement reporting an execution the standard
does not have it report, and the run does not conform. Where it does not -- a violation, an
operation's run-time error -- the case is written so that only the statement under test can issue a
report of that kind, since a claim naming the kind alone is met by any report of it; a run issuing
more of them than the case asks for leaves the verdict unresolved rather than met. Either way the
count is exact, so a report lacking something the claim names, or a line in the shape of a report
that the reader could not place, may be one more, and leaves the verdict unresolved even where every
report asked for was read. A case whose requirement is something besides the report is better split,
so that the report is the one obligation the case states.

**Where the standard lets a tool end the run at the report, the case says it is ready for the report
instead of printing its sentinel there.** That is a fatal error, a violation reported through the
tool's own mechanism, `$stop`, and an error, after which nothing promises the run goes on. The case
makes its checks, prints `Ready for the report` before the statement that issues it, and prints
`All checks passed` in the same procedure after the last report, where only a run that went on past
them reaches it -- never in a final procedure, which a tool ending the run at the report runs too.
Whether readiness comes before the report shows only in a run that ends there; a run that goes on
past the report says nothing about it, which is one thing a second tool's run of the corpus is for:

```systemverilog
// @reports: error
...
    $display("Ready for the report");
    word = Word'(bytes);        // an error (LRM 6.24.3)
    $display("All checks passed");
  end
```

The run is held to being ready and to the report. A check after the report holds where the run
reaches it -- a `$fatal` there fails the case -- and a run that went on past every report is held to
every report the case asks for. Where a tool ended it at a report before the rest, that fails
nothing, but the rest were never observed, so the verdict is unresolved; a case is therefore decided
on every tool only where it asks for one such report. `info` and `warning` leave the run going, and
`$finish` runs the final procedures (LRM 9.2.3), so a case claiming one of those prints its sentinel
at its end as any case does.

`@reports-none:` names the kinds of report the standard forbids where the case puts them, and the
run is held to issuing none of them; `error` forbids `fatal` too. A tool may write anything else --
a banner, a summary, a remark of its own -- so silence as a whole is never the claim.

```systemverilog
// @reports-none: violation
//
// An explicit else covers every value no condition matched, so a priority-if
// carrying one issues no violation report (LRM 12.4.2).
```

**A requirement of this kind is only covered when both directions are written**, as two cases. An
implementation that never reports satisfies every claim of absence on its own, and one that reports
everywhere satisfies every claim of a report; neither half discriminates alone.

## Checking what a program prints

Format-string behaviour is checked in the language, not against recorded output. `$sformatf` returns
the formatted text as a value, so it compares like anything else:

```systemverilog
if ($sformatf("%b", partial) != "10x1")
  $fatal(1, "%%b of a partly unknown value was '%s'", $sformatf("%b", partial));
```

That keeps the failure message specific, keeps the case one file, and keeps a change to how some
value prints from failing every case that happens to print one. Where the standard leaves the
spelling open, the case checks only what it fixes: `%p` text goes through the harness's recognizer
(`@includes`), which takes any spacing, index labels and defaults the clause allows. Recorded output
is reserved for the cases whose subject is the output channel itself -- how stdout and stderr
divide, the order in which output interleaves, and the dump files of LRM 21.7 (`expected/`).

## How a run is judged

What a tool did is read by a reader written for that tool, which knows how to ask it for each use
model a case needs and how to read what it wrote: whether it refused the program, whether the
program ran and printed the sentinel, how it ended, and each report it issued, by kind, line, time,
scope and message, each where the tool gave it. A line in the shape of one of its reports that the
reader cannot place is kept apart as unplaced, never passed over. One judgement then turns the claim
and that reading into a verdict, the same for every tool:

- **conforms** -- the run did what the claim says the standard requires;
- **does not conform** -- it did something else: a wrong answer, a refusal of a legal program, a
  legal refusal not made, or no use model the standard requires every tool to offer;
- **unresolved** -- what was read cannot decide the claim: the run was stopped at a limit -- the
  time it was given, the memory it was given, or a kill whose cause is not known; the report the
  claim asks about could be among the lines the reader could not place, or is one that lacks
  something the claim names; more reports of a kind asked for by kind alone were issued than asked;
  or the run ended at a report before the rest the case asks for were observed;
- **not run** -- the case never reached the tool: the harness could not hand it the files, or had no
  directory to run it in. It says nothing of the tool.

A reader exists for Lyra and for Verilator, each under `adapters/`. `runner/` holds one tool to the
suite, named by `--tool`, with the tool's own binaries named or found on PATH:

```bash
bazel run //conformance/runner:conformance -- --tool verilator 09_processes/always_comb_combinational_value
bazel run //conformance/runner:conformance -- --tool lyra --lyra ./bazel-bin/lyra
```

It prints a verdict per case and the evidence for each that does not conform, and decides nothing
else: whether a tool is known to give a verdict is that tool's own business, as Lyra's records are.
Past reading its own command line, it exits non-zero only where a case was not run or its results
could not be written.

How wide it runs, how long each case may take with all its commands together, and how much memory
each program it starts may take are the caller's to say (`--jobs`, `--time-limit`,
`--address-limit`), since they are facts of the machine and not of the corpus; a whole run belongs
inside one memory ceiling the caller sets, and its first line states what it ran under. A program
stopped at one of those limits is unresolved, never a failure.

## What a run leaves

`--output <file>` writes the whole run as JSON, for whatever presents the verdicts or runs them
again; nothing reading it judges anything a second time. It is written only once every case asked
for has been judged exactly once -- a case not run is judged so -- and is moved into place whole, so
a file at that name is an account of every case it names. A case id the corpus does not hold, and a
suite in which no case is found, are refused before anything runs. It holds:

- `format_version`, which moves whenever a field goes or changes meaning;
- `suite.revision` and `tool.revision`, which only the caller knows and states (`--suite-revision`,
  `--tool-revision`), and `suite.directory`, where the run read the suite from, under which a step's
  arguments name each case's files;
- `tool`: its name, each program its reader runs with the path it was found at and the version it
  says, and the options every case's build is given;
- `environment`: the variables every program's behaviour can follow and the ones the tool's reader
  says the tool reads, each as the run had it or `null` where unset. No other variable is written,
  so a credential in the environment never is;
- `host`, `limits` and `selection` (the case ids named, or `null` for the whole suite), and when the
  run `started` and `ended`;
- `cases`, in id order: each case's `verdict`, how its run `end`ed, what the reader read off it
  (markers, `reports`, `unplaced` lines, the `outputs` the case expects), every command it ran as
  one of its `steps` -- program, arguments, directory, how it ended, both streams -- and the
  `seconds` it took.

What nobody could know is `null` rather than a guess: a revision the caller did not state, a version
a tool does not say, a field of a report the tool did not give. The case's clause and claim are not
repeated; the id is the case's path, whose first part is its clause, and the claim is in the case at
the revision named, which whoever reads the two together has to hold to the same revision. A step's
directory is a scratch directory that no longer exists; it says where the command ran, not where to
run it again.

Verilator is a second opinion here, never an oracle: what it answers is judged by the same claim.
