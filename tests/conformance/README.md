# The conformance corpus

Every case here answers one question: does Lyra implement IEEE 1800 correctly? A case states what
the standard requires of a program and checks itself against it, so it is valid under any conforming
simulator and says nothing about Lyra.

## A case is a directory, entered through `main.sv`

```
tests/conformance/09_processes/always_comb_combinational_value/main.sv
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

There is no manifest beside it. The harness runs the case and reads exactly two things: the exit
status, and whether `All checks passed` appears on stdout. `$fatal` sets a non-zero exit (LRM 20.10)
and a `final` procedure runs once at the end of simulation (LRM 9.2.3), so the two signals together
separate "a check failed" from "the checks never ran".

Five rules follow from that shape:

- **The expected value is written by hand and is never one Lyra produced.** A comparison whose two
  sides both come from the implementation proves only that it agrees with itself.
- **Every target is set to something the check would reject before the case writes the value it
  checks.** Otherwise "the operation never ran" passes as "the operation answered correctly", and a
  whole class of requirements walks into this by construction: `.name()` of a non-member is the
  empty string, `.len()` of an empty string is zero, an unequal comparison is zero, an out-of-range
  read is the element default. For all of those the right answer _is_ the target's default, so the
  check can only discriminate if the target held something else first.
- **Every check has run by the time the sentinel is printed.** A case that reports success without
  having checked anything is the one failure the harness cannot catch, so the sentinel goes last in
  the same `final` procedure the checks are in. A case whose subject is that something does _not_
  run is the exception that proves the rule: it prints the sentinel and then places a `$fatal` where
  nothing should reach, which still fails the case, because a non-zero exit is read before the
  sentinel is.
- **Checks use `if`, case equality, and `$fatal`, and nothing else.** Whatever the corpus leans on
  is something whose defects fail every case, so the foundation stays as small as it can be.
  Immediate assertions are a subject the corpus tests, not a tool it uses.
- **A case names no backend, no command, and no host tool.**

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
- Every `.c` and `.cpp` is built and linked as a source of foreign symbols (LRM 35.4).

Name a companion for what it is within its case (`width_pkg.sv`, `foreign.c`), not for the case --
the directory already says which case it belongs to.

## Where a case lives

The outer directory is the LRM clause the case tests and the inner one is the subject:

```
tests/conformance/09_processes/always_comb_combinational_value/
tests/conformance/27_generate_constructs/conditional_scope_elaborates/
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
parsed, and an unrecognized key is an error rather than a line quietly ignored.

Most cases carry none.

### `@top`

The instances to elaborate, whitespace-separated. A case that omits this elaborates `Top`.

```systemverilog
// @top: Producer Consumer
//
// Two top-level modules elaborate independently, and each one's initial
// procedure runs (LRM 23.3.1).
```

### `@args`

Options for the front end, whitespace-separated.

```systemverilog
// @args: --single-unit
//
// Declarations in one file's compilation-unit scope are visible in another
// when both are compiled as a single unit (LRM 3.12.1).
```

### `@argv`

The simulated program's own arguments, which is where LRM 21.6 plusargs reach a design.

```systemverilog
// @argv: +width=3
//
// $value$plusargs matches a plusarg given on the simulation command line and
// assigns the matched text to its variable (LRM 21.6.3).
```

### `@error`

For a program IEEE 1800 requires a tool to reject. Such a case makes no checks and prints no
sentinel; what it is held to is the rejection and the reason given. The value is text the diagnostic
must contain.

```systemverilog
// @error: cannot invoke a task
//
// A function shall not enable a task, so a conforming tool rejects this
// program (LRM 13.4.1).
```

## Checking what a program prints

Format-string behaviour is checked in the language, not against recorded output. `$sformatf` returns
the formatted text as a value, so it compares like anything else:

```systemverilog
if ($sformatf("%b", partial) != "10x1")
  $fatal(1, "%%b of a partly unknown value was '%s'", $sformatf("%b", partial));
```

That keeps the failure message specific, keeps the case one file, and keeps a change to how some
value prints from failing every case that happens to print one. Recorded output is reserved for the
cases whose subject is the output channel itself -- how stdout and stderr divide, and the order in
which output interleaves.

## What a path cannot do

A path is the artifact a run produces -- the C++ project, or the LLVM module -- not the way that
artifact is executed. What a path currently refuses is recorded once for the path, in
`tests/paths/<path>.yaml`, never on a case:

```yaml
16_assertions/immediate_assertion_severity: "assertions are not yet supported"
```

A case listed there is expected to be refused, with a diagnostic containing that text. One that
starts running instead fails until its entry is removed, so the file only ever shrinks. That is what
makes it the coverage report: the cases absent from it are the cases the path runs.

Absence of an entry is therefore a claim, which is why no case carries a per-path tag. A tag's
absence cannot tell "this path refuses the construct" from "nobody has tagged it yet".

A compiler bug is neither. A run that reports one fails whatever the records say, because "not
implemented yet" and "implemented wrongly" recorded the same way would leave nothing able to report
coverage.

## Where a path answers wrongly

A wrong answer is not a refusal, so it is recorded separately, in `tests/paths/<path>.defects.yaml`,
holding text the failing run produces:

```yaml
11_operators/packed_power_operator: "zero_base_negative_exponent was"
```

The case keeps every check it makes. It is expected to fail, and to fail saying that; the day the
answer becomes right the case passes, and the run then fails until the entry goes. That is the point
of recording a defect here rather than commenting the check out inside the case, where the check
stops running and nothing notices when the behaviour is fixed.

A case appears in one of the two records or neither, never both -- refused and wrong are different
outcomes and a case recorded as both leaves no saying which it is held to. So the cases the path
genuinely runs are the ones absent from both, and that is the coverage the corpus reports.

Neither record is a place for a failure that is merely inconvenient. What goes in is a defect
someone has diagnosed, with an entry in the matching progress doc saying what the standard requires.

## A case that cannot run at all

A case Lyra cannot get far enough into to fail usefully is parked by renaming its entry to
`main.sv.deferred`, which the harness does not collect; the reasoning stays in the file rather than
being written twice. This is the last resort, because a parked case is the one shape here that
nothing watches.

## Running

```bash
bazel test //tests:llvm_tests
bazel test //tests:cpp_tests
```

To run one case, filter by its path under this directory with `/` written as `.`. Nothing names the
path that runs it, because the target already does:

```bash
bazel test //tests:llvm_tests \
  --test_filter='27_generate_constructs.loop_increment_forms'
```

A parked case's elaboration check answers to `Parked.` and then the same name, that being the one
thing the target does not already say.

Adding a SystemVerilog feature means adding or extending a case first, then implementing until it
passes.
