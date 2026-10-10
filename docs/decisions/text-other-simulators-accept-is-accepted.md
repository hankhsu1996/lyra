# Text Other Simulators Accept Is Accepted

## Date

2026-10-09

## Status

Accepted.

## Why this decision matters

The designs someone brings to a new simulator are designs that already run on another one. Every one
of the open designs Lyra was measured on carries text the standard does not allow and the simulators
it was written against take anyway: a comma after the last port of a list, a variable assigned both
continuously and from a procedure, a name reaching into an unnamed generate block, a system task one
tool has and the standard does not. A front end that holds each of these to the letter refuses the
design before a line of it is lowered, and the reader of that refusal cannot do anything about it,
because the text is somebody else's and it works everywhere else.

So how strict the front end is by default decides whether a real design is refused for what Lyra
cannot yet do, which is worth knowing, or for what its authors were never told was wrong.

## What the front end offers, and what others do

The front end keeps three readings. Its own is the standard's. One follows a commercial simulator,
turning a set of the standard's errors into warnings. The third is the union of everything any
reading it knows tolerates: twelve errors become warnings -- a definition given twice, a variable
with more than one continuous driver or with a continuous and a procedural one, a procedural `force`
of something that may not be forced, a system name nobody defines, a misplaced trailing separator
among them -- and four rules of elaboration are relaxed.

Verilator reports most of the same text as warnings, and the benchmark suite its own developers keep
runs every design with those warnings made non-fatal. That is a tool's maintainers saying how their
tool has to be run for real designs to build.

Our condition does not differ from theirs. It differs from the front end's own: slang is also a
linter, and a linter's default is the standard. A simulator's users are judged by whether the design
runs.

## Decisions

### D1. The default reading is the most tolerant one

Lyra asks the front end for the union reading unless told otherwise. What the standard forbids and
another simulator accepts is accepted with a warning that says so, and `--compat default` asks for
the standard's own strictness, which is what a project that wants to stay portable should build
under.

A warning is what is owed here and not silence. The text is still wrong by the standard, and the
author of a design is the one person who can fix it.

### D2. Where the standard states what a tolerated text means, that is what it means

Tolerating a text needs a meaning for it, and where the standard gives one Lyra takes it rather than
another tool's. A cell described twice in one library is the worked case: the standard says the
later description is the one written to the library and that a warning is issued (33.3.1), so the
later one is what runs, although Verilator runs the first. No reading here is chosen to match a tool
where the standard has spoken.

### D3. A build that fails shows the warnings it withheld

`run` shows a design's output and withholds the front end's warnings, since they are not what the
reader asked for. When the build then fails, they are shown before the failure. A text that was
tolerated and could be given no meaning is refused downstream as something that cannot be simulated,
and the front end's warning is the only place that says why.

### D4. What the front end could give no meaning is a refusal, never a fault of the compiler

A tolerated text can leave a statement or an expression the front end marks as having no meaning: a
call to a system task nobody defines is one. Lowering refuses it as unsupported, at the place it was
written, and where it sits inside a macro at the place the macro was used.

## Rejected

- **Keeping the commercial simulator's reading as the default.** It was the default before, and it
  refuses, among others, the trailing comma and the unnamed generate block reference that designs
  written against Verilator carry. A default chosen to match one tool fails designs written against
  the next.
- **Turning on single diagnostics as designs trip on them.** Each would be found by a user being
  refused, one design at a time, and the list would be a private copy of one the front end already
  maintains.
- **Keeping a definition given twice an error because two tools disagree on which one runs.** The
  standard says which, so the disagreement is one tool departing from it and not an open question.
- **Tolerating a design in which some elements state a time scale and others do not.** The standard
  makes it an error (3.14.2.3) and gives the tolerated text no meaning: which time scale the others
  get is each tool's own choice. A design in that state names a default time scale to be built.
