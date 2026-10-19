# A Dump Is Chosen When The Run Starts

## Date

2026-10-19

## Status

Accepted.

## Why this decision matters

A person debugging a design wants its values over time in a file a waveform viewer reads (LRM 21.7).
What they choose -- which part of the hierarchy, which stretch of time, which format -- changes from
one run to the next, while building a large design takes minutes. A dump that has to be chosen when
the design is built makes every new question a rebuild; one chosen when the run starts makes it a
rerun. And a dump nobody asked for has to cost nothing, because most runs ask for none.

## What the survey says

- **Visibility and selection are two choices in every simulator that has both.** Which values still
  exist as named storage is fixed when the design is compiled; which of them a run records is chosen
  when it runs. Icarus keeps every value in its object model and takes `$dumpvars`' scopes and
  levels, and a `-vcd`, `-fst` or `-none` option, at run time (`iverilog/vpi/sys_vcd.c`). nvc and
  GHDL take the format and include and exclude patterns as run options. Verilator is the exception.
  Tracing is compiled in (`--trace`), and the run's `$dumpvars` only stops the output, its scope and
  levels ignored.
- **A change is found either per write or by a scan per step.** Icarus enrolls a value-change
  callback on each dumped signal, which marks it and queues it, and writes the last values once per
  step at the read-only point after the step's events (LRM 38.36.2). nvc watches each dumped signal
  in the postponed region (`nvc/src/rt/wave.c`). GHDL and Verilator scan every dumped signal each
  step, comparing it with its last value. The first costs what changes; the second costs what is
  dumped.
- **Encoding off the simulation thread pays only when the encoding is split.** Verilator measured
  FST tracing at about five times the run without it (PR #5806); a thread of its own for the output
  was removed as of doubtful benefit while encoding stayed on one thread (PR #7443), and splitting
  the encoding by signal gave 2.04x on four threads. arcilator appends each tapped write as a (tap,
  value) pair on the simulation thread and encodes on a worker.
- **One output with several sources is settled by a precedence everyone can predict.** rustc reads a
  lint level from the command line as the crate's default, lets the source override it, and lets
  `--force-warn` and `--cap-lints` override the source (the rustc book, "Lint Levels"). LLVM's
  profile runtime takes a file name from its default, then the compile option, then the environment,
  then the program's own call (`InstrProfilingFile.c`, `parseAndSetFilename`).
- **A built program takes a tool's run options as plusargs.** Verilator's executables read
  `+verilator+...` (`verilator/docs/guide/exe_sim.rst`), so the program needs nothing beside the
  argument list LRM 21.6 already defines.

**Where our conditions differ.** Lyra keeps every variable, net and port of a design as storage of
its own and lets a run be built once and kept by what built it, so making the dump a run-time choice
costs nothing in visibility that Lyra was not already paying. It is Icarus's position on the first
axis with an optimizing compiler's code behind it.

## Decisions

### D1. Every scope class describes what a dump may record of it, and a run chooses from that

A class of the design hierarchy is compiled with one more method beside its elaboration phases,
which states each data object the class declares that a dump may record -- a variable, a net, a
port, a named event, the static variables of a named block, task or function -- as a constant
holding its name, its kind, its width, its declared range and, for a port, its direction and
position, together with the member that holds it. The runtime walks its own object tree and asks
each instance it selects to describe itself; nothing is listed for child scopes, which the tree
already holds.

It is a method rather than a table of offsets because no layer below MIR states a member's offset as
a constant, and C++ guarantees `offsetof` only for a standard-layout class, which a scope class with
virtual functions is not. A member's address comes from code in both backends, as it does everywhere
else. The method reaches the dump starting through the run rather than taking it as an argument,
because the library's own scope class defines it too, for a class that describes nothing, and would
have no use for one.

What is never recorded is what the standard leaves out (LRM 21.7.2.1), which is an unpacked array or
structure, a string, a handle and a dynamic container, and a parameter, which is no storage.

### D2. A run's own options are the defaults and the design's tasks override them

The program reads `+lyra+dump=<file>`, `+lyra+dump+scope=<path>[:<levels>]` and
`+lyra+dump+window=<from>:<to>` as what to dump where the design says nothing. Where the design
calls `$dumpfile` or `$dumpvars`, those win, because the design's author wrote them on purpose and
the options are only the run's defaults. A `$dumpvars` run by the end of the time step the options'
dump starts in takes it over; one the design runs after that finds the dump started, as a second
`$dumpvars` would (LRM 21.7.1.2). `+lyra+dump+force` reverses all of that, the options deciding
alone and every `$dump*` call of the design doing nothing, and `+lyra+dump+none` dumps nothing at
all. It is rustc's lint order and LLVM's profile-file order, and either is explained in one
sentence.

A path in an option is resolved against the run's object tree once, when the dump starts; a
`$dumpvars` item is resolved by the front end like any other hierarchical name, so the runtime
matches no name the design wrote.

### D3. What is recorded is found per write and written once per time step

Each selected thing enrolls one membership on the storage it is, the way a process waiting on it
would enroll, so a net is enrolled through what it shows and follows a force. A write that changes
it marks it changed once in the step; at the postponed region the dump records each changed thing's
value under one time. Storage nobody dumps pays nothing new, and storage that is dumped pays the
comparison a wait on it already pays.

A whole-design dump makes every write of every variable compare. The other realization, a scan per
step over what is dumped, is confined to this decision, and which is cheaper for a design that dumps
everything is a measurement still to make.

### D4. The file is written on a thread of its own, in an order the simulation alone decides

The simulation copies each step's values into a block of records and hands full blocks to the dump's
own writer, which formats and writes them. At most a fixed number of blocks wait, so a writer that
falls behind makes the simulation wait rather than grow without bound. `$dumpflush` returns once
everything recorded before it is in the file. What the file holds does not depend on when the writer
runs.

### D5. A format is a writer, chosen by the file's name

Every format is handed the dump in the order LRM 21.7.2.1 lays a VCD file out, definitions scope by
scope and then changes time step by time step, with checkpoints, comments, a size bound and a flush.
A file named `.fst` is written in FST through libfst, `.txt` as text a person reads, and any other
name as VCD; a new format is a new writer and a new extension. FST is the default a large design
wants, since third-party measurements put it at thirty to seventy times smaller than VCD.

### D6. Each file of ports is a dump of its own

`$dumpports` names a file and the instances whose ports go to it (LRM 21.7.3.1), and its control
tasks name a file, or every file where they name none. Each file is a dump of its own with its own
writer, recording only ports, each instance as one module scope named by its hierarchical name, its
ports in the order the declaration gives them (LRM 21.7.4.2), and every value read by the port's
direction (LRM 21.7.4.3.1). A port's value is recorded as driven strongly toward the values it holds
and not toward the others. The values a dump reads carry neither the strength of a net's resolution
nor which side of a port drives it, so an input is written in the characters of a test fixture's
drive, an output in those of the device's, and an inout, whose driver is not told apart, in those of
a port of unknown direction (LRM 21.7.4.3.1).

### D7. What a scope is recorded as is a constant of its class

Whether an instance is recorded as a module or a block (LRM 21.7.2.1 `scope_type`) is known when its
class is declared, so it is held with the class's other constant properties, beside its timescale. A
unit's own instance is a module; a generate block and a named procedural block are blocks. A level
`$dumpvars` counts is an instance. A block belongs to the instance holding it, so
`$dumpvars(1, top)` records the variables of `top`'s generate blocks as well as its own.

## Rejected alternatives

- **Choosing what to dump when the design is built.** It is Verilator's shape, and it lets the
  compiler leave the dump code out of a build that wants none. Rejected because every new question
  becomes a rebuild, which is the cost this decision exists to remove, and because D3 already makes
  a run that dumps nothing pay nothing.
- **A command-line option of `lyra run` for each plusarg.** It would be a second spelling of the
  same thing; a run's own arguments already follow the `--` that ends Lyra's command line.
- **Printing each change of one variable on the run's output.** The text format, selected to one
  variable, records the same changes in a file. Written to the run's output, its lines would have to
  keep their order with `$display`, which a writer on its own thread cannot do.
- **Splitting the encoding across threads by signal.** The measurement that recommends it was taken
  on a single-threaded encoder, and libfst already compresses a block on a thread of its own; it
  waits for a measurement of Lyra's own.

## Consequences

- A design gains one method per scope class and one constant per dumped declaration, emitted whether
  or not any run dumps it.
- A run that dumps nothing enrolls no storage and starts no thread.
- The strength of a net's resolved value is what an exact extended dump of ports would need, and it
  is not kept.

## Cross-references

- [project-file](project-file.md) -- the line between what is true of a design and what is true of
  one run, which D2 applies to the options.
- `../architecture/runtime_model.md` -- the scheduling regions the postponed record of D3 runs in.
