# A Dump Is Chosen When The Run Starts

## Date

2026-10-19 (revised 2026-11-09: each recorded variable names the declaration it lies in, and a scope
is recorded as what the declaration it fills is; revised 2026-11-12: a class states each data object
as a row naming the methods that read it and subscribe to it; revised 2026-11-13: a package states
its variables the same way and is recorded as a package; revised 2026-11-15: each port is a row of
its own naming the object it connects, and two methods per class reach every row by its position)

## Status

Accepted. Revised 2026-11-05: D4's thread is also what an ending hands the dump to, so the
simulation publishes each unit it records whole rather than handing over full blocks, and a file the
system refuses to write is reported as one that cannot be opened is.

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

A class of the design hierarchy states, beside its definition, one row for each data object it
declares that a reader reaching the design by name may record -- a variable, a net, a named event,
the static variables of a named block, task or function. A row holds the object's name; what it is
declared as, a variable or a net of its net type; its type as the language declares it -- the kind
of type, its bits, the outermost packed range, an enumeration's base; and the declaration of the
block, task or function it lies in, which the class states once among the scopes it declares. Beside
the rows the class states each of its ports, in the order the declaration gives them, by the port's
name, its direction and the position of the row of the object it connects inside (LRM 21.7.4.2), so
two ports connecting one object are two ports, as the standard counts them; and two methods of the
class, each handed the instance, the position of a row and what the reader gave, which switch on the
position to the case for that row. One reads the object's name as the class's own code reads it and
hands the value to the reader: an integral value's bits, a real, or the object an event variable
names. The other builds the trigger a wait for a change of the object would enrol and hands it to
the reader, which enrols on it as a waiting process would; an event variable also changes with each
trigger of the object it names, which the reader follows from the object each read hands it. A
package and the compilation unit's scope, each one object of the run beside the instance tree (LRM
37.10), state the same rows for the variables they keep, and their two bodies are the unit's own:
they take an object as a class's do and leave it unread, as Java's `Field.get` does for a static
field. The runtime walks its own object tree, reads each selected instance's rows from the
definition the instance's first word names, and calls those methods on the instance with the row's
position; it reads a package's rows from what the package handed it as its storage came up. Nothing
is listed for child scopes, which the tree already holds, and nothing opens or closes a block, which
the declaration each variable names already places. The keyword, width and range a file declares are
the dump's reading of the row (LRM 21.7.2.1, 21.7.5), since they are one format's spelling of the
declaration rather than the declaration. Each name is recorded under a code of its own, though two
may share one (LRM 21.7.2.3 `$var` b): a port is a member a force rebinds (LRM 10.6.2), so the value
it shows can part from its connection's, and nothing else two names of one walk stand for reaches
one place.

The reading and the subscribing are methods of the class because the class is where the object's
type is known, and the runtime is handed values and triggers rather than told how to read storage. A
method reads through a net's resolution and a port's binding as the design does, follows a force
(LRM 10.6.2), and passes the runtime nothing it would have to interpret. It is how the runtime
library already reaches a method of a class it does not know -- the entry of the method, handed the
object as the root every object is -- and how serde has generated code hand a value to a serializer
the library supplies. They are two for the whole class rather than two for each row, as serde
generates one serialize function for a type that handles every field of it: what a design costs to
compile grows with the code it states, and a row is data.

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

The simulation copies each step's values into a unit of records -- a time step's changes, a
checkpoint, the definitions -- and publishes each unit whole to the dump's own writer, which formats
and writes them; the writer is woken once enough units wait, or one has to be written now. At most a
fixed number of units wait, so a writer that falls behind makes the simulation wait rather than grow
without bound. `$dumpflush` returns once everything recorded before it is in the file. What the file
holds does not depend on when the writer runs.

The thread is kept for more than taking the encoding off the simulation, which Verilator measured to
be of doubtful worth. It is the one thread an ending can hand the dump to: a run stopped from
outside or by a fault leaves the simulation wherever the signal found it, and encoding the units it
recorded, and ending an FST file, is work no signal handler may do. Every unit published is written
and the file ended before such a run ends ([ending-a-run](ending-a-run.md) D5); the simulation may
be stopped holding anything, so no lock stands between the two sides, and an FST block is compressed
on this thread, not on one of libfst's own, so the file waits on no other.

A dump whose file cannot be opened, or whose two sides the system gives no pipes to wait on, is the
failure of whoever asked for it: the `$dumpvars` or `$dumpports` call at its place, or the run's own
options. So is a file the system refuses to write -- full, gone, or failing as it is ended. The
writer meets the refusal, writes nothing more, and the simulation reports it where it next hands the
writer a unit, or waits on it. libfst checks no write it makes, so the build carries a change to it
that keeps the first one refused and answers it from a query and from the file's close, as `ferror`
and `fclose` do for a C stream; the same change is gtkwave/libfst#34.

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

### D7. What a scope is recorded as is a constant of the class declaring it

Whether a scope is recorded as a module, a block, a fork, a task or a function (LRM 21.7.2.1
`scope_type`) is what the declaration it fills is, which the class of the scope declaring it states:
an instance is a module and a generate block is a block. A named procedural block, a task and a
function have no object, so a variable declared in one names its declaration, and the dump opens a
scope of that declaration's kind around the first variable it records there; a scope nothing is
recorded in is not written. A level `$dumpvars` counts is an instance. A block belongs to the
instance holding it, so `$dumpvars(1, top)` records the variables of `top`'s generate blocks as well
as its own. A package is recorded as a scope of its own beside the tops, which a `$dumpvars` naming
levels alone records whole, under the type `package`. The five scope types of LRM 21.7.2.1 predate
packages; GTKWave reads that word, and FST names the same type. The compilation unit's scope, which
VPI shows as a package (LRM 37.10 detail 5), is recorded the same way, under the name `$unit`. The
text format names what a package declares after the package's name and `::`, `p::v` and
`p::touch.s`, as LRM 26.3 names it, so a member of a package `p` is never spelled as one of a top
module `p` would be; a VCD or FST file tells the two apart by the scope's type. An option naming a
package by its name alone, where no top has that name, records the package whole, as a name handed
to the design reads it.

## Rejected alternatives

- **Choosing what to dump when the design is built.** It is Verilator's shape, and it lets the
  compiler leave the dump code out of a build that wants none. Rejected because every new question
  becomes a rebuild, which is the cost this decision exists to remove, and because D3 already makes
  a run that dumps nothing pay nothing.
- **One method per class handing the dump each member's address and a probe of its kind.** It was
  this decision's shape until 2026-11-12. It left the runtime reading storage through a kind it
  chose, where the rows of D1 hand it only what the design's own code produced.
- **Two methods per row, each naming its row.** It was this decision's shape from 2026-11-12 to
  2026-11-15. Each data object then cost four functions to compile, the two methods and an entry for
  each, so a design's compile cost grew with what a dump may record rather than with the code it
  states.
- **A command-line option of `lyra run` for each plusarg.** It would be a second spelling of the
  same thing; a run's own arguments already follow the `--` that ends Lyra's command line.
- **Printing each change of one variable on the run's output.** The text format, selected to one
  variable, records the same changes in a file. Written to the run's output, its lines would have to
  keep their order with `$display`, which a writer on its own thread cannot do.
- **Splitting the encoding across threads by signal.** The measurement that recommends it was taken
  on a single-threaded encoder; it waits for a measurement of Lyra's own, and a split would have to
  leave an ending one thread to hand the dump to.

## Consequences

- A design gains one row per data object a dump may record and one per port, one constant per
  declared type, and two bodies per scope class, package and compilation unit's scope, which grow by
  a case per row; all are emitted whether or not any run dumps it.
- A run that dumps nothing enrolls no storage and starts no thread.
- The strength of a net's resolved value is what an exact extended dump of ports would need, and it
  is not kept.

## Cross-references

- [project-file](project-file.md) -- the line between what is true of a design and what is true of
  one run, which D2 applies to the options.
- `../architecture/runtime_model.md` -- the scheduling regions the postponed record of D3 runs in.
