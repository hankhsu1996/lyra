# Runtime Model

The compiler does not yet answer a timescale function or `$printtimescale` that names `$root`, or an
instance reached upward or from the top: such a call is refused, though the timescale paragraph
below gives each its answer -- except a simple name that reaches an ancestor instance, which is
folded as though the unit's own source fixed it. Everything else here binds every layer as written.

A compilation unit is a class. Compile-time produces the class definition: its parameters, its
constructor body, its callable bodies. Runtime has two contexts. The **constructor** runs once per
object at time zero and builds the object graph. The **simulation** runs at time >= 0 and drives
behavior under scheduler control. The two contexts do different jobs; HIR splits its lowering paths
because of this split. Around both stands the run itself -- the runtime library a body reaches
without being handed it, the generators every draw comes from, and what a run records -- which a
body neither owns nor carries.

## A module is a class

Read every SystemVerilog construct in a module as a piece of a C++ class:

- `module Foo #(parameter N = 4);` is class `Foo` with a constructor parameter `N`.
- `for (genvar i = 0; i < N; i = i + 1) begin : g child u(); end` is a `for` loop in the class
  constructor. It instantiates `N` children.
- `if (N > 0) begin ... end` is an `if` in the class constructor. It picks which sub-tree to
  instantiate. Which blocks were built is no part of which class an instance is
  ([compiling_a_design.md](compiling_a_design.md)): the class holds every alternative the source
  wrote, each a child the constructor may build.
- `initial begin ... end` is a class method that the scheduler calls once at simulation time zero.
- `always_ff @(posedge clk) begin ... end` is a class method registered with an event source and
  called by the scheduler on each matching event.

Generate constructs are constructor code. `initial` and `always*` are runtime methods. They run in
different contexts and they do different jobs.

## Storage and binding follow the type; construction is the variable part

Reading a module as a class separates two things that are easy to conflate.

A scope's **members** are uniform: each is a name and a type (see [mir.md](mir.md)). A signal, a
child instance, an instance array, and a generate scope are all members; they differ only in their
type. How a member is stored follows from that type alone; who owns a child and how its parent holds
it is [hierarchy_and_generate.md](hierarchy_and_generate.md) invariant 4. Every walk over the
hierarchy -- a phase, a dump, a traversal -- descends the tree's attachments, whose kind of scope,
module instance or generate scope, is read off the object's class. One walk covers a scalar
instance, an instance array of any dimensionality, and a generate scope, with no per-form branch and
no separate classification.

**Construction** -- the constructor statements that fill those members -- is the part that
legitimately varies, because SystemVerilog construction semantics vary. A signal takes its default
value. A single instance is built once. An instance array is built by replication over its element
count. A generate-for is a loop driven by a genvar. A generate-if is a branch. These are statements
in the constructor body, and the variation lives there and only there: it never leaks into how
members are stored or bound.

## Constructor runs at t = 0, simulation runs at t >= 0

The constructor allocates per-object state, binds the parameter values it was called with, walks
generate-if / generate-for / generate-case to decide which children to instantiate, instantiates
them, and returns. It does not advance simulation time. It does not yield to the scheduler. It does
not run any `always` body. When it returns, the object graph is fixed.

The simulation runs after the constructor returns. Processes get pulled off scheduler queues and
resumed. They read and write per-object state. They do not create or destroy objects of the
hierarchy. The graph shape does not change once the simulation starts.

Between construction and simulation -- still at t = 0, before any process runs -- the runtime
executes the binding graph and binds every reference: each route reaches its target through typed
navigation over the classes the referrer compiled against, its own and those other scopes published,
and nothing is looked up by name, and each binding's endpoint is bound once and read directly
thereafter (see [reference_resolution.md](reference_resolution.md),
[emission_model.md](emission_model.md)).

[elaboration_lifecycle.md](elaboration_lifecycle.md) refines this construction-vs-simulation
boundary into ordered phases (build / resolve / initialize / activate) and is the authority on
_when_ each piece runs: every cross-instance reference is bound in the Resolve phase, and variable
initializers run in Initialize after Resolve, not inside the constructor. The constructor allocates
the shell; it is not the executor of elaboration semantics.

## The design root is a unit of the link

At program entry the design is the one object tree rooted at the implicit `$root` (see
[hierarchy_and_generate.md](hierarchy_and_generate.md)), and a single scheduler drives the processes
of every top-level block on one time axis; several top-level modules are one simulation, not
several.

`$root` is a synthetic unit, made where the program is linked rather than by lowering any source
module, because its construction needs the list of tops, which no single module has. Its constructor
is the design's elaboration: it constructs each top and attaches it to the tree under the root,
exactly as any unit's constructor builds its children, and each top does the same one level down.
The root is built through its unit's object entry, as an instance of any other unit is, so both
backends hand the runtime the same thing and neither invents the design's structure. `$root` is not
a scope a source name reaches, and it takes no part in hierarchical names or diagnostics.

One time axis needs one tick, and the run fixes it from every timescale the design states, packages'
and `$unit`'s included, before anything of the design runs: the global time precision is the finest
precision a stated timescale gives (LRM 3.14.3). A scope left to the default time unit (LRM
3.14.2.3) states none and takes no part, and where no scope states one the default is the precision.
A scope may measure time more finely than the tick: what it reads of the time is in its own unit,
and a delay it names rounds to the nearest tick, which the standard leaves to the tool. The classes
of the built-in package `std` are Lyra's own definitions (LRM 26.7) and state no timescale.

A scope's time unit and precision are a fact of the definition its class is of, one per definition
(LRM 3.14.2.2), and a timescale function or `$printtimescale` answers for the design element it
names (LRM 20.4.1, 20.4.2). Where the unit's own source fixes which element that is -- the element
the call is written in, `$unit`, or an instance the unit's own hierarchy reaches downward -- the
answer is a constant of the unit, the same in every instance its code serves. Which case a name is
falls out of where its route goes, never how it is spelled: `Top.child` written inside `Top` reaches
the unit's own hierarchy, while a simple name that resolves to an ancestor instance (LRM 23.8) is
reached upward. An instance named upward or from the top is whichever the design puts there, so its
answer is read at run time from the definition of the scope the name's route reaches
([reference_resolution.md](reference_resolution.md) invariant 10), and `$root` answers the global
precision the run fixed (LRM 20.4.1). A constant folded in for either would answer for one placement
of an instance whose code serves every placement ([north_star.md](north_star.md) invariant 2).

What surrounds that construction is runner policy and is never MIR: creating the run, handing it the
built tree to bind (resolve, initialize, activate), running the scheduler, the exit status, and how
an executable or an in-process runner starts. Those change with how a design is executed while the
design does not, and a design artifact written as one runner's program would tie it to that runner.

## What a body reaches without being handed it

**The run is reached ambiently.** Every body -- a process, a method of a class, a package
subroutine, a static method, an initializer -- reaches the runtime library's effects through one
handle the attached run publishes on its thread for the whole of its life, never through its
receiver or a parameter. A receiver is the object a body runs on, and a package subroutine or a
static method has none, while every one of them may print or schedule; and threading the run through
every signature would make adding a `$display` a change to the signature of every caller up the
chain, against incremental compilation ([north_star.md](north_star.md) invariant 4). The handle is
per thread rather than per program, so two runs in one process never alias. What generated code may
reach of the run is its effects; binding a design and running it are the host's, and a body cannot
name them.

**A scope is structure and nothing else.** It contributes its place in the tree -- its parent, the
segment it fills, its child scopes -- the class it is an object of, and, for a module, interface or
program instance, the initialization generator of its instance (LRM 18.14.1). Queues, the processes
that exist, what each effect is attributed to and what is running all live on the run.

**A process is kept by whoever started it.** A process keeps the processes it started (LRM 9.5), a
program its initial procedures (LRM 24.3), and the run every other execution no process started.
What a process started is reached from it, for `wait fork` and `disable fork` (LRM 9.6.1, 9.6.3),
and no index by scope stands beside that; `%m` reads the scope tree, never a process.

**What is running is the ambient execution identity, and there is no ambient scope.** The run holds
one thing as running at a time -- a thread of frames, a reaction, or code on no thread
([scheduling.md](scheduling.md)) -- and a body that has an instantiated position reaches it through
its receiver. What the standard attributes to the executing process is attributed to it: a pending
violation report of a `unique`, `unique0` or `priority` check is scheduled on a queue of the
currently executing process (LRM 12.4.2.1), as a deferred assertion's report is (LRM 16.4.1), and
only that process reaching a flush point discards it ([assertions.md](assertions.md) holds which
points flush and how a report matures). Each suspending construct states whether resuming from it is
a flush point, so the resume path never branches on which construct a wait came from. A check
reached before any process exists -- a static variable's initializer (LRM 6.8), or a function
computing a parameter -- makes a report nothing can discard, which matures in the Observed region;
such a facility is usable from the moment the run exists, before the design is built.

**Foreign code finds the run the same way.** A symbol an export defines receives only C arguments,
so it reaches the run through the handle above, and the context it runs in -- the scope a `context`
import's call established, which `svGetScope` reports and `svSetScope` replaces (LRM 35.5.3) -- is
state of what is running, so two foreign calls suspended on different processes never share one. An
export reached outside the call of a `context` import is an error of the design, reported where the
export looks up its entry, before any of the export's body runs
([foreign_interface.md](foreign_interface.md)). Which scope the foreign side established is not
something the compiler can know, so it is checked where the export looks up its entry: a handle the
C layer never handed out is no scope, and a scope publishing no entry under the name is reported
rather than passed.

## Random stability

The standard leaves the generator to the implementation and fixes how the generators of a run relate
(LRM 18.13, 18.14): every thread and every class object has its own; a thread's is seeded with the
next random value of the thread that created it, a static process's with the next value of its
container's initialization generator, and an object's with the next value of the thread that created
it; each is "determined solely by its parent" (LRM 18.14.2); a seed the design states is an `int`
and the same seed gives the same sequence. So the generators of a run are a tree, as many as the
design has threads and objects, each made from its parent's when its owner is.

**A generator is split from its parent.** It is the L64X128 member of the LXM family -- four 64-bit
words, a linear congruential state, its addend and the two words of a xoroshiro128 generator --
which is the generator Java's `java.util.random` recommends where threads are created dynamically. A
child takes four draws of its parent, so nothing narrower than the parent's state stands between
them and two children walk different cycles rather than two stretches of one; a seed the design
states is spread over the whole state by mixing functions unlike the step, so neighbouring seeds
name unrelated generators. `$urandom` answers the high 32 bits of a draw. The state text
`get_randstate` answers is the four words, and only a text this implementation produced can be put
back; another ends the run with an error, since LRM 18.13.5 leaves it undefined and the text is one
the design computed.

**A draw uses the generator installed for whatever runs.** A subroutine is one body reachable from a
process and from a static initializer alike, so which generator a draw uses cannot be stated at the
call; it is state the run holds while something runs, as the run's handle is, and every
randomization entry draws from what it is handed without branching on what kind of owner that is. A
process installs its own. A static initialization -- the variable initializers of one scope (LRM
10.5), or a package's bring-up (LRM 26.2) -- installs one too, from where it starts to every way out
of it, so no draw ever finds nothing installed.

**A static initialization draws from a generator seeded by its container, not from the container's
initialization generator.** LRM 18.14.1 makes the initialization generator the one generator that
cannot be manually seeded, and a static initializer may write `$urandom(seed)`, so what it draws
from is seeded from the initialization generator and is not it. Each scope's static initialization
takes one seed from its module, interface or program instance -- a generate scope from the instance
holding it -- since a scope's initializers run before any child's and the run deliberately keeps no
record of which scopes are instances. A package or `$unit` is not instantiated, so its generator
starts from the default seed, which keeps one package's draws from moving when another is added. An
object a static initializer creates takes its seed from a split of the container's initialization
generator, as LRM 18.14.1 names. Seeds leave an instance in a fixed order: its static
initializations, one more for each object they make, then each static process as it is created.

Work handed over to run where no thread runs it draws from a generator derived from the giver's
without stepping it ([scheduling.md](scheduling.md)). The final procedures are function calls from
one thread that no thread created (LRM 9.2.3), so 18.14.1 does not say where their generator comes
from: it is split from the design root's initialization generator once the simulation has ended, the
last draw that generator is asked for. `$random` and the `$dist_` functions are the algorithm LRM
20.14 and Annex N state, not this generator's, with Annex N's `long` read as 32 bits; a `$random`
with no seed advances one seed the run keeps, starting at zero.

## A run chooses what it records

A person debugging a design wants its values over time in a file a waveform viewer reads (LRM 21.7),
and which part, which stretch of time and which format change from one run to the next while
building a large design takes minutes. So the dump is chosen when the run starts, never when the
design is built, and a run that dumps nothing pays nothing: it enrolls no storage and starts no
thread. Lyra keeps every variable, net and port as storage of its own, so this costs no visibility
Lyra was not already paying.

**Every scope class describes what may be read of it, and a run chooses from that.** A class of the
design hierarchy states, beside its definition, one row per data object a reader reaching the design
by name finds -- every static variable, a net, a named event, the static variables of a named block,
task or function: its name, what it is declared as, whether `const`, whether it is the name a `ref`
port gives its connection, its type as the source declares it (LRM 37.25), and the block, task or
function it lies in; then each port, in declaration order, by name, direction and the row of the
object it connects (LRM 21.7.4.2). Beside the rows the class has one body per operation for the
whole class -- settle a selection, read, write, force, release, subscribe -- each handed the
instance and a row's position and switching on it; a dump and VPI read the same rows. The bodies
read through a net's resolution, a port's binding and a force as the design does, so the runtime is
handed values and triggers and never told how to read storage, and one body per operation keeps a
design's compile cost growing with the code it states rather than with what may be recorded. A
package and `$unit` state the same rows for the variables they keep (LRM 37.10). What a dump never
records is what the standard leaves out (LRM 21.7.2.1): an unpacked array or structure, a string, a
handle, a dynamic container, and a parameter, which has no storage.

**A run's own options are the defaults and the design's tasks override them.** The program reads
`+lyra+dump=<file>`, `+lyra+dump+scope=<path>[:<levels>]` and `+lyra+dump+window=<from>:<to>` as
what to dump where the design says nothing, as LRM 21.6 plusargs, so a built program needs nothing
beside its argument list. A `$dumpfile` or `$dumpvars` the design runs wins, a `$dumpvars` run by
the end of the time step the options' dump starts in taking that dump over; `+lyra+dump+force`
reverses the order and `+lyra+dump+none` dumps nothing -- the order rustc gives lint levels and
LLVM's profile runtime its file name, each explained in one sentence. An option's path is resolved
against the run's object tree once, when the dump starts; a `$dumpvars` item is resolved by the
front end like any hierarchical name.

**A change is found per write and written once per time step.** Each recorded object enrolls on its
storage as a waiting process would, so a net is followed through what it shows and through a force;
a write that changes it marks it once in the step, and at the Postponed region the dump records each
marked value under one time. Storage nobody dumps pays nothing new. An event variable has no value
to mark: a dump of one records the time steps in which the object it names then is triggered (LRM
21.7.2.2), so its watch holds a share of that object and moves where the variable is given another.

**The file is written on a thread of its own, in an order the simulation alone decides.** The
simulation publishes each unit of records -- a time step's changes, a checkpoint, the definitions --
whole to the dump's writer, and at most a fixed number wait, so a writer that falls behind makes the
simulation wait rather than grow. The thread is kept because it is the one thing an ending can hand
the dump to: a run stopped from outside or by a fault leaves the simulation wherever the signal
found it, and finishing an encoded file is work no signal handler may do, so every unit published is
written and the file ended before such a run ends. `$dumpflush` (LRM 21.7.1.6) returns only once
every unit published before it is in the file, since its purpose is that a program reading the file
then finds that data there. A file that cannot be opened or written is the failure of whoever asked
for it -- the `$dumpvars` call at its place, or the run's options.

**A format is a writer, chosen by the file's name.** Every format is handed the dump in the order
LRM 21.7.2.1 lays a VCD file out. A name ending `.fst` is written as FST, the default a large design
wants; `.txt` as text a person reads; any other as VCD. A scope is recorded as what the declaration
it fills is (LRM 21.7.2.1 `scope_type`): an instance as a module, a generate block as a block, a
task, function or named block around the first variable recorded in it, and a package or `$unit` as
a scope of type `package` beside the tops, whose member the text a person reads names `p::v` (LRM
26.3), so it is never spelled as a top module's member would be. Each file `$dumpports` names (LRM
21.7.3.1) is a dump of its own recording only ports, each instance as one module scope, each value
read by the port's direction (LRM 21.7.4.3.1): an input as the test fixture drives it, an output as
the device does, and an inout, whose driver a value does not tell apart, in the characters of a port
of unknown direction.

## Generate is not compile-time expansion

A generate-for with 1024 iterations whose blocks are alike does not produce 1024 things at compile
time. Compile time produces one body for the block and a `for` loop in its constructor, and at run
time the constructor runs that loop 1024 times to build 1024 children. The compile-time artifacts
then follow the distinct unit specializations, not the instance count or the generate iteration
count ([north_star.md](north_star.md) invariant 2, which bounds artifacts and not the front end's
work).

Sharing that body is an optimization over a correct baseline, not what makes a lowering correct.
Lowering each block concretely is always correct, and the shared body is what a program gets where
its blocks are shown to be alike ([specialization_model.md](specialization_model.md) invariant 1).
So artifacts that follow instance count are a defect where the blocks could be shown alike, and the
baseline where they cannot.

## Structural code writes only its parameters and its loop indices

Constructor-side code is not a general procedural language: what a generate construct evaluates is
constant expressions (LRM 27.4, 27.5), and a procedure or continuous assignment a generate block
holds runs in the simulation, not at construction. The only writes during construction are:

- **Parameter binding** -- the constructor records the parameter values it was called with. This is
  owned by the constructor, not by a free-standing assignment.
- **A loop generate's step** -- the expression the source wrote to move the loop's index, in any of
  the forms LRM 27.4 admits (`i = i + 1`, `i += 2`, `i++`), each of which writes the genvar alone.
  HIR keeps the step as the source wrote it, held by the loop, and it is lowered as the write it is
  ([hierarchy_and_generate.md](hierarchy_and_generate.md) invariant 13).

So a structural scope holds no assignment statement, and the constructor body is a tree of generate
constructs whose expressions write nothing but a loop's own index.

## Simulation code does have general assignment

Inside an `initial` or `always*` body, `=` and `<=` are first-class assignment forms. Compound
assignments (`+=`, etc.) flow through slang's `LValueReference` machinery. Every variable the
process side declares or reaches may be written; a write is lowered the same way on both sides, and
what differs is only what each side may write.

A continuous assignment is simulation-time behavior too, so its right-hand side carries every value
expression a procedural body may -- a call, `$isunknown`, `inside`, a replication -- whichever scope
encloses it.

## Why this matters for lowering

HIR carries the split explicitly. A **structural scope** holds generate constructs, parameter
expressions, and child scopes. No procedural variables. No assignment beyond a loop's step. A
**process scope** holds an `initial` or `always*` body. Procedural variables. Assignments. It may
read structural variables but does not own them.

HIR-to-MIR lowers the two scopes through different functions because they target different runtime
contexts. A process body cannot run constructor code; it cannot instantiate a child. A constructor
body cannot run process code; it cannot wait for an event. A single lowering path that erased the
distinction would force one side to carry information the other side does not need, or hide a
constraint that the type system should be enforcing.

## What this rules out

Each shape names the requirement it fails.

- A process that instantiates a child, evaluates a generate construct, or otherwise changes the
  object graph. It fails [north_star.md](north_star.md) invariant 2: the graph is built by
  constructor-time logic at time zero, and references bound against it would go stale.
- A structural expression that reads or writes a procedural variable, or depends on simulation-time
  state (the current time, an event, a process activation). It fails the same invariant: structural
  code runs before any process or time exists.
- A write in a structural scope to anything but a loop generate's own index. It fails the structural
  split above and [hierarchy_and_generate.md](hierarchy_and_generate.md) invariant 7: a construction
  builds what the front end elaborated by evaluating the source's constant expressions from the
  values the object was handed, and a write of anything else would make what it builds depend on
  state the front end never saw.
- A pipeline where the frontend-elaborated instance graph drives compile-time work. It fails
  per-unit artifacts ([north_star.md](north_star.md) invariant 2).
- A constructor body that yields to the scheduler or advances simulation time. It fails
  [north_star.md](north_star.md) invariant 2: the graph is built at time zero, before any process
  that could be scheduled exists.
- The design's top-level construction written by a backend's host shell, or engine creation, binding
  and running stated in MIR. The first fails [backend_contract.md](backend_contract.md)'s mechanical
  translation, since each backend would invent the design's structure; the second ties a design
  artifact to one way of running it.
- The run reached through a body's receiver or threaded as a parameter. It fails LRM 26.3 and 8.10,
  which give subroutines no object, and incremental compilation, which a parameter every caller must
  pass would break.
- A pending violation report owned by a scope or a package. It fails LRM 12.4.2.1: the report
  belongs to the executing process, and two branches of a `fork` share a scope but are two
  processes.
- A draw that asks whether it runs inside a process, or one generator shared by every static
  initialization of a design. The first fails the single-body requirement above, since one
  subroutine is reached from both; the second fails LRM 18.14.1, since one package's draws would
  move whenever another package is added.
- A generator seeded through a 32-bit value between parent and child. It fails LRM 18.14.2's unique
  seeding at the scale a testbench reaches: among a hundred thousand children of one parent two are
  expected to share a seed.
- What a run records chosen when the design is built. It fails iteration time
  ([north_star.md](north_star.md) invariant 1): every new question about a run becomes a rebuild.
- A trace or update-set subsystem layered over writes. It fails LRM 4.3's update event, which a
  store raises as it happens, and run time ([north_star.md](north_star.md) invariant 1), since every
  write would be visited twice. The one thing reconciled afterwards, in a design built to coalesce
  combinational updates, is the net change an execution of an `always_comb` or `always_latch` left,
  for the waits of such procedures alone.

## Adjacent docs

- [compiler_overview.md](compiler_overview.md) covers the compile-time / runtime split at the level
  above.
- [compilation_unit_model.md](compilation_unit_model.md) defines what is class-level (compile-time
  artifact) and what is per-instance (constructor input).
- [hierarchy_and_generate.md](hierarchy_and_generate.md) defines the object graph that the
  constructor builds.
- [scheduling.md](scheduling.md) defines how the simulation context dispatches processes, regions,
  and work run on no thread.
- [object_model.md](object_model.md) defines the class's definition, which the runtime reads to
  drive a scope through its phases and to end an object.
