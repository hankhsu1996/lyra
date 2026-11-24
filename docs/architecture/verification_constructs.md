# Verification Constructs

A testbench drives and samples a design through clocking blocks (LRM 14), measures what it reached
through covergroups (LRM 19), and chooses stimulus under constraints (LRM 18). Each of the three is
carried out by what the layers already state and a library, never by a node or a runtime type of its
own: a clocking block is variables, events, executions and placements of the scope declaring it; a
covergroup is a class the step to MIR writes over a coverage library written in SystemVerilog; a
constraint is code describing a problem to a solver in the runtime. Below MIR nothing is any of the
three. What each means depends on values only the running program has -- a skew is an instance's
parameter, a bin may be computed from a queue at `new`, a constraint reads the variables it does not
choose -- and both backends translate one MIR without deciding anything
([backend_contract.md](backend_contract.md)), so a node for any of them would make a backend decide
how it is carried out.

Where a construct's work is placed in a time slot -- a placement naming a region outright, the end
of a pass, the order executions woken at one occurrence run in, a sample taken under `strobe` -- is
[scheduling.md](scheduling.md)'s; the sampled value a `#1step` input reads is
[assertions.md](assertions.md)'s; an object's generator and random stability are
[runtime_model.md](runtime_model.md)'s; and the disputed texts each construct met are ruled on in
[language_conformance.md](language_conformance.md).

## A clocking block is storage of its scope and an observer of its clock

A clocking block is used in four ways -- an input is read and waited on, the block's event is waited
on, a cycle delay counts the block's events, an output is driven -- from the scope declaring it, a
unit holding an interface port to it, and a class holding a virtual interface to it (LRM 14.9,
25.9). Each use has to mean the same wherever it is written, while what the block is, its signals
and its skews, is known only to the unit declaring it. LRM 14.7 makes the block "both a declaration
and an instance of that declaration ... one copy is created for each instance of the block
containing the declaration", of static lifetime; nothing names it as a value, and a clockvar is a
variable of the block whose initializer is the signal it names.

**The block is what the source wrote, held by its scope.** The scope declares it as its clocking
event and its clockvars, each with its signal and its skew per direction as written or as LRM 14.3
defaults it, and the variables the standard gives the block: each input's sample, which a read reads
and `@(cb.x)` waits on (LRM 14.13, 14.15), the event the block's name stands for (LRM 14.10), and
for each output the variable inside the block holding what the block drives onto its signal, which
every drive writes (LRM 14.16). These are declarations rather than storage a lowering adds, because
a use in any unit names them as it names a variable and the standard defines each. Nothing a
lowering invents to carry the block out -- what counts its occurrences, what keeps a skewed input's
copy, what lands a drive, what puts a variable output's value onto its variable -- is part of the
declaration. A net output is the one exception the standard writes: its net is driven "as if by a
continuous assignment from" its variable (LRM 14.16), so the declaration names that variable's read
and what it watches, as a continuous assignment states its own, and the driver built from it is the
lowering's.

**Sampling is an execution of the scope woken inside each occurrence of the clocking event.** It
counts the occurrence, samples every input, and places the trigger of the block's event into that
time step's Observed region (LRM 14.10). The samples change before the block's event, as the event
is processed (LRM 14.13), except one skewed by `#0`, which LRM 14.4 places in the Observed region,
ahead of the trigger. So a process woken by a sample changing before that region, waiting on the
block from there, is reached by the event of that same time step, and one a `#0` sample wakes runs
after it. Running inside the occurrence means anything the occurrence wakes sees it counted, which
is what gives "the event of its own time step" one answer. It belongs to no lineage (LRM 9.6.1).

**Each skew reads from the earliest moment that knows the answer.** A `#1step` input is the signal
over its leaves' sampled values, settled before the time step began, so it is stored as the event is
processed. A delay -- `#0`, the delay of none, among them -- is a copy of the signal kept that far
behind it: an execution of the scope following every change of what the signal reads lands each
value in the Active region of the slot the delay names. The sample is a deferred effect of the
event's slot, in that same Active region, placed behind every update of the copy for the slot,
whichever region the event came in -- a blocking write's, a nonblocking one's, a program's, a
foreign callback's in Pre-Active -- and whichever region set the scope's code runs in; or, for a
skew of no step, read and rounded as a delay control's amount is, in the Observed region. Which of
the two is the lowering's to say, from the steps the run counts the skew as, so the run's placements
stay ones naming a region outright. A delay sample a program's event takes is changed only once the
active set runs again, which a program reading it without waiting on the block races with (LRM
14.13). No reader at the event can know what the signal was before it, and a skew "can be specified
as a parameter" (LRM 14.4), so its value is the instance's and no compile-time test of it is made.

**A drive is landed by the scope declaring its block, on the instance its target is reached on.** It
never blocks the statement (LRM 14.16): it reads its value, its target's selects, its count of
cycles and the instance it reaches where it is reached, and hands the write to an execution of no
lineage. Through a virtual interface the drive names the member access of its target whose instance
the landing is made on, so one evaluation serves both. The execution calls the declaring scope's
landing of the output, a task taking the count: it waits for the occurrence the drive matures at --
`n` on, or the next where `n` is none and its time step has had no occurrence yet -- and from that
occurrence for the skew, returning in the Re-NBA region of the slot the skew names, a skew of none
being that occurrence's own slot. Executions waiting on one event are woken in the order they began
to wait, so drives of one block maturing together land in the order they were made. `##n` is the
same count over the block's event (LRM 14.11). Drives of different blocks on one edge maturing at
one occurrence land in the order the blocks' clocking events wake them, which LRM 14.16.2 leaves
unordered.

**An output onto nets drives them from its variable, and an output onto a variable takes the last
value of each pass.** A net output's driver is the continuous assignment LRM 14.16 names, driving
`(strong1, strong0)` and `'z` until the first drive, and it runs once after the pass, so it reads
only what the pass's drives left. Every output onto one variable, of one block or several, shares
one driving variable, put onto the signal once each Re-NBA pass that drives it has run: the first
drive of the pass to land keeps an effect for the end of the pass, which sets the driving variable
to what the signal holds then, lets the waiting drives write it in the order they landed, and writes
the signal once from it. So only the last value each part was given reaches the variable, a value
the pass took back is no change, a part no drive wrote keeps what the signal had, and the drive
landing last sets each part, as "the value most recently assigned by either clocking block" asks
(LRM 14.16.2). Each pass commits by itself, so an output still changes once per pass, since Re-NBA
may run more than once in a time step.

**What another unit reaches is published, and what carries it out is not.** A block publishes its
event, its inputs' samples and its net outputs' driving variables as members under the block, the
driving variable outputs onto one variable share as a member of the scope named after that variable,
and each output's landing as a callable whose body the declaring unit writes. The names of what the
standard keeps invisible to user code -- the block's event, the driving variables, the landings --
each carry a space, which no identifier can, so name lookup in the language finds none of them. The
count of occurrences, the copies and what a variable output commits by are fields the declaring unit
adds after its published fields and nothing names, so how a skew or a commit is carried out moves
nothing a referrer compiled against ([emission_model.md](emission_model.md)).

**The runtime learns no clocking block.** It has placements naming a region outright, the end of the
running pass as a placement, and a start kind for an execution of a scope woken at an occurrence
([scheduling.md](scheduling.md)); everything else is variables, events, closures, waits and
executions MIR already states.

## A covergroup is a class over a coverage library

A covergroup (LRM 19) is a user-defined type whose instances count, at each sample, which of the
value sets and value sequences it declares the sampled values fall in, and answer coverage for an
instance, for the type and for the run (LRM 19.11). Its bins may be computed from its arguments and
from queues the design builds, so they are known only when an instance is made (LRM 19.3, 19.5.1.2),
and what an instance counted outlives it: a type's coverage covers every instance ever made, and a
database written at the end of the run holds it (LRM 19.9).

**A covergroup is a class the compiler writes from its declaration, over a coverage library written
in SystemVerilog.** What depends on the declaration -- which expressions are evaluated, when, and
over which bins -- is code of the covergroup's own class: its constructor evaluates the arguments,
the options and every bin's values and hands them to the library; its `sample` evaluates each
coverpoint's guard and expression and hands the values over; its built-in methods call the library.
What does not -- distributing values into bins, matching a value or a sequence, the cross product,
the arithmetic of LRM 19.11, naming bins, keeping what was counted -- is the library, written in
SystemVerilog as `std`'s classes are, and read into a compilation as a compilation-unit scope of its
own only where the design asks for coverage, by declaring a covergroup or calling a coverage
function. A design asking for none compiles nothing of it and depends on nothing of it, as clang's
driver adds its profile runtime to a link only where the invocation asks for profiling.

**HIR holds the covergroup as the source declares it, and the class is written in the step to MIR.**
A layer writes a construct as a class where its own language can say the result: clang writes a
lambda's closure class in Sema because C++ has a reference member, while rustc builds a coroutine's
state machine as a MIR transform because its surface language cannot say one. The covergroup's class
needs what SystemVerilog cannot say -- a property naming another variable's storage, an execution no
process owns, code added to another declaration's blocks -- so it is written where it can be, by one
lowering of the declaration, and each use (`new`, a built-in method, an option) is lowered where it
stands to the class's construction or a call. A unit naming a covergroup of another unit reads what
that unit published of it -- its formals, its items with their value types, the class embedding it
and the covergroup it extends -- and lays the class out by the same function the declaring unit lays
its own out with, from that same published form.

**Bin membership is data built at `new`, and matching is a library operation over it.** A bin's
values are handed to the coverpoint's library object one range list element at a time; a `with`
filter is a compiled loop over the values the library lists, and a cross's over each candidate
tuple. The library takes no callable. A coverpoint of an integral expression is the library's
integral coverpoint specialized at the coverpoint's type (LRM 19.5.7), and a real one is the real
coverpoint.

**The record outlives the instance, and the options are fields of it.** The library keeps each
instance's record, items and counts, and each type's record with every instance ever made of it. A
covergroup is a user-defined type (LRM 19.3), and one declared inside an instance of the design
hierarchy is a type of that instance (LRM 6.22), so its type record, which `get_coverage()` reads
and the database names, is the instance's, named by the covergroup's hierarchical name (LRM 23.6),
while the class it is written as is one for every instance; one a package or `$unit` declares is one
type. The program reads and writes `option` and `type_option` as structures (LRM 19.10): a member
the source names is the library structure's member of that name, read and written in place.

**A clocking event samples inside its occurrence.** LRM 19.3 samples "the instant the clocking event
takes place, as if the process triggering the event were to call the built-in sample() method", as
many times as it occurs in a time step, so the execution waiting on the event is an observer run
inside the occurrence that woke it ([scheduling.md](scheduling.md)). It is started by `new` and
belongs to no process (LRM 9.6.1, 9.6.3 do not reach it). Under `strobe` the first occurrence of a
slot places one sample in the slot's Postponed region instead (LRM 19.7).

**A block event is triggered by the block's own code.** A named block, task, function or method a
covergroup samples at the beginning or end of (LRM 19.3) triggers an event of the covergroup's
before its first statement and after its last. A `return`, `break` or `continue` jumping out of the
block triggers the end of every block it leaves, innermost first and after a returned value is
evaluated, while a `disable`, a run-time error and the release of a process stopped when the run
ends leave the block without reaching its end. The lowering of a block states what its end owes, and
a jump runs what each scope it leaves owes, as clang pushes a normal cleanup for a scope and emits
it on every normal edge out of it. Where the block belongs to the scope declaring the covergroup,
the event is a cell the scope's instance keeps for the covergroup's type, so every instance of the
covergroup that instance holds samples and no other does; where a class embeds the covergroup and
the block is its method, the event is the record of the covergroup the object holds.

**A `ref` argument is a property naming storage elsewhere**, bound once as the object is constructed
and read through for every sample (LRM 19.3 reads it as a `const ref`). A body of the class that
reads the covergroup's arguments takes each into a variable of its own -- an input's value, and a
`ref` one's reference -- so a wait on one watches what it names as a wait watches any `ref` formal.

**A run that made a covergroup instance writes the database at its end**, to the name
`$set_coverage_db_name` gave or else `coverage.xml` in the working directory, in the XML format of
Accellera's Unified Coverage Interoperability Standard, which `$load_coverage_db` reads (LRM 19.9).
The writing is owed because the simulation is over, so it is a call held to run after the final
procedures ([scheduling.md](scheduling.md), How a run ends), and a run the tool could not carry on
writes none, its counts being no longer trustworthy. A covergroup in the source is itself the
request for coverage, as `-fprofile-instr-generate` is clang's, so the database is written unasked.

**A covergroup extending another (LRM 19.4.1) is a class extending the other's class.** Each class
holds only what its own declaration adds. The first class of the lineage makes the instance's record
and starts the sampling, once; the record is of the object's own type, which a virtual method of the
lineage answers, since dispatch during construction reaches the object's class. An item replaces the
item of the same kind and name nearest it up the lineage, which then no longer counts, and which
items count is a fact of the type -- a derived covergroup and its base are separate types -- so the
type's record states it, and an item built for an instance asks its type. A replaced item is built
and sampled still, for the crosses over it. The base sets its options first and the extension its
own after; `sample` is virtual and the extension's samples the base's first. The embedding class
makes the covergroup through a virtual method of its own, which a class extending it overrides, so
the base's `new` makes the extension, and the variable the extending class declares is the base's,
with no storage of its own that its class's construction would reset (LRM 8.7).

**Every part of a covergroup is named by its declaration, settled where names are resolved.** A
coverpoint and a cross are named by the covergroup declaring them and their place there, and a bin
construct by its place in its coverpoint, counted from the front end's declaration so every unit
naming an item counts alike. Which item a derived one replaces is found once along the lineage, as
clang's Sema finds an overridden method. The library's members a covergroup's class calls are
resolved once, where the unit names the library, so a member left out is a build error rather than
an entry missing at run time. An item's name is what its report and the database print; the run is
handed an item's place and a bin construct's number, and finds by name only the bins of instances
merged (LRM 19.11.3) and what a database read back holds.

## A constraint describes a problem the runtime solves

LRM 18 makes `randomize()` choose values for a class's random variables, at random from the object's
own generator, so that every constraint in force holds -- uniformly over the assignments that do,
unless `solve ... before` orders them (LRM 18.5.9), and leaving every variable as it was where none
does (LRM 18.6.3). What is in force is decided while the program runs: a constraint reads the values
of variables it does not choose, `rand_mode()` and `constraint_mode()` switch variables and blocks
per object (LRM 18.8, 18.9), a call may list what it chooses or add constraints of its own (LRM
18.7, 18.11), and a random handle brings the object it reaches into the call (LRM 18.4). A class
extending another adds variables and blocks, often in another compilation unit, and a block of the
name of one it inherits replaces it (LRM 18.5.2).

**A constraint is code that describes a problem to a solver in the runtime, as a C++ program using a
constraint library writes it; MIR has no node for a constraint or a random variable.** Every system
that randomizes under constraints makes the description at run time, by code, from the run's state,
which is the part the standard forces; clang and rustc have no construct for one, since a solver is
a library.

- **A problem is a runtime object made by each call.** A variable enters it through a reference to
  its storage, so a random variable is the storage it names and an object reached twice is one
  variable; a value a constraint reads that names no storage enters as a constant; each operator is
  a call answering a term, and each constraint a requirement or a preference on one. A random array
  is walked by its own type's code: the call begins a walk on the variable's reference, and the
  array type's body names the array to the problem -- one whose size the program chooses before its
  elements (LRM 18.5.7.1), with its type's own body making it a count long -- then enters each
  element at its position by a reference formed from the array's as a write through it would form
  one. So the runtime is handed references and positions and never a type to walk, and a write the
  solver makes is told at the element it wrote.
- **Every class object starts with the part the runtime library declares for it**, holding the
  object's generator beside the modes every object keeps, and its class's definition states five
  bodies: declare the random variables, state the constraints, name the random objects its handles
  reach, and the two hooks. Each source class states the first three, doing what the class it
  extends does first by calling that body directly, whichever unit declares it, so a class of
  another unit can call its base's without knowing what the base declares. A constraint block is a
  virtual method of its class, called where its mode is on, so a block of the same name in an
  extension replaces it by the ordinary means. A class declaring `pre_randomize` or `post_randomize`
  overrides the hook with a call to it, which gives the hooks the standard describes as not virtual
  but reached as though they were (LRM 18.6.2). A class the library defines, a process, states none
  of the five, so randomizing one chooses nothing. A variable's mode and a `randc` variable's cycle
  are kept for the variable the class declares and the positions of an element within it, never for
  an element's storage, which moves when its array is resized; `rand_mode()` on an element hands the
  positions its selects name, which the compiler states.
- **A call of `randomize()`** runs `pre_randomize` on the object and each object a random handle
  reaches, has every object declare its variables before any states a constraint, adds the inline
  constraints, settles what has to be chosen first, and solves. Settling chooses array sizes and the
  variables functions are handed (LRM 18.5.11) by the constraints those reach, resizes and writes
  them, and states the description again with them held. A call that fails puts back what settling
  changed.
- **The solver builds an and-inverter circuit of the terms**, and draws variables no constraint ties
  together apart. A group's bits are decided in one fixed order -- its variables as declared, each
  from its most significant bit -- so what a call draws follows from the problem and the object's
  generator alone, never from what any call drew or built before (LRM 18.14). Where the condition
  fits a decision diagram of a bounded number of live nodes, built in that order, the diagram counts
  every completion and the draw is exactly uniform (LRM 18.5.9); a diagram is kept by the condition
  it holds and reused by the next call stating the same one. Where it does not fit, each variable
  tries a few values drawn at random and keeps the first a satisfiability solver says something
  completes, and a bit still open takes a random side among those something completes: every answer
  meets the constraints and none is out of reach, but where one side of a bit holds far more
  completions than the other the two are taken equally often, which the run reports the first time a
  call draws so; the run's command line may raise the bound. It finds an answer wherever one exists.
- **A `randc` variable takes the next value of a keyed permutation of its range**, or, where
  constraints reach it, a value among those they allow that the permutation has not yet taken; a new
  permutation begins when none is left, and when the constraints on it differ from the previous
  call's. A variable of an enumerated type the call chooses is required to equal one of its members,
  which the compiler states beside its declaration, and a `randc` one permutes the members'
  positions. A distribution chooses an item by weight among those the constraints leave some value
  of, and a value uniformly within it.

Two conditions decide the solver. A built program runs with nothing installed beside it, so the
runtime links its solvers -- CUDD and CaDiCaL, built from source by the repository's own build
files, so a program optimized together with the runtime compiles them too -- rather than piping a
description to a solver process. And drawing uniformly among the solutions of an arbitrary condition
is #P-hard, so no solver meets LRM 18.5.9 for every constraint: a decision diagram meets it exactly
where it fits, and where it does not, no open tool draws uniformly either. What Lyra keeps that they
leave out, and the standard asks for, is that an object's values depend on its own generator alone,
by deciding every bit by whether something completes it and never by which completion a solver
happened to find.

## Forbidden shapes

- **A MIR node, or a runtime type, for a clocking block, a covergroup, a constraint or a random
  variable.** It fails the requirement that both backends translate one MIR without deciding
  anything: a backend meeting the node would decide how the construct is carried out.
- **A drive carried as a nonblocking assignment behind `repeat (n) @(clock)`.** It fails LRM 14.16's
  order of landing: its carrier is woken into Active after processes the same occurrence woke, so a
  drive made later in a cycle could land before one made earlier.
- **A runtime clocking type holding the occurrence count and the pending drives.** It fails the
  engine's additivity, which gains no per-construct branch ([scheduling.md](scheduling.md)), and it
  restates what an event and the wake order at one occurrence already decide.
- **What carries a clocking block out declared as part of the block -- the count, a skew's copy and
  its process, a task waiting out the skew.** It fails the requirement that how a skew is carried
  out moves nothing a referrer compiled against.
- **A pass's writes collapsed in the engine by telling the waits once the pass ends.** It fails the
  rule that a write tells what it changed as it is made, and would teach the engine which writes are
  a clocking block's.
- **The signal copied into the driving variable at the first drive of a pass, or the commit made in
  the next pass.** The first fails LRM 14.16.2 where another block's commit wrote a part later in
  the pass; the second lets what the pass woke in Reactive read the variable as though the drives
  had not landed.
- **A covergroup's class written in HIR, or at AST-to-HIR.** It fails the requirement that HIR
  states SystemVerilog: the class needs a reference member, an execution no process owns and
  statements added to another declaration's blocks, none of which SystemVerilog has.
- **Each bin compiled into a condition.** It fails LRM 19.5.1.2's bins computed from arguments and
  queues at `new`, which have no condition to compile.
- **The coverage library in the text of `std`, or in the runtime's C++.** The first fails the
  requirement that a design asking for no coverage compiles nothing of it; the second has the
  compiler and the runtime both lay out the option structures the program names as fields.
- **A covergroup's end event as a cleanup of the block's body.** It fails LRM 9.6.2: a cleanup runs
  on every way out, a `disable` included, and a normal-exit-only cleanup has no realization where a
  stopped frame is destroyed rather than unwound.
- **Sampling from a process woken in the Active region.** It fails LRM 19.3's "the instant the
  clocking event takes place": the triggering process has gone on, and occurrences before it waits
  again are merged.
- **A constructor told by a hidden argument which items count or whether it builds the most derived
  object.** It fails the requirement that what the object's type says is answered by the type, by
  dispatch (LRM 19.4.1).
- **The runtime walking a random array from its type's description.** It fails the requirement that
  an array's layout is stated once, by its type's own code, and names no part, so a write the solver
  made would be told as a change of the whole variable.
- **Solving at compile time.** It fails LRM 18: what a constraint reads, which variables are chosen
  and which blocks are on are values of the running program.
- **An external solver process.** It fails the requirement that a built program runs with nothing
  installed beside it.
- **Returning the assignment a solver finds, aimed by random values given it.** It fails LRM 18.14:
  the solver's choice depends on what it learned answering earlier questions, so one object's values
  would depend on what every other object asked.
- **A decision diagram whose variables are reordered to keep it small.** It fails a draw that
  follows from the problem and the generator alone: whether a condition fits the budget would come
  to depend on what the package did before.
- **A compiled table of each constraint read by an interpreter in the runtime.** It fails the
  requirement that each expression has one evaluator: the table needs a second evaluator of every
  expression a constraint may contain, where code calling the problem reuses the lowering of each.
