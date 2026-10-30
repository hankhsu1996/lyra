# A covergroup is a class over a coverage library

Date: 2026-10-22 Status: accepted.

## Context

A covergroup (LRM 19) is a user-defined type whose instances count, at each sample, which of the
value sets and value sequences it declares the sampled values fall in, and answer coverage for an
instance, for the type and for the run (LRM 19.11). Its bins may be computed from its arguments and
from queues the design builds, so they are known only when an instance is made (LRM 19.3, 19.5.1.2).
What an instance counted outlives it: a type's coverage covers every instance ever made, and a
database written at the end of the run holds it (LRM 19.9). The program reads and writes the
`option` and `type_option` of a covergroup, a coverpoint and a cross as structures (LRM 19.10).

The front end declares the covergroup, its coverpoints, crosses, bins and option assignments, and
the built-in members of each (`option`, `type_option`, `sample`, `get_coverage`,
`get_inst_coverage`, `set_inst_name`, `start`, `stop`) with no bodies.

## Decision

**D1. A covergroup is a class the compiler writes from its declaration, over a coverage library
written in SystemVerilog.** What depends on the declaration -- which expressions are evaluated,
when, and over which bins -- is code of the covergroup's own class: its constructor evaluates the
arguments, the options and every bin's values and hands them to the library; its `sample` evaluates
each coverpoint's guard and expression and hands the values over; its built-in methods call the
library. What does not depend on it -- distributing values into bins, matching a value or a
sequence, the cross product, the arithmetic of LRM 19.11, naming bins, keeping what was counted --
is the library, written in SystemVerilog as `std`'s classes are
([the-classes-of-std-are-written-in-systemverilog](the-classes-of-std-are-written-in-systemverilog.md))
and read into a compilation where the design asks for coverage -- it declares a covergroup or calls
a coverage function (LRM 19.9) -- as a compilation-unit scope of its own. A design asking for none
compiles nothing of it and depends on nothing of it, as clang's driver adds its profile runtime to a
link only where the invocation asks for profiling (`ToolChain::needsProfileRT`), and Verilator
compiles its covergroup runtime only under coverage.

The class is written in the step to MIR, and HIR holds the covergroup as the source declares it: its
arguments with their directions, how it is sampled, its options, coverpoints, crosses and bins as
expressions, and each use -- `new`, a built-in method, an option -- as a use of the covergroup. A
layer writes a construct as a class where its own language can say the result. clang writes a
lambda's closure class in Sema (`Sema::createLambdaClosureType`, `Sema::BuildCaptureField` in
`SemaLambda.cpp`) because C++ has the reference member a by-reference capture is; rustc keeps a
closure as `hir::ExprKind::Closure` and builds a coroutine's state machine as a MIR transform
(`rustc_mir_transform/src/coroutine/`) because its surface language has neither. The covergroup's
class needs what SystemVerilog cannot say: a property naming another variable's storage (D6), an
execution no process owns (D4, D7), and code added to another declaration's blocks (D5). So it is
written where the language can say it, as rustc's coroutine is: one lowering of the declaration
writes the MIR class -- its properties, its construction, `sample`, the execution sampling it -- the
way a concurrent assertion's declaration is written as the callables of its scope, lowering the
declaration's own expressions where they are evaluated. Each use is lowered where it stands, to the
class's construction or a call. Below MIR nothing is a covergroup.

A unit naming a covergroup of another unit reads what that unit published of it -- its formals, its
items with their value types, the class embedding it and the covergroup it extends -- and lays the
class out from that by the same function the declaring unit lays it out with, so the two cannot
place a property differently. What a unit publishes of its covergroup is computed once, where the
covergroup is declared, and the declaring unit lays its own class out from that same form. A class
embedding covergroups lists them in the order its signature publishes the method making each, and
the methods are placed from that one list. Verilator, the open tool that implements covergroups,
also makes a covergroup a class with a runtime member per coverpoint, but compiles each bin into a
condition, which cannot hold bins computed from a queue at `new`.

**D2. Bin membership is data built at `new`, and matching is a library operation over it.** A bin's
values are handed to the coverpoint's library object one range list element at a time, a `with`
filter is a compiled loop over the values the library lists, and a cross's `with` filter a compiled
loop over each candidate tuple's value tuples. The library takes no callable. A coverpoint of an
integral expression is the library's integral coverpoint specialized at the coverpoint's type (LRM
19.5.7), so matching compares at that type; a real one is the real coverpoint.

**D3. The record outlives the instance, and the options are fields of it.** The library keeps each
instance's record, items and counts, and each type's record with every instance ever made of it, so
the type's coverage and the database read instances already gone. A covergroup is a user-defined
type (LRM 19.3), and one declared inside an instance of the design hierarchy is a type of that
instance (LRM 6.22), so a module's or a generate block's covergroup is a type per instance of the
scope declaring it: its type record, which `get_coverage()` reads (LRM 19.11) and the database
names, is the instance's, named by the covergroup's hierarchical name (LRM 23.6), while the class it
is written as is one for every instance. One a package or `$unit` declares is one type, named by its
declaration. Revised 2026-10-30: the record was named by the declaration everywhere, which merged
the covergroups of every instance of a module into one type. The front end's `option` and
`type_option` structures are the library's typedefs, so `g.option.weight` is a field of the record
read and written in place.

**D4. A clocking event samples inside its occurrence.** LRM 19.3 samples "the instant the clocking
event takes place, as if the process triggering the event were to call the built-in sample()
method", as many times as it occurs in a time step. So the execution waiting on the event is an
observer: woken, it runs inside the occurrence that woke it, as Qt's direct connection and a C#
event handler run in the emitter, and returns there at its next wait. Where a wake places it is held
with what it runs in, beside its region set, as an asynchronous runtime holds a coroutine's
dispatcher in its context; no process carries a note of its own for the scheduler. Unlike a
dispatcher it is not passed on to what the execution starts, since the standard places only the
sampling there. It runs once every wait the occurrence reached has been asked, as a C# event or a
Node emitter calls a copy of its listener list, so no sampling runs while that list is walked. It is
started by `new` and belongs to no process (LRM 9.6.1, 9.6.3 do not reach it). Under `strobe` the
first occurrence of a slot places one sample in the slot's Postponed region instead (LRM 19.7).

**D5. A block event is triggered by the block's own code.** A named block, task, function or method
a covergroup samples at the beginning or end of (LRM 19.3) triggers an event of the covergroup's
before its first statement and after its last. A `return`, `break` or `continue` that jumps out of
the block is the last statement it executes -- a `disable` "is not a shorthand for the return
statement" (LRM 9.6.2) -- so the jump triggers the end of every block it leaves, innermost first and
after a returned value is evaluated; a `disable`, a run-time error, and the release of a process
stopped when the run ends leave the block without reaching its end. The lowering of the block states
what its end owes, and a jump runs what each scope it leaves owes without knowing what that is, as
clang's code generation pushes a normal cleanup for a scope and emits it on every normal edge out of
it; the block's own statements are not rewritten. Where the block belongs to the scope declaring the
covergroup, the event is a cell the scope's instance keeps for the covergroup's type, published with
the scope's other members, so every instance of the covergroup that instance holds samples and no
other does. Where a class embeds the covergroup and the block is the class's method, the event is
the record of the covergroup the object holds. A block of another scope or class is refused: its
code is another scope's, which would have to signal an observer it does not declare.

**D6. A `ref` argument is a property naming storage elsewhere.** It is bound once, as the object is
constructed, to what the argument named, and read through for every sample (LRM 19.3 reads it as a
`const ref`). A body of the class that reads the covergroup's arguments begins by taking each into a
variable of its own -- an input's value, and a `ref` one as a reference to what the property names
-- since nothing a covergroup evaluates can assign them, so a wait on one watches what it names as a
wait watches any `ref` formal.

**D7. A run that made a covergroup instance writes the database at its end**, to the name
`$set_coverage_db_name` gave or else to `coverage.xml` in the working directory, in the XML format
of Accellera's Unified Coverage Interoperability Standard, and `$load_coverage_db` reads that format
(LRM 19.9). The writing is owed because the simulation is over, so it is held as a call run after
the final procedures, as C's `atexit` holds a function rather than a thread: nothing remains for it
to wait for. Like the final procedures it is design code, compiled from the library, so a run the
tool could not carry on writes none, the counts it would write being no longer trustworthy. clang
writes `default.profraw` unasked once a program is built with coverage, and Go writes nothing
without `GOCOVERDIR`; here a covergroup in the source is itself the request for coverage, as
`-fprofile-instr-generate` is clang's, so clang's answer is the one taken.

**D8. A covergroup extending another (LRM 19.4.1) is a class extending the other's class.** It takes
its base's arguments and items, and each class holds only what its own declaration adds, so an
extension of a covergroup of another unit is laid out from what that unit published. The first class
of the lineage makes the instance's record and starts the sampling, once; the record is of the
object's own type, which a virtual method of the lineage answers, since dispatch during construction
reaches the object's class (LRM 8.17 NOTE 2). Each class states for its base which of the base's
items it declares again: the base builds those uncounted, still sampled for its crosses, rather than
having them taken out of the count afterwards. The base sets its options first and the extension its
own after, which override them; `sample` is virtual and the extension's samples the base's first.
The embedding class makes the covergroup through a virtual method of its own, published with its
methods, which a class extending it overrides, so the base's `new` makes the extension. The
extension's instance replaces the base's, and every class of the lineage names that one instance
(LRM 19.4.1), so the variable the extending class declares is the base's: it holds no storage of its
own, which its class's construction would otherwise reset after the base's `new` made the instance
(LRM 8.7), and a use reaching what only the extension declares sees the instance as one of the
extension.

## Rejected

- **The class written in HIR inside the step to MIR, and lowered as a source class.** What HIR
  cannot state of it -- a property that is a reference, a method whose body runs apart from its
  call, the cell a block triggers -- would travel beside it as facts the class lowering consults, so
  neither the class nor the facts would be the program alone, the pair would have to be kept in step
  by whoever wrote it, and every body of the unit would be rewritten to place the uses and the
  triggers.
- **A base constructor told by a hidden argument whether it builds the most derived object.** The
  object's type answers it, by dispatch, everywhere construction asks.
- **The class written at AST-to-HIR, as clang's Sema writes a lambda's.** HIR states SystemVerilog,
  and the class needs a reference member, an execution no process owns and statements added to
  another declaration's blocks, none of which SystemVerilog has; HIR would have grown a flag or a
  node for each, and a unit naming the covergroup would have read its class as a class signature
  carrying them.
- **Each bin compiled into a condition.** Bins computed from arguments and queues at `new` have no
  condition to compile, and an array of bins over a wide range would be a condition per value.
- **The library in the text of `std`'s classes.** Every design then compiled it and depended on it:
  a design printing one line emitted 782 KB of C++, 770 KB of it the `std` unit, and the merge
  gate's test compiling every case twice went from 21.6 s to 186.4 s when the library joined it.
- **The library in the runtime's C++, with options read through calls.** The program names the
  options as fields of a structure, so the compiler and the runtime would both lay the record out.
- **The end trigger as the cleanup of a body.** A cleanup runs on every way out, a `disable` and the
  release of a stopped process included. One run on normal ways out only, as clang's `NormalCleanup`
  is, has no realization in a target whose stopped frames are destroyed rather than unwound: C++'s
  `scope_success` and its kin tell a normal exit by no exception being in flight, which a destroyed
  frame also satisfies. And a cleanup may not leave, while a trigger can, when the sampling it runs
  meets an error.
- **Sampling from a process woken in the Active region.** It samples the values after the triggering
  process has gone on, and merges occurrences that come before it waits again.
- **The embedding object held weakly.** A class handle has no weak form; an object and its
  covergroup are a cycle the language reclaims (LRM 8.29), and the cost of a reference count that
  cannot is the object model's, not the covergroup's.
- **A `ref` argument bound through the bindings ports use.** Those keep a record of every member for
  a later force, by the member's address, and an object's member ends with the object.
