# A function reports what a call of it reads

Date: 2026-09-29, revised 2026-10-18: what a call reads is recorded by the evaluation that runs it,
as it runs, where it was stated ahead of the body. Status: accepted. Realizes
[object-is-an-event-source](object-is-an-event-source.md) D3 and D7 for an expression that calls a
function.

## Context

A wait on an expression ends when the expression's value changes (LRM 9.4.2: a non-edge event is
"detected on any change in the value of the expression"), and the same clause adds that "changing
the value of object data members, aggregate elements, or the size of a dynamically sized array
referenced by a method or function shall cause the event expression to be reevaluated", while an
implementation "may" reevaluate for members the function does not reference. LRM 9.4.3 holds a
`wait (cond)` until the condition becomes true, and its examples are 9.4.2's own,
`wait (AOR.size() > 0)`. Only `@*` is limited to a call's arguments (LRM 9.4.2.2).

The value of an expression is a function of what its evaluation read, so it cannot change unless one
of those changes: what the last evaluation read is the whole of what a wait on it watches, and it is
known only once the evaluation has run. The waiting process evaluates the expression anyway, to
compare ([the-waiting-process-evaluates-its-wait](the-waiting-process-evaluates-its-wait.md)), so
finding out what it reads must be that evaluation and not a second one -- a second one runs what the
first runs, and a function with an effect then has it twice.

Two conditions shape the answer:

- **Units compile separately.** What a function reads is known where the function is declared, and a
  unit waiting on a call of it may read only what that unit published -- never its body (north star
  4 and 5; `unit-signature` D6). A package function calling another package's function is the
  ordinary case.
- **A body is compiled once and reached from every caller, and receives what it needs through its
  parameters** (`mir.md`, callables). Where the reading is recorded has to be handed in.

## Decision

**Every function and every class constructor takes one more parameter, the report of the evaluation
it runs in, and hands it to every call and construction it makes. An ordinary call hands none. A
waited evaluation hands its own. Handed one, the body states the cells its text reads as it is
entered, and each object it reads a property on and each interface variable it reads through a
virtual interface as it reaches it, so what the wait watches is what this evaluation read.**

- **A function is compiled in two forms from its one body.** The form every call reaches states
  nothing and, handed a report, answers with what the other answers; the other states what it reads
  and hands the report to every call it makes. It is called by the first alone, from the function's
  own unit, so it has no name in any space a function is reached through -- no dispatch slot, no
  published name -- and a call, a virtual method and an export reach it unchanged. Past that test
  the first form is known to hold no report, so it hands the functions it calls none, which is what
  lets a body run for no evaluation carry nothing of one: handing on a report it holds keeps every
  callee's test live, and on `call-chain` (sixteen nested calls, `-O2`, execution backend) the chain
  stopped inlining, 170 instructions an iteration becoming 485; with the two forms it is 170. A
  constructor has one form, so the calls its body makes are handed what it was handed, and a call
  chain inside a constructor keeps its tests.
- **The cells a body reads are a fact of its text**, stated by the unit declaring it: the storage
  outside the body it names, by the longest static prefix (LRM 11.5.3). They are stated whole at
  entry, however the body branches, which LRM 9.4.2's permission to reevaluate covers.
- **What is reached through a handle is stated where it is reached**, by the one point every
  property read and every virtual interface member read is lowered through, guarded by a test that
  the report is there. A place is held for the statement by a pointer to it, which copies nothing.
- **An object read is kept by the wait for as long as it waits on it**, as a subscription keeps what
  it is subscribed to: the evaluation may have reached it through nothing but a variable of a
  function that has since returned. One nothing owns yet is being constructed by this evaluation,
  which a later evaluation constructs afresh, so it is not watched.
- **A place reached many times is watched once**: a function a loop calls states its cells on every
  call, and the wait enrols once per place.
- **What runs as part of the evaluation states what it reaches there**: an array method's `with`
  clause runs at its call, and the watch an event control evaluates its expression in captures the
  report. A closure that outlives the evaluation -- a forked branch, a deferred effect -- captures
  nothing.
- **Foreign code is reached only through the exports it calls** (LRM 35.5.3), and a parameter cannot
  cross the foreign boundary, whose prototype is the standard's. So while a waiting process
  evaluates its expression, the evaluation is the running execution's too, and an export called from
  foreign code is handed it there. That is the one place the report is found rather than handed.
- **What cannot be stated is refused where a wait asks, not where the function compiles**: a read of
  something no wait can watch yet fails the simulation at the wait that reached it
  (`SimulationError`), and the function is otherwise unaffected.
- **An implicit list is collected through the same parameter**
  ([an-implicit-list-asks-each-function-once](an-implicit-list-asks-each-function-once.md)). It is
  static by its clause (LRM 9.2.2.2.1) and collected before the procedure runs, so a function handed
  a list's report runs nothing: it states what its text reads and writes, makes each call the list
  follows -- one the clause analyzes as a function, never one made on a class object -- with every
  argument defaulted, and returns its defaults. A bound on how deep those calls nest stops a cycle.

The shape is the field's. rustc records each query read into the running task's dependencies through
its implicit context and returns at once where nothing tracks (`DepGraph::read_index`,
`compiler/rustc_middle/src/dep_graph/graph.rs`, `TaskDepsRef::Ignore`), and keeps only reads that
are new (`reads.insert`). Vue's `track()` subscribes the `activeEffect` the running effect set
(vuejs.org, "Reactivity in Depth"); MobX tracks "only those observables that are read while the
function is executing" (mobx.js.org, "Understanding reactivity"). Ours differs in two conditions: a
body takes what it needs as a parameter rather than from an implicit context, so the report is one;
and a read here is a native load, not a getter, so a cell's read is stated once at entry from the
text rather than at each load.

## Rejected

- **Stating ahead of the body what a call will read.** The decision this entry took first: each
  function carried a report stated from its text and run before its body, and one another function's
  report called stood in for a body that did not run. It answers a question asked before the answer
  exists, and every case it could not answer needed a mechanism of its own -- an object reached
  through a variable of the function's own covered by every object, objects numbered as made so
  waits did not wake each other on objects they never read, operands with an effect handed as
  defaults, `ref` actuals lent defaulted storage, constructions reporting from the runtime, a depth
  bound -- and a virtual interface held in a variable of the function's own was refused outright,
  which rejects a correct program (north star 3). Its stated reason for rejecting what replaces it,
  "a check in every read of every variable and property ... a cost on all code for a rare
  construct", held of itself as well: it paid a parameter, an entry test and a report body in every
  function. What is recorded now pays the same parameter and entry test where no evaluation runs the
  function, and a second compiled form of its body.
- **Summarizing the callee's body in the waiting unit.** It is how slang infers an `always_comb`
  list (`AnalysisManager::getFunctionValUses`, which skips a non-static method's body). It reads
  another unit's body, and composing what a callee's callees read reaches units the waiter does not
  reference -- the dependency north star 5 forbids.
- **Publishing each function's read summary in its unit's signature.** A signature is derived from
  declarations alone and never grows a body's facts (`unit-signature` D6), and the transitive part
  still needs a signature of a unit the referrer does not reference.
- **A second form that callers reach by a name of its own.** One identity per body is what a call, a
  dispatch slot, an interface's promise class and both backends' names already reach, and a form
  callers named directly would need its own in every one of them; one missed is a wait that silently
  never ends. The recording form is reached only through the first, which is why it needs none.
- **Finding the report through the running execution in every body**, as rustc and Vue do. A body
  receives what it needs through its parameters, and every call already carries one more.
- **Polling the expression every evaluation round (Verilator, `V3Timing.cpp`).** Lyra wakes only
  what a write reached, and a change undone within one process run is still an event a poll misses.

## Consequences

- **A function call costs one more argument**, a null pointer, and the function one test of it on
  entry, which a call handing a known null folds away. Measured 2026-10-18 against `9680fd52` on the
  execution backend under callgrind: `call-chain` (`-O2`, 20,000 iterations of sixteen calls)
  5,708,612 instructions before and 5,708,304 after; a function walking a 64-node list through
  handles 2,000 times, 21,607,414 before and 21,604,031 after.
- **Every function's body is compiled twice**, once in each form; what that costs a build is in the
  pull request that measured it.
- **A wait whose expression calls a function is decided by its process**, which evaluates the
  expression on every candidacy, as one reaching through a handle is; a wait over cells alone
  registers them once, as before.
- **One change can reach a wait through several places it watches.** The places only wake the
  process, which evaluates once however many of them the change reached, so `@(p.a + p.b)` compares
  one new value against the last one.
- **A wait keeps alive the objects its last evaluation read** until it is satisfied or evaluates
  again.
- **Each export call asks for the evaluation in progress**, and nothing else does.
