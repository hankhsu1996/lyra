# A wait is storage of the activation that waits

Date: 2026-10-05. Status: accepted. Moved a wait's memberships off the activation
(`a-membership-is-one-record.md`), and revises `waiting-is-an-operation.md` D4 and
`disable-scope-invalidation.md` D2 in place.

## Context

A process is one body with any number of places it stops (LRM 9.2, 9.4): an `always` is its
statement run repeatedly, an `initial` runs it once, and each timing control holds the rest of the
body until a time or an event. Every stop on storage asks the same of the runtime: which storage it
watches, and what decides that a change there is an event.

What it watches is, almost everywhere, the same every time the body reaches the stop. Measured
across RTLMeter's 18 designs, 98.8% of event controls name a variable or a constant select and the
rest are fixed on inspection; in Ibex's UVM testbench the moving case is a class handle to a virtual
interface, written once at build. LRM 9.4.2 defines the event by the value of the whole expression
and permits reevaluating more often than needed, so nothing in the standard requires rebuilding what
a stop watches each time it is reached.

The runtime rebuilt it anyway: every stop allocated a wait, copied its triggers, enrolled the frame
on every place, and revoked every enrolment on waking. On Ibex, under callgrind over the whole run,
that was about 3,700 of the 5,700 instructions each activation cost, against about 270 for a
hand-written model with the same event counts.

## Decision

**Every timing control is a wait the body holds in its frame, built where what it waits for stops
changing, and stopped at from there on.** Each stop says only that the activation is parked on it
now; an occurrence while one is parked wakes it, and one while none is, or while the activation is
stopped, is no occurrence. Every timing control stops through the same entry, and nothing about a
stop is decided twice; the one other way a body gives up control is a foreign task carried on a
fiber (LRM 35.9), which waits for no occurrence at all.

- **Where it is built is decided by what it names.** A wait whose leaves are cells of the design,
  and whose observations evaluate nothing but such cells where a change happens, is built once in
  the outermost block of the body, which lasts for the body's whole run -- a process's, a call's, a
  branch's. One that watches, or evaluates, a variable the body declares is built at its stop,
  because that variable exists only from its declaration on (LRM 6.21). Every other wait is built at
  its stop too, because what it waits for is known only there: a delay's amount (LRM 9.4.1), a
  fork's branches (LRM 9.3.2), the reports an evaluation the process made
  (`the-waiting-process-evaluates-its-wait.md`, LRM 9.4.3). The shape is the same in every case;
  only the block differs.
- **A wait's memberships stand for the wait's life.** It enrols on each place it watches -- an
  observable, a named event, a `wait fork` condition, a process's termination -- when it is built,
  and leaves when it ends. Reaching a place asks the wait whether an activation is parked on it, and
  takes that activation if the occurrence is an event for it. One stop answers every change that
  arrives before the activation runs, and a change the activation makes itself while running is no
  occurrence.
- **A wait for a state is reached by each change on the way there.** A `join` watches each branch's
  termination (LRM 9.3.2 -- a killed branch has terminated as surely as one that ran out),
  `wait fork` the process's children (LRM 9.6.1), an `await` its target's termination (LRM 9.7).
  Each is asked, when reached, whether what it waits for holds now, so the last branch to terminate
  is the one that wakes a `join`.
- **What a stop carries on at is the wait's answer, and arranging it is the scheduler's.** A wait
  answers that the execution carries on without stopping, waits for an occurrence, resumes later in
  this time step in a named region, or resumes at a time; only the scheduler knows the time it is
  and how to queue an activation, so it does the rest, the same way for every construct.
- **Every membership belongs to what shares its life.** What a wait watches belongs to the wait. A
  process's being inside a disable target (LRM 9.6.2) belongs to the process's record of that
  target, enrolled when it enters and gone when it leaves, and a `disable` wakes the process's frame
  if it is blocked right now. An activation's place in a scheduler queue belongs to the activation.
  Each target's list holds one of these kinds, so none can name the wrong owner.
- **Process control is unchanged in meaning** (LRM 9.7): stopping a process takes it off the wait it
  is parked on and keeps the wait; starting it again asks the wait for its answer afresh -- an event
  control measures each observation from what its expression is worth then -- and parks it there
  again. A `disable` that landed meanwhile is taken at the restart.

Every process stays one kind of thing: a coroutine resumed where it stopped. MIR states every wait
as a local of the runtime's one wait type and every stop as the existing wait on a call
(`waiting-is-an-operation` D2) taking that local; neither backend decides anything new.

## Survey

- **SystemC** (`sysc/kernel/sc_thread_process.h`, `trigger_static`): a thread's static sensitivity
  is a list on each event, filled at elaboration and never refilled; the gate is whether the thread
  is waiting on its static set now (`m_trigger_type == STATIC`), plus a suspended and an
  already-runnable check. A dynamic `wait(e)` registers per call.
- **Verilator** (`verilated_timing.h`, `VlTriggerScheduler`): one trigger per distinct sensitivity,
  computed once per evaluation step; an await pushes the coroutine handle onto that trigger's
  vector, with no allocation in steady state. A trigger it cannot evaluate statically
  (`needDynamicTrigger` in `V3Timing.cpp`) is reevaluated by its coroutine at every step.
- **C++ and Rust** write the same thing: the awaitable is a local of the coroutine declared outside
  the loop and awaited in it, and only what differs per await is an argument of the await. A C++
  awaiter built at its `co_await` is a temporary of the frame, and cppcoro's event awaiter carries
  its own list link (`async_manual_reset_event_operation::m_next`, in cppcoro's
  `async_manual_reset_event.hpp`); tokio's `Notified` future holds its waiter node inside the task's
  future (`Notified::waiter`, `tokio/src/sync/notify.rs`), while the run-queue link lives in the
  task header (`Header::queue_next`, `tokio/src/runtime/task/core.rs`). A future's `poll` answers
  whether it is ready and leaves registering a waker to the future; here the answer names how to
  resume, because the scheduler, not the wait, owns the time axis.

Our condition differs from Verilator's in one respect -- units compile separately, so there is no
whole-design trigger table -- and from SystemC's in none that matters: SystemC's static set is per
process and ours is per stop, which is the same mechanism at a finer grain.

## Rejected

- **Two kinds of process**: a function called each time what a single fixed wait watches changes,
  beside a coroutine for everything else (SystemC's `SC_METHOD` and `SC_THREAD`). Built and measured
  first: Ibex went from 14.08 G to 9.96 G instructions on the execution backend. It put the split in
  every layer -- a HIR fact saying whether a body waits mid-way, two lowering paths, three
  registration entries, two runtime body kinds, two-armed memberships and process control, two
  detached carriers -- and the cost it removed was never the coroutine's; it was rebuilding the
  wait, which this decision removes for every stop on fixed storage, including the many-stop
  testbench processes the split left paying it. SystemC split because it had only stackful threads;
  that condition is not ours.
- **Enrolling at each stop through nodes the wait owns.** Allocation-free, but every stop of an
  `always_comb` pays one link and one unlink per leaf; a standing membership pays one gate test per
  occurrence instead.
- **The waits only the stop can name built by the runtime and owned by the activation**, beside the
  frame-held ones. What the tree had after the first step of this decision: two lifetimes of wait,
  an activation holding both a pointer and an owner, three ways to stop, memberships owned sometimes
  by the activation and sometimes by a wait, and a membership type naming either, with a refusal for
  the arm a target could not take. None of it was needed: every wait is reached from the body's own
  code, so the frame can hold each one.
- **A join counted by its branches' completion callbacks.** A branch's frame reports on reaching its
  end, which a killed branch never does, so its join waited forever; a process's termination is the
  one transition every way of ending passes through, and it already had a list for `await`.
- **Re-enrolling on enclosing disable targets at every stop.** A process inside a named block joined
  the block's target at each stop and left it at each wake; the process's own record of the target
  already lasts exactly as long as the membership has to.
- **Watching a fixed superset for a wait through a handle.** Conformant (LRM 9.4.2), but the census
  found the handle written once, so the precise form costs nothing in the steady state.

## Consequences

- A wait built once costs its construction once per run of the block that builds it -- the awaiter,
  and its memberships -- and nothing per stop. A wait built at its stop -- a delay, a join -- costs
  that at the stop, which every wait did before. Ibex, under callgrind over the whole run, went from
  14.08 G to 10.40 G instructions on the execution backend and from 11.63 G to 8.65 G on the C++
  backend.
- Against the rejected function shape (9.96 G and 7.63 G), every run of a process still pays for
  being resumed and parked -- 0.44 G of Ibex on the execution backend, 1.02 G on the C++ one. That
  cost is the coroutine's and is the same whatever the process waits on, so it is a cost of
  resuming, not of this decision.
- A cell some wait is enrolled on is watched for as long as that wait stands, parked there or not,
  so a write to it describes what it changed even while the waiting process is running; the numbers
  above include that.
- A wait in a loop whose body declares what it watches or evaluates is rebuilt each iteration, as
  every wait was before.
- An implicit list is collected once, without running the functions it asks
  (`an-implicit-list-asks-each-function-once.md`), and the wait is built on it there.

## Cross-references

- `a-membership-is-one-record.md` -- a membership is owned by what shares its life, and a wait's
  stands for the wait's life.
- `waiting-is-an-operation.md` -- D4, revised in place: every wait is held by the frame, built where
  what it waits for stops changing.
- `disable-scope-invalidation.md` -- D2, revised in place: a process enrols on a target for as long
  as it is inside, and a `disable` wakes it where it is blocked.
- `the-waiting-process-evaluates-its-wait.md` -- which controls a change decides.
