# Foreign Code Is Called Back Where Its Reason Places It

## Date

2026-11-19

## Status

Accepted. Revises [scheduling](../architecture/scheduling.md), whose region structure gains the PLI
regions a callback is placed in, and extends
[foreign-interface-routines-are-defined-against-the-standard-s-headers](foreign-interface-routines-are-defined-against-the-standard-s-headers.md)
D5 and [a-vpi-handle-is-a-reference-to-an-object](a-vpi-handle-is-a-reference-to-an-object.md) D11.

## Why this decision matters

Foreign code asks the running simulation to call it back with `vpi_register_cb` (LRM 36.9.2, 38.36):
after a change of an object, at a point of a time slot, or for a simulator action, until it
withdraws the request or the moment is spent. A methodology library's C code depends on it -- UVM's
polling of signals places a value-change callback on each signal it watches and, from inside the
callback, writes a bit a process of the design waits on. The questions this settles come back with
every reason: what a callback is attached to, where its foreign code runs, what it is handed, what a
moment the run will not reach is, and what the standard leaves to the tool.

## What the survey says

- **libuv** keeps a handle per loop phase -- prepare, check, idle -- in an intrusive queue the phase
  runs: `uv_*_start` links it, `uv_*_stop` unlinks it, and the runner moves the queue out before
  calling, so a callback stopping itself or another is safe (`loop-watcher.c`). A timer is a handle
  too, and `uv_signal_t` traps a signal and delivers it through the loop.
- **Icarus Verilog** attaches a value-change callback to the signal's filter and calls it as the
  value propagates; a part-select callback keeps its bits and compares. Time callbacks are events in
  the time step's own queues, start-of-time and end-of-time ones at an absolute time, the rest after
  a delay. End-of-compile callbacks run before initialization, start-of-simulation after it,
  end-of-simulation after the final blocks (`vpi_callback.cc`, `schedule.cc`).
- **Verilator** keeps lists per reason and compares every watched variable against a saved copy when
  the user's loop asks (`verilated_vpi.cpp`); it has no observer per variable to be told by.

**Where our conditions differ.** Every write already tells the observers enrolled on what it wrote
as it lands (LRM 4.3), and no code of the design runs while one of those lists is walked; foreign
code is code of that kind. So a change is pushed, as Icarus pushes it, and the foreign code is
called once the telling has ended, never inside it.

## Decisions

### D1. A callback is a registration attached to the source its reason names

What foreign code registered is kept as copies of its own, since what it was handed belongs to the
caller (LRM 38.36), and attached to what already owns the occurrence: a change of a variable to the
place a wait on it enrols on (LRM 9.4.2), a disable to the target it ends (LRM 9.6.2), a point of a
time slot to a timer in that slot's region, the time slot next moved to to the advance of time, a
simulator action to the run, a signal to the one owner of the process's signals. Nothing else
watches storage, so a run that registers no callback pays nothing to have its storage watched. A
source the run holds rather than storage or a time slot -- a disable target, a simulator action, a
signal -- has the callbacks waiting for it listed under it, so an occurrence finds its own without
walking the rest (revised 2026-11-20; a disable, an action and a signal walked every callback).

Removing a callback and destroying it are two events, each decided by its own owner. Removal takes
it out of the registry and of its source at once, so it is called no more and keeps no time slot
(LRM 38.39). It is owned by the registry while it stands and by each call of it while that call
runs, and goes when the last of them lets it go, taking with it its handle to the object it was
registered with, since a call is handed that handle (LRM 38.36). A timer is owned the same way, by
what it attaches and by whoever fires it while it fires, which is the scheduler's rule for any timer
its owners share, not one of the callbacks'.

### D2. The slot holds the PLI regions LRM Table 4-1 places a callback in

Pre-Active, Pre-NBA, Post-NBA and Pre-Postponed are regions of the slot as every other is, run where
LRM 4.5 runs them: Pre-Active once as the slot begins, the next two among the active set's regions,
Pre-Postponed once nothing from Active to Re-NBA holds anything, where what it places runs before
Postponed. A time callback is a timer the scheduler runs there like any other. The PLI regions no
callback names hold nothing and are not kept.

### D3. A callback runs as an application routine of no thread

Its foreign code runs on no thread of the design and as no procedure, inside an application routine
call of its own: `vpi_control` is carried out as it returns (LRM 38.4), no DPI scope is current and
no export may be called (LRM Annex H.9). A value-change, size-change or disable callback runs at the
occurrence, once every observer the occurrence reached has been told and before the execution that
made it goes on, as a covergroup sampled on its event does.

A disable is the occurrence, and it has happened once the disable has run, whenever what it ended
leaves. It ends the activity of every named block and task an execution still active inside its
target runs in (LRM 9.6.2): a block nested in the one named, a task down the chain of enables from
the one named. Each such block or task is called back once for the disable, however many of its
activations it ended, and an activation of the same task elsewhere is untouched. An execution an
earlier disable or a kill already ended is no longer active, though it has not left yet, so a
disable of a target nothing active is inside ends nothing and calls nothing back. Which ones a
disable ends is read by the disable itself, before it ends them. A function is never one of them: no
`disable` names a function, none from elsewhere can find one running since a function never waits,
and a function disabling what called it is undefined (LRM 9.6.2). A callback placed on one is kept
and never called.

### D4. A call is handed what its occurrence was

A change of an object is read as it is made, and the callbacks it made due are called once what was
due before them has been, so each is handed the changes in the order they were made, each its own
value; a size change is called ahead of a value change one operation made with it (LRM 37.17 detail
14). A callback on a part of a variable is enrolled where the design's own wait on that select is,
so a write is told to it where it may have changed the part, and to no other
([a-write-compares-what-it-reached](a-write-compares-what-it-reached.md)). Whether the part changed,
the callback decides: it keeps what it last read -- a value, a size, or which object a class
variable names -- and is called where what it reads now is another, whatever the write said of what
it did. A class variable's object is told apart by its ownership record, held weakly, so an object
made where an ended one lay is another, and a callback keeps no object alive; none of it is handed
to foreign code, whose value is none (LRM 38.36.1). Time and value are in the forms the registration
asked for, a form left out being no structure (LRM 38.36.1.1). Each callback for an error reads that
error through `vpi_chk_error`, whatever a callback called before it for the same error cleared.

### D5. A moment the run will not reach is refused

Every time callback names a delay from now, the clause speaking of a delay of zero for a
start-of-time callback and a read-write one alike (LRM 38.36.2). A callback for a point of the
current slot that the slot does not run again is refused, which is the read-write error the clause
lists, but for an after-delay one (D6). The start-of-time error is stricter than that: once the slot
is under way one of no delay is refused even in Pre-Active, unless it is asked for from inside a
start-of-time callback, which runs where it would. So is a callback for a simulator action the run
has passed -- the end of compilation, which no foreign code reaches the run before, and the start of
the simulation once it has started. So is a time callback for a later slot once none will run -- the
run has been asked to end, or is over -- since nothing would ever call it. Whether a moment is one
the run still reaches is the scheduler's one answer for any moment, which a callback asks rather
than working it out from the scheduler's state. Writing a value in the read-only Postponed region is
refused too (LRM 4.4.2.9, 4.4.3.10), whoever asks.

### D6. What the standard leaves to the tool

- A read-write synchronization runs in Post-NBA, since it runs "after execution of events for a
  specified time" (LRM 38.36.2).
- An after-delay callback of no delay asked for once Pre-Active has run is no error the clause
  names, so it runs in the slot's Active region, the earliest the slot still runs.
- An end-of-simulation callback runs once the final procedures and what the design asked for at its
  end have run, and not where the tool could not carry the run on, which is neither ending the
  clause names. A signal a callback trapped is handled as it was before once the run is over.
- An error callback is called for each run-time error the run reports of the design, where
  `vpi_chk_error` reads it at that error's place; a routine-error callback for each error of a VPI
  routine at an error's level or above. Neither is called again from inside a call of its own,
  however deep -- through a callback of the other kind among them.
- A signal callback traps the signal; it is called between two region passes, before the next region
  is chosen, since what it writes may fill an earlier one, and while one stands the signal is its, a
  stop signal's report included. A signal the system lets no program catch, or that reports a fault
  of the instruction raising it, is refused.
- A reason for a feature Lyra lacks -- save, restart, reset, the interactive mode, an unresolved
  system task the front end refuses before any foreign code runs -- is refused as absent; a reason
  Lyra owes and does not carry out is refused as not yet supported, never accepted to wait silently.

## Rejected

- **Comparing every watched variable against a copy at points of the loop, as Verilator does.** It
  costs a pass over every watched variable whether or not anything changed, and loses changes made
  and undone between two passes; every write here already tells its observers.
- **Calling the foreign code while the observers of a write are being told.** It runs code that can
  remove an observer, or write again, in the middle of walking the list it reached it through.
- **Reading a callback's value when it is called.** A callback that writes the variable again would
  hand a later callback of the first change the second change's value.
- **Calling a callback on what a write says it did, without a comparison.** It was built twice: once
  taking every change told as the callback's object's own, and once taking only a change the write
  marked as proven. Each time a review found kinds of write whose account was wrong for some
  position -- a reference naming no position, an element made or ended, a member of a union, a path
  joined past its length, a size not carried below a whole write -- and none was found by a test.
  The comparison costs one read, which a call has to make anyway to hand the value, and nothing a
  write says can then be wrong in a way a foreign program sees (D4). A class variable was then
  watched only where every write reaching it compared before it told, which wrote the limits of how
  Lyra writes into what the standard allows; it reads which object the variable names instead.
- **Accepting a callback whose occurrence never happens here** -- a timing check, which no run
  carries out yet. It reads as a feature the run has and silently never calls back.
- **Letting a removed callback go the next time the run calls into the callbacks.** It works, but
  when that is has nothing to do with whether anything still uses it, and it may never come; who
  owns it decides that.
- **A disable callback per activation ended.** The callback is placed on a block or a task, not on
  an execution of one; a frame's and a thread's own callbacks are what follow those.
- **A signal handler of the callbacks' own beside the stop-signal report.** Two owners of one signal
  hand it back and forth; one owner decides what each signal does.
