# Scheduling

A lyra simulation runs as a set of coroutines coordinated by a stratified event scheduler. Each
SystemVerilog process -- `initial`, `always*`, `final` -- compiles to a C++ coroutine. The scheduler
pulls coroutines from time-slot region queues and resumes them; a coroutine runs until it yields
with a named reason (delay, event wait, `$finish`), and the scheduler parks it on the queue matching
that reason. Deferred effects (`<=` writes, `$strobe` prints) are not yields: the coroutine
snapshots its inputs into a closure, hands the closure to the engine, and continues running. The
engine invokes those closures when their owning region runs.

This doc covers the decisions behind the engine itself: process model, suspension protocol, region
structure, deferred work, `$finish` semantics, and the MIR boundary. Sensitivity tracking, change
detection, and edge detection are separate runtime subsystems.

## Processes and tasks are coroutines

A process body -- and a task body, which may consume time -- compiles into a `Coroutine` whose
`co_await` expressions hand a `WaitRequest` to the scheduler. The scheduler holds a
`std::coroutine_handle` and calls `.resume()` to continue the body. Process state -- procedural
locals, the implicit program counter -- lives in the coroutine frame, which the engine does not
introspect.

A task is enabled with `co_await`: enabling a time-consuming task suspends the enabling process
until the task completes (LRM 13.3). The scheduler resumes the innermost suspended frame, and each
completed task transfers control back to its enabler, so suspension flows through the whole enable
chain without the engine tracking call depth.

Fibers are rejected: per-process stacks would multiply memory by orders of magnitude for any
nontrivial design. Coroutine frames hold only the locals live across suspension points. Hand-rolled
state machines are rejected: they would force HIR-to-MIR lowering to thread resumption state through
every statement that might suspend, defeating the structured form MIR is designed to carry.

## Suspension is a named reason

A coroutine surrenders control only by `co_await`-ing an awaitable that sets a `WaitRequest`.
`WaitRequest` is a closed `std::variant` of the reasons the engine knows how to dispatch:
`DelayWait`, `EventWait`, `FinishWait`. A new suspension kind is added by extending the variant;
there is no implicit yield path.

The engine sees nothing about a suspended coroutine beyond the reason it yielded with. There is no
priority field the engine reads, no out-of-band hook for a process to leave notes for the scheduler.
The reason is the protocol.

## Deferred work is a closure submit, not a suspension

When a process needs an effect to happen later in the same time slot -- a non-blocking write to a
signal, a postponed `$strobe` print -- it does not yield. It snapshots its inputs into a closure
(see `mir.md` for the MIR-level shape) and hands the closure to the engine via a region-specific
submit call, then keeps running.

Yield-based alternatives are rejected because they would entangle the deferred effect's commit time
with the process's resumption schedule. A process that does `q <= d; #5; ...` should not be forced
to suspend before `#5` just because the NBA write has not yet committed; the write commits in the
NBA region of this same time slot regardless of where the process is by then.

## Region structure

A time slot runs regions in LRM 4.4 order: Preponed, Active, Inactive, NBA, Observed, Reactive,
ReInactive, ReNBA, Postponed.

The **active group** (Active, Inactive, NBA) iterates as a unit. Active drains, then Inactive
promotes; Inactive drains, then NBA commits; if NBA's committed writes wake any sensitivity-bound
processes, Active runs again. The cycle repeats until all three queues are empty. Without this
iteration, a process woken by an NBA commit would be deferred to the next time slot, which is
incorrect -- it belongs to the slot whose NBA woke it.

The reactive group (Reactive, ReInactive, ReNBA) iterates by the same rule, for program-block work.
The two groups are independent of each other.

`#0` delays land in **Inactive**, not Active. A user writing `#0` is asking the engine to let other
already-pending active work finish first, then come back at this same simulation time. Pushing
`#0`-suspended processes to the back of Active would let them re-enter ahead of work that arrived
earlier, defeating the intent.

## `$finish`

`$finish` yields a `FinishWait`. When the scheduler dispatches it, it sets a `finished_` flag and
the time-slot loop exits without running further regions in the in-progress slot. Pending NBA writes
for that slot do not commit; queued active processes do not resume.

Registered `final` actions then run in registration order. A `$finish` raised from inside a `final`
aborts the remaining finals. This three-cornered behavior (active shutdown, NBA discard, final
abort) follows LRM 9.2.3.

## Closure capture lifetime

A closure's captures are either by-value or by-reference (see `mir.md` for the MIR-level shape). The
two have different lifetime rules:

- **By-value capture** is always safe. The snapshot lives inside the closure's storage; the engine
  destroys it when the closure fires or when the closure is dropped without firing.
- **By-reference capture of a `StructuralVarRef`** is always safe. Structural-scope storage
  (module-level variables) lives for the entire simulation; any closure can hold a reference into it
  indefinitely.
- **By-reference capture of a `ProceduralVarRef`** is safe only if the closure fires while the
  process's coroutine frame is still alive. The procedural frame is destroyed when the process body
  returns or when `$finish` tears down the simulation; references into the frame are invalid after
  either event.

The third case is the one a lowering pass must reason about. A lowering that emits a closure
capturing a procedural lvalue is responsible for establishing the fire-before-frame-dies guarantee,
or it must refuse the capture.

## MIR boundary: no pointers, only captures

MIR carries no pointer type, no reference type, no `AddressOf`, no `Deref`. Where a runtime
mechanism needs the shape of "this place, written later" -- NBA commit, postponed strobe, callback
registration -- MIR expresses it as a closure whose capture list includes the place to write. The
C++ backend renders by-reference captures as `&` lambda captures; the resulting C++ holds the
reference directly.

Pointers are not added to MIR. Captures already express the "hold a place for later" pattern in the
shape a software engineer would write by hand, and a parallel pointer mechanism would split the same
concept across two MIR vocabularies. Pointers would also invite storage-layout decisions to leak
into MIR; the engine, not MIR, owns where storage lives.
