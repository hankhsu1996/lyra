# A departure stops at a frame this compiler did not emit

Date: 2026-09-16 Status: accepted

## Why this decision matters

An execution can be told to stop while part of its call chain is code written in another language
and compiled by another compiler: LRM 35.9 states it for a `disable` reaching a block that is
"currently executing a mixed language call chain", and LRM 9.6.3 and 9.7 reach the same execution by
terminating its process. Those frames cannot be unwound and cannot be skipped. They end only by
returning.

Six positions were measured before any of this was designed, and every one of them was wrong. A
`disable` of a named block whose execution sat inside an imported task threw the runtime's own
control effect through the C frame and aborted the simulator; `disable fork` and `kill` left the
process terminated and then resumed its parked stack into it; `$finish` aborted while releasing the
stack; an exported function whose body executed a `disable` had its foreign caller's frame silently
skipped, with no report and no abort; and a C program calling `svIsDisabledState`, which the
standard's own header declares, did not link.

## The model

**Whatever would have left by unwinding stops at the boundary, and what crosses instead is a value
the foreign side is obliged to act on.** Three things follow, and together they are the whole
mechanism.

```text
the question   whether the execution now running must stop: a disable has reached a block it is
               inside, or its process has been terminated. One question because foreign code can
               do only one thing about either.

the answer     an exported task's entry returns it as the int LRM 35.8 gives that entry, and
               svIsDisabledState answers it to any foreign frame that asks.

the departure  is not carried across. Whether one is due is derived where control comes back, so
               the boundary loses nothing by landing it.
```

The third is what makes the first two sufficient. Membership of a disable target and the generation
captured on entry live on the running execution, and a termination is recorded there too, so the
question has the same answer before the foreign call and after it -- and the execution leaves at the
first point of its own that it reaches.

## The decisions

```text
D1. No departure crosses a frame this compiler did not emit. An exported subroutine's entry is a
    region that lands whatever no region of its body claimed, because past it there is no frame of
    ours for a departure to travel through.

D2. What the entry hands back in its place is the standard's own answer. An exported task returns
    the disable-active int (LRM 35.8) whichever way its boundary ended, so its two paths meet at one
    return. An exported function has no such value, so it answers with its declared result type's
    default and the foreign side is required not to read it.

D3. The simulator's half of the protocol is implemented, and the foreign side's half is checked.
    Item a is D2. Items b, c and d are obligations on the foreign programmer that LRM 35.9 also
    obliges a simulator to verify, each checked where its evidence is: what an imported task
    returned, whether an imported function acknowledged, and whether an exported subroutine was
    reached at all once the state was entered.

D4. A check reports and ends the run; it never departs. The frame each one stands in was reached
    from foreign code, so the composition a fatal makes is taken apart: the report and the stop
    happen there, and the departure happens at the boundary where control comes back.

D5. The boundary is where the execution leaves. Every other point at which an execution regains
    control is a resumption, which each backend already gates its own way; a foreign call that
    consumed no simulation time suspended nothing, so this is the one such point a body states for
    itself. Only a `context` import can reach an exported subroutine (LRM 35.5.3), so only its
    boundary carries any of this.

D6. Terminating an execution that holds an unreturned foreign call is a request, not a settlement.
    It is asked to stop and handed control once more; it leaves at its own gate, its foreign frames
    return of their own accord, and the terminal state is published when the body finally settles.
    This is the two-step transition an execution whose frame is still running already takes, applied
    to the one other case where a frame cannot be torn down where it stands.

D7. The gate is one question with two sources. A target disabled since this execution entered it,
    and a termination this execution has yet to unwind for, are asked together wherever control is
    regained; a departure naming no target is one no region may claim, which is what a termination's
    is.

D8. When the run itself ends, nothing is handed control. A foreign call that never returned is
    released the way every other suspended execution is -- the run is over, and the same rule that
    forbids unwinding its frames forbids running them.
```

## Rejected alternatives

- **Let the control effect unwind through the foreign frames.** It is what the code did, and on a
  stack whose root is the runtime's own fiber it calls `std::terminate`. Where it does not -- a
  foreign frame reached directly, on a platform whose C frames carry unwind information -- it
  silently skips that frame instead, which is the same defect without the crash: the foreign side
  loses the cleanup the protocol exists to give it.

- **Hand the foreign side a pointer to a departure and let it decide.** It is what a managed runtime
  does for a native method, and the standard forecloses it here: LRM 35.8 fixes the exported task's
  return as the channel, and item d forbids the foreign side from calling back in to ask anything
  else. Every system that faces this problem lands on one shape -- the request is recorded on the
  execution and the foreign frame reads it at a point of its own choosing -- and the standard has
  already chosen where the read happens.

- **Carry the departure across the boundary and re-raise it on the other side.** It needs a place to
  keep one, an owner for it, and a rule for a boundary re-entered before it was consumed. Deriving
  the answer again needs none of the three, and the state that answers it is already kept for every
  other resumption.

- **Check the protocol by refusing the call rather than reporting it.** An entry reached in the
  disabled state cannot refuse: it has no way to tell its caller anything except through the value
  it returns, which is the channel the protocol already spends. Reporting and ending the run is what
  the standard asks for, and it is the only thing that reaches the user.

- **Give the boundary its own departure state instead of widening the gate.** It would answer the
  same question in a second place, kept in step with the first by nothing, and it would leave a
  terminated execution that resumes anywhere else still running its next statement.

## Consequences

- An exported task's entry returns 1 where it used to return 0 unconditionally, so foreign code
  following the protocol now sees the state it is written against.
- `svIsDisabledState` and `svAckDisabledState` resolve. A C program written to the standard's own
  header used to fail to link against a design that declared any DPI name.
- A process terminated while inside a foreign call stays live and un-nameable until that call
  returns, so a `process` handle reports it terminated only once nothing of it can run again.
- The gate every resumption passes now answers for a termination as well as for a disabled target,
  which is what lets a kept-alive execution leave at a point it was already going to reach.
- A cleanup that withdraws something from whatever is running tolerates nothing running, because an
  execution still suspended when the run ends has its frame released where it stands. Without that,
  `$finish` aborted the simulator for any design with a task enable suspended at the time -- which
  needed no foreign code at all.

## Cross-references

- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- the callable model this boundary is an arm of,
  and the export entry that carries it.
- [dpi-context-scope-is-an-extent](dpi-context-scope-is-an-extent.md) -- the extent around a context
  import's call, whose cleanup this decision makes true of a departure as well as of a return.
- [disable-scope-invalidation](disable-scope-invalidation.md) -- the generation model that makes a
  departure derivable rather than carried, and the gate this widens.
- [ending-a-run](ending-a-run.md) -- the report / stop / departure composition a check takes apart.
- `../architecture/nonlocal_control_flow.md` -- the region, the raise and the cleanup, and why a
  frame that lands nothing carries nothing.
- `../architecture/activation.md` -- the two-step termination for an execution that cannot be torn
  down where it stands.
- IEEE 1800-2023: 35.8 (an exported task's return value), 35.9 (the disable protocol, its four
  items, and the checks a simulator owes), 35.5.3 (only a context import reaches an export), 9.6.2
  and 9.6.3 (`disable`, `disable fork`), 9.7 (`kill`), 20.2 (`$finish`).
