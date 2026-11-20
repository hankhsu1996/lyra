# Where an unwind goes is stated in LIR, and how it travels is the target's

Date: 2026-11-20. Status: accepted.

## Context

LIR plays the role rustc's MIR plays (`../architecture/compiler_overview.md`), and is meant to carry
no target's realization (`../architecture/lir.md`, Purpose). On unwinding it carried LLVM's. A call
to a callee that could be unwound out of was always a terminator of its own, and its block began
with a landing that took what was unwinding as a pointer, said whether the frame caught it, and
stored it in a slot of the frame; a handler began and ended the C++ ABI's catch as statements; a
body left by unwinding resumed with that pointer as an operand. So the lowering built a landing for
every such call, whether or not anything there would run: in VeeR-EH1 139,132 of 244,643 calls
landed on a block that only resumed, and at `-O0` LLVM's instruction selection sends every block
ending in an invoke, and every landing, through its slow path, whose cost grows faster than the
block. Two readers of LIR rebuilt an ordinary call from an invoke to treat both alike.

rustc's MIR states on every call where an unwind from it goes -- on out of the frame, to a cleanup
block, nowhere because the callee cannot unwind -- and marks the blocks reached only by unwinding;
it holds no landing instruction and no exception value, and its code generation makes the landing
pads, the slot and the resume (`rustc_middle/src/mir/syntax.rs` `UnwindAction`,
`rustc_codegen_ssa/src/mir/block.rs` `do_call`, `landing_pad_for`). A call whose cleanup does
nothing is made to go on out of the frame, because otherwise "LLVM generates terrible code for
these" (`rustc_mir_transform/src/remove_noop_landing_pads.rs` `RemoveNoopLandingPads`), and code
generation checks the same per instance (`rustc_codegen_ssa/src/mir/mod.rs`,
`find_noop_landing_pads_for_instance`). Clang gives a call a landing only where the scope stack
needs one (`CodeGenFunction::getInvokeDest`).

## Decision

**D1. A call names a block to unwind to exactly where the body owes something on that way out.**
Where its callee cannot be unwound out of, or nothing would run, the call is an instruction and an
unwind from it leaves the body. Where something is owed, the call names the first block of it and so
ends its block. The call is one payload in both places. A value whose type's end does nothing is
owed no end on any way out, as rustc schedules no drop for a type that needs none; taking it copies
its bytes, which is also what separates it from the storage it was read from. Leaving the body ends
every byte of its frame, so a slot's use is owed only on a way out that stays in the body, where a
coroutine's split still asks what is in use across a suspension; this is where Lyra differs from
rustc, whose unwinding always leaves the frame and which states no slot's end on it outside a
coroutine. Every other obligation -- a value's end, a lease let go, a write in progress ended -- is
owed on every way out.

**D2. A block reached only by unwinding is marked as one.** It calls nothing that can be unwound out
of, since a cleanup leaves only by running to its end, and it goes on only to another such block, on
out of the body, or to where the body takes what is unwinding.

**D3. Unwinding is taken in one step.** Where a region of the body or the top of an execution
encloses it, a terminator asks the runtime what arrived: a departure of the design is handed over as
a value and control goes on in an ordinary block; anything else -- an error already reported, the
release of a suspended stack -- goes on unwinding as the same raise, to what the body owes outside
the region or out of it. Nothing in a body is ever still being handled, and declining a departure is
a raise of that value like any other. This is the one construct rustc's MIR has no counterpart for,
because SystemVerilog continues after a region that took a departure (LRM 9.6.2) and Rust has no
such construct, only its `catch_unwind` intrinsic, which code generation realizes the same way: a
catch, then the payload handed on.

**D4. A coroutine's body states every way it is left by unwinding.** Its top is a region, and what
it does not take goes on through a block that says so, because leaving it ends the execution.

**D5. How an unwind travels is the target's.** The execution backend makes a landing pad for each
block a call names, catching where the way out through it ends at a taking point and only passing
otherwise; one slot per frame for what arrived; at a taking point the C++ ABI's own begin of a
catch, the runtime's question and the end of the catch on both ways out, as clang realizes the C++
backend's `catch (...)` around the same question; and the coroutine's end before resuming out of
one. The C++ backend hands the structured region to C++ as before.

**D6. Two verification rules.** A block reached only by unwinding calls nothing that can be unwound
out of and is reached only by unwinding, as rustc's MIR validation holds its cleanup edges. A
function declared not to unwind lets no unwinding out of it; since a taking point carries on
whatever is not the design's, and LIR has no end that stops unwinding as rustc's `UnwindTerminate`
does, such a function calls nothing that can be unwound out of.

## Rejected

- **Every call a terminator, as rustc's MIR has it.** rustc's call names where it returns, so it
  ends its block whatever its unwind action; LIR's call answers into its destination and falls
  through, as LLVM's does, so a block ends only where control has two ways to go.
- **A landing built where nothing is owed.** It runs nothing, and costs the target a landing pad and
  a block it cannot select quickly.
- **The extent of a catch in LIR.** It is one target's ABI, which has two realizations in LLVM alone
  (landing pads, and Windows' funclets); handing the departure over once leaves no extent to state.
- **Giving each call that leaves the body its own block that only resumes.** That is the landing
  this record removes, in another place.

## Consequences

- VeeR-EH1 stated 105,502 invokes where it stated 244,643, each one entering a block that runs
  something.
- `lir.md` states where an unwind goes and the two verification rules; `nonlocal_control_flow.md`
  states what the execution backend makes of it.
- A body that owes nothing has no unwinding blocks at all.

## Cross-references

- `../architecture/lir.md`, `../architecture/nonlocal_control_flow.md`
- [ending-a-run](ending-a-run.md) D9 -- the first region or activation that takes a run-time error
  reports it.
- LRM 9.6.2, 9.7, 13.4, 20.2, 35.9
