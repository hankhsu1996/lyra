# The execution backend leaves coroutine splitting to LLVM

Status: Accepted, 2026-11-22.

## Requirement

A SystemVerilog process or task suspends -- `#5`, `@(posedge clk)`, `wait (cond)` -- and resumes
where it stopped. The execution backend stays a mechanical translation of what LIR states
([`backend_contract.md`](../architecture/backend_contract.md)), and realizes a construct as clang
realizes the C++ backend's rendering of it, which for a process is a C++20 coroutine that clang
splits with LLVM's coroutine passes.

## Choice

**LIR states where a body suspends and when each slot is in use; LLVM's coroutine passes derive the
resumable form.** A coroutine body is emitted as an ordinary function carrying LLVM's coroutine
intrinsics, a suspension at each suspend edge and a final suspension at completion, and slot
lifetimes as clang states them for a coroutine. The frame's layout, the resume state machine, the
resume and destroy entries, and which values survive a suspension are the passes' to decide.

## Alternatives turned down

- **A coroutine state machine written by the emitter** -- a resume-state parameter, an entry switch,
  hand-written spills of what crosses a suspension. It fails a mechanical backend, turning it into a
  compiler pass that re-implements, less correctly, what the passes derive, with its failures
  concentrated in the values live across a suspension, where they are hardest to see.
- **Lyra owning the representation of suspended execution and describing it**, as rustc's MIR
  transform splits coroutines and as the mainstream managed runtimes lay out frames a collector
  reads. It fails one realization shared with clang's for the C++ backend. The failure rests on
  LLVM's passes splitting a body well given where it suspends and when each slot lives; a need they
  cannot meet -- a frame a collector must read in place, a layout a debugger must know -- reopens
  this.

## Consequences across layers

- A frontend cannot enumerate what the passes spilled, so every managed value that can survive a
  safepoint lives in storage Lyra describes, never in the coroutine frame
  ([`lifetime.md`](../architecture/lifetime.md), [object-model](object-model.md)). That premise is
  this choice's, and moving off it moves the collector's root discovery with it.
- LIR states coroutine-ness by a body's result type and suspension as a control edge, and owns
  nothing of the frame ([`lir.md`](../architecture/lir.md)).
- The engine resumes only a coroutine the runtime owns, which drives the generated body through its
  handle, so no generated frame embeds a runtime C++ type
  ([`activation.md`](../architecture/activation.md)).
