# A run-time failure is not an activation's outcome

Date: 2026-09-01 Status: accepted

## Context

An execution the runtime drives settles one terminal outcome, which its consumer reads.
`activation.md` invariant 2 says that outcome is one of three things: the value it produced, a
failure, or a cancellation. Both backends were built to that shape.

On the backend whose bodies are generated code, the failure alternative caused a silent wrong
answer. A failure raised inside a called body was stored as that body's outcome; the caller only
asked whether the body had finished, so it carried on and read a result nothing had written. That
bug is easy to patch, and patching it would have kept the mistake underneath it.

The mistake is that the three alternatives were never checked against a simple question: **who reads
each one?**

SystemVerilog has no exceptions. Nothing in the language produces, observes, or recovers from a
failure -- even division by zero is defined to give `x` rather than to fail (LRM 11.4.3). So of the
three, only two have a reader:

| Alternative      | What SystemVerilog does with it                                      |
| ---------------- | -------------------------------------------------------------------- |
| the value        | reads it: a task's `output` reaches its actual at the return (13.5)  |
| the cancellation | lands it: `disable` resumes execution after the named region (9.6.2) |
| the failure      | **nothing -- there is no way to write it**                           |

If nothing can read it, it is not an outcome. It is Lyra saying something about itself, in a shape
that means "a value the program produced". So the machinery that stores outcomes stored it, which is
right for the two that have readers and is how the third one got lost.

## Decision

### D1. An outcome carries only what the source language can read

An activation settles either the value it produced or the departure a region lands. A failure is
neither, and is not represented as an outcome anywhere -- not in an IR, not in a backend.

### D2. Three kinds of failure, told apart by who has to act

Not by when we notice them. Sorting by when is what put two of them in the wrong place.

| Kind                                             | Who acts              | What it is                       |
| ------------------------------------------------ | --------------------- | -------------------------------- |
| Lyra cannot translate this construct             | user asks for support | a lowering stating its own limit |
| the design does something the LRM calls an error | user fixes the design | a severity report (LRM 20.10)    |
| Lyra contradicts itself                          | a developer           | an abort, not a language event   |

### D3. "Unsupported" is a lowering's answer, never a run-time event

A construct Lyra does not implement is the first kind whenever we find it, because the user's next
step is the same either way. If one reaches run time, the lowering did not say what it can handle
and left a fallback to say it later. `design-process.md` already names this shape: whatever a
fallback arm cannot name, some producer upstream knew and did not write down. Fix the producer.

### D4. A design's run-time error is a severity report, and we choose the severity

LRM 20.10 gives four levels and only `$fatal` ends the run; `$error` reports and the simulation
keeps going. The LRM often says "it shall be an error" without saying how severe -- a negative
`new[]` size is one (7.5.1) -- so the level is ours to choose per condition. **The model therefore
has to allow a report that does not end the run.**

That settles the mechanism on its own: something that need not end the run cannot be an unwind,
because there is nothing to unwind to.

### D5. Ending the run has a tail, and every way of ending it walks the same tail

`$fatal` causes an implicit call to `$finish` (20.10), and a `final` procedure runs when simulation
ends from an explicit **or implicit** `$finish` (9.2.3). So ending the run means: stop, run every
`final`, then stop. That tail is short and cannot consume time, because a `final` may only contain
what a function may.

A fatal Lyra raises for the design goes through that same path. If it unwound past the engine
instead, the same design would run its `final` blocks or not depending on who noticed the error
first, which is two behaviours for one event.

### D6. Lyra's own bug does not run the design's shutdown

An internal error means the compiler has contradicted itself. Running the design's `final` blocks
after that would run design code on a state we already know is wrong. It aborts and asks for a bug
report. It is not a language event and does not borrow the language's ending.

## Rejected

- **Keep the failure as a third outcome and hand it to the awaiting body.** This is what C++, C#,
  Python, JavaScript and Kotlin all do, and it is right in each of them because their awaiting code
  can catch. Generated code cannot, and no SystemVerilog construct could read the result if it
  could. Copying the shape brings along a reader that does not exist here.

- **Put the failure in the completion value, the way Rust and Go do.** This is where language design
  has been heading, and it does not fit for the same reason: an error carried as a value earns its
  place by letting the consumer branch on it, and here there is no way to write the branch. Its real
  lesson is used elsewhere -- the departure a region lands **is** carried as a value and taken as an
  explicit edge, which is what `disable-scope-invalidation.md` arrived at independently.

- **A check after every operation that can fail.** It puts failure handling into bodies that have
  nothing to do with it, which is why a check after every resumption was rejected for cancellation.

## What this reverses

`activation.md` invariant 2's third alternative. Its stated reason is that "the value and the
exception travel together as one outcome, read by the activation's consumer, never transported
through a side channel the scheduler or a backend invents."

The reason is good and its premise is not: it assumes the exception has a consumer. That holds in
the async runtimes it was drawn from, whose languages have exceptions. It does not hold for
SystemVerilog. And what the invariant was protecting against -- a backend inventing a side channel
-- is better served by the failure not being an outcome than by every backend carrying one it cannot
deliver.

## Consequences

- An outcome has two alternatives, and both have a reader in the source language.
- No await site carries machinery for failures, because how a failure travels is not the awaiting
  body's business.
- **Owed, and not built here:** a design's run-time failure has to reach the engine's stop, so that
  ending the run this way walks the same shutdown as `$fatal` and every `final` procedure executes.
  It does not yet; such a failure reaches the tool's top level without the engine seeing it, and a
  design carrying a `final` block can tell which way the run ended.
- Which severity a given LRM run-time error takes is left open per condition. This entry only fixes
  that both a terminating and a non-terminating report must be expressible.

## Cross-references

- `architecture/activation.md` -- the outcome model this reduces, and the invariant it reverses.
- `architecture/nonlocal_control_flow.md` -- the region, the raise and the cleanup, which are the
  language's own non-local control flow and do not change.
- `decisions/disable-scope-invalidation.md` -- cancellation answered as a value where an execution
  regains control, which is the shape kept here.
- IEEE 1800-2023: 11.4.3 (division by zero gives `x`), 13.5 (output values pass at the return),
  20.10 (severity tasks; `$fatal` implies `$finish`), 9.2.3 (`final` runs on an implicit `$finish`),
  7.5.1 (a negative `new[]` size "shall be an error", severity unstated).
