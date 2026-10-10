# What Starts at Time Zero Starts in Turn

## Date

2026-10-10

## Status

Accepted

## Why this decision matters

A design's processes all become runnable at time zero, and the standard does not say which runs
first: events of one region are processed in any order (LRM 4.7), and "there shall be no implied
order of execution between initial and always procedures" (LRM 9.2). A simulator therefore picks an
order, and designs come to lean on whatever order the simulators they were written against picked.

Lyra's order used to be the order processes were registered in, parent scope before child, with a
net's driver given its value before any process ran. Under it a register whose only reset is an
asynchronous one tied to a constant was never reset: nothing ever changed at its reset input. Three
of the external designs (the VeeR cores, whose JTAG registers are reset that way) ran forever with
the core unknown. This record fixes the order, so a later change to how a process is started lands
on a stated rule instead of on registration order.

## What the standard fixes

- A continuous assignment, and the implicit one a port connection is, "is also evaluated at time
  zero in order to propagate constant values" (LRM 4.9.1, 4.9.6).
- A variable's declaration initializer is set before any initial or always procedure is started (LRM
  6.8, 10.5), so it is a change to no one.
- An `always_comb`, and with it an `always_latch`, is triggered at time zero "after all initial and
  always procedures have been started" (LRM 9.2.2.2, 9.2.2.3).
- "The initial procedures need not be scheduled and executed before the always procedures" (LRM
  9.2).

## The decision

1. **A continuous driver has produced nothing before time zero.** A net holds what an undriven one
   does and a variable its declared initial value; the driver's first value arrives as a process of
   the time-zero slot.

2. **What starts at time zero starts in turn**, each turn once the one before it has run to its
   first waits:
   1. every `always` and `always_ff` procedure, and the clocked processes Lyra makes for a sampled
      value and an assertion;
   2. every continuous assignment and port connection;
   3. every `initial` procedure;
   4. every `always_comb` and `always_latch`.

   The first before the second makes a driver's first value a change a waiting procedure sees, as an
   edge where it is one. The second before the third lets an `initial` read what a constant drives,
   an input port tied to a parameter being the common case. The fourth is the standard's.

3. **A process is registered as the kind it is, and only bring-up knows the order.** The lowering
   hands each body to the registration of its kind; nothing in a unit's code states a position. The
   engine's loop is untouched: a turn is released into the Active region, and the next is placed
   behind the point where Active empties, which is where the Inactive region begins (LRM 4.4.2.3).

## Consequences

- An `always @(negedge r)` sees the first value given to `r` by a net's declaration assignment, a
  continuous assignment, or a port connection, a tied constant included.
- A net sampled at time zero holds the default of its type, as LRM 16.5.1 gives a signal that
  declares no value.
- An `initial` that itself waits for a driver's first value does not see it, and an `always` that
  reads a driven name before its first wait reads the declared value. LRM 9.2 says a program may
  rely on neither.
- An `always_comb` sees what an `initial` assigned at time zero in its first evaluation.

## Alternatives considered

**Every procedure first, then every driver.** One rule, taken from the standard's sentence about
`always_comb`. Rejected: an `initial` reading an input tied to a constant then reads the declared
value, which broke two cases of our own corpus written the natural way.

**A driver's value in place before any process runs.** What stood before. Rejected: it is the
property that hides the edge, above.

**An order kept by the engine.** A turn number each activation carries, read by the slot loop.
Rejected: the engine branches on queue and region and never on what a coroutine came from; the order
is a fact of bring-up, and the engine's existing placements express it.

## Background

The committee discussed this order while drafting IEEE 1800-2005 and left it out of the text; the
discussion describes always-before-initial as what simulators did
(<https://accellera.org/images/eda/sv-bc/2788.html>).
