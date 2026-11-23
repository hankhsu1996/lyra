# A class handle is a managed reference, reclaimed by precise tracing

Status: Accepted, 2026-06-25.

## Requirement

A SystemVerilog class object lives while any handle reaches it and is never freed by the program
(LRM 8.3, 8.29), and graphs of such objects routinely form cycles. Two objects naming each other,
`a.next = b; b.next = a;`, are ordinary LRM 8 code, and setting both handles to null leaves two
objects no handle reaches. Memory stays bounded under a testbench that allocates without end, such
as a `forever` loop making a transaction per cycle. The standard states this as Java's model,
garbage collection "as in Java" (LRM 8.7).

## Choice

- **A class handle is the managed kind of reference**, distinct from the owning, borrowed and shared
  kinds. Null is a legal value, identity is comparable, copies are shallow, and the object is
  reachable while any handle reaches it.
- **Its lifetime is reachability, realized by a precise tracing collector at a runtime safepoint.**
  Reclaiming an unreachable cycle is part of the model, not a gap.
- **Every language-visible value that can survive a safepoint lives in storage Lyra describes and
  the collector can trace** -- an activation frame, a traceable closure -- reached through records
  the runtime holds, never in a backend's opaque execution state. That is what makes tracing precise
  where a backend's coroutine machinery lays out a suspended frame
  ([coroutine-splitting-is-llvms](coroutine-splitting-is-llvms.md)).

## Alternatives turned down

- **Riding the shared reference, which lowers to a counted pointer.** It fails reclaiming a cycle.
  The failure rests on the shared reference being committed to an acyclic graph of ownership by
  construction ([`compiler_generated_storage.md`](../architecture/compiler_generated_storage.md)),
  so putting cyclic object graphs under it breaks that contract.
- **A separate reference type with its own kind and nullability axes.** It fails keeping the
  observable cell's reference distinct from a plain pointer, which it would fold into one
  classifier. Nullability stays a fact of a value, a null literal, rather than an axis of a type.
- **A simulation-lifetime arena, reclaiming nothing until the run ends.** It fails bounded memory.
- **Pure reference counting.** It fails reclaiming a cycle, so it leaks object graphs a legal
  program creates.
- **Reference counting plus a cycle collector.** It is sufficient, and fails run time, since it pays
  every cost tracing does -- roots, traversal, safepoints, references in suspended state -- plus
  count traffic on every handle copy, in two lifetime systems. The failure rests on counting not
  materially helping the hot path; a profile showing it does reopens this.

## Consequences across layers

- [`object_model.md`](../architecture/object_model.md) states the managed reference as contract, and
  MIR never states a counted realization, or the absence of cyclic reclamation, as its meaning.
- [`lifetime.md`](../architecture/lifetime.md) owns the storage discipline, the roots and the
  safepoint contract this requires of every layer that holds a value across a suspension.
- SystemVerilog's classes take Java's model throughout
  ([the-class-model-is-javas](the-class-model-is-javas.md)).
