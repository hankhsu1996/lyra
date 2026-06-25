# Simulation RNG

Tracks the seeded simulation random number generator and the language features built directly on it.
None of these exist yet: the RNG is the shared foundation, and each consumer below is blocked until
it lands. Constraint-based randomization is a separate, larger workstream (see Out of Scope).

Done when:

- A seeded, reproducible simulation RNG exists, with the per-thread / per-object stability and
  hierarchical seeding the LRM requires (LRM 18.14), reachable from the call sites that consume it.
- The system functions and the array method below reproduce their LRM-defined behavior on it.

## Sub-Steps

- [ ] RNG core. A seeded, reproducible random number generator with the random-stability model of
      LRM 18.14: an initialization RNG per module / instance, an independent per-thread and
      per-object RNG, and hierarchical seeding (a new thread or object draws its seed from its
      parent). Manual seeding and state save / restore through `srandom` / `get_randstate` /
      `set_randstate` (LRM 18.13.3 -- 18.13.5). This is the first state an array method or system
      function consumes that is not a pure function of its arguments, so it also establishes how
      that state threads into those call sites.

- [ ] The random number system functions: `$urandom` and `$urandom_range` (LRM 18.13.1 -- 18.13.2),
      plus the legacy signed `$random`. Each draws from the RNG core and observes its thread /
      object stability.

- [ ] `shuffle()` (LRM 7.12.2) on every unpacked container (dynamic array, queue, fixed unpacked).
      The array-manipulation method family is otherwise complete (see `aggregate.md`); `shuffle` is
      the one member held back here because it permutes its receiver using the RNG rather than being
      a pure function of it.

## Out of Scope

- Constraint-based randomization: `rand` / `randc` class members, `constraint` blocks, the
  `randomize()` method, and `std::randomize()` (LRM 18, excluding the random-number functions
  above). This needs a constraint solver and is gated on the object model; it is a separate
  workstream.
- The `randcase` (LRM 18.16) and `randsequence` statements. They consume the RNG but are
  procedural-control / grammar features, tracked with control flow when picked up.
