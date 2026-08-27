# Randomization

Two workstreams share this name, and separating them is the first thing this file does.

**Part one, random number generation**, is a seeded RNG carrying the stability model of LRM 18.14,
plus the system functions, the array method, and the statements that draw from it. It is the first
state a system function consumes that is not a pure function of its arguments, so it also settles
how that state reaches a call site. Only one item is blocked, and on the front end rather than on
anything here.

**Part two, constraint-based randomization**, is `rand` and `randc` members, constraint blocks, and
`randomize()`. It needs a constraint solver and it needs the object model finished, so its items
stay unchecked until that lands. It is recorded here rather than in a file of its own because a
reader asking "what does randomization do" should find one answer.

Done when:

- A seeded, reproducible RNG exists with LRM 18.14's stability model -- per unit instance, per
  process, per object, hierarchically seeded -- reachable from every call site that consumes it.
- Every construct below reproduces its LRM-defined behavior.

## Part One -- Random Number Generation

- [x] The RNG core, with `$urandom` and `$urandom_range` as its first consumers. LRM 18.14.1
      establishes two seed domains that must exist before anything can draw. An **initialization
      RNG** belongs to each module, interface, and program instance and to each package; it seeds
      static processes and static initializers and the language reaches it no other way -- it cannot
      be manually seeded and its state cannot be read. An independent **process RNG** serves every
      randomization system call made from that process. Seeding is hierarchical: a dynamically
      created process draws its seed from the process that created it, and a static process draws
      from the initialization RNG of the unit instance whose declaration it belongs to, so a whole
      hierarchy subtree is determined by the seed of its root process. A static process is every
      elaborated `initial`, `always`, `always_comb`, `always_latch`, and `always_ff` procedure and
      every elaborated continuous assignment (Annex P), so the continuous assignments of a unit
      instance take initialization seeds alongside its procedures. `$urandom` returns 32 unsigned
      bits and `$urandom_range` bounds the result, reversing its two arguments when they arrive in
      the other order (LRM 18.13.1 -- 18.13.2). Both are thread stable: the values a process
      observes do not depend on the order in which processes execute.

- [ ] Manual seeding and state save / restore: `srandom`, `get_randstate`, and `set_randstate` (LRM
      18.13.3 -- 18.13.5), reached on a process through its process handle (LRM 9.7). The RNG state
      is an opaque string whose length and content the standard leaves to the implementation, and
      only a state this implementation produced can be restored. The object-side form of the same
      three methods lands with part two, because until `randomize()` exists nothing draws from an
      object's RNG for a saved state to be about.

- [x] The probabilistic distribution functions -- `$dist_uniform`, `$dist_normal`,
      `$dist_exponential`, `$dist_poisson`, `$dist_chi_square`, `$dist_t`, `$dist_erlang` (LRM
      20.14.2). These sit outside the stability model above: each carries its seed as an `inout`
      argument rather than drawing from a process, and returns the same value whenever it is given
      the same seed. The standard fixes their generation algorithm rather than leaving it to the
      implementation -- Annex N states it as C source and is normative -- so a design that seeds one
      gets the sequence every other simulator produces, and the corpus asserts those values outright
      instead of asserting an invariance about them. Annex N's `long` is 32 bits wide, and what
      forces that reading is the branch `$random` takes, whose rescaling lands within 32 signed bits
      only under it. A mean, degree of freedom, or stage count that is not positive is the design's
      own failure and ends the simulation rather than answering with a number the distribution
      cannot produce.

- [ ] `$random`, whose seed the caller owns (LRM 20.14.1). The unseeded form works and draws from
      the calling process, which the standard leaves open. The seeded form is refused: it is Annex
      N's uniform draw over the whole signed range (Table N.1), but the front end reads its argument
      as a value rather than as the variable the draw advances, so the seed to store back into does
      not reach lowering. `$dist_uniform(seed, -2147483648, 2147483647)` is the same draw and does
      carry its seed, which is what the refusal points a design at.

- [ ] `shuffle()` on every unpacked container -- dynamic array, queue, and fixed unpacked array (LRM
      7.12.2). The array manipulation family is otherwise complete; this is the one member held
      back, because it permutes its receiver using the process's RNG instead of being a pure
      function of it. It is thread stable for the same reason `$urandom` is.

- [ ] `randcase` (LRM 18.16): a case statement that selects a branch at random, each item's weight
      an arbitrary expression evaluated at most once. A branch's probability is its weight over the
      sum of all weights, a zero weight is never selected, and an all-zero statement selects nothing
      and may warn. The weights are compared as unsigned values at the precision the sum requires,
      and the draw goes through `$urandom_range`, which is what makes the statement thread stable.

- [ ] `randsequence` (LRM 18.17): grammar-driven stimulus generation. Productions and production
      lists, random weights on alternatives, `if`-`else` and `case` production statements, `repeat`,
      `rand join` interleaving, `break` and `return` aborting a production, and value passing
      between productions. Thread stable like the rest.

## Part Two -- Constraint-Based Randomization

Blocked on the object model: every item below is a property of a class instance, solved against that
instance's own state, so none of it is reachable until class members, inheritance, and managed
object lifetime are in place.

- [ ] Random variables: the `rand` and `randc` modifiers on class properties (LRM 18.4). A `rand`
      variable is uniformly distributed over its range; a `randc` variable cycles through a random
      permutation of its range without repeating, recomputing the permutation when its constraints
      change. Both apply to integral and real variables, to arrays and their elements, and to object
      handles, each with its own rule for what "randomize this" means.

- [ ] Constraint blocks (LRM 18.5), which is the bulk of the surface: the block itself and its
      inheritance rules, external and pure constraint prototypes, set membership and weighted
      distributions, implication and `if`-`else`, iterative constraints over arrays, global
      constraints reaching into contained objects, variable ordering with `solve ... before`, static
      constraint blocks, function calls inside constraints, constraint guards, and soft constraints
      with their priority rules.

- [ ] The `randomize()` method and the object RNG (LRM 18.15). Each object owns an RNG used
      exclusively by its own `randomize()`, seeded from the process that created the object -- or,
      for an object built by a static declaration initializer, from the initialization RNG of the
      unit instance the declaration sits in. Object stability means a call on one instance is
      independent of calls on every other instance and of every other randomization function.
      `pre_randomize` and `post_randomize` run around each call (LRM 18.6.2).

- [ ] Inline constraints added at the call site, and the checker form: `randomize(null)` assigns
      nothing and returns only whether the current values satisfy every constraint (LRM 18.11.1).

- [ ] Scope randomization, `std::randomize()` (LRM 18.12), which randomizes variables of the current
      scope with optional inline constraints and needs no class. It behaves as a class `randomize()`
      does, so it needs the solver, but it draws from the calling process's RNG rather than from an
      object's.

- [ ] Mode control: `rand_mode` to disable a random variable so it behaves as an ordinary one (LRM
      18.8), and `constraint_mode` to turn a named constraint block off and on (LRM 18.9).

- [ ] The constraint solver itself. It must select uniformly over legal value combinations, honor
      the declared variable ordering, solve `randc` variables ahead of the rest, and report failure
      rather than guess. It runs while the simulation runs, because a constraint may reference state
      whose value is only known then, so it belongs to what an emitted program carries with it
      rather than to the compiler.

## Open Questions

- A `final` procedure is not among the static processes Annex P lists, so nothing states where its
  RNG is seeded from, and a `$urandom` call inside one is legal SystemVerilog. Part one has to pick
  a seed source for it and record the choice.

- LRM 18.14 lists what random stability applies to and does not list `$random` among them, while LRM
  20.14 gives `$random` an optional seed argument rather than a required one, so what an
  argumentless `$random` draws from is not stated. It draws from the calling process, the same
  generator `$urandom` reads, which makes an unseeded draw a signed reading of the same bits and
  gives it that call's thread locality without introducing a design-wide generator.

## Cross-References

- `../architecture/elaboration_lifecycle.md` -- the phase in which static processes are created is
  the phase in which they are seeded.
- `../architecture/activation.md` -- the process is the runtime identity a process RNG belongs to,
  and its parent-child lineage is what hierarchical seeding follows.
- `../architecture/compilation_unit_model.md` -- the module, interface, and package boundary the
  initialization RNG is per-instance of.
- `../architecture/object_model.md` -- the object identity part two's per-object RNG belongs to.
