# Randomization

Two workstreams share this name, and separating them is the first thing this file does.

**Part one, random number generation**, is a seeded RNG carrying the stability model of LRM 18.14,
plus the system functions, the array method, and the statements that draw from it. It is the first
state a system function consumes that is not a pure function of its arguments, so it also settles
how that state reaches a call site. Nothing in it is blocked.

**Part two, constraint-based randomization**, is `rand` and `randc` members, constraint blocks, and
`randomize()`, solved by a constraint solver the runtime carries. It is recorded here rather than in
a file of its own because a reader asking "what does randomization do" should find one answer.

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
      created process's generator is split from that of the process that created it, whole and not
      through a 32-bit seed, so no two of however many children one parent has draw alike; and a
      static process's is split from the initialization RNG of the unit instance whose declaration
      it belongs to, so a whole hierarchy subtree is determined by the seed of its root process. A
      static process is every elaborated `initial`, `always`, `always_comb`, `always_latch`, and
      `always_ff` procedure and every elaborated continuous assignment (Annex P), so the continuous
      assignments of a unit instance take initialization seeds alongside its procedures. A port
      connection is a continuous assignment the instantiation creates (LRM 4.9.6), so it is seeded
      from the instantiating instance, and whether an output is connected moves no child's draws.
      `$urandom` returns 32 unsigned bits and `$urandom_range` bounds the result, reversing its two
      arguments when they arrive in the other order (LRM 18.13.1 -- 18.13.2). Both are thread
      stable: the values a process observes do not depend on the order in which processes execute.

- [x] A randomization call made from a static initializer. LRM 18.14.1 requires the initialization
      RNG to serve static initializers as well as static processes, and a variable declaration
      assignment runs before any procedure starts (LRM 10.5, and LRM 26.2 for a package), so such a
      call has no process to draw from. Every position reaches one now: a module-level variable, a
      static declaration inside a procedural block, a package variable, a `static` class property,
      and a `static` local of a subroutine. Two instances of one module draw alike, and a package
      draws independently of every other, because each container's generator starts from the same
      default seed and none of them advances another's. What runs with no thread creating it -- a
      deferred assertion's action, the update a nonblocking assignment's event control carries --
      draws from the next generator of its instance's initialization RNG, so reaching it moves no
      procedure's draws.

- [x] Manual seeding and state save / restore: `srandom`, `get_randstate`, and `set_randstate` (LRM
      18.13.3 -- 18.13.5), reached on a process through its process handle (LRM 9.7), whichever
      process makes the call, so a thread seeded or restored through another's handle draws from
      what was set. The RNG state is an opaque string whose length and content the standard leaves
      to the implementation; only a state this implementation produced can be restored, and setting
      any other ends the run with an error, since the standard leaves it undefined and the string is
      one the design computed. `process::self()` called where no process runs -- in a declaration
      assignment, which runs before any procedure starts -- answers a null handle. The same three
      methods reach an object's own generator, the one its `randomize()` draws from (LRM 18.15).

- [x] The methods every class has, on the classes of `std` and on classes extending them:
      `randomize()` with and without an inline constraint, the two hooks, the generator's three
      methods and the mode methods on a `semaphore`, a `mailbox` or a `weak_reference`, and a
      subclass's hook reaching its base's through `super` (LRM 8.30, 15.1, 18.6). Through an
      interface class handle, `rand_mode()` and `constraint_mode()` are refused by the front end, as
      LRM 8.26.9 requires.

- [x] The same methods on a `process` (LRM 9.7, 18.6 -- 18.9): it declares no random variable and no
      constraint, so `randomize()` answers 1, and with an inline constraint answers whether it holds
      over the state it reads, `status()` included, leaving that state as it was; its hooks and mode
      methods change nothing, and its generator is the one the process draws from. A `rand` handle
      to a process is randomized with the object holding it. Randomizing a process draws nothing
      from its generator, a choice the standard leaves to the tool.

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

- [x] Random variables: the `rand` and `randc` modifiers on class properties, static ones included
      (LRM 18.4). A `rand` variable is uniformly distributed over the values its constraints allow;
      a `randc` variable runs through a random permutation of its range, every value once before any
      repeats, skipping a value its constraints rule out at one call and taking it at a later one,
      and beginning a new permutation when its constraints change. Integral and enumerated
      variables, an enumerated one taking only the values its enumeration names and a `randc` one
      running through them, fixed-size and dynamic arrays and queues of them at any depth, an
      unpacked structure whose members are declared random, and an object handle, whose object's
      variables and constraints join the call, an array of handles each of its objects. A `randc`
      variable wider than 64 bits is refused where it is declared.

- [ ] Random variables of a real type, an associative array declared random, and an array of
      structures declared random: refused when a call reaches one, with "randomizing a random
      variable of this type is not yet supported". A replication in a constraint, and a member of a
      tagged union in one, are refused where the class is compiled.

- [x] Constraint blocks (LRM 18.5): inheritance and overriding by name, pure and external
      prototypes, static blocks whose mode is the class's, expressions over every integral operator,
      members of packed structures and unions, set membership with ranges and the elements of
      unpacked arrays, weighted distributions (`:=`, `:/`, a default weight), implication and
      `if`-`else`, `foreach` over fixed-size and dynamic arrays and queues, array sizes chosen
      before their elements and named in `solve ... before` and `disable soft`, the reduction
      methods with and without a `with` clause, `unique` over variables and array elements, global
      constraints through random handles, `solve ... before`, soft constraints with the later
      winning a conflict and `disable soft`, and functions, whose arguments' variables are chosen
      first and whose answer is read as a value.

- [x] The `randomize()` method and the object RNG (LRM 18.6, 18.15). Each object owns a generator
      split from whatever made it, which its own `randomize()` draws from; a process's is the one
      its thread draws from. `pre_randomize` and `post_randomize` run around each call, on the
      object and on every object a random handle reaches; a call that finds no answer returns 0,
      runs no `post_randomize` and leaves every variable as it was. A call the source writes naming
      a hook of a class that declares none, by `super`, by its name, through `this` or through a
      handle, runs the one the nearest class above declares, or nothing where none does (LRM
      18.6.2). `randomize()` of a class extending a class of another unit runs the hooks that class
      declares, as it does for a class of its own unit.

- [x] `randomize()`, `srandom`, `get_randstate`, `set_randstate` and the hooks called through an
      interface class handle (LRM 8.26.9) act on the object the handle refers to, and a hook called
      that way runs the one the object's own class runs, since an interface class's hooks are
      virtual.

- [x] Inline constraints added at the call site, a name in them -- a method's included -- looked up
      in the randomized object's class first and then where the call is written, a call listing the
      variables it chooses, an array among them standing for its elements and its size, every handle
      it does not list held, and the checker form `randomize(null)` (LRM 18.7, 18.11).

- [ ] A call listing a random handle: refused where it is compiled, since the object it reaches
      would choose its variables by its own modes while the call's listing governs the rest.

- [x] Scope randomization, `std::randomize()` with and without inline constraints (LRM 18.12),
      drawing from the calling process's generator.

- [x] Mode control: `rand_mode` on a variable, on one element of an array, which keeps its mode when
      the array is resized, on every member of a structure, or on every variable of an object, and
      its query (LRM 18.8), and `constraint_mode` on a block, a static block, or every block of an
      object, and its query (LRM 18.9).

- [x] The constraint solver. An object's values depend on its own generator alone, whatever other
      calls drew before. It draws exactly uniformly where the constraints fit a decision diagram of
      bounded size, reuses that diagram when a later call states the same constraints, and reports
      failure only where no answer exists.

- [ ] Nothing bounds the memory a call may take while it counts a constraint's solutions. The bound
      on the diagram's size is approximate, and running out of memory stops the simulation with an
      error only where the allocation itself fails; a system that hands out memory it does not have
      may instead stop the program from outside.

- [x] A run says, once, that a call drew past the diagram's bound, and how to raise the bound for
      that run.

- [ ] Constraints past the diagram's bound are drawn legally but not uniformly: where one side of a
      bit holds far more solutions than the other, the two are taken equally often. How large the
      bound should be by default waits on measuring how often a diagram is reused in real
      testbenches, and a summary of how often each call drew past it is not yet written.

## Open Questions

- LRM 20.14 makes the algorithm `$random` generates by part of the standard, with or without its
  optional seed argument, and states no start for the stream a call with no seed advances; LRM 18.14
  does not list `$random` among what random stability applies to. Such a call advances one seed the
  run keeps, starting at zero, by the algorithm of Annex N. A seed per process is what was not
  chosen: started alike, every process would draw the same values, and started from the process's
  own generator, the values would be the implementation's and no longer the standard's.

## Cross-References

- `../architecture/elaboration_lifecycle.md` -- the phase in which static processes are created is
  the phase in which they are seeded.
- `../architecture/activation.md` -- the process is the runtime identity a process RNG belongs to,
  and its parent-child lineage is what hierarchical seeding follows.
- `../architecture/compilation_unit_model.md` -- the module, interface, and package boundary the
  initialization RNG is per-instance of.
- `../architecture/object_model.md` -- the object identity part two's per-object RNG belongs to.
