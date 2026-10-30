# A generator is split from the one that made it

Date: 2026-10-10 Status: accepted

## Context

LRM 18.13 and 18.14 say what a `$urandom` draw comes from and leave the algorithm to the
implementation. What they do fix:

- Each thread has an independent RNG, and so does each class object (18.14.1).
- A thread's RNG is seeded with the next random value of the thread that created it, a static
  process's with the next value of its container's initialization RNG, an object's with the next
  value of the thread that created it (18.14.1). "Each thread is seeded with a unique value,
  determined solely by its parent" (18.14.2).
- A seed the design states is an `int`, and the same seed gives the same sequence (18.13.1,
  18.13.3).
- A generator's state can be read out as a string and put back (18.13.4, 18.13.5), of a length and
  format the implementation chooses.

So the generators of a run are a tree. There are as many of them as the design has threads and
objects, which for a testbench is millions; each is made when its owner is, by drawing from its
parent's; and they are meant to behave as though unrelated.

The runtime first used the standard library's Mersenne Twister, started from a 32-bit value its
parent drew. Two things were wrong with that. Its state is 624 words, held by every process whether
or not it ever drew, which was most of what a waiting process cost. And 32 bits name about four
thousand million generators, so among a hundred thousand children of one parent two are expected to
be handed the same one and to draw identical sequences.

## Decision

**A generator is the L64X128 member of the LXM family (Steele and Vigna, "LXM: better splittable
pseudorandom number generators", 2021), and a new one is split from its parent.**

Its state is four 64-bit words: the state of a linear congruential generator, that generator's
addend, and the two words of a xoroshiro128 generator. A draw mixes the sum of the congruential
state and one xoroshiro word, then steps both. `$urandom` answers with the high 32 bits of a draw.

A generator is made in one of two ways and no other.

- **From a seed the design states.** The 32 bits are spread over the whole state by mixing functions
  unlike the generator's own step, so neighbouring seeds name unrelated generators.
- **Split from its parent.** Four draws of the parent become the child's addend, congruential state
  and two xoroshiro words. Nothing 32 bits wide stands between a parent and a child.

The addend is what makes this a splittable generator. Two generators with different addends walk
different cycles, so two children are not two stretches of one sequence that might overlap; and
where two do share an addend they start at unrelated points of a cycle of 2^192.

The initialization RNG of a container is the same generator, started from the default seed, and
hands out its static processes' and static initializers' generators by splitting.

The final procedures are function calls from one thread (LRM 9.2.3), which is neither a static
process -- Annex P lists every kind and leaves it out -- nor one any thread created, so 18.14.1 does
not say where its generator comes from. It is split from the initialization RNG of the design's root
once the simulation has ended. That split is the last draw the root's generator is asked for, so it
moves no generator made before it; which values the finals draw is this choice's and no stronger
promise than that.

## Why this one

The question the standard poses -- generators created dynamically, each from its creator, in
unbounded number -- is the one a splittable generator exists for, and the survey was of who answers
that question rather than of which generator is fastest.

- **Java** (`java.util.random`, the package documentation) describes exactly this use: a generator
  "is then considered to 'belong' to the initial thread", and "whenever any thread needs to fork a
  new thread, it first uses the `split()` method of its own generator". It recommends
  `L64X128MixRandom` where nothing special is needed and for applications that create many threads
  dynamically, and says of the family that testing found it "far superior in quality to the SplitMix
  algorithm" it replaced. The constants and the split used here are that generator's.
- **NumPy** (the page on parallel random number generation) spawns children from a 128-bit pool and
  gives the arithmetic: with n streams the chance of two coinciding is about n^2 / 2^128. It names
  seeding parallel streams from small neighbouring integers as unsafe.
- **The xoshiro authors** (prng.di.unimi.it) call their 128-bit generators "suitable only for
  low-scale parallel applications" and their 256-bit ones "large enough for any parallel
  application"; for parallel streams they offer jump functions, which divide one sequence into a
  number of parts fixed ahead of time, and they require that a generator be initialized by one
  "radically different in nature".
- **Verilator** gives each process a xoroshiro128\*\* generator of two words and makes a child's
  state from two outputs of its parent's.
- **Go** keeps a ChaCha8 generator of 300 bytes per core, chosen so that its output cannot be
  predicted, which nothing here asks for.

## Rejected alternatives

- **The Mersenne Twister.** 2.5 kilobytes of state in every thread and object, five on a platform
  whose fast 32-bit integer is 64 bits wide.
- **A large generator made at the first draw.** A thread or an object that draws once pays for the
  whole of it from then on, so the cost moves and does not go; and every draw first asks whether
  there is a generator yet.
- **xoroshiro128\*\*, a child's state taken from its parent's outputs.** Half the state, and its
  authors' own page rules it out on both counts: the size for anything but small numbers of streams,
  and seeding a generator from another of the same kind.
- **xoshiro256\*\* with jump functions.** A jump divides one sequence into parts counted in advance,
  and a tree of threads forked on demand, any of which may be reseeded by the design, has no such
  count.
- **A seed of 32 bits between parent and child.** It is what the design may state, and it is not
  what the standard says a child is seeded with: a "random value" of the parent's, of whatever width
  the parent has.

## Consequences

- A process holds 32 bytes of generator where it held 5,000.
- **Every class object will hold 32 bytes of generator once objects have one.** The standard seeds
  an object where it is created, in creation order, so the state cannot wait for the first
  `randomize()`; and objects are where generators are most numerous, which is where a narrower one
  would repeat.
- The values `$urandom` and `$urandom_range` return for a given seed are this generator's, and
  changing the generator changes every one of them. A seed a user has recorded names a run only for
  as long as this stands.
- `$random` and the `$dist_` functions are not this generator's: LRM 20.14 makes their algorithm
  part of the standard. Annex N's `long` is read as 32 bits wide, because `$random`'s rescaling
  branch stays within 32 signed bits only under that reading.
- A `$random` call with no seed advances one seed the run keeps, starting at zero, by the algorithm
  of Annex N. 20.14 states no start for that stream, and 18.14 does not list `$random` among what
  random stability covers. A seed per process was not chosen: started alike, every process would
  draw the same values, and started from the process's own generator, the values would be this
  implementation's and no longer the standard's.
- A port connection is a continuous assignment the instantiation creates (LRM 4.9.6), so its
  generator is split from the instantiating instance's initialization RNG, and whether an output is
  connected moves no draw of the child's.
- The state `get_randstate` answers is this generator's four words as a string, and only a string
  this implementation produced can be put back. Setting any other ends the run with an error, since
  18.13.5 leaves it undefined and the string is one the design computed.

## Cross-references

- LRM 18.13.1 -- 18.13.5, 18.14.1 -- 18.14.3
- `docs/decisions/static-initializer-draws-from-its-container.md` (which generator a draw reaches)
