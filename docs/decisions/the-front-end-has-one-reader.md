# The Front End Has One Reader

## Date

2026-09-24

## Status

Accepted. Moves lowering a unit's bodies to HIR into that unit's own pipeline, which
[a-build-is-told-how-wide-to-run](a-build-is-told-how-wide-to-run.md) runs as wide as the build was
told, and records what reading the front end from several units at once requires, and why until then
it is read one unit at a time.

## Why this decision matters

Every unit's bodies were lowered to HIR before any unit went further, so the barrier held the
elaborated AST and every unit's HIR at once, and that moment was the compile's live memory peak: a
heap profile of a generated design of 600 distinct units peaked there and only fell after it. On the
largest design measured, the front end is 5.63 GiB and all HIR adds about 5 GiB on top of it.

The obvious next step is to lower each unit's bodies on its own worker, all reading one elaborated
AST the way slang's own analysis reads it from a thread pool. That was built, and it does not hold.
What follows is why, so the next attempt starts from here rather than from the thread pool.

## The model that shaped the decision

- **The front end elaborates what a reader first touches.** Its own full diagnostic pass visits most
  of the design, but not all of it: an instance it found to duplicate another is never visited, and
  some parts are computed only when asked for even on a visited symbol -- the default of an input
  port nothing connected, for one.
- **A unit reads past its own body.** A hierarchical name walks the elaborated hierarchy to whatever
  instance it lands on and reads that instance -- its port connections, its body's members -- which
  is where the first read elaborates it. So what the units read is not divided by unit and cannot be
  listed before they run.
- **Several readers at once therefore race on the front end's allocator.** With slang's checks on
  and the AST frozen after the diagnostic pass -- and every unit's own body elaborated before the
  freeze -- two corpus designs still stopped at an allocation: an input port's default, and a route
  computing the specialization of an instance no unit had read.
- **Elaborating everything first is what turning instance caching off does**, and it triples the
  front end of the largest design measured: 5.63 GiB to 16.88 GiB, carried unchanged into every
  later stage.
- **Releasing the AST early bought nothing.** The resident size of a build did not fall at the point
  the AST was released, so holding it until the last unit has been lowered costs no memory the build
  was not already holding.

## Decisions

### D1. A unit's HIR exists only while that unit is in flight

Lowering a unit's bodies is the first step of that unit's own pipeline. The barrier waits for every
unit's declarations and not for any unit's HIR, so what is resident across the wide stage is the
AST, every unit's declarations, and the units in flight. The AST is held until the last unit has
been lowered, and no longer.

### D2. The front end's lazy computation is what has to be synchronized, and until it is, the units read it in turn

What the front end answers a unit must not depend on which other units read it first or at once.
Computing everything before the units start meets that and fails the memory bound; readers taking
turns meets it and serializes independent work, which the north star rules out as an architectural
choice. The answer that meets both is the one rustc's query system gives a lazily computed
whole-program form read by many workers: the computation synchronizes itself -- a read that finds
its answer shares it, a read that must compute marks it in progress, and a second reader of the same
part waits.

slang does not do that, and that is a gap in the fork this project builds rather than a condition of
the problem. Until it is closed, declaring runs in design order before the barrier and a unit's
bodies are lowered under one lock around the front end; everything from the unit's HIR onward reads
no front end and runs beside the other units. The lock is the serialization the north star forbids,
standing only on that gap, and it goes when the gap does.

## Rejected alternatives

- **Every unit reading the frozen AST at once.** It needs everything the units read to be elaborated
  before the freeze, which the model above shows cannot be listed short of elaborating every
  instance.
- **Turning instance caching off**, so that the front end elaborates every instance during its own
  pass and the AST can then be read concurrently. It is the one way to meet the alternative above,
  and it costs the largest measured design 11 GiB of front end, which this compile's memory cannot
  carry.
- **Reading a duplicate instance through the body it duplicates.** It avoids elaborating the
  duplicate, and it is wrong: the front end's notion of a duplicate is coarser than a specialization
  here, and two instances of one module bound to differently parameterized interfaces were recorded
  as duplicates while the children they build are not.

## Consequences

- Lowering the front end overlaps with the rest of the other units' work rather than preceding all
  of it, and its own time is still one unit after another. Widening it waits on the front end
  synchronizing its own lazy computation.
- A unit's bodies read the shared sensitivity analysis under the same one-reader rule, so its cache
  stays shared across the design.
- A thread-sanitizer run of the corpus compiled four units at once is what shows the rule holds: it
  is clean with it, and reports 130 races with the lock removed.

## Cross-references

- [a-build-is-told-how-wide-to-run](a-build-is-told-how-wide-to-run.md) -- where the width comes
  from, and why it reaches every per-unit stage it can.
- [declarations-before-bodies](declarations-before-bodies.md) -- the same ordering inside one unit.
- [front-end-semantic-boundary](front-end-semantic-boundary.md) -- the front end owns resolution;
  this entry adds that reading its answer can change it.
