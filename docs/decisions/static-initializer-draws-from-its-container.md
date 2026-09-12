# A static initializer draws from its container's generator, not from a process

Date: 2026-09-11 Status: accepted

## Context

LRM 18.14.1 gives every module, interface, and program instance, and every package, an
_initialization RNG_ seeded with an implementation-dependent default seed, and requires it to "be
used in the creation of static processes **and static initializers**". A variable declaration
assignment runs before any procedure starts (LRM 10.5, and LRM 26.2 says the same for a package), so
a randomization call written in one has no process to draw from.

The runtime had built the first half only. Each scope carried an initialization RNG and handed a
seed to each static process it created, and a randomization call asked the runtime which process was
executing -- which is a question with no answer during initialization, so five positions refused at
run time with one sentence: a module-level variable, a static declaration inside a procedural block,
a package variable, a `static` class property, and a `static` local of a subroutine. Two neighbours
bound the set and worked: an `automatic` declaration, and a per-object property initializer of an
object constructed from inside a process.

## Decision

**A randomization call draws from the generator installed for whatever is running, and a static
initialization installs one just as a process does.** A static initialization starts, draws in
declaration order, and ends, exactly once. So the question a draw asks is which generator, never
what kind of thing it is inside, and there is no state in which nothing has installed one.

### Which generator is ambient, because a subroutine body is compiled once

A subroutine is one body reachable from both a process and a static initializer:

```systemverilog
function int F(); return $urandom; endfunction
int m = F();            // reached from a static initializer
initial x = F();        // reached from a process
```

So which generator a draw uses cannot be a property of the call site, and cannot be an operand the
lowering supplies. It is state the runtime holds while something runs, for the same reason the
engine handle is, and the entry reads it rather than being told.

Read from the other end and the same answer falls out: every randomization entry -- `$urandom`,
`$urandom_range`, `$random`, and `shuffle`, `randcase`, `randsequence` and `randomize` when they
land -- does one thing with what it is given, which is draw the next value from it. None branches on
it, so what they are given is one generator and never a choice between two kinds of owner.

### The container is the one the standard names, and it names the seed

An installed generator starts from a seed its container's initialization RNG chose. A scope's static
initialization names the module, interface, or program instance holding those seeds, which is the
scope itself unless it is a generate scope -- a generate scope keeps no seeds of its own, so it
reaches the instance over the distance the elaboration walk already knows, exactly as a process
registration does. A namespace is not instantiated and names nothing: its initialization RNG exists
for its one bring-up, so its generator starts from the default seed. That is what gives every
package the same starting point and keeps one package's draws out of another's.

Seeds therefore leave an instance in one order: its static initialization first, then each static
process as it is created, because initialization runs before activation. The standard accepts that
adding work shifts what comes after it -- 18.14.1 tells a user adding threads to add them at the end
of a code block to keep earlier draws where they were -- so an order fixed by structure is the whole
of what stability requires.

### One static initialization per scope, which the standard leaves open

The standard's containers are the module, interface, and program instance and the package; a
generate scope is not among them. So whether a generate scope's initializers continue their
instance's stream or start one of their own is a question 18.14.1 does not answer, and no consumer
can tell the two apart: what a randomization entry is handed is one generator either way.

It is one per scope, each taking its own seed from the instance. What decides it is the phase
ordering rather than the standard: a scope's initializers run before any child scope's, so an extent
opened by a generated body cannot span its children, and one per container would take a structural
fact -- which scopes are instances -- that the runtime deliberately does not hold. The cost of the
choice is that adding a generate scope moves the seeds of the ones after it, which is the shift
18.14.1 already tells a user how to avoid.

### A static initializer takes a seed rather than drawing from the initialization RNG directly

LRM 18.14.1's "used in the creation of" is explicit for a static process, whose RNG it seeds, and
for a class object built by a static declaration initializer, whose object RNG it seeds. For a plain
`$urandom` in a static initializer the standard does not say in so many words whether the draw comes
out of the initialization RNG itself or out of a generator seeded from it.

It is seeded from it, and one clause settles it: LRM 18.14.1 says **all noninitialization RNGs can
be manually seeded**, which makes the initialization RNG the one generator the language may not
reseed. A static initializer may write `$urandom(seed)`, and LRM 18.13.1 gives that form a generator
to restart. Were the generator it restarted the initialization RNG, a legal program would reseed the
one the standard exempts -- and with it every static process of that container. So what a static
initializer draws from is seeded from the initialization RNG and is not it.

### An extent, not a flag

The generator is installed where the generated body runs: named before the initializers, and given
back on every way out of them, including a control effect passing through. Nothing carries a mode
that a draw then reads, and nothing asks whether a process happens to be executing. A randomization
call reaching the runtime with nothing installed is a gap in that bracketing rather than anything a
legal program can express, so it is a compiler-invariant failure rather than a refusal.

## Rejected alternatives

- **Refuse the call, as the runtime did.** The standard requires the generator to exist and names
  it. Refusing turns a normative requirement into a gap, and the message it printed -- "outside any
  process" -- reads like an exotic case while naming ordinary testbench code.

- **The call names the generator it draws from, so nothing is ambient.** A subroutine is one body
  reached from both a process and a static initializer, so no operand the lowering supplies can be
  right at both of its call sites. Threading the generator through every call instead is the shape
  the engine handle already rejected for the same reason.

- **Draw from the enclosing process where there is one and from the container otherwise.** Every
  randomization entry would branch on which it was handed, and there are seven of them once the
  clause's full list lands. Installing covers both with no branch: a process installs its own, so a
  draw inside one already reaches it.

- **One generator for the whole design's static initialization.** Cheaper, and it destroys the
  property the clause exists for: one package's draws would then depend on how many other packages
  there are and in what order the design root brought them up.

- **A generator per declaration rather than per static initialization.** Every declaration would
  consume a seed, so adding a declaration that draws nothing would move every draw after it -- the
  instability 18.14 is written against.

- **The initialization RNG itself as the drawing generator.** A legal `$urandom(seed)` in a static
  initializer would then reseed the one generator LRM 18.14.1 exempts from manual seeding, taking
  every static process of that container with it.

## Consequences

- A randomization call is legal wherever the language allows an expression, with no position left
  that refuses.
- Two instances of one module see the same static-initializer draws, and a package's draws do not
  move when another package is added.
- Each scope's static initialization consumes one seed from its instance ahead of that instance's
  static processes, so a design's process seeds shift by one against what the runtime handed out
  before. Nothing observable about a conforming program depends on which seeds those are.
- The generator type is named for what it does rather than for a process, because a static
  initialization is not one.
- **An object created by a static initializer is not seeded from what that initializer draws from.**
  LRM 18.14.1 names the source outright: with no active thread, the created object's RNG takes the
  next value of the container's own initialization RNG. What is installed here is one step removed
  from that, so object stability will need the container reachable during a static initialization as
  well as the generator -- a second thing for the extent to carry. Nothing asks for it yet, because
  no object holds an RNG until `randomize()` lands.

## Cross-references

- LRM 18.14.1 (random stability properties: the initialization RNG, thread stability, object
  stability), 18.14.2 (thread stability), 18.13.1 -- 18.13.2 (`$urandom`, `$urandom_range`)
- LRM 10.5 (variable declaration assignment), 26.2 (a package's declaration assignments run before
  any procedure starts, and a package contains processes inside checkers only)
- `docs/decisions/elaboration-lifecycle-phases.md` (Initialize runs variable initializers, before
  Activate registers processes)
