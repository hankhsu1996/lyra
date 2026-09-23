# A prepared header carries the work, not only the text

A translation unit pays for what it contains, not for what the library it uses could offer.

Compiling a header in advance is usually described as saving the parse. Measured here, the parse was
never the cost. Every emitted translation unit includes one umbrella header naming the whole runtime
surface, and a unit holding nothing else -- no design code at all -- cost 0.63 s: 605 ms of it
instantiating templates, and 37 ms reading the prepared header. The instantiations are the same set
every time, because they come from the header rather than from the design.

## The requirement

**Work that is the same for every unit of every build is done once, where the artifact those units
share is made.** What is genuinely per-unit is the design's own text.

This is the end-to-end iteration loop rather than an internal tidiness: the cost falls on whoever
builds a design, once per unit, on every build, and it grows with the number of units a design has.

## What was wrong

A prepared header held declarations and not instantiations, so each unit including it performed the
same 296 function and 477 class instantiations again. Two thirds of that was one standard facility's
formatting engine, half of which is its wide-character half -- reachable from the umbrella header
through `<memory>`, and unusable by any simulated program, since nothing in SystemVerilog formats
wide characters.

The size of the effect is what makes it worth recording rather than fixing quietly. Measured on one
emitted project, per unit:

| Unit                                      | Before | After  |
| ----------------------------------------- | ------ | ------ |
| the design's own, 19,492 bytes            | 1.60 s | 0.42 s |
| the entry point, 219 bytes                | 0.71 s | 0.21 s |
| the umbrella header alone, no design code | 0.63 s | 0.11 s |

Across ten conformance cases end to end -- emit, build and run -- a case went from 3.12 s to 1.54 s.

## The decision

**The command that prepares a header asks for the templates it reaches to be instantiated into it.**
A unit that includes the result performs none of them. Only the preparing command carries this: it
decides what the artifact holds, where the including command decides how the artifact is read.

It asks that every header named compile on its own, which is what an umbrella header is, and is
already checked -- a header reachable from the emit but absent from the umbrella would be re-parsed
per design and nothing would report it, so the two sets are one set by construction.

**A prepared header is therefore named by how it was prepared, not only by what it was prepared
from.** The name already covered the compiler, the installation, the header bytes and the
optimization; the last of those was standing in for the whole command line, and now is it. Two
entries with the same name stay equivalent by construction, which is what lets a name match count as
a content match.

**The header bytes it is named by are the surface's and nothing else's**, which is what keeps an
entry from being renamed by an edit no design can observe. A compiler is built from far more headers
than it hands out, and those it hands out are gathered where nothing else is; without that, every
edit to a lowering or an IR node would rename every entry and the next build of any design would
prepare one again.

Both things that build an emitted project owe this -- the build the compiler drives and the recipe a
project ships -- and the shipped recipe names its cache entry by its own bytes, since the recipe is
what decides the contents.

## What was rejected

**Merging a design's units into one.** The fixed cost is per unit, so one unit pays it once. This is
what a simulator that flattens a whole design before emitting anything does, and it is closed here
by `north_star.md`: partial recompilation and concurrent compilation of independent units are
first-class constraints, and one unit has neither. It also expires -- once the instantiations move
into the shared artifact there is nothing left for it to save.

**Compiling a design's units several at a time to hide it.** That is already available and is a
different question. It divides waiting rather than removing work, and it buys nothing where
something else already holds every processor, which is exactly the case a conformance run is.

**Header modules, for the whole-blob invalidation they would also fix.** They address parsing, and
parsing is 37 ms of the 631 ms measured above.

## What it costs

Preparing a header costs about 0.9 s more and is paid once per cache entry, so it is repaid by the
second unit compiled against it. The artifact grows about a tenth.

A header that is not self-contained now fails while being prepared rather than at its first use.
Preparing one is an attempt in exactly the sense using one is: a build that cannot have the fast
path takes the plain path instead of stopping, so this changes what a build costs and not whether it
succeeds.
