# A precompiled header is an attempt

Making a build faster must never change whether it succeeds.

Compiling a design against the runtime headers costs about two and a quarter seconds per translation
unit; compiling it against a header prepared from those same headers costs about half a second.
Preparing one costs two and a half seconds and is worth it from the first unit. Nothing about that
trade is in question. What is in question is who gets to decide whether a precompiled header is
still good, and what happens when the answer is no.

## The requirement

**An accelerator may change how long a build takes. It may not change whether the build succeeds,
and it may not change what the build produces.**

Both halves matter and they fail in opposite directions. A precompiled header that is refused must
not fail the build. A precompiled header that is stale must not be used. Anything that only makes
one of those rarer is tuning a probability where the requirement is absolute.

## What was wrong

The cache names its entries by content: the compiler's identity, the include root's path, the bytes
of every header under it, and the optimization the header is prepared at. Two entries with the same
name are equivalent by construction, so a name match is a content match and no staleness check is
needed to decide which entry to offer.

The compiler does not decide that way. It records each input header's size and modification time
when the header is prepared, and re-checks them when it loads it. So there were two authorities over
one question, and the one that could fail the build was the one nobody chose.

They disagree whenever a file's bytes stay put and its timestamp moves. That is not an edge case: it
is what a code generator does every time it rewrites its own output, and what a checkout does every
time it restores a file. A branch switch that lands a change and then brings it back byte for byte
leaves every later build refusing a cache entry that was correct.

## The decision

**A precompiled header is offered to a compile and never required by one.** Where a compile that was
given one fails, it is run again without one before its output counts as a failure. A compile that
then succeeds was refused the header rather than the source: the build goes on, the user hears
nothing about it, and the header is dropped so that the next build prepares it again.

That places the decision where the field places it. Ccache, facing the same question, falls back to
invoking the real compiler whenever it cannot be certain a cached result applies, and exposes the
checks it is prepared to relax as configuration rather than letting any of them fail a build. The
plain path is what makes that possible, and a build here has the same plain path available: compile
without a precompiled header.

**The fallback is the whole guarantee, so nothing else has to be exact.** The cache key does not
have to cover the system headers no fingerprint of ours can see. The compiler's rule for accepting a
header does not have to be known, predicted, or kept in step with. A precompiled header that turns
out to be unusable for any reason at all, including reasons that do not exist yet, costs one
recompile and no correctness.

**Two measures keep the fast path fast, and neither is load-bearing.** The compiler is asked to
decide currency by content rather than by timestamp, on both the command line that prepares the
header and every command line that loads one, so that a moved timestamp over unchanged bytes stays
on the fast path. And whatever writes a file leaves it alone when it already holds those bytes, so a
re-emit does not move a timestamp at all. Either one failing costs speed.

## What was rejected

**Disabling the compiler's own validation.** The compiler offers this, and it would end the
disagreement by removing one of the two authorities. It is refused by the second half of the
requirement: a genuinely stale header would then be used, and the build would produce something
other than what the sources say. An accelerator may not change the answer.

**Making the key cover everything the compiler checks.** The dependency list of a precompiled header
can be recovered and every file in it fingerprinted, which would make the key exact and the
compiler's check redundant. This is a great deal of machinery to reach a guarantee the fallback
already gives for nothing, and it would still be a second authority, merely a better-informed one.

**Proving the precompiled header acceptable before use.** Compiling the preamble alone against it
answers the question exactly, before any compile of the design, which is where the field puts the
decision. Measured at 0.46 s, against the 1.8 s a precompiled header saves on each unit. That is
affordable at any size and still wrong: it is paid on every build forever to avoid a cost incurred
almost never, and the fallback answers the same question for nothing.

**Recognising the compiler's complaint.** The refusal is reported in prose and could be matched. A
build path that reads compiler diagnostics to decide control flow is wrong whatever the text says
today, and running the compile again answers the same question by construction.

## What it costs

Nothing when the precompiled header is good, which is nearly always: the retry loop is entered only
by a compile that already failed.

When a source genuinely does not compile, its compile runs a second time without one. That is the
slower spelling, and it buys a better message: the diagnostics the user sees come from the compile
that had nothing in front of it.

When a precompiled header is refused, the compiles that were given it run again and the header is
prepared afresh on the next build.

## Where it has to hold

Two things build an emitted project and they are separate implementations: the in-process build
behind `build` and `run`, and the `build.sh` a project ships, which may assume nothing beyond POSIX
sh. Both prepare a header, both cache it, and both therefore owe this. The shipped recipe compiles
everything again rather than only what failed, because tracking which jobs failed costs more shell
than the rare path is worth; it also holds back what the compiler said until the build is known to
have failed, so a retried build that succeeded says nothing.
