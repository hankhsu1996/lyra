# The Standard Decides What Text Is Legal

## Date

2026-10-11

## Status

Accepted. Replaces the decision of 2026-10-09 that made the front end's most tolerant reading the
default. The rule itself, how a disputed text is ruled on, and every ruling made so far are in
`architecture/language_conformance.md`; this record holds why the default changed.

## What was decided before, and on what

On 2026-10-09 Lyra began asking the front end for the union of every tolerance it knows
(`--compat all`): a text the standard forbids and some simulator takes was accepted with a warning.
The argument was that a design brought to a new simulator already runs on another, that its text
"works everywhere else", and that refusing it helps nobody. The designs it was argued from were
three of a benchmark suite that stopped in the front end under the stricter reading.

## Why it did not hold

The suite's testbenches and wrappers were written for one open simulator and run on nothing else,
and the forbidden texts found in them are ones that simulator takes. That it takes them is why they
were never noticed. It says nothing of any other tool, and "works everywhere else" had no other
evidence behind it.

Held against the standard construct by construct, the premise fails in a second way: a forbidden
text has no meaning to adopt, only a tool to copy. The standard says nothing of what `addr[2:3]`
selects from a `[63:0]` vector. The simulator the suite was written for swaps the bounds; the front
end, asked to tolerate it, warns and produces no expression at all, so the user saw a warning and
then a refusal that could not say why. A module described twice runs its first description in that
simulator, where 33.3.1 would have the last.

And the default had already been asked to carry a decision it could not: a design built under it
meant whatever the tolerant reading chose, and nothing told the author which texts had been chosen
for.

## What changed

The default is the front end's own reading, which is meant to be the standard's. Where that reading
is wrong it is corrected in the front end and not in Lyra's settings, so Lyra passes no
compatibility mode and keeps no list of diagnostics deciding what is legal. A caller can still ask
for a tolerant reading by name.

Two texts the standard allows are refused by the front end's default today -- an `always_comb`
writing one constant key of an associative array, and `$bits` of a member reached through an
interface port -- and each is reported upstream to be corrected there.

A constant select outside its vector or array is a third kind: the standard gives it a value
(11.5.1, 7.4.5) and also lets a tool refuse it while compiling, so the front end's refusal is a
choice and not a defect, and there is nothing to correct upstream. Lyra makes the other choice and
runs it with a warning. Refusing it stopped designs whose code, written for a larger configuration,
selects past the end of a vector in a smaller one, while every reading of the standard gives that
code a meaning.

## Rejected

- **Keeping the tolerant default and refusing only what the front end gives no meaning.** It keeps a
  design meaning one tool's choice wherever the front end does produce a meaning, which is most of
  the time, and the author is still not told.
- **A list in Lyra of single diagnostics to raise or lower where the front end judges what is
  legal.** A second statement of the standard beside the front end's, which nobody argues where it
  could be settled. A choice the standard leaves to the tool is not such a judgement.
- **Accepting a forbidden text that every tool takes and gives one meaning.** The assignment of a
  non-enumerated value to an enumerated variable with no cast is the strongest such case, and it is
  refused: an exception for agreement has no boundary anyone can state in advance.
- **Having the front end give a tolerated text the meaning one tool gives it.** Proposed for the
  reversed part-select and withdrawn: it makes one tool's choice the definition of a text the
  standard left undefined.
