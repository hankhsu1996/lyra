# A change is counted on real designs first

Date: 2026-10-28. Status: accepted.

## The decision

What each merge is counted on has two kinds of row, and they answer different questions.

```text
real designs   designs written elsewhere, for other simulators, counted whole:
               what Lyra executes building one, and what its program executes running it
               -> did the change move the work of someone who builds a design

cost families  the benchmark corpus, one case per cost, counted per unit of work
               -> which path did it move
```

The real designs are reported first and are the reason the report exists; the cost families are how
a move in one is traced to the part of Lyra that made it. A comment goes on a merged pull request
only where a row moved by two percent or more, or could no longer be counted.

Which rows stand is decided by a person, the designs listed by hand and the corpus cases written by
hand. What is automatic is the alarm: a row that cannot be counted, or whose count cannot tell a
change of two percent from its own variation, is red on every merge until someone fixes it or takes
it out. It is never dropped by the report, since a row nobody can read that disappears is the same
as one nobody reads.

## Why

The report stood on the corpus alone for its first two weeks, and in that time it said one true
thing: #1469 made a module four times as costly to compile. The same cost was nearly nothing on the
one real design anyone measured, because the corpus case that showed it was a hundred copies of one
module, which a real design almost never is. Meanwhile the corpus said nothing of what a real design
spends most on. Ibex, counted, spends 42.8% of its instructions on port copies and continuous
assignments run as processes, and sixteen of the corpus's twenty-one cases had no port at all. A
suite of isolated costs can say which cost moved, but not whether the move matters. It also cannot
cover what it was not written to isolate.

rustc-perf, which this follows, splits its benchmarks the same way. Its primary benchmarks are "real
programs that are important in some way", mostly published crates. Its secondary ones are
"artificial programs or real crates that stress one particular aspect of the compiler". The summary
it posts on a pull request is split by the two (`site/src/github/comparison_summary.rs`). A run
whose changes do not deserve attention is not posted at all, as "just another noisy ping"
(`deserves_attention_icount` in `site/src/comparison.rs`). LLVM's compile-time tracker counts
CTMark, a set of real programs from its test suite. Verilator's own measurement is RTLMeter, a set
of real designs, and those designs are the ones counted here.

## Where our conditions differ

- **No history, so each row measures its own resolution.** rustc-perf marks a change as significant
  against that benchmark's own history (Q3 plus a multiple of the interquartile range). A count here
  moves from one hosted machine to the next, so it is compared only with one taken beside it, and
  there is no history to read. Instead a corpus case is counted twice at one amount, and a case
  whose two counts disagree by more than a third of the least change worth marking fails outright.
  History would never fail it; it would sit unmarked forever, which is what a case that measures
  nothing should not be allowed to do.
- **No hardware counter, so the designs are few.** rustc-perf and the LLVM tracker count with the
  processor's own counter, on a machine of their own. A hosted runner gives none that can be relied
  on, so the count is callgrind's, at about fifty times a native run. That bounds the list to the
  designs whose count of two compilers fits about half an hour. A machine with a counter would make
  it every design that gets all the way.

## What a real design's rows are

- **Its build, counted as an edit loop builds it**: the default optimization level, one unit at a
  time, so that no count takes in threads waiting on one another. That is the build a user waits on
  while iterating.
- **Its run, counted on the program a long run ships**, optimized with the runtime, over the case
  the suite marks as the design's quick check. Each count is a total rather than a difference of
  two. The suite fixes how much its quick check does, and a design being constructed is part of what
  a user waits for.
- **It is one the external design record says gets all the way.** A design that stops has no work to
  count, so listing one fails the count.
- **On the execution backend only.** The C++ backend's costs are counted per cost family, and that
  is where a move of its own is seen.

## Rejected

- **Every design, every merge.** Under callgrind the largest that gets all the way builds and runs
  for hours. Taking the field's counter needs a machine we do not have, so that is the gap to close
  rather than a cost to pay.
- **Keeping the corpus as the report and adding more cases until it looked like a design.** Each
  added case answers a question somebody thought to ask, and the report would still not say whether
  a move matters to a design nobody wrote a case for. The two kinds of row do different work.
