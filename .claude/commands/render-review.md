---
description: Check backend render and MIR-consuming lowering code against the mechanical-translation contract, and report each problem it finds and where that problem is fixed, without changing any code
allowed-tools: Read, Grep, Glob, Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(ls:*), Bash(grep:*), Bash(sed -n:*), Bash(wc:*)
---

# Render Review

Find the places where a consumer of MIR is deciding something MIR did not state, and report them.

**This command changes no code, and writes no file.** It does name the fix, at the root rather than
at the site -- which layer states the fact, and what it states -- because a finding whose fix nobody
can name is not finished being investigated. What it never hands over is a patch at the site,
because the site is usually the wrong place to apply one.

## Context

- **Argument:** $ARGUMENTS
- **Changed files:** !`git status --short`
- **Change summary:** !`git diff HEAD --stat`

## 1. Read the contract first, and judge only by what it says

Read these before looking at any code. They hold the test; this file deliberately does not.

- `docs/architecture/backend_contract.md` -- what a backend entry may and may not do, the test that
  separates a spelling from an operation, and the cross-check a second backend gives.
- `docs/architecture/mir.md` -- Forbidden Shapes, and what counts as reading structure versus
  re-deriving.
- `docs/progress/mechanical-translation.md`, if it exists -- the instances already known and being
  worked, so a finding can say whether it is one of them or new. Its absence means the workstream
  finished; the contract still governs.
- `docs/decisions/`, every record touching the subject. One of them has usually settled the shape
  already, with the reason; a finding that contradicts a record is either wrong or a reversal that
  has to be argued as one.

**A record whose mechanism was superseded is not a record whose problem was solved.** Where a later
record replaced the mechanism, check that the replacement exists in the code before treating the
earlier defect as closed. A superseded mechanism whose replacement was never built leaves the defect
live and unowned, and it is the hardest kind to see, because both records read as settled and
neither one is wrong.

**Do not restate the criteria from memory, and do not carry them in your head from a previous
session.** They have been wrong before: the contract once said different syntactic shape was the
defect, which condemns every mechanical case in the tree. If what you remember and what the doc says
disagree, the doc wins, and say so in the report.

## 2. Resolve what to check

**An argument names the scope.** A path, a directory, a function name, or a described area -- check
that and nothing else.

**No argument, and the changed files include a MIR consumer** -- a backend render, a MIR-to-LIR
lowering, or the HIR-to-MIR lowering that feeds them. Check the changed regions of those files,
reading each file whole so a local edit is judged against its surroundings.

**No argument and nothing relevant changed** -- survey. Prefer the value-emission entries, which is
where the contract's test applies most directly, and say in the report that a survey samples rather
than audits.

**Whatever the scope, it bounds where you start looking, not what you read.** A finding names a
root, and a root is only checkable once every place that produces it and every place that consumes
it has been read -- grep for it, then read those files whole, whether or not the scope named them.
Stopping earlier yields a finding whose fix cannot be stated, and the fix is the half of the report
the human actually acts on.

## 3. Where to look

These are starting points, not criteria. A hit is a candidate; the contract decides.

- A branch inside an entry that emits a value, a statement, or a member.
- A value-emission entry with a body rather than one composed expression. Length is a cheap first
  reading -- a tell, never a criterion. Attribute it before reporting: a result assembled in steps
  or across a branch is usually a decision made a piece at a time, while a hand-rolled list join is
  only a helper nobody reached for.
- A condition built from more than one input -- a conjunction, or an entry taking a discriminating
  parameter beside the node. Each input is its own candidate, and settling one leaves the rest
  choosing at a site that now reads as done.
- A closed set whose alternatives differ only in something the surrounding structure already fixes.
- A predicate answered by a guard (`if (!x.empty())`) where the fact could have been looked up.
- An absence standing for a kind -- an optional or an empty container read as a discriminator.
- A `default:` or catch-all arm that returns a real answer rather than refusing.
- A set whose members no node ever carries.
- The same question answered in two places: grep one backend for a fact and then the other.
- A helper that reads past its node -- matching on what an operand happens to be, or reaching an
  enclosing declaration.

## 4. Report

Two questions are being answered, in this order: **is there a problem**, and **is the fix known**.
Everything else is the backup that makes those two checkable, and backup does not belong in the
chat.

Findings only. Most branches in a mature backend are legitimate; a report that flags them is noise
and will be ignored, which costs more than saying nothing. If nothing is wrong, say so in one line
and stop.

**The whole report goes in the chat.** A finding that does not fit there is not finished being
investigated, and the room a second document buys is room to sound certain in rather than room to
check in. Give per finding:

1. **The root, in one sentence** -- the fact nobody states, not the site where it surfaced.
2. **Whether it is a problem**, by the contract's test: operation, spelling, or presentation. One
   clause of why, not the derivation.
3. **The fix** -- which layer states the fact, and what it states. Where the root is upstream, say
   plainly that fixing it at the site would be wrong: it moves the decision rather than removing it,
   and the other consumer still works it out alone. Where the fix reopens a settled decision, name
   the record. Where the fix is genuinely not known yet, say so in those words -- that is a real
   answer, and a better one than a fix that has not been derived.

   A fix counts as derived only when what it changes has been read to its edges. If naming it needed
   an assumption about how something downstream behaves, it is not derived, and the honest report is
   the open question rather than the fix -- the more so where a wrong answer would be silent, such as
   what crosses a foreign ABI, because there the build does not catch the guess.

4. **Whether it is closable now, or blocked, and on what.** This is what the next step reads. A
   finding taken up in the same session needs nothing written down; one that is not belongs in the
   queue the progress docs keep, where a reviewer sees it.

**Before reporting a fix, write down what the site looks like once it lands.** If a branch of the
same kind survives -- fewer arms, or the same arms chosen on a different input -- the fix is partial
and the survivor is a second finding at the same site. This is the check that a report is otherwise
structurally unable to make: naming one root reads as accounting for the whole branch, and the
reader has no way to tell that it did not.

Order findings by root, not by file: two sites with one cause are one problem, and reporting them
apart invites two wrong fixes. The inverse is not symmetric and has to be looked for on purpose:
one site with two causes is two problems, and "one site, one root" is an assumption, never a
finding.

**Do not close with what was left unread.** A finding ready to report is one read to its edges; one
that is not is a reason to keep reading, never a caveat to hand over. The single exception is the
no-argument survey, which samples by construction -- one line saying what it sampled.
