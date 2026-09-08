---
description: Check backend render and MIR-consuming lowering code against the mechanical-translation contract, and report what it finds without changing anything
allowed-tools: Read, Grep, Glob, Bash(git status:*), Bash(git diff:*), Bash(git log:*), Bash(ls:*), Bash(grep:*), Bash(sed -n:*), Bash(wc:*)
---

# Check Render

Find the places where a consumer of MIR is deciding something MIR did not state, and report them.

**This command changes nothing.** It has no edit tools, and it does not propose patches. Its output
is a reader's judgement the human acts on. A finding handed over with a fix attached invites the fix
to be applied at the site, and the site is usually the wrong place -- see Report below.

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

## 3. Where to look

These are starting points, not criteria. A hit is a candidate; the contract decides.

- A branch inside an entry that emits a value, a statement, or a member.
- A closed set whose alternatives differ only in something the surrounding structure already fixes.
- A predicate answered by a guard (`if (!x.empty())`) where the fact could have been looked up.
- An absence standing for a kind -- an optional or an empty container read as a discriminator.
- A `default:` or catch-all arm that returns a real answer rather than refusing.
- A set whose members no node ever carries.
- The same question answered in two places: grep one backend for a fact and then the other.
- A helper that reads past its node -- matching on what an operand happens to be, or reaching an
  enclosing declaration.

## 4. Report

Findings only. Most branches in a mature backend are legitimate; a report that flags them is noise
and will be ignored, which costs more than saying nothing. If nothing is wrong, say so in one line.

For each finding:

1. **Where** -- file and line.
2. **What each arm emits** -- the evidence, quoted or paraphrased from the code. This is checkable;
   the verdict below is not, without it.
3. **The verdict, by the contract's test** -- operation, spelling, or presentation. Name which, and
   why the test lands there.
4. **Which layer declined to state the fact.** This is the part that matters most and the part a
   reader cannot supply. Say what is not stated and where it would be stated. Where the root is
   upstream, say plainly that fixing it at the site would be wrong -- it moves the decision rather
   than removing it, and the other consumer still works it out alone.

Then close with what was checked and what was not, so the human knows the report's reach. A survey
that read six files says six files.

Order findings by root, not by file: two sites with one cause are one problem, and reporting them
apart invites two wrong fixes.
