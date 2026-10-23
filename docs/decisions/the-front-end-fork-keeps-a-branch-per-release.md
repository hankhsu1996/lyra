# The Front End Fork Keeps a Branch per Release

## Date

2026-10-22

## Status

Accepted.

## Why this decision matters

Lyra reads slang from a fork, at the one commit `MODULE.bazel` names, and every Lyra commit names
the commit it was built with. A commit of the fork that no branch reaches is kept by the host only
until it collects it, and from then on every Lyra commit naming it fails to fetch its front end and
cannot be built again.

That happened once. The fork's one branch for Lyra was rebased onto the 11.0 release in place, and
the two commits Lyra had named on the 10.0 line were left on no branch. They were still served when
this was found, by chance rather than by anything the fork promised, and are now kept again: one on
the branch for 10, and the other, on a line that branch does not carry forward, by a protected tag.

The same question comes back whenever upstream releases. A fix Lyra needs may already be upstream,
in a release newer than the fork's base, so the fork has to move to new releases; moving must not
cost the history the move leaves behind.

## What the field does

Rust reads LLVM from its own fork the same way: `rust-lang/rust`'s `.gitmodules` names the branch
`rustc/23.1-2026-07-22` of `rust-lang/llvm-project`, and that repository still holds every such
branch back to `rustc/8.0-2019-01-16`. Each replay of Rust's changes onto LLVM starts a new branch,
several within one LLVM version, so its names carry a date as well as the version. Lyra replays only
when it moves to a release, and between releases adds commits on top, so the major number alone
names a line.

## Decision

**D1. One branch per upstream release the fork is based on, named `lyra/v<major>`, and a branch only
ever grows.** A change Lyra needs, whether its own or upstream's taken early, is a commit added on
the branch Lyra builds from; nothing on such a branch is rewritten or removed. The host refuses a
force-push to it and its deletion, so the rule is held by the place it lives rather than by whoever
pushes.

**D2. Moving to a new release starts a new branch.** The commits the fork carries are replayed onto
the release, as `lyra/v<new major>`, and Lyra's pin moves to it; the old branch stays where it is,
so every commit Lyra named on it is still reachable. A carried commit upstream has since taken is
not replayed, so the list of what the fork carries shrinks at each move to what is still owed
upstream.

**D3. The release Lyra's `MODULE.bazel` names says which branch its pinned commit must be on, and a
check holds the two together.** `tools/policy/check_pinned_sources.py` fetches that branch's history
and refuses a pin it does not contain.

## Rejected

- **One branch rebased onto each release.** It is what lost the 10.0 pins: a rebase writes new
  commits and leaves the old ones on no branch.
- **The release tag plus patch files kept in Lyra's repository**, as a module registry carries a
  dependency's fixes. Each Lyra commit would then hold its whole front end and need no fork at all,
  but the carried changes are developed and sent upstream as commits on a branch, and keeping them
  as patch files too is the same change in two forms.
- **Tagging every pinned commit.** A tag per pin keeps each one reachable without a branch rule, but
  it is one more step at every pin move, while a commit Lyra needs is made on a branch anyway, and a
  branch that only grows keeps it with no step of its own.
