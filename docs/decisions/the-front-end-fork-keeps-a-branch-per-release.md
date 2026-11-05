# The Front End Fork Keeps a Branch per Release

## Date

2026-10-22; revised 2026-11-03 (a branch is named by the whole release tag).

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
the branch for 10.0, and the other, on a line that branch does not carry forward, by a protected
tag.

The same question comes back whenever upstream releases. A fix Lyra needs may already be upstream,
in a release newer than the fork's base, so the fork has to move to new releases; moving must not
cost the history the move leaves behind.

## What the field does

Rust reads LLVM from its own fork the same way: `rust-lang/rust`'s `.gitmodules` names the branch
`rustc/23.1-2026-07-22` of `rust-lang/llvm-project`, and that repository still holds every such
branch back to `rustc/8.0-2019-01-16`. The version in the name is the LLVM release's major and
minor, so `rustc/18.0-...` and `rustc/18.1-...` are separate lines. Each replay of Rust's changes
starts a new branch, several within one release, so its names carry a date as well. Swift's
`swiftlang/llvm-project` names its release lines by the whole release, `swift/release/6.2` beside
`swift/release/6.2.1`. Lyra replays only when it moves to a release, and between releases adds
commits on top, so the release alone names a line, and it is named as the release's own tag spells
it.

## Decision

**D1. One branch per upstream release the fork is based on, named `lyra/` and that release's tag
(`lyra/v12.1` for the tag `v12.1`), and a branch only ever grows.** A change Lyra needs, whether its
own or upstream's taken early, is a commit added on the branch Lyra builds from; nothing on such a
branch is rewritten or removed. The host refuses a force-push to it and its deletion, so the rule is
held by the place it lives rather than by whoever pushes.

**D2. Moving to a new release starts a new branch.** The commits the fork carries are replayed onto
the release tag as its branch, and Lyra's pin moves to it; the old branch stays where it is, so
every commit Lyra named on it is still reachable. A carried commit the release already has, as
written or reworked by upstream, is not replayed, and one upstream took after the release is
replayed as upstream's own commit, so the list of what the fork carries shrinks at each move to what
is still owed upstream.

**D3. The release Lyra's `MODULE.bazel` names says which branch its pinned commit must be on, and a
check holds the two together.** `tools/policy/check_pinned_sources.py` fetches the branch named for
the release the pin claims (`lyra/v12.1` for `12.1`) and refuses a pin its history does not contain.

## History

The branches were first named `lyra/v<major>` (`lyra/v10`, `lyra/v11`), on the premise that the fork
moves only to a new major release. Upstream makes minor releases too (9.1, 12.1), and a second
release of one major would have needed a name already taken, which D1 forbids rewriting. On
2026-11-03, with the move to 12.1, the two lines were renamed `lyra/v10.0` and `lyra/v11.0` at the
same commits, and the tag `lyra/v10-a9e580e7` became `lyra/v10.0-a9e580e7`. A build fetches the
front end by its commit, never by a branch, so no earlier Lyra commit stopped building: every commit
`MODULE.bazel` had ever named was checked reachable from the new names before the old ones were
removed.

## Rejected

- **One branch rebased onto each release.** It is what lost the 10.0 pins: a rebase writes new
  commits and leaves the old ones on no branch.
- **A branch named by the major release alone.** It cannot tell apart two releases of one major, and
  the second needs the name the first holds.
- **The release tag plus patch files kept in Lyra's repository**, as a module registry carries a
  dependency's fixes. Each Lyra commit would then hold its whole front end and need no fork at all,
  but the carried changes are developed and sent upstream as commits on a branch, and keeping them
  as patch files too is the same change in two forms.
- **Tagging every pinned commit.** A tag per pin keeps each one reachable without a branch rule, but
  it is one more step at every pin move, while a commit Lyra needs is made on a branch anyway, and a
  branch that only grows keeps it with no step of its own.
