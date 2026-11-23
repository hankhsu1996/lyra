# The front end fork keeps a branch per release

Status: Accepted, 2026-10-22; revised 2026-11-03 (a branch is named by the whole release tag).

## Requirement

Every Lyra commit can be built again. Lyra reads slang from a fork, at the one commit `MODULE.bazel`
names, so a fork commit no branch reaches is kept by the host only until it collects it, and from
then on every Lyra commit naming it fails to fetch its front end. That happened once. The fork's one
branch for Lyra was rebased onto the 11.0 release in place, and the commits Lyra had named on the
10.0 line were left on no branch. The fork also has to move to new releases, since a fix Lyra needs
may already be upstream, and moving must not cost the history it leaves behind.

## Choice

- **One branch per upstream release the fork is based on, named `lyra/` and that release's whole tag
  (`lyra/v12.1`), and a branch only grows.** A change Lyra needs, its own or upstream's taken early,
  is a commit added on the branch Lyra builds from, and nothing on it is rewritten or removed. The
  host refuses a force-push or a deletion, so the rule is held by the place it lives.
- **Moving to a new release starts a new branch.** The fork's carried commits are replayed onto the
  release tag, the pin moves, and the old branch stays, so every commit Lyra named on it stays
  reachable. A carried commit the release already holds is not replayed, so what the fork carries
  shrinks at each move to what is still owed upstream.
- **A check holds the pin to the branch of the release `MODULE.bazel` names.**

Rust reads LLVM from its own fork the same way, and `rust-lang/llvm-project` keeps every `rustc/...`
branch back to 2019, each named by the LLVM release, and Swift names its lines by the whole release.

## Alternatives turned down

- **One branch rebased onto each release.** It fails every pinned commit staying reachable, since a
  rebase writes new commits and leaves the old ones on no branch.
- **A branch named by the major release alone.** It fails telling two releases of one major apart,
  since the second needs the name the first holds and a branch's name cannot be rewritten. The
  failure rests on upstream making minor releases too, which the first naming assumed it did not.
- **The release tag plus patch files kept in Lyra's repository.** It fails one form per change,
  since the carried changes are developed and sent upstream as commits on a branch, so patch files
  would be a second form of each.
- **A tag per pinned commit.** It fails no requirement, but adds a step at every pin move, where a
  branch that only grows keeps each pin with no step of its own.

## Consequences across layers

- Every correction of the front end Lyra makes lands on such a branch and moves the pin
  ([the-standard-decides-what-text-is-legal](the-standard-decides-what-text-is-legal.md)).
- `tools/policy/check_pinned_sources.py` is the check, run on every pull request.
