# A Supported Library Is A Port Lyra Ships

## Date

2026-11-19

## Status

Accepted. Extends [project-file](project-file.md), whose dependency table gains a second kind of
source and whose "not built" list loses versions and a lock file.

## Why this decision matters

A testbench is written against libraries its author did not write and does not edit: a methodology
class library such as UVM above all. Such a library is published once and used as published, so a
user cannot be assumed to patch it, and a release written before Lyra existed may need preparing
before Lyra can build it -- its foreign code compiled with the right macros, a back end the standard
does not require replaced by one that it does. That preparation is Lyra's work, never the user's,
and once a project has built against one preparation it has to keep getting that one: a later Lyra
preparing the same release differently must not change what an existing project builds.

So three things have to be sayable: which release a project asks for, which preparation of that
release it got, and that the preparation it gets later is that same content.

## What the survey says

- **vcpkg** calls an upstream release as the tool prepares it a _port_, and counts preparations of
  one release with a `port-version` that "resets to 0 whenever the version changes"; a project's
  baseline pins which port it takes. The recipe and patches live in the tool's own repository, not
  the library's.
- **Cargo** writes `Cargo.lock` the first time a dependency resolves, holding "exact information
  about your dependencies", and changes it only when told to; `--locked` makes a build that would
  change it an error instead. Each entry carries a checksum of the package's content.
- **The Bazel Central Registry and crates.io** never change a published version: a fix is a new
  version beside it, so a pin means the same bytes forever.
- **Bender**, the closest tool in kind, pins each dependency's commit in `Bender.lock`, and a
  `frozen` setting refuses to move it.

**Where our conditions differ.** Each of these fetches from a network registry. Lyra fetches
nothing: the ports it supports ship inside it, prepared when Lyra itself is built, so a build needs
no network and no account. That is a gap rather than a condition, and the shape is chosen so that
fetching later changes where a prepared tree comes from and nothing else.

## Decisions

### D1. A dependency names a path or a version, never both

`[dependencies] uvm = { version = "1.2" }` asks for a release of a library Lyra supports;
`prim = { path = "../prim" }` names a library of the project's own, as before. A dependency naming
both, or neither, says nothing a build could follow and is refused. A version is an upstream
release's own spelling and is matched exactly; there is no range, because a methodology library's
releases are few and a project moving between them reviews the move.

### D2. A port is an upstream release as Lyra prepares it, and is never changed once published

A port is identified by the library's name, the release, and a port version counting Lyra's
preparations of that release from 1, as the Bazel Central Registry's `.bcr.<n>` does. Its content is
an ordinary library declaration (`lyra.toml`) beside the release's files, with Lyra's patches
applied, so a port is read exactly as a library of the project's own is and the compiler cannot tell
the two apart. Every published port version ships, and its content's digest is committed beside its
recipe and stated in the metadata the shipped tree carries. A test of the merge gate reads every
shipped port back against that digest as resolution does, and fails where the content differs, so a
changed recipe can only ever be a new port version.

The digest is SHA-256 over the tree's files in the order of their paths relative to the tree,
written with `/`, each path and each file's bytes preceded by its length; links are followed, and
neither an absolute path nor a time enters it. It is published and kept forever, so it is a
cryptographic hash any other tool can recompute, unlike the digests Lyra keys its build cache by,
which may change with Lyra.

### D3. A project's lock records the port it took, and only the root's lock counts

`lyra.lock`, beside the root's `lyra.toml`, records for each supported library the build took its
name, release, port version and digest. It is written by Lyra, committed by the user, and decides
which port is taken. A release the lock records is taken at the port version it names, and a release
it does not record is taken at the latest port version shipped, and recorded. The lock is written
only after the whole graph resolves, whole and by replacing the file, so a failure leaves the lock
that was there; an entry for a library no longer declared is dropped. A library reached through
another is locked by the root, as a dependency's own lock is not read: the build has one answer per
name (project-file's one-library-per-name rule), so it has one lock.

A port is verified against the digest it was published with before the lock's, so a modified
installation is refused even where nothing is locked yet and is never written into a lock. The lock
states its format, which a manifest does not. A later Lyra may change what an existing key of the
lock means without adding one, which strict keys cannot detect, and Cargo.lock's `version` records
the same. A run told `--locked` refuses where the lock would have to change, and writes nothing.

### D4. The store a port is taken from is a property of the run

By default the ports are those shipped beside the Lyra binary. `--ports <dir>` replaces that store
for one run -- an organization's own prepared ports, or a test's -- and is not a search path: a
locked port missing from it is an error, never a reason to look elsewhere. The lock does not record
the store, because the digest already says whether the content is the one recorded; a port an
organization changes takes an identity of its own rather than reusing a published one.

## Rejected

- **A version range, or a resolver choosing among releases.** A range resolves differently as
  releases appear, which is what the lock exists to prevent, and nothing here has enough releases to
  need one.
- **Patching the library in the project, or asking the user to.** A user takes the library as
  published; the preparation is the toolchain's.
- **A recipe file Lyra reads at run time.** It would restate what Lyra's build already states while
  Lyra fetches nothing; it has a reader only once fetching does.
- **Taking a tree on its directory's name alone.** A store finds a port at
  `<name>/<version>/<port_version>`, and the tree states its identity in metadata written when it
  was prepared; a tree whose metadata names another port is refused, so a directory renamed or
  copied under another name is never taken as the port that name says.
