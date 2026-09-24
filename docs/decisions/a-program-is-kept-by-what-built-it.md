# A program is kept by what built it

Date: 2026-09-23 Status: accepted

## The requirement

> Compiling a design and running it are separate acts, and a run whose inputs did not change pays
> for none of the compile. Many copies of one design, differing a little and edited at once, share
> that saving without ever handing one another a program the other did not ask for.

The second sentence is not a hypothetical. A verification engineer keeps several checkouts of a
design, and one checkout is compiled under many configurations -- a different top, a different
define, a different parameter per test. Whatever keeps compiled programs has to be right across all
of them at once, and it has to stay right while they are being edited and built concurrently.

## What the field does, and where Lyra differs

**Cargo** keeps everything a workspace builds under one directory beside its manifest, `target/`,
laid out by profile (`target/debug`, `target/release`) and by target triple. **CMake** has no name
of its own for the directory: the user names each build tree, and one tree is one configured build.
**Zig** splits what Cargo keeps together, into a cache that can be deleted at any time and an output
prefix holding what the user runs. **Go** keeps no directory in the project at all: build outputs go
to a cache in the user's cache directory, which "correctly accounts for changes to Go source files,
compilers, compiler options, and so on", so cleaning it "should not be necessary in typical use";
`go build` writes the one executable where it is asked, and `go run` leaves nothing behind.
**Bazel** is the hybrid the rest converge on: each workspace has an output tree of its own, and
every workspace draws on one content-addressed store of action results, on disk and remote.

Lyra's conditions are not Cargo's, in four ways, and each is structural rather than something not
yet built:

- **One design compiles under many configurations.** A top, a define, or a parameter override is
  chosen per invocation, and in verification per test. A layout by a handful of profiles has no
  place for that axis.
- **A design need not have a manifest.** A command line naming sources is a complete, anonymous
  design, so "beside the manifest" is not always a place.
- **A simulator's unit of isolation is the directory it runs in.** Each run writes its own logs and
  waveforms; Verilator writes `obj_dir` and VCS writes `simv` into the directory they are invoked
  from, not into a project root.
- **A hardware repository is shared by many tools.** FuseSoC writes `build/<core>/<target>`, and
  Makefile flows commonly own `build/`. A directory a tool deletes whole has to be one no other tool
  writes, which `build/` is not, and `target` already means a device or a flow in this domain.

So the answer is Go's split, extended the way Bazel extends it: a store shared by every checkout,
named by content, and a private copy of whatever a command hands back.

## The decisions

### D1. Compiled programs are kept in one store per user, and an entry never changes

The store sits in the user's cache directory, and it also keeps the prepared headers the C++ path
reads and, on the LLVM path, each unit's object (D9): `lyra` under `$XDG_CACHE_HOME` when that is an
absolute path, and under `$HOME/.cache` otherwise, which is the rule Go's `os.UserCacheDir` follows.
Finding the store creates nothing; the directories appear when the first entry is kept, so a command
that keeps nothing leaves no trace. When the platform names no such directory there is no store, and
a build is slower and nothing else. An entry is written once, under a name computed from what
produced it, and is never modified afterwards. Two checkouts that differ get different entries; two
that agree share one. Nothing in the store is mutable, so nothing one checkout does can change what
another reads.

### D2. An entry is named by what the expensive step reads, never by the SystemVerilog sources

For the C++ path the name is computed from the emitted project -- every file the host compile reads
from it -- together with the runtime's headers and library, the foreign sources' objects, the host
compiler's identity, and the settings the compile runs under. For the LLVM path it is computed from
the names of the objects the program links (D9), the runtime library, the foreign objects, and the
host driver the link runs under. How many compiles run at once is in no name, because the objects
are the same however many there were. The compiler's own build is part of an LLVM object's name and
not of the C++ path's, because on the LLVM path the code generator is Lyra itself, while on the C++
path it is the host compiler, which is named already.

This makes the name complete by construction. Tracking what the front end read -- every file an
`` `include `` reached, every define in effect -- is a second authority over the same question, and
the first input it misses is a stale program that runs without a word. A program cache has no
fallback the way a prepared header has one: a stale header is refused and the compile runs again,
while a stale program is simply executed. The cost is that the front half of the pipeline always
runs; the saving is the compile and the link, which is where the time goes. On the LLVM path a kept
program saves the link and the kept objects save the compile.

The one input no name here can see is the system the host compiler brings with it -- its standard
library and its C runtime. The compiler's identity stands in for it, as it does for the prepared
header, which is the same trade ccache makes by default. That identity is the file the compiler's
path resolves to, its modification time, and its size -- ccache's default `compiler_check = mtime`,
which its manual describes as hashing "the compiler's mtime and size, which is fast". Hashing the
executable's bytes instead would be exact, and Lyra's own executable is some 190 MB, read on every
build whether the program is then found or not.

### D3. A command hands back a private copy

`build` copies the program out of the store to where it was asked for, and `run` executes a private
copy of its own, in a directory of the run's that is removed with everything built in it when the
run ends. Neither hands back a link into the store. So clearing the store, or trimming it, never
breaks a program a checkout already holds or a run already started.

### D4. Writing is atomic, and a missing entry is a miss

An entry is written under a temporary name and renamed into place, so a concurrent reader sees a
whole entry or none. An entry that disappears between being found and being copied -- cleared or
trimmed by another process -- is treated as never having been there, and the program is built again.
Nothing a second process does to the store can fail a build.

### D5. The store trims itself

An entry records when it was last used. At most once a day, a build removes entries unused for five
days, which is Go's policy. Many checkouts and many configurations would otherwise grow the store
without bound, and nobody should have to remember to clear it.

### D6. There is no directory in the project

`build` writes one file, the program, to `-o` or to the working directory under the design's name --
the manifest's `name` for a declared design, the top for an anonymous one with a single top, and
nothing for an anonymous design with several, which then has to be named. It refuses to replace a
directory. `run` writes nothing. So there is no project directory to name, and no command to remove
one; `cache clear` empties the store, prepared headers and objects included.

### D7. A run can be told not to read the store

`--rebuild` compiles as though the store were empty and writes what it built. It is the one way to
take the store out of a question when a result is suspected of being stale, and it repairs the entry
while it is at it.

### D8. Running a design is building it and executing the program, on both paths

`run` on the LLVM path used to compose the modules into an execution session inside the compiler and
run the design there, writing nothing. Measured on this machine against compiling and linking the
same design:

| Design                | Session, whole run | Compile and link | Program alone |
| --------------------- | ------------------ | ---------------- | ------------- |
| 256 small modules     | 93 s               | 44 s             | 0.06 s        |
| One compute-heavy top | 8.9 s              | 0.58 s           | 1.9 s         |

The session was not faster on a first run, and a second run with the store pays the program alone.
Part of the second row's gap is the runtime the session used, which is the compiler's own build of
it rather than the shipped, optimized library a program links; the first row, which simulates almost
nothing, has no such confound. The session is removed rather than kept beside the program: two ways
to run one design are two things to keep agreeing, and the one kept is the one a user ships. A
tiered execution engine, if one is wanted, is its own subject and starts from the program.

The price is that running a design on the LLVM path needs a host C++ driver to link, as the C++ path
and every foreign source already did. Lyra takes the one named with `--cxx`, and otherwise the first
of `clang++` -- which alone can use a prepared header -- and `c++`, the name every system with a C++
compiler answers to. So tests run wherever the machine executing them has either, which is what
keeps them on remote executors rather than on a developer's machine.

### D9. On the LLVM path each unit's object is kept on its own

Added 2026-09-24. A unit's module is compiled to its object inside that unit's own pipeline, and the
object is kept under a name computed from the module's text, the build of the code generator, and
the level of the pipeline that compiled it -- everything the object is a function of, since a module
is complete in itself. A kept object of that name is taken instead of compiling.

Without it the program's name was the only name, and computing it needed every unit's module, so no
object could be written until the last unit was lowered: a join the per-unit pipeline has no reason
to have, and every module of the design held in memory until then. It is the answer every build tool
with a content-addressed store gives -- Bazel keys each action and ccache each compilation by what
that one step reads, and rustc's incremental build reuses each codegen unit's object -- so the only
join left after the barrier is the link. Measured on Ibex at `-j 4`: 6.9 s from an empty store, 2.9
s when every object is kept and the program is not, 2.5 s when the program is kept too; what is left
in the last two is lowering the design to know the names.

The C++ path does not do the same yet, because a translation unit includes other units' headers, so
what one compile reads is not one unit's output alone; its program is named from the whole emitted
project as before.

## Rejected alternatives

- **A directory per project, as Cargo keeps it.** It isolates checkouts by giving each its own copy
  of everything, so every checkout recompiles every configuration, and the layout has no axis for
  the configuration a verification test chooses. Its isolation is what D1 gets from immutable,
  content-named entries, without the recompilation.
- **Naming that directory `build/` or `target/`.** Moot once there is no directory, and wrong
  before: `build/` is written by other tools in the same repository, and `lyra clean` would delete
  their work.
- **Naming entries by the SystemVerilog sources and the options.** It needs every file the front end
  opened and every define in effect, and the first one missed produces a stale program that runs. D2
  names what the expensive step reads instead, which cannot miss what it did not read.
- **Linking the program out of the store instead of copying it.** Cheaper by the size of one file,
  and it makes every held program depend on the store not being cleared.
- **Keeping the execution session for `run`.** It was measured and was not faster, and keeping it
  keeps a second path whose runtime is not the one a program links.

## Consequences

- `compile` is `build`, and `-o` is optional. `run` on either path draws on the store, so a second
  run of an unchanged design pays neither the host compile nor the link.
- The prepared-header cache moves under the same store and the same trimming. `--pch-cache-dir`
  becomes `--cache-dir`, naming the whole store.
- Every conformance case links a program with whatever C++ compiler the executing machine has, so
  the corpus, foreign cases included, runs on remote executors as one target per path; only the
  tests about the prepared header, which is clang's alone, stay where clang is.

## Cross-references

- `a-unit-states-what-it-declares.md` -- what makes a unit's module a whole program, and the entry
  the program starts at.
- `a-precompiled-header-is-an-attempt.md` -- the store's other occupant on the C++ path, and why it
  can afford a fallback a program cannot.
- `project-file.md` -- where `name` comes from, and why `-o` and the cache location are invocation
  and machine properties.
- `../architecture/incremental_build.md` -- the query model this is an instance of: a memoized
  result per program, and on the LLVM path per unit's object, each keyed by content.
