# Project Model

A design of any size is a long command line: its sources in dependency order, the include
directories its headers live under, its defines, its top, the compilation-unit model it was written
for, and the libraries it uses. `lyra.toml` declares that once, as an object with an identity made
of parts, which the compiler compiles and the command line selects within. A design is the closure
of units with explicitly declared dependencies (`north_star.md` invariants 2 and 5), and a library
is that same relation one scale up, which is what a body of SystemVerilog somebody else depends on
-- a methodology library, a company's shared IP -- has to be. Fetching a library from a registry is
not done: a dependency is a library already on the machine or a release Lyra ships, and everything
below holds whichever way that later changes.

This doc owns what a project declares, how a library is read as a dependency, how a supported
library is taken as a port, and how the command line combines with the declaration. What a build of
the declared design does is [building_a_program.md](building_a_program.md)'s, and how a kept result
is found again is [incremental_build.md](incremental_build.md)'s; a resolved manifest is a declared
input of the compilation like any source file.

## A file declares a library, and the design run of it apart

A library is "a named collection of cells" (LRM 33.2.1), and the `design` statement of a
configuration names which cells are the roots (LRM 33.4.1.1). The file takes both words as they are:
`[library]` is the named collection, which is what a dependent receives, and `[design]` is the roots
and what only the roots need -- their testbenches, the defines and include directories only they
read, the foreign code only they link -- which a dependent never receives.

**The declared library is the build's default library, under its own name.** Every cell compiled
where the file is, the library's and the design's alike, belongs to it, so roots, checking and
selection behave as for any source; a library a build depends on is a named library beside it, as
Cargo checks the root package whole and treats only its dependencies as libraries. The name is
required and is an identifier: the front end is told it as the name of the library the cells are in,
a configuration in the source may name it (`design soc.soc_tb; default liblist soc;`), and a
dependent's declaration names the library by it. **What `[design]` holds is added to what
`[library]` holds**, so a define of the design's reaches the library's sources in the build made
here, as a `-D` does. `[design]` is optional; without it the roots are the standard's, every cell
nothing instantiates (LRM 23.3.1).

**A command line naming sources compiles an anonymous design**, which stays the ordinary case. There
is one way to run the compiler, and a manifest that is absent is no declaration, never a mode and
never a failure: `lyra check` with no arguments and no manifest answers "no input files".

## What a library of a build is

A named body of SystemVerilog is used by another through its name, and what its cells are is decided
by its own declaration alone, whoever uses it and whatever is used beside it; a library whose cells
change with their user can be neither published nor compiled once for two users. The standard's
configuration language cannot extend a library list per library (LRM 33.4.1.4, 33.4.1.5), and with
no configuration every instance searches the libraries in one order for the whole build (LRM
33.6.1), so the search order per library is the tool's to supply, which LRM 33.8.1 expects of a
tool. A library of a build, the one being built and one it depends on alike, is:

```text
its cells           in a library of its own name
its text            read in compilation units of its own (LRM 3.12.1): one for all its files where it
                    declares single_unit, else one per file
read under          the macro naming the tool, the command line's defines and include directories,
                    then its own
never under         another library's defines or incdir, or a macro another unit defined
a cell it names     searched for in its own library, then in each library it declared, in the order
                    it wrote them, and nowhere else
a cell no file of   searched for by name in its own searchdir, read as its other text is, and in its
its lists holds     library
its dpi files       compiled where a source, under its dpi_defines and dpi_incdir alone, and
                    linked into the program
```

Two things differ by where a library stands. The one being built is also read under its `[design]`,
and a dependency's `[design]` is not read at all, since roots, their parameters and their sources
are for where a library is developed. A cell held only by a library the instantiating one did not
declare is not found, so a library cannot come to rely on what a dependency happens to depend on.
One thing does cross: an include a library does not find in its own directories is searched for in
every exported directory of the build, which can supply a header a library is missing and never
replace one it has, so a library that builds alone is read the same here. The command line stays the
invocation's and reaches every library, as it does in every SystemVerilog tool.

**What crosses to a dependent is cells and exported include directories.** Every cell is public,
because the language has no private one. `incdir` is searched by the library's own text alone and
`export_incdir` by the library and everything that reaches it, after what each was given for itself,
since a library's macros are part of what it offers and its private headers are not, as Bender draws
the same line. No define crosses, and no setting of `[compile]`.

**One build has one language version and one default time scale.** The front end reads one build
under one of each -- the command line's where it gives one, else the root's -- so a dependency
declaring a `std` or a `timescale` other than the build's is refused rather than read as a different
library from the one its own build makes, and one leaving either undeclared is read under the
build's. A library that needs its own time unit states it in its text with `timeunit` or
`` `timescale ``, since a design element stating none takes the tool's default (LRM 3.14.2.3, 22.7);
one that needs its own keyword version states it with `` `begin_keywords `` (LRM 22.14).
`coalesce_comb_updates` is each library's own: each unit is lowered under its own library's answer
([scheduling.md](scheduling.md)). `assertions` is the root's, since eliding an assertion changes
nothing the design computes.

A cell is named `cell` in the build's default library and `library.cell` in any other, so a
library's cell has one name where the library is built and another where it is depended on, and the
two builds are not expected to share a kept unit. Two libraries declaring a package of one name
cannot be used together: the standard has one package name space and forbids a configuration to
rebind a package (LRM 3.13, 33.4), and the front end reports the duplicate.

## A dependency names a path or a version

```toml
[dependencies]
prim = { path = "../prim" }    # a directory whose lyra.toml declares library prim
uvm  = { version = "1.2" }     # a release of a library Lyra supports
```

The key is the library's name, because that name is written in source and is the library's own to
choose; a declaration found under another name is refused. A dependency naming both a path and a
version, or neither, says nothing a build could follow and is refused. A version is an upstream
release's own spelling, matched exactly, with no range, because a methodology library's releases are
few and a project moving between them reviews the move. A build holds one library of a name, each
read once however many libraries depend on it, and a library that reaches itself is refused.

**A supported library is a port Lyra ships.** A testbench is written against libraries its author
does not edit, published once and used as published, and a release written before Lyra existed may
need preparing before Lyra can build it -- its foreign code compiled with the right macros, a back
end the standard does not require replaced by one that it does. That preparation is the toolchain's,
never the user's, and once a project has built against one preparation it keeps getting that one. A
port is an upstream release as Lyra prepares it, identified by the library's name, the release, and
a port version counting Lyra's preparations of that release from 1, as vcpkg's `port-version` and
the Bazel Central Registry's `.bcr.<n>` do. Its content is an ordinary library declaration beside
the release's files with Lyra's patches applied, so a port is read exactly as a library of the
project's own is.

**A published port version is never changed.** Its content's digest is committed beside its recipe
and stated in the metadata the shipped tree carries, and a test of the merge gate reads every
shipped port back against that digest, so a changed recipe can only ever be a new port version, as
crates.io and the Bazel Central Registry never change a published version. The digest is SHA-256
over the tree's files in the order of their paths relative to the tree, written with `/`, each path
and each file's bytes preceded by its length, links followed, and no absolute path or time entering
it: it is published and kept, so any other tool can recompute it. A store finds a port at
`<name>/<version>/<port_version>`, and a tree whose metadata names another port is refused, so a
directory renamed or copied under another name is never taken as the port that name says.

**A project's lock records the port it took, and only the root's lock counts.** `lyra.lock`, beside
the root's `lyra.toml`, records for each supported library the build took its name, release, port
version and digest; Lyra writes it and the user commits it, as Cargo writes `Cargo.lock`. A release
the lock records is taken at the port version it names, and one it does not record is taken at the
latest port version shipped and recorded. The lock is written only after the whole graph resolves,
whole and by replacing the file, so a failure leaves the lock that was there, and an entry for a
library no longer declared is dropped. A dependency's own lock is not read, since the build has one
answer per name. A port is verified against the digest it was published with before the lock's, so a
modified installation is refused even where nothing is locked yet and is never written into a lock.
The lock states its format, because a later Lyra may change what an existing key means. A run told
`--locked` refuses where the lock would have to change, and writes nothing.

**The store a port is taken from is a property of the run.** By default the ports are those shipped
beside the Lyra binary ([runtime_distribution.md](runtime_distribution.md)). `--ports <dir>`
replaces that store for one run -- an organization's own prepared ports, or a test's -- and is not a
search path: a locked port missing from it is an error, never a reason to look elsewhere. The lock
does not record the store, because the digest already says whether the content is the one recorded.

## The manifest holds what is true of the design for everyone who builds it

A manifest is committed and shared, so a value that differs per developer poisons it, and one that
differs per run makes it a record of somebody's last experiment. The line is drawn by one question:
**does the option change what program is elaborated and lowered, or how this run produces, executes
or reports on it?** Sources, include directories, defines, library search, the top, the
compilation-unit model, the language version and the assertion policy change the program. `-o`,
`--backend`, `-O`, `--lto`, `--sanitize`, `--rebuild`, `--no-pch`, `--format`, `--color`, `--jobs`,
`--cxx`, `--cache-dir`, `--ports`, `--locked`, `--time-trace` and `--stats-file` change only how
this run produces it -- `--backend` cannot change the program at all (`north_star.md` invariant 3)
-- and each is refused in the file by name, with the rule. The front end's tool limits, diagnostics
and thread count have no key at all, and the file refuses one as it refuses any key it does not
know: raising a limit changes only whether the tool gives up before saying what the design computes,
so a manifest carrying one would record one machine's patience as a fact of the design. Every
established manifest format draws the same line as a file boundary: Cargo splits `Cargo.toml` from
`.cargo/config.toml`, npm `package.json` from `.npmrc`.

**An unknown key is an error, and the top-level table namespace is closed.** A key this version does
not know is reserved for free, and an older Lyra meeting a newer manifest fails loudly instead of
building a subtly different design, which is also why there is no schema-version field.

**A construct family may be given a policy only if eliding it cannot change what the design
computes.** An assertion observes and never drives (LRM 16), so `assertions = "check" | "skip"`
passes; a covergroup fails, since `get_coverage` and `$get_coverage` hand the design what was
counted (LRM 19.8, 19.9). Each family gets its own field and its own values, never a shared on-off
switch. `coalesce_comb_updates` is no policy in this sense but a dialect: a different schedule of
the standard's, asked for as the one the design is written in ([scheduling.md](scheduling.md)).

**One table per question the design answers, not per consumer.** `[compile]` holds what the front
end reads (`std`, `timescale`, `single_unit`) beside what Lyra's lowering reads (`assertions`,
`coalesce_comb_updates`), because from the design's side "compiled as SV-2023, as a single unit,
with its assertions skipped" is one statement, and which component acts on each part is not the
writer's to know.

## The command line selects within the declaration

**Material accumulates; selection is replaced.** What the design is made of, and where to look for
more of it, accumulates: the manifest's values and the command line's are both in effect, and where
the two give one name a value -- a macro, a parameter -- the command line's stands, being what this
invocation said over what the file says every time. Which of several declared things to do this time
is replaced outright. A top is selection, though the standard permits several (LRM 3.11): a
developer typing `--top alu` under a manifest naming the testbench wants one module elaborated, and
under accumulation the option would have no visible effect, so `--top` replaces the manifest's list,
as `cargo run --bin x` selects among the binaries a manifest declares.

| Field                              | Kind      | `lyra.toml` says | command line says               | result      |
| ---------------------------------- | --------- | ---------------- | ------------------------------- | ----------- |
| `defines`, `incdir`, `searchdir`   | material  | `TRACE`          | `-D DEBUG`                      | both        |
| `files`, `dpi`                     | material  | the source list  | (none: naming one skips it)     | the file's  |
| `dpi_defines`, `dpi_incdir`        | material  | `MODEL_TRACE`    | (none)                          | the file's  |
| `top`                              | selection | `soc_tb`         | `--top alu`                     | `alu` alone |
| `std`, `single_unit`, `assertions` | selection | `check`          | `--assertions skip`             | `skip`      |
| `coalesce_comb_updates`            | selection | `true`           | `--coalesce-comb-updates=false` | `false`     |

**A field is admitted only if the command line can express both of its values, or flipping it per
invocation is not a real operation.** A flag that only sets true cannot unset what a manifest set,
so `--assertions` takes a named value and `--coalesce-comb-updates` takes `=false`; `single_unit`
needs no negative spelling, since a design is written for one compilation-unit model.

The merge is per field, over parsed values, never by splicing the manifest's values into the
argument list: Lyra's options share the front end's command line, where a single-valued option keeps
the first value it is given, so splicing would make the manifest win every selection or land its
material after the caller's.

## Where the file is, and what a path in it means

**The manifest is found by walking up from the working directory to the first `lyra.toml`**,
stopping at a directory holding `.git` or at the filesystem root. The first one found is the whole
answer, and two manifests are never merged, so the effective declaration is always readable from one
file. `--config <path>` names one directly and skips the walk; when none is found, the diagnostic
says where the walk began and where it stopped. **Naming a source input on the command line skips
discovery entirely** -- positional files and the front end's command-file options are source inputs,
a search path is not -- because that command line is already a complete design, and merging a
declaration found above it would make a third nobody asked for. So `lyra run --top Test test.sv`
means the same in every directory, and no flag to turn discovery off is needed.

**Every relative path resolves against the directory of the manifest that declares it, never against
the process's working directory.** A declaration is committed and read from every subdirectory
beneath it, so it may never mean different things depending on where it was invoked from; this is
the difference between the front end's `-F` and `-f`. Every path read out of a manifest is made
absolute against its directory before it reaches the front end, which would otherwise canonicalize
it against the working directory, and a path Lyra reports is shown resolved. `files` is an ordered
explicit list, and no path may be a pattern: a pattern makes the library a function of the directory
rather than of its declaration, and leaves source order to the filesystem where source order is
significant. Finding a cell by name is what `searchdir` and `searchext` are for, the search every
Verilog tool spells `-y` and `+libext+`.

**The manifest supplies compiler inputs and nothing else.** It never changes the simulation's
environment: the program runs in the user's working directory, with the argv given after `--`,
identically whether or not a manifest was used. Every text of a build -- the library being built, a
library it depends on, a file named on the command line -- is read with `__lyra__` defined as 1,
beside the front end's own `__slang__`, so a source selects on the tool that will run it by the
first and on the parser reading it by the second; an invocation's `-U` takes either away and its
`-D` gives neither another value, and no macro states a version.

**What a build makes names a source by its path from the directory of the manifest declaring the
design**, and a source outside that directory by its absolute path; a design the command line names
is named from the directory the command was given in. That is every name a build writes into what it
makes: where a report says it was made, what `__FILE__` expands to (LRM 22.13 leaves the form to the
tool) and the `__FILE__` of foreign code Lyra compiles. As Cargo hands rustc a workspace's files by
their path from the workspace's root, a build is then the same build from a subdirectory or from
beside it, where a name from the working directory would make every unit new. A message Lyra prints
while it builds keeps the path from the working directory, since that is what an editor opens, and a
file is named as the front end opens it, its links followed.

## The schema

```toml
# Every relative path below resolves against this file's own directory.

[library]
name      = "soc"                                    # identity, required, an identifier
files     = ["rtl/alu.sv", "rtl/regfile.sv", "..."]  # material, ordered
incdir    = ["rtl/include"]                          # material, this library's alone
export_incdir = ["include"]                          # material, a dependent's too
defines   = ["WIDTH=8"]                              # material
undefines = ["VENDOR_HACK"]                          # material
searchdir = ["vendor/prim"]                          # material
searchext = [".v"]                                   # material
dpi       = ["rtl/model.c"]                          # material, the foreign half
dpi_defines = ["MODEL_TRACE"]                        # material, the foreign half's macros
dpi_incdir  = ["rtl/c_include"]                      # material, the foreign half's headers

[design]
top       = ["soc_tb"]                               # selection
params    = ["DEPTH=16"]                             # material
files     = ["tb/soc_tb.sv"]                         # material, read after the library's
defines   = ["TRACE"]                                # material
dpi       = ["tb/dpi_stubs.c"]                       # material

[dependencies]
prim      = { path = "../prim" }                     # material: another library, by name
uvm       = { version = "1.2" }                      # material: a supported library, by release

[compile]
std         = "1800-2023"                            # selection
timescale   = "1ns/1ps"                              # selection
single_unit = true                                   # selection
assertions  = "check"                                # selection: "check" or "skip"
coalesce_comb_updates = false                        # selection: the dialect the library is written in
```

The keys from `files` to `dpi_incdir` are one thing, a source set: source text and what reading it
needs, the SystemVerilog and the foreign code its imports call alike. `[library]` holds one beside
its name and `[design]` one beside its roots, the same keys meaning the same in both, and `params`
is the design's alone, because a parameter override is applied to a root. The foreign half's macros
and include directories are its own, since its preprocessor is another program with another name
space of macros, and they are the part of a C build true for everyone who builds it, as Bazel's
`defines` and `includes` stand beside `copts`; there is no key for a compiler flag. A `config` block
(LRM 33.4) chooses which definition an instance binds to and is SystemVerilog source the front end
reads; the manifest says which library is built and what it is made of, and never grows a field for
a configuration's work.

## Forbidden shapes

- **An invocation or machine property in the manifest** -- an output path, a compiler, an
  optimization level, a tool limit. It fails the requirement that a committed manifest is true for
  everyone who builds the design.
- **A path in a manifest resolved against the working directory.** It fails a declaration meaning
  the same wherever it was invoked from, which discovery from a subdirectory depends on.
- **A pattern in a source list.** It fails a library decided by its own declaration, and source
  order where order is significant.
- **Sources on the command line added to a discovered manifest's.** It fails the requirement that a
  command line naming sources is a complete design: it builds a third design assembled from a file
  the user may not have known was above them.
- **Manifests merged up the tree, or discovered per source file.** The first fails a declaration
  readable from one file; the second lets two sources in different directories disagree about the
  root.
- **A mode the manifest switches on, with a flag to escape it.** It fails one way to run the
  compiler: every invocation would carry the flag.
- **A dependency's `[design]`, defines or `[compile]` settings reaching its dependent, or a cell
  found in a library the instantiating one did not declare.** It fails the requirement that a
  library's cells are decided by its own declaration alone.
- **A version range, or a resolver choosing among releases.** It fails a project keeping the port it
  built against: a range resolves differently as releases appear.
- **A supported library patched in the project, or the user asked to patch it.** It fails using a
  library as published; preparing it is the toolchain's.
- **A published port version changed in place, or a port tree taken on its directory's name alone.**
  Each fails the requirement that what a lock records is the content a later build gets.
