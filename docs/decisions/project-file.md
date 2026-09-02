# `lyra.toml` declares a design, and the command line selects within it

Date: 2026-09-01. Status: accepted.

## The question this settles

A design of any size is described by a long command line: its sources in dependency order, the
include directories its headers live under, its defines, its top, and the compilation-unit model it
was written for. The count grows with the design and the line is respelled at every invocation.
North Star invariant 1 makes end-to-end iteration time the primary optimization target, so retyping
the design at every turn of the loop is a cost that falls squarely on it.

That much is uncontroversial and it is not the decision. **The decision is what the file
describes.** There are two candidates, they are not two file formats, and every other question here
follows from which one is chosen:

```text
A. The file describes the invocation.  It is a place to keep arguments. No new concept: the
   compiler still compiles a set of files with a set of options, and the file is sugar for typing
   them. Fields are named after command-line options. Precedence is "arguments given earlier".
   Paths resolve against the working directory, because that is where the invocation happens.

B. The file declares a design.  A design is an object with an identity, made of parts. The
   compiler compiles a design; a command line naming loose files is the degenerate case of an
   anonymous one. Fields are named after the design's parts. The command line selects within what
   the design declares. Paths resolve against the design's root, because they are parts of it.
```

`-f` command files are A, and so is every simulator whose project description is an argument list.
Cargo and npm are B.

**This entry chooses B.** The rest of it is that choice worked out, plus what it deliberately does
not build.

## Why B, on Lyra's own terms

The argument does not rest on any particular design being hard to type, and it must not: what a
downstream user's repository happens to contain is a measurement, never a premise for a permanent
decision here.

- **North Star invariant 5** already says the compiler is organized around independently compilable
  units with explicitly declared dependencies, and invariant 2 makes the compilation unit the
  compile-time scope. A design is the closure of units; a distributable set of units is that same
  relation one scale up. B is the shape the architecture already has, applied one level higher. A is
  a shape the architecture does not have anywhere.
- **`incremental_build.md` invariant 1** forbids implicit data flow: every query depends only on its
  explicitly declared inputs. A design whose input set is "whatever was on the command line this
  time" is implicit data flow at the very top of the query graph. An explicitly declared input set
  is what a manifest is.
- **The identity work is already done one level down.** `unit-signature.md` gives a unit a published
  signature that a referrer compiles against, and `specialization-identity.md` gives a
  specialization a content-derived identity. Both exist so that something outside a unit can name
  what is inside it without reading its source. That is precisely the machinery a cross-design
  reference would need, and it argues that the missing concept is the container, not the mechanism.
- **The stated destination is a body of SystemVerilog nobody will rewrite** -- a verification
  methodology library, a company's shared IP. That is not a set of files a user lists. It is
  something a design depends on, which is a relation A cannot express at all: two argument lists
  cannot be composed without knowing the contents of both.

The counter-argument is real and worth stating: A is smaller, needs no design today, and matches
what users coming from EDA already know. It loses because the choice is not a feature, it is a
concept, and concepts are decided by cost of deferral rather than by how many consumers want them
today. Adding "a design is an object" later re-bases every field name, every precedence rule, and
every path in every file anyone has written by then.

## What was there before, and why this entry re-derives instead of restoring

The record needs correcting, because the correction is what decides the method.

There was a project mode. It was built, alongside a non-project mode, before the Architecture Reset
of 2026-04-24 -- and it went with everything else that reset replaced. The pre-reset tree was
audited before being deleted, and what was judged worth carrying forward was carried forward; this
was not on that list. Its history is gone, so its shape is not recoverable and no argument here is
made from it.

What crossed the reset was the escape hatch, without the thing it escaped:

```text
if (!args.no_project)
  error: project mode is not implemented yet; pass --no-project to run in direct file mode
```

That stood for four months and eight days, until it was deleted on 2026-09-01. During that window
every invocation in every document and every test carried the flag, and `lyra check` with no
arguments answered "project mode is not implemented" instead of "no input files" -- the correct
diagnostic was unreachable behind a sign hanging on an empty room.

**So nothing was rejected, and the September deletion was not the decision.** The decision was the
reset's, four months earlier and deliberate. What September removed was the vestige. Calling that
"the rejected project-mode design" -- which the surrounding notes did, and which the first draft of
this entry inherited -- turns a stub into a verdict and then invites the next reader to derive from
it. Reading the shape of deleted code and calling the result a requirement is the same mistake as
reading current code and calling it a design; this entry therefore derives from requirements, above.

**The general shape, which is the part worth keeping.** A reset that audits what to carry forward
still lets vestiges through wherever nobody thought to audit -- here, the CLI's option list. And a
vestige is worse than an absence, because it advertises. A stub reading "not implemented yet" is
indistinguishable from a reservation for planned work, so for four months the surface looked
claimed, and every document that described it described a feature that had already been decided
against. **After a reset, the option list is part of the audit.**

The one thing worth keeping from the vestige is its name. Whoever wrote "project" was reaching for
an object rather than for an argument list, and that instinct is what section B above arrives at
independently. Only the mechanics -- a mode, a default that fails, a flag to escape it -- were
wrong.

## The axis inside B: shared versus local

Choosing B says the file describes the design. It does not yet say which facts are the design's.
That line is drawn by who the value is true for:

```text
true of the design, for everyone who builds it   -> the manifest
true of this invocation, or of this machine      -> the command line
```

Lyra's spelling of it: **does the option change what program is elaborated and lowered, or how this
run produces and executes it?** Sources, include directories, defines, library search, the top, the
compilation-unit model, the language version and the assertion policy all change the program. `-o`,
`--backend`, `--release`, `--no-pch`, `--format`, `--color`, `--cxx` and `--pch-cache-dir` change
only how this run produces or executes it -- and `--backend` cannot change the program at all, since
North Star invariant 3 makes correctness independent of which path lowers it.

Every established manifest format draws the same line, and draws it as a file boundary: Cargo splits
`Cargo.toml` from `.cargo/config.toml`, npm splits `package.json` from `.npmrc`. A manifest is
committed and shared, so a value that differs per developer poisons it and a value that differs per
run makes it a record of somebody's last experiment.

## The decisions

```text
D1. `lyra.toml` declares a design: a named object whose parts are its sources, its search paths, its
    defines, its roots, and the foreign sources its DPI-C imports resolve against. There is one way
    to run the compiler. A command line naming sources compiles an anonymous design, which stays the
    ordinary case; a manifest that is absent is simply no declaration, never a mode and never a
    failure.

D2. A field is admitted only if it is true of the design for everyone who builds it. An invocation
    property (`-o`, `--release`, `--backend`, `--format`) and a machine property (`--cxx`,
    `--pch-cache-dir`, `--no-pch`) are refused by name.

D3. Every relative path resolves against the directory of the manifest that declares it, never
    against the process's working directory. A path Lyra reports is shown resolved, so the base it
    used is visible rather than inferred.

D4. The manifest declares; the command line selects within the declaration. Material -- what the
    design is made of, and where to look for more of it -- accumulates: the manifest's values first,
    the command line's after, both in effect. Selection -- which of several declared things to do
    this time -- is replaced outright by the command line.

D5. A top is selection, not material. IEEE 1800-2023 permits multiple top-level blocks (3.11), so a
    top is a list on both sides, but `--top` on the command line replaces the manifest's list rather
    than extending it.

D6. The manifest is found by walking up from the working directory to the first `lyra.toml`,
    stopping at a directory holding `.git` or at the filesystem root. The first one found is the
    whole answer; two manifests are never merged. `--config <path>` names one directly and skips the
    walk. There is no flag to suppress discovery.

D7. Naming a source input on the command line skips discovery entirely: that command line is already
    a complete design, and it is anonymous by construction. Positional files and the front end's
    command-file options are source inputs; a search path is not.

D8. An unknown key, and a key naming a property D2 refuses, is an error that names the rule. The
    top-level table namespace is closed.

D9. The manifest supplies compiler inputs and nothing else. It never changes the simulation's
    environment: the program runs in the user's working directory, with the argv given after `--`,
    identically whether or not a manifest was used.
```

## The schema

```toml
# Every relative path below resolves against this file's own directory.

[design]
name      = "soc"                                    # identity, required
top       = ["soc_tb"]                               # selection
files     = ["rtl/alu.sv", "rtl/regfile.sv", "..."]  # material, ordered
incdir    = ["rtl/include"]                          # material
defines   = ["TRACE", "WIDTH=8"]                     # material
undefines = ["VENDOR_HACK"]                          # material
params    = ["DEPTH=16"]                             # material
libdir    = ["vendor/prim"]                          # material
libext    = [".sv", ".v"]                            # material

[compile]
std         = "1800-2023"                            # selection
timescale   = "1ns/1ps"                              # selection
single_unit = true                                   # selection
assertions  = "check"                                # selection: "check" or "skip"

[dpi]
sources = ["tb/dpi_stubs.c"]                         # material, the foreign half
```

`name` is what makes this a declaration rather than a bag of options, and it is the field a reader
of A would leave out. It is **required**, because an optional identity is not one: a file that
declines to say what it declares is the bag of options the command line already carries better. It
has a reader the day it is written -- a declaration that named no sources says which design named
none, which matters exactly when the declaration in effect is several directories above the caller
and the bare message would describe a design the reader is not looking at. Every later cross-design
mechanism refers to the same field.

`files` is an ordered explicit list, and no path in the declaration may be a pattern. A pattern
names whatever the filesystem happens to hold, which makes the design a function of the directory
rather than of the file that declares it, and it leaves source order to the filesystem when source
order is significant. Finding a module by name is what `libdir` and `libext` are for, so nothing is
lost.

`assertions` names what the compiler does with an assertion rather than what it currently cannot do.
`check` is the default and today refuses the forms Lyra does not implement; `skip` elides them,
which changes no behaviour because an assertion observes and never drives.

**This is not a configuration in the standard's sense, and does not become one.** LRM 3.10 and
Clause 33 define library map files and `config` blocks, which bind particular instances to
particular source and are named as invocation options in the way this manifest's fields are. Lyra
implements none of it. A configuration chooses _which definition an instance binds to_, which is a
language construct with its own syntax and elaboration semantics; the manifest only says what the
compiler is pointed at. If configurations are ever implemented they are SystemVerilog source, read
by the front end like any other, and a `config`'s presence is not a reason to grow a field here.

## The schema is a partition of the front end's option surface, not a selection from it

The fields above are not chosen by taste. The front end registers roughly ninety options, and D2
partitions all of them; what the schema carries is the design side of that partition, minus what
nothing yet reads.

The half of the partition that is easy to get wrong is the **tool limits** -- maximum hierarchy
depth, generate steps, constant-expression depth and size, instance array bounds, error limit. They
look like design properties, because it is a large design that runs into them. They are not: raising
a limit does not change what the design computes, only whether the tool gives up before saying so.
By D2's own test they are invocation properties, and a manifest carrying them would be recording one
machine's patience as a fact about the design. The same reading puts diagnostics (`-W`, warning
suppression, waiver files), dependency-file output, thread count, and the compatibility shims for
reading another tool's command files on the invocation side.

The design side is larger than what is implemented here, and the remainder is named so the next
person adds a field under the rule rather than re-deriving the line:

- **Named libraries** -- library files, library maps, library order, the default library name. The
  search half of the same system is carried, in `libdir` and `libext`; what is absent is everything
  that requires a design to be divided into named libraries, because LRM 33 library support is not
  implemented and a field for it would declare something the compiler cannot act on.
- **The dialect knobs** -- legacy protect envelopes, translate-off formats, ignored directives,
  keyword-version mapping, local-include and include-order behaviour. Design material, because each
  changes what program the source text denotes. Absent because nothing has needed one.

## One table per question the design answers, not per consumer

`[compile]` holds `std`, `timescale` and `single_unit`, which the front end reads, beside
`assertions`, which Lyra's own lowering reads. Organizing by ownership would split them, and that is
the wrong axis for a file a person writes: from the design's side, "compiled as SV-2023, as a single
unit, with its assertions skipped" is one statement, and which component acts on each part is an
implementation fact the writer has no reason to know.

## A policy field is named for the construct family, and admitted by one test

`assertions = "check" | "skip"` is the first of a kind that will grow -- coverage is the obvious
next -- so what matters is the rule for adding the second, not the first field's spelling.

**A construct family may be given a policy only if eliding it cannot change what the design
computes.** LRM 16 assertions pass: an assertion observes and never drives. LRM 19 covergroups pass
the same test for the same reason. A family that fails it does not get a policy at any spelling,
because the option would then be a way to ask for a different answer.

**Each family gets its own field and its own values; there is no shared on-off switch.** The values
are not the same question: an assertion is checked or elided, while coverage is collected or not,
and forcing both onto one `on|off` is a tag beside spare fields. What the families share is the
admission test above, which is a rule rather than a type. Two families spelled identically are not
evidence for unifying them; three consumers would be.

## Precedence, worked

| Field                                   | Kind      | `lyra.toml` says | command line says   | result      |
| --------------------------------------- | --------- | ---------------- | ------------------- | ----------- |
| `defines`, `incdir`, `libdir`, `libext` | material  | `TRACE`          | `-D DEBUG`          | both        |
| `files`, `[dpi] sources`                | material  | the source list  | (D7: none)          | the file's  |
| `top`                                   | selection | `soc_tb`         | `--top alu`         | `alu` alone |
| `std`, `single_unit`, `assertions`      | selection | `check`          | `--assertions skip` | `skip`      |

The test that assigns a field is whether a second value adds to the first or chooses instead of it.
A second include directory searches both; a second define defines both; a second top is where the
question gets interesting, because the LRM genuinely allows several and so does the front end.

**It is still selection, and the reason is what the alternative does.** A manifest names the
testbench as the design's root; a developer wants one module on its own and types `--top alu`. Under
accumulation the whole design elaborates as well, so the option the developer typed has no visible
effect at all -- and a command that silently does nothing is the worst outcome class available here,
worse than an error. Under replacement it does the obvious thing, and both roots stay expressible on
either side with `--top A --top B`. The manifest names the design's roots; the command line says
which of them to elaborate this time, the way `cargo run --bin x` selects among the binaries a
manifest declares rather than adding one.

**A field is admitted only if the command line can express both of its values, or if flipping it per
invocation is not a real operation.** A flag that only sets true cannot un-set what a manifest set,
so this is a constraint on the schema rather than a hole in it. It has one live consequence: the
assertion policy is a named value on the command line rather than a flag, because choosing to see
what Lyra refuses is a real thing to do on one run and not on the next. `single_unit` needs no
negative spelling: a design is written for one compilation-unit model and does not alternate between
them, and LRM 3.12.1 requires a tool to offer both models, not a caller to switch per run.

## Where the file is, and what a path in it means

D3 and D6 are one answer read from two sides, and the front end already demonstrates both halves of
the choice. slang has two command-file options that differ in exactly this: `-f` resolves the paths
inside the file against the process's working directory, `-F` against the file's own directory. The
`-f` form is the one that makes a file mean different things depending on where it was invoked from,
which is what a design's declaration may never do, since it is committed and read from every
subdirectory of the design.

So the walk in D6 is safe: a manifest found three directories up still names its own parts
correctly, because it never depended on where the walk started. When no manifest is found, the
diagnostic says where the walk began and where it stopped, so a `.git` boundary is visible rather
than mysterious.

**D7 is what keeps the walk from reaching where it is not wanted.** A command line naming sources is
already a complete design, and merging a declaration found somewhere above it produces a third
design nobody asked for. Skipping discovery there makes `lyra run --top Test test.sv` mean the same
thing in every directory on the machine, makes a test invocation independent of every file outside
its own inputs -- which incremental-build invariant 1 requires anyway, since a discovered manifest
is an input and an input has to be declared -- and removes the need for a flag to turn discovery
off. That last point is the direct lesson of the placeholder: an option every invocation must pass
is not an option.

## What the front end's parser forces

The tidiest-sounding implementation of D4 is to splice the manifest's values into the argument list
ahead of the real command line and let one parser sort it out. It does not work, and the reason is
worth recording because nothing about it is visible from the design.

Lyra's options are registered on the same `slang::CommandLine` as the front end's, and there a
single-valued option keeps the **first** value it is given: a second one is an error, or with
duplicates ignored, is silently dropped. Manifest-first therefore makes the manifest win every
selection field -- the exact inverse of D4 -- and command-line-first makes an accumulating field's
manifest values land after the caller's rather than before. So the merge is an explicit per-field
one over parsed values, never argument splicing. That is not a workaround: it is the same conclusion
D4 reaches from the other end, since a rule stated per field has to be applied per field.

The second constraint is on D3. An option registered as a file path is canonicalized **against the
process's working directory** when it is parsed. A manifest path handed to the parser as written
would therefore resolve against wherever the user happened to stand, which is precisely what D3
forbids. Every path read out of a manifest is made absolute against that manifest's own directory
before it reaches the front end, and that step is the only thing standing between a declaration and
the `-f` behaviour this entry rejected.

## The direction, and what is deliberately not built

Choosing B commits to a concept, not to a package manager. Whether Lyra grows publishing, fetching
and a dependency graph is open; what follows makes either future cheap.

Five moves buy it, and each is worth more than the field it protects:

1. **Fix the boundary, not the feature.** What `[dependencies]` would look like is unknowable; that
   "what the design is" and "how this run produces it" are different questions is not. Every future
   feature lands on one side of that line.
2. **Close the namespace.** D8 makes an unknown key an error, usually justified as typo detection.
   Its larger effect is that a table this version does not know -- `[dependencies]` and
   `[workspace]` being the obvious two -- is reserved for free, and an older Lyra meeting a newer
   manifest fails loudly instead of building a subtly different design. That is also why there is no
   schema-version field: strict keys already give the loud failure a version field would give, and a
   version with one value is speculation.

   **There is no `[package]` to reserve, and that is a decision rather than an omission.** Cargo
   separates `[package]` from `[lib]` and `[bin]` because one package holds several build targets; a
   `lyra.toml` declares exactly one design, so a second identity would have nothing to distinguish.
   `[design] name` is the identity, and what an ecosystem adds is `[dependencies]` -- a statement
   about _other_ designs. Splitting identity out later would move `name` between tables, which is
   the migration reserving space is supposed to avoid.

3. **Resolve every reference against what declares it.** D3 decides on its own whether a second
   manifest could ever contribute sources. Without it, dependencies are impossible; with it, they
   are ordinary.
4. **Reserve space, never fields.** No `version`, no registry, no lockfile, no `[workspace]`,
   because nothing reads them. A namespace costs nothing to reserve; a field with no reader costs a
   migration.
5. **Refuse inference.** Manifests are never merged (D6). A workspace, if one is ever wanted, is
   declared by a root manifest -- not inferred by walking a tree, the way a configuration cascade
   does. An ecosystem needs relationships that can be published, and an inferred relationship cannot
   be.

The move that would forfeit all of it is the tempting one: letting an invocation option into the
file because it is convenient once. `out_dir` and `cxx` are the two that will be asked for. If
per-machine defaults are ever genuinely wanted they belong in a separate configuration file, which
is the split every package manager arrived at.

## Rejected alternatives

- **A. The file as a supplement to the argument list.** The live alternative, and the one the first
  draft of this entry chose while calling itself a manifest. It is smaller and needs nothing decided
  today. It loses on cost of deferral: field names, precedence and path resolution all differ under
  it, so adopting it now and B later rewrites every manifest anyone has written. It also cannot
  express a dependency at all, and the destination is a body of SystemVerilog that is depended on.

- **A mode.** The shape the vestige's name suggests, and the one a reader will propose again. It
  makes the file a second way to run the compiler, which forces a flag to escape it, which every
  invocation then carries. The escape flag is the tell, visible before any of the history is known:
  an option every invocation must pass is not an option.

- **Command files (`-f`) as the whole answer.** They already exist, they already splice Lyra's own
  options as well as the front end's, and they genuinely solve the retyping. Two things they do not
  solve: discovery, since `-f design.f` still has to be typed and typing it is the cost being
  removed; and declaration, since a flat argument list has no identity, no schema, and no way to
  keep an invocation option out. They keep working unchanged; they are the front end's and cost
  nothing.

- **Sources on the command line accumulating onto the manifest's.** The EDA convention for filelists
  and the wrong rule for a discovered file. Every other merge failure produces a missing option;
  this one produces a different design, assembled from a file the user may not have known was above
  them.

- **Per-file discovery, walking up from each source.** What `clang-format` does, and correct there
  because formatting is per file. A design has one root, and per-file discovery would let two
  sources in different directories disagree about the top.

- **A cascade that merges every manifest up the tree.** It makes the effective declaration
  unreadable from any single file, and it collides with workspace semantics later, which are
  declared rather than inferred.

- **`--no-config`.** Nothing has to pass it once D7 holds, and adding it before a caller needs it
  repeats the mistake this entry exists to undo.

- **Globbing in `files`.** Rejected for the reason the schema section gives: a pattern names
  whatever the filesystem holds, and it leaves source order to the filesystem when source order is
  significant.

## Consequences

- From a design's root, `lyra run` is the whole command line. From a subdirectory of it, so is it.
- `lyra check` with no arguments and no manifest still answers "no input files", which is the
  diagnostic the placeholder made unreachable.
- `--disable-assertions` is replaced by `--assertions check|skip`, and every caller of it moves in
  the same change.
- The TOML parser costs nothing to add: `tomlplusplus` was already a declared dependency, left
  behind by the same removal as the vestige and wired into no target since. This is the change that
  gives it a reader. The argument parser the first CLI used, superseded when the front end's command
  line became Lyra's own, was dead beside it and goes at the same time.
- The manifest, once resolved, is a declared input of the compilation like any source file, so
  nothing about incremental reuse has to discover it a second time.
- These are ordinary tests, not conformance cases: the command line is not an IEEE 1800 requirement,
  which `conformance-case-shape.md` D9 already settles.

## Naming

`lyra.toml`, lowercase, which is what every document and every existing file already spells. A
capitalized name would be one more thing that behaves differently on a case-sensitive filesystem
than on a case-insensitive one, for no benefit.

## Cross-references

- `../architecture/north_star.md` -- invariants 2 and 5, which are why a design is an object rather
  than an argument list; invariant 1, which is why the retyping is worth removing; invariant 3,
  which is why `--backend` cannot be design material.
- `../architecture/incremental_build.md` -- invariant 1, which is why a discovered manifest has to
  become a declared input.
- `unit-signature.md` -- the identity machinery one scale down, and what a cross-design reference
  would be built from.
- `conformance-case-shape.md` -- D9, which puts command-line behaviour outside the corpus.
- `dpi-foreign-boundary.md` -- what `[dpi] sources` supplies symbols to.
