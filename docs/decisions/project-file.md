# `lyra.toml` declares a library and what is run of it, and the command line selects within it

Date: 2026-09-01, revised 2026-10-09 (what the declared object is). Status: accepted.

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

B. The file declares an object.  It has an identity and is made of parts. The compiler compiles
   that object; a command line naming loose files is the degenerate case of an anonymous one.
   Fields are named after the object's parts. The command line selects within what is declared.
   Paths resolve against the object's root, because they are parts of it.
```

`-f` command files are A, and so is every simulator whose project description is an argument list.
Cargo and npm are B.

**This entry chooses B.** The rest of it is that choice worked out, plus what it deliberately does
not build. Which object the file declares is its own section below: a library, and the design run of
it apart.

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
was not on that list. Its history is gone. Its file format outlived it, in the declarations the
shipped examples went on carrying: a `[package]` table holding a name and a top beside a `[sources]`
table holding the files, which is the shape a package manager gives a manifest. No argument here is
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

## What the object is: a library, and the design run of it apart

The destination named above is a body of SystemVerilog that something else depends on. So the file
has to say two things a single list of sources cannot: what the body is, which is what a dependent
receives, and what is run where the body is developed, which a dependent must never receive -- its
testbenches, the defines and include directories only they read, the foreign stubs only they link.

The standard already has both words. A library is "a named collection of cells" (LRM 33.2.1), and
the `design` statement of a configuration names which cells are the roots (33.4.1.1). The file takes
them as they are: `[library]` is the named collection, `[design]` is the roots and what only the
roots need. `package` was not available, because the language owns it; Go met the same collision and
named its distributed unit a module for that reason.

**The declared library is the build's default library, under its own name.** Every cell compiled
where the file is -- the library's and the design's alike -- belongs to it. The alternative reads
more literally and loses: making the library's sources library files of a named library, and leaving
the design's in `work`.

| Candidate                                                     | What it costs                                                                                                                                                              |
| ------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| The library's sources are a named library beside `work`       | The front end treats a library file's cells as used only if referenced and never as roots: `--top <a library cell>` is refused and `check` says nothing about unused cells |
| Everything compiled here is the default library, by that name | Nothing: roots, checking and selection behave as for any source                                                                                                            |

The first also puts the roots in a library the file never declared. The second is what the field
does with the thing being built: Cargo compiles and checks the root package whole and treats only
its dependencies as libraries, and VHDL's `work` denotes whichever library is being compiled. It
also maps onto the front end's two kinds of file as they are: the library being built is the build's
own source, and a library it depends on is a named library beside it.

The name has a reader today: a configuration in the source may name its own library by it
(`design soc.soc_tb; default liblist soc;`), which resolves only because the front end is told the
name. It is therefore an identifier, as a library's name is in a library map (Syntax 33-2).

**What `[design]` holds is added to what `[library]` holds**, by the same rule that adds the command
line to both: material accumulates. A define of the design's reaches the library's sources in the
build made here, exactly as a `-D` does. A dependent's build hands a library nothing of its own,
which is the next section.

**One design, and room for more.** A library is commonly run several ways -- a simulation testbench,
a lint top, a compliance harness. That is not built: a named design is a table beside the keys
`[design]` has now, so adding it moves nothing anyone has written.

## A library depends on another by name

The requirement is one sentence: a named body of SystemVerilog is used by another through its name,
and what its cells are is decided by its own declaration alone, whoever uses it and whatever is used
beside it. The second half is what makes the first worth having. A library whose cells change with
their user cannot be published, and cannot be compiled once for two users.

```toml
[dependencies]
prim = { path = "../prim" }    # a directory whose lyra.toml declares library prim
```

The key is the library's name, because that name is written in source (`prim.fifo` in a
configuration) and is the library's own to choose; a declaration found there under another name is
refused. A build holds one library of a name, each read once however many libraries depend on it,
and a library that reaches itself is refused.

**A named library beside the root's is not yet a dependency.** Run against the front end as it
stood, with a root and one named library each holding a cell `fifo`:

| What was run                                         | What happened                     |
| ---------------------------------------------------- | --------------------------------- |
| The root instantiates its own `fifo`                 | Bound to the named library's      |
| The root is one compilation unit and defines a macro | Defined in the library's text too |

The first is the standard's rule read literally: with no configuration every instance searches the
libraries in one order for the whole build (LRM 33.6.1). The standard has the other rule too --
under a configuration that selects no library list, the list is "the library in which the cell
containing the unbound instance is found" (33.4.1.5) -- but its configuration language cannot extend
that list per library: a library list is inherited by instance, and a cell clause naming a library
may not expand to one (33.4.1.4). So the search order per library is the tool's to supply, which
33.8.1 expects of a tool anyway.

**What a library of a build is, then, the one being built and one it depends on alike:**

```text
its cells           in a library of its own name
its text            read in compilation units of its own (LRM 3.12.1): one for all its files where it
                    declares single_unit, else one per file
read under          the command line's defines and include directories, then its own
never under         another library's defines or incdir, or a macro another unit defined
a cell it names     searched for in its own library, then in each library it declared, in the order
                    it wrote them, and nowhere else
a cell no file of   searched for by name in its own searchdir, read as its other text is, and in its
its lists holds     library
its dpi sources     linked into the program
```

Two things differ by where a library stands. The one being built is also read under what its
`[design]` declares -- the design's sources are part of its units, and the design's defines reach
the library's text as a `-D` does -- and a dependency's `[design]` is not read at all: roots, their
parameters and their sources are for where a library is developed. Everything else is one path, so a
library is the same thing in both positions.

A cell held only by a library the instantiating one did not declare is not found, so a library
cannot come to rely on what a dependency happens to depend on. One thing still crosses that should
not: an include a library does not find in its own directories is searched for in every exported
directory of the build, whether or not it declared the library exporting it. That can supply a
header a library is missing and can never replace one it has, so a library that builds alone is read
the same here. The command line stays the invocation's and reaches every library, as it does in
every SystemVerilog tool; a kept object is found by its content, so that costs sharing and never
correctness.

**What crosses to a dependent is cells and exported include directories.** Every cell is public,
because the language has no private one. A header is different: `incdir` is searched by the
library's own text alone, and `export_incdir` by the library and by everything that reaches it,
after what each was given for itself. Bender draws the same line with `include_dirs` and
`export_include_dirs`, for the same reason -- a library's macros are part of what it offers, and its
private headers are not. Nothing else crosses: no define, no setting of `[compile]`.

| Candidate for a difference from the field                        | Still true once the front end can do more? |
| ---------------------------------------------------------------- | ------------------------------------------ |
| A cell is searched for in its own library, then its dependencies | Yes                                        |
| A dependency's `[design]` is not read                            | Yes                                        |
| One build has one `std` and one `timescale`                      | No: a gap                                  |

The gap is refused rather than papered over. The front end reads one build under one language
version and one default time scale, so a dependency declaring another `std` or `timescale` than the
root would be read under the root's answer -- a different library from the one its own build makes.
`assertions` is not on that list: eliding an assertion changes nothing the design computes, so the
root's choice covers the build.

**A cell is spelled two ways, and that is left.** A unit is named `cell` in the build's default
library and `library.cell` in any other (`specialization-identity.md` F8), so a library's cell has
one name where the library is built and another where it is depended on. A kept object is found by
content that includes its unit's name, so the two builds are not expected to share one; that was
reasoned and not measured. Spelling a declared library's cells `library.cell` everywhere would give
one name, and costs the readable file names of every declared project on the C++ path for as long as
an emitted file is named after its unit.

**Not built:** fetching, versions, a lockfile, a registry; a dependent choosing among variants of a
dependency (Cargo's features, Bender's `pass_targets`); dependencies only the design needs (Cargo's
dev-dependencies), so the one list serves the library and its design alike. Two libraries declaring
a package of one name cannot be used together: the standard has one package name space and forbids a
configuration to rebind a package (LRM 3.13, 33.4), and the front end reports the duplicate.

## The axis inside B: shared versus local

Choosing B says the file describes what is built. It does not yet say which facts are the design's.
That line is drawn by who the value is true for:

```text
true of the design, for everyone who builds it   -> the manifest
true of this invocation, or of this machine      -> the command line
```

Lyra's spelling of it: **does the option change what program is elaborated and lowered, or how this
run produces and executes it?** Sources, include directories, defines, library search, the top, the
compilation-unit model, the language version and the assertion policy all change the program. `-o`,
`--backend`, `--release`, `--rebuild`, `--no-pch`, `--format`, `--color`, `--jobs`, `--remarks`,
`--cxx` and `--cache-dir` change only how this run produces, executes or reports on it -- and
`--backend` cannot change the program at all, since North Star invariant 3 makes correctness
independent of which path lowers it.

Every established manifest format draws the same line, and draws it as a file boundary: Cargo splits
`Cargo.toml` from `.cargo/config.toml`, npm splits `package.json` from `.npmrc`. A manifest is
committed and shared, so a value that differs per developer poisons it and a value that differs per
run makes it a record of somebody's last experiment.

## The decisions

```text
D1. `lyra.toml` declares a library -- a named object whose parts are its sources, its search paths,
    its defines, and the foreign sources its DPI-C imports resolve against -- and, apart from it,
    the design run of it: its roots, their parameter values, and the same kinds of part where only
    the roots need them. There is one way to run the compiler. A command line naming sources
    compiles an anonymous design, which stays the ordinary case; a manifest that is absent is
    simply no declaration, never a mode and never a failure.

D2. A field is admitted only if it is true of the design for everyone who builds it. An invocation
    property (`-o`, `--release`, `--rebuild`, `--backend`, `--format`, `--color` / `--no-color`,
    `--remarks`) and a machine property (`--cxx`, `--cache-dir`, `--no-pch`, `--jobs`) are refused
    by name.

D3. Every relative path resolves against the directory of the manifest that declares it, never
    against the process's working directory. A path Lyra reports is shown resolved, so the base it
    used is visible rather than inferred.

D4. The manifest declares; the command line selects within the declaration. Material -- what the
    design is made of, and where to look for more of it -- accumulates: the manifest's values and
    the command line's are both in effect, and where the two give one name a value -- a macro, a
    parameter -- the command line's stands. Selection -- which of several declared things to do
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

[design]
top       = ["soc_tb"]                               # selection
params    = ["DEPTH=16"]                             # material
files     = ["tb/soc_tb.sv"]                         # material, read after the library's
defines   = ["TRACE"]                                # material
dpi       = ["tb/dpi_stubs.c"]                       # material

[dependencies]
prim      = { path = "../prim" }                     # material: another library, by name

[compile]
std         = "1800-2023"                            # selection
timescale   = "1ns/1ps"                              # selection
single_unit = true                                   # selection
assertions  = "check"                                # selection: "check" or "skip"
```

The seven keys from `files` to `dpi` are one thing, a source set: source text and what reading it
needs. `[library]` holds one beside its name and `[design]` holds one beside its roots, with the
same keys meaning the same in both. `[design]` is optional; without it the roots are the standard's,
every cell nothing instantiates (LRM 23.3.1). `params` is the design's alone, because a parameter
override is applied to a root.

`name` is what makes this a declaration rather than a bag of options, and it is the field a reader
of A would leave out. It is **required**, because an optional identity is not one: a file that
declines to say what it declares is the bag of options the command line already carries better. It
has readers the day it is written -- the front end, as the name of the library the cells are in, and
the message for a declaration that named no sources, which matters exactly when the declaration in
effect is several directories above the caller. Every later mechanism by which one library refers to
another uses the same field.

`files` is an ordered explicit list, and no path in the declaration may be a pattern. A pattern
names whatever the filesystem happens to hold, which makes the library a function of the directory
rather than of the file that declares it, and it leaves source order to the filesystem when source
order is significant. Finding a cell by name is what `searchdir` and `searchext` are for, so nothing
is lost. Those two are the search every Verilog tool spells `-y` and `+libext+`; they are not named
after a library, because here that word means the named collection and not a directory to look in.

`assertions` names what the compiler does with an assertion rather than what it currently cannot do.
`check` is the default and today refuses the forms Lyra does not implement; `skip` elides them,
which changes no behaviour because an assertion observes and never drives.

**This is not a configuration in the standard's sense, and does not become one.** A `config` block
(LRM 33.4) chooses _which definition an instance binds to_, which is a language construct with its
own syntax and elaboration semantics; the manifest says which library the compiler is building and
what it is made of. A configuration is SystemVerilog source, read by the front end like any other,
and its presence is not a reason to grow a field here.

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

- **Library maps and a library order of the build's own.** The default library's name is carried, as
  `[library] name`; the search by cell name, in `searchdir` and `searchext`; another library, as a
  dependency, with its search order. A library map file (LRM 33.3.1) and a `-L` order have no field.
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

| Field                              | Kind      | `lyra.toml` says | command line says   | result      |
| ---------------------------------- | --------- | ---------------- | ------------------- | ----------- |
| `defines`, `incdir`, `searchdir`   | material  | `TRACE`          | `-D DEBUG`          | both        |
| `files`, `dpi`                     | material  | the source list  | (D7: none)          | the file's  |
| `top`                              | selection | `soc_tb`         | `--top alu`         | `alu` alone |
| `std`, `single_unit`, `assertions` | selection | `check`          | `--assertions skip` | `skip`      |

The test that assigns a field is whether a second value adds to the first or chooses instead of it.
A second include directory searches both; a second define defines both, and a second definition of
the same macro is the one case where two materials meet, which the command line wins: it is what
this invocation said, over what the file says every time. A second top is where the question gets
interesting, because the LRM genuinely allows several and so does the front end.

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
which is what a declaration may never do, since it is committed and read from every subdirectory
beneath it.

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

Choosing B commits to a concept, not to a package manager. A library depends on another that is
already on the machine; whether Lyra grows publishing and fetching is open, and what follows makes
either future cheap.

Five moves buy it, and each is worth more than the field it protects:

1. **Fix the boundary, not the feature.** What fetching a dependency would look like is unknowable;
   that "what the design is" and "how this run produces it" are different questions is not. Every
   future feature lands on one side of that line.
2. **Close the namespace.** D8 makes an unknown key an error, usually justified as typo detection.
   Its larger effect is that a table or a key this version does not know -- `[workspace]`, a `git`
   or `version` beside a dependency's `path` -- is reserved for free, and an older Lyra meeting a
   newer manifest fails loudly instead of building a subtly different design. That is also why there
   is no schema-version field: strict keys already give the loud failure a version field would give,
   and a version with one value is speculation.

   **There is no `[package]` above `[library]`, and that is a decision rather than an omission.**
   Cargo separates `[package]` from `[lib]` and `[bin]` because one package holds several build
   targets each with a name of its own. Here the identity is the library's, every cell compiled is
   in that library, and a design is roots within it rather than a second named thing. What an
   ecosystem adds is `[dependencies]` -- a statement about _other_ libraries.

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
file because it is convenient once. `out` and `cxx` are the two that will be asked for. If
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

- From the directory holding the file, `lyra run` is the whole command line. From a subdirectory of
  it, so is it.
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
- `dpi-foreign-boundary.md` -- what a source set's `dpi` supplies symbols to.
