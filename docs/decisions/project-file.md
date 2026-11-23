# A manifest declares an object, never an invocation

Status: Accepted, 2026-09-01.

## Requirement

A design of any size is a long command line -- its sources in dependency order, include directories,
defines, its top, its compilation-unit model -- respelled at every turn of the edit-compile-run loop
([`north_star.md`](../architecture/north_star.md) invariant 1). Writing it once is uncontroversial.
What has to be decided is what the file describes, because every field name, precedence rule and
path meaning follows from it, and a body of SystemVerilog nobody will rewrite -- a methodology
library, a company's shared IP -- has to be something a design depends on.

## Choice

**`lyra.toml` declares an object with an identity, made of parts, and the command line selects
within it.** The object is a library and, apart from it, the design run of it. Fields are named
after the object's parts; a path resolves against the object's root; a command line naming loose
files compiles an anonymous design, which stays the ordinary case. There is one way to run the
compiler, and an absent manifest is no declaration, never a mode and never a failure.

**What the file holds is decided by whom a value is true for.** A value true of the design for
everyone who builds it belongs to the manifest; a value true of this invocation or this machine
belongs to the command line, and is refused in the manifest by name. The question is whether an
option changes what program is elaborated and lowered, or only how this run produces and executes
it. Cargo and npm draw the same line as a file boundary.

This is the shape the architecture already has one level down. A design is the closure of units with
declared dependencies (`north_star.md` invariants 2 and 5), and a library is that relation one scale
up. A design whose inputs are whatever this command line held is implicit data flow at the top of
the build ([`incremental_build.md`](../architecture/incremental_build.md) invariant 1).

## Alternatives turned down

- **The file as a place to keep arguments**, fields named after options, paths against the working
  directory. It fails a design that something else can depend on, since two argument lists cannot be
  composed without knowing both. Its being smaller does not decide it, because the concept is chosen
  by cost of deferral, and adopting it and the object later rewrites every manifest written by then.
- **A mode the manifest switches on.** It fails one way to run the compiler, and forces a flag to
  escape it that every invocation then carries; an option every invocation passes is not an option.
- **Command files (`-f`) as the whole answer.** They fail discovery, since the file still has to be
  typed, and declaration, since a flat argument list has no identity and cannot keep an invocation
  option out. They keep working as the front end's own.
- **Invocation or machine properties admitted for convenience** -- an output path, a compiler. Each
  fails a committed manifest being true for everyone who builds it; per-machine defaults belong in a
  separate configuration file, the split every package manager arrived at.

## Consequences across layers

- The resolved manifest is a declared input of the compilation like any source file.
- A library's own declaration decides what it is, whoever depends on it, which is what lets ports
  and dependencies exist.
- The schema, discovery, precedence and path rules are
  [`project_model.md`](../architecture/project_model.md)'s.
