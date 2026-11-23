# Compiling a design and running it are separate acts

Status: Accepted, 2026-11-22.

## Requirement

What a user runs is the program a user ships, and there is one way to run a design to keep correct.
What compiling produces is complete without the process that compiled it. A program runs, is kept,
is moved to another machine and is run again any number of times with nothing of the compiler alive,
and an edit rebuilds only what it changed ([`north_star.md`](../architecture/north_star.md)
invariants 1 and 4).

## Choice

**Compiling a design produces a program, and running the design is building that program and
executing it.** `run` builds as `build` does, from the same kept results, and executes a private
copy of the program; nothing a design runs reads state the compiling process holds. Both backends
end in a program linked against the runtime library, by a host linker.

## Alternatives turned down

- **Running a design inside the compiler, writing nothing** -- an execution session in the compiling
  process, as an interpreter or an in-process JIT runs it. It saves the link step, and fails one way
  to run a design, since its runtime is not the one a program links, so a defect could show in one
  and not the other, and what it computed dies with the compiler. The failure rests on the link
  being cheap beside compiling, and on the store keeping every unit compiled already
  ([`incremental_build.md`](../architecture/incremental_build.md)). The execution backend ran this
  way until 2026-09-23; measured then, the session was not faster on a first run, and a second run
  with the store pays the program alone.
- **A program run from records the compiling process built after compiling and looked up there.** It
  fails a program complete without the compiler, since what compiling produced is then half a
  program.
- **Two ways, an in-process run for the edit loop and a program for shipping.** It fails one way to
  keep correct. The edit loop is served instead by the default build, unoptimized with the prebuilt
  runtime
  ([the-design-is-optimized-with-the-runtime-at-the-link](the-design-is-optimized-with-the-runtime-at-the-link.md)),
  and by the store.

## Consequences across layers

- Every backend produces object code and a link, the execution backend included, and nothing below
  MIR may assume the compiler is alive when the program runs.
- Linking needs a host C++ driver on either backend.
- What a unit states of itself is stated as data in the program, never as calls a running compiler
  answers ([`compiling_a_design.md`](../architecture/compiling_a_design.md),
  [`emission_model.md`](../architecture/emission_model.md)).
