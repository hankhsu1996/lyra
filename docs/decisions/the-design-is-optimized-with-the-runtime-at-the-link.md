# The design is optimized with the runtime at the link

Status: Accepted, 2026-10-18; revised 2026-10-20.

## Requirement

Where a user asks for it, the code a design compiles to and the runtime it calls are optimized as
one program by the compiler that compiles the design, at a cost that stays parallel and incremental;
a build that does not ask is unchanged, and a program is correct whether or not any of this applies.
Every value operation, write and wake is a call into a runtime compiled before any design exists,
which an optimizer working on the design alone cannot look into, and the execution backend reaches
it through a C ABI with not even a header's inline body in front of it. Measured over the benchmark
corpus, the runtime's code seen by the optimizer was the one build option worth more than a few
percent.

## Choice

- **One number names the LLVM release Lyra links and the clang release that compiles the runtime.**
  Bitcode is read reliably only by the release that wrote it, so the runtime is compiled by that
  clang whichever compiler builds Lyra, and ships as fat LTO objects, machine code and bitcode in
  one, so one library serves a link with and without link-time optimization.
- **The execution backend runs the link that optimizes, itself, as ThinLTO**, as rustc runs its own
  LTO with its own LLVM, since its compiler is Lyra's LLVM and the user's linker is not.
- **The C++ backend's host compiler compiles the runtime from its source beside the design**, since
  that compiler may be any clang or any GCC and source is the one form every such compiler reads, in
  the way of asking for link-time optimization its toolchain accepts.
- **A user names the optimization level and link-time optimization apart**, `-O0` to `-O3` and
  `--lto`, as rustc and Cargo keep `opt-level` and `lto` apart. A build naming neither compiles the
  design unoptimized and links the prebuilt library as machine code, which serves the edit loop.

## Alternatives turned down

- **Full LTO.** It fails parallel, since the merged module is optimized and compiled on one thread,
  which on Ibex took several times ThinLTO's wall time at four jobs.
- **Each unit importing the runtime's bodies at its own compile.** It fails reaching the runtime's
  file-local functions, which only the runtime's own build could export and no C or C++ toolchain
  ships a step for, and it never inlines one unit into another.
- **The host linker optimizing with the prebuilt bitcode.** It fails every compiler, since a user's
  linker reads only bitcode of its own release or older, so it would refuse Lyra's everywhere but on
  a matching toolchain.
- **The C++ backend handing the prebuilt bitcode to the user's compiler.** It fails every compiler
  likewise, since GCC never reads it, and a clang older than the runtime's cannot.
- **One flag naming both the level and link-time optimization.** It fails a level without it, which
  a user wants because the link-time step can be most of a build's time.

## Consequences across layers

- Moving Lyra's LLVM moves the clang that compiles the runtime, and by default the clang that builds
  Lyra
  ([a-build-compiles-for-the-oldest-platform-it-supports](a-build-compiles-for-the-oldest-platform-it-supports.md)).
- The forms the runtime ships in are
  [`runtime_distribution.md`](../architecture/runtime_distribution.md)'s; how a build under `--lto`
  links, on each backend, is [`building_a_program.md`](../architecture/building_a_program.md)'s.
