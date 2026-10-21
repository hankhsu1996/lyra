# The Design Is Optimized With the Runtime at the Link

## Date

2026-10-18

## Status

Accepted. Says how a build optimizes the design's code together with the runtime it calls, and how a
user asks for that apart from how hard the design is optimized; corrects the reason
[a-packed-value-is-its-words](a-packed-value-is-its-words.md) gave for rejecting runtime bitcode.
Revised 2026-10-20: under D4 a way the probe accepted and the build then refused is the build's
failure, where it used to be built again the plain way. Revised 2026-10-20 as well: the compiler
that builds Lyra is by default this clang too, so D1 no longer keeps the two apart.

## Why this decision matters

Every value operation, write and wake a design performs is a call into the runtime library, which is
compiled before any design exists. An optimizer working on the design's code alone sees a call it
cannot look into, so it can neither inline the operation nor fold what the operation does with
values it already knows. The execution backend sees even less than the C++ backend. It reaches the
runtime through the C entry points the library publishes, so not even a header's inline body is in
front of it.

Measured on 2026-10-18 over the benchmark corpus, the runtime's code seen by the optimizer was the
one build option worth more than a few percent. Everything else -- `-O3`, a newer instruction set,
the runtime's own flags, which compiler built the runtime -- moved the instruction count by 6% or
less.

**The requirement.** Where a user asks for it, the code a design compiles to and the runtime it
calls are optimized as one program by the compiler that compiles the design, at a cost that stays
parallel and incremental; a build that does not ask is unchanged, and a program is correct whether
or not any of this applies.

## What the field does

- **rustc** runs link-time optimization itself, with its own LLVM, and hands the system linker
  machine code. An upstream crate's bitcode travels inside its rlib's objects ("Rust's standard
  library ships with embedded bitcode since users link to it both with and without LTO", the rustc
  book, on `-C embed-bitcode`), and `compiler/rustc_codegen_llvm/src/back/lto.rs` adds those modules
  to its ThinLTO set. The two are one LLVM by construction, because rustc builds its standard
  library. Optimizing across a library a different compiler built is an opt-in that works only at a
  matching LLVM release (the rustc book's page on linker-plugin LTO).
- **clang** makes one library serve a link with and without LTO by the fat LTO object, which holds
  machine code in `.text` and pre-link bitcode in `.llvm.lto`, a section an ordinary link ignores
  (LLVM's FatLTO document). The link that optimizes is the linker's, at the linker's LLVM release.
- **Full LTO against ThinLTO.** Full LTO merges every module into one and optimizes and compiles it
  on one thread; ThinLTO keeps one summary of the whole program, then optimizes and compiles each
  module with what it imports from the others, in parallel, keeping each result under a key of
  everything it read (clang's ThinLTO document).
- **Verilator** compiles its runtime from source with every design (`verilated.mk`), so the compiler
  of the design always reads it.
- **How a user asks.** clang and GCC take the level as `-O0` to `-O3` and link-time optimization as
  `-flto`, two choices; rustc takes `-C opt-level` and `-C lto` apart, and a Cargo profile holds
  `opt-level` and `lto` as two keys (recalled, not opened). Optimizing across a library is the one a
  user switches on, in all three.
- **An incremental ThinLTO link** keeps each module's result in a cache named by whoever runs the
  link. clang's driver has no option for it, so it is told to the linker in the linker's own
  spelling, the gold plugin's `-plugin-opt,cache-dir=` (which bfd and mold read too) or lld's
  `--thinlto-cache-dir=`, and lld refuses the plugin's (clang's ThinLTO document, Incremental).
  LLVM's own build adds it only where it knows the linker (`HandleLLVMOptions.cmake`). GCC 15 keeps
  one with `-flto-incremental=` (the GCC talk "Incremental LTO in GCC", FOSDEM 2025).
- **A build that cannot know its toolchain** asks it: CMake's `check_ipo_supported` compiles and
  links a small project with the link-time flags and enables them only if that works. A fat LTO
  object is what a producer ships when it cannot know its consumer; a build whose compile and link
  are one toolchain needs none.

Where our conditions differ, each in a sentence.

- The execution backend's compiler is Lyra's own LLVM and the user's linker is not, so the link that
  optimizes is Lyra's to run, in process, as rustc runs it.
- The runtime is C++ that Lyra's build compiles, not code Lyra's compiler emits, so "one LLVM by
  construction" has to be made true by choosing the clang that compiles it.
- The C++ backend's design is compiled by the user's compiler, which may be any clang release or any
  GCC, and the only form of the runtime every such compiler can read is its source.

## Decisions

### D1. The runtime is compiled by the clang of the LLVM release Lyra links, and carries both forms

One number in `MODULE.bazel` names the LLVM release Lyra links and the clang release that compiles
the runtime library, because bitcode is read only by the release that wrote it or a newer one, and
an older one is not reliably upgraded. LLVM 23 does not upgrade clang 20's coroutine end intrinsic,
and its verifier refuses the module. The library is compiled by that clang whichever compiler builds
Lyra, the GCC of the merge gate's second build included, since how it runs a user's design is a
property of the artifact.

Each of its objects is a fat LTO object, so a build without `--lto` links it as machine code and one
with it reads its bitcode, from one library.

The compiler that builds Lyra itself stays a setting of its own, since it reads none of this
bitcode. By default it is this same clang
([a-build-compiles-for-the-oldest-platform-it-supports](a-build-compiles-for-the-oldest-platform-it-supports.md)).

### D2. The execution backend's link under `--lto` is ThinLTO, run by Lyra

Under `--lto` each unit is compiled as clang compiles a file for `-O<n> -flto=thin`, through the
ThinLTO pre-link pipeline at the build's level, and written as bitcode with its summary. The link
reads that bitcode and the runtime's, resolves each symbol as a linker does, and optimizes at the
same level and compiles each module in parallel at the build's width, keeping each module's result
in the store under LLVM's own key. What it produces is machine code, linked by the host as before.

Two rules make the result the program the host link would have made.

- A runtime member joins the program the way an archive member joins a link, when it defines a
  symbol the program so far leaves undefined.
- A definition anything outside the bitcode names -- a foreign object calling an exported function,
  the system's start-up code calling `main` -- stays visible, so it is not internalized away. A
  member that carries no bitcode, the fiber switch written in assembly, is left to the host link,
  which takes it from the archive.

### D3. Every function states the processor it is compiled for

Whether a call may be inlined depends first on whether caller and callee are compiled for compatible
processors, and clang states the processor on every function it emits. The execution backend states
the same one on every function it defines, in every build, namely the processor clang names for the
target when given none. Without it no runtime function is ever inlined into a design's, whatever
else is in place.

The processor is enough, with no list of features beside it. X86 inlines a callee whose features are
a subset of its caller's, and the features the x86-64 processor implies already cover the ones clang
lists for it (`X86TTIImpl::areInlineCompatible`, `X86.td`).

### D4. Under `--lto` the C++ backend compiles the runtime from its source, in the way its toolchain accepts

Under `--lto` the C++ backend's host compiler compiles the runtime's own sources beside the design,
at the build's level, and links the two with its own link-time optimization. The sources, their
include directories and their macros come from the runtime's own build, staged beside the compiler,
and an emitted project built with `--lto` carries a copy. A compile is kept per compiler and per
flag, so the runtime is compiled once per machine and compiler, not once per design.

How to ask for it differs by compiler and by linker, so one ordered list of ways serves them all:
ThinLTO with the gold plugin's cache, with lld's, and with none, then GCC's with its cache and with
none. A one-line program is compiled to bitcode alone and linked each way in turn, and the first
that works is the one the build takes; that question is an action keyed by the compiler and the
list, so it is asked once per compiler. The link keeps its cache in the store, LLVM's layout beside
the execution backend's and GCC's in a directory of its own, and an emitted project keeps it under
its objects. Its recipe asks the same question the same way.

Where no way is accepted, the program is built the plain way, with the prebuilt runtime, so a
toolchain that cannot do this still builds. Where one is, the program is built that way, and a
compile or link that then fails is the build's failure and is reported as one, as CMake's is once
`check_ipo_supported` has enabled the flags. The question is asked once rather than attempted in
every build, as the prepared header is, because objects that could serve a refused link too carry
their machine code beside their bitcode, and that cost falls on every compile of the accepted path.

### D5. A user names the level and link-time optimization apart

How hard the design's own code is optimized is `-O0` to `-O3`, the levels a C++ compiler names and
LLVM's pipeline runs, on either backend. Whether it is optimized together with the runtime is
`--lto`, a choice of its own: what it buys depends on the design and what it costs is a whole
optimization again at the link, so a level without it is a build a user may want. A build that names
neither compiles the design unoptimized and links the prebuilt library as machine code, as before,
since the default serves the edit loop, as clang's and rustc's do. Under `--lto` the runtime is
optimized at the same level, so a level below the second makes it slower than the prebuilt library,
as it does under clang. It is spelled `--lto` rather than a C++ compiler's `-flto`, because the
command line is shared with the front end, where a word of several letters takes two dashes and `-f`
already reads a command file.

## Rejected

| Alternative                                                         | The requirement it fails                                                                                                                                                                                               |
| ------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Full LTO                                                            | Parallel. On Ibex the merged module is one 40 s optimization and one 41 s code generation on a single thread, 92 s in all, against 14.7 s for ThinLTO at four jobs.                                                    |
| Each unit importing the runtime's bodies at its own compile         | Its imported bodies would reach the runtime's file-local functions, which only the runtime's own build could export ahead of time; no C or C++ toolchain ships that step. It also never inlines one unit into another. |
| The host linker optimizing with the prebuilt bitcode                | Every compiler. A user's linker reads only bitcode of its own release or older, so the execution backend's would be refused everywhere but on a matching toolchain.                                                    |
| The C++ backend handing the prebuilt bitcode to the user's compiler | Every compiler, again. GCC never reads it, and a clang older than the runtime's cannot.                                                                                                                                |
| Every emitted project carrying the runtime's sources                | A default build. A project would copy about 1,200 files it never compiles.                                                                                                                                             |
| The C++ backend attempting it in every build, on fat objects        | The cost. A fat object compiles 20% to 25% slower than a thin one, on every compile under `--lto`, only to serve a link the toolchain refuses.                                                                         |
| Building the plain way after an accepted way is refused             | The cost against what it serves. The probe already answers whether the toolchain can do this; a refusal after it is a toolchain the probe misjudged, and serving it compiles the runtime a second time.                |
| The C++ backend's link keeping no cache                             | Incremental. Every link redoes every module's optimization; on Ibex one edited unit rebuilt in 16.2 s, and 8.1 s with the cache.                                                                                       |
| One flag naming both the level and link-time optimization           | A level without link-time optimization. On OpenTitan the link-time optimization took 635 s of an 818 s build.                                                                                                          |

## What was measured

Instructions per unit of work under callgrind, execution backend, against `main` at `a53369a9`. The
eight scheduling cases 23.6% to 29.7% fewer, the arithmetic, control-flow and array cases 2% to 20%
fewer, and the two packed reads 1.4% and 1.6% more. The packed reads sit in an `initial` block that
ends in `$finish`, which marks the whole block as rarely run, and the inliner then refuses runtime
functions it has imported. The C++ backend's `-O2` build of nba-heavy ran 57,800 instructions a
cycle with the prebuilt runtime and 42,000 with the runtime compiled beside it. Compiling the
runtime from source took about 40 s at eight jobs, once per compiler.

Build time on Ibex at four jobs. The execution backend's `-O2 --lto` build took 19.8 s from nothing,
3.8 s with nothing changed and 5.6 s after one unit was edited, against 6.8 s, 3.9 s and 4.6 s for a
default build. One unit of Ibex's compiled in 15.4 s as a thin object and in 18.3 s and 19.7 s as a
fat one.
