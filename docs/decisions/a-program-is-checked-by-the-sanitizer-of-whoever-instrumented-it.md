# A Program Is Checked By The Sanitizer Of Whoever Instrumented It

## Date

2026-10-20

## Status

Accepted. Builds on
[the-design-is-optimized-with-the-runtime-at-the-link](the-design-is-optimized-with-the-runtime-at-the-link.md),
whose runtime sources the C++ backend compiles here too, and on
[a-program-is-kept-by-what-built-it](a-program-is-kept-by-what-built-it.md), whose keys gain what a
program is checked for.

## Why this decision matters

A read of storage that has ended usually reads what was there, so a program that makes one prints
the right answer and every test of its output passes. About half of the lifetime defects Lyra has
had were in code it generates or in its runtime as a generated program drives it: an exported
function's string handed to C after it was freed, a wait on a member that was gone, a force reaching
a cell that had ended. None of that code is in a source file of Lyra's, so no analysis of Lyra's
source sees it, and the sanitizer run over Lyra's own tests instruments the compiler and not what
the compiler builds. What sees such a read is the program itself, built to check every load and
store it makes and to stop at the first that touches storage it does not own.

## What the field does

- **clang** adds the address sanitizer's pass where every optimization pipeline ends its work
  (`clang/lib/CodeGen/BackendUtil.cpp`, `addSanitizers`, at the optimizer-last extension point),
  which the per-module, the unoptimized and the ThinLTO pre-link pipelines all reach
  (`llvm/lib/Passes/PassBuilderPipelines.cpp`), and marks every function it emits as one to check.
  Its driver links the release's own sanitizer runtime, whole, with the symbols it exports and the
  system libraries it needs (`clang/lib/Driver/ToolChains/CommonArgs.cpp`, `addSanitizerRuntime`).
- **rustc** runs the same pass and links a copy of that runtime built from its own LLVM, by path
  (`rustc_codegen_ssa/src/back/link.rs`, `link_sanitizer_runtime`). Its standard library stays
  uninstrumented unless rebuilt, and heap reads in it are still caught, because the runtime replaces
  the allocator; reads of other storage inside it are not.
- **The instrumentation names its runtime's version.** Code the pass instruments refers to a
  versioned check symbol (`llvm/lib/Transforms/Instrumentation/AddressSanitizer.cpp`), so a module
  links the runtime of the release that instrumented it and no other.
- **The release's sanitizer runtime names the functions in a report only through a symbolizer it
  starts.** It takes the one its `external_symbolizer_path` option names, which is what
  `ASAN_SYMBOLIZER_PATH` names unless `ASAN_OPTIONS` says otherwise
  (`compiler-rt/lib/asan/asan_flags.cpp`), else `llvm-symbolizer` on `PATH`
  (`compiler-rt/lib/sanitizer_common/sanitizer_symbolizer_posix_libcdep.cpp`,
  `ChooseExternalSymbolizer`). clang ships that tool in the release beside `clang`, and rustc
  documents that reports are symbolized when it is on `PATH`; neither sets anything for the program.
  LLVM's own test runner, which starts the programs it tests, sets the variable to the symbolizer it
  built wherever the environment has none (`compiler-rt/test/lit.common.cfg.py`). GCC's sanitizer
  runtime symbolizes in process and needs no tool.

## Decisions

### D1. A program is built to check itself on request, and nothing else changes

`--sanitize address` builds the program so that its first read or write of storage that has ended,
or was never its own, ends the run with a report naming where it was made and where the storage was
freed. It is an invocation property, refused in `lyra.toml` like `-O` and `--lto`, and it changes
what a build compiles and links and nothing the program computes. It is spelled with two dashes for
the reason `--lto` is: the command line is shared with the front end, whose `-f` reads a command
file.

### D2. Whoever instruments the code links the sanitizer's runtime

On the execution backend Lyra instruments a design's modules itself, as clang does, so the program
links the sanitizer runtime of the LLVM release Lyra links, which ships beside the compiler, and a
build of the runtime library that release compiled for the sanitizer. A user's foreign sources are
compiled by the user's compiler, whose instrumentation goes with its own runtime, so on that backend
they are not instrumented; their reads through the C library are still checked, since the runtime
intercepts it.

On the C++ backend the host compiler instruments everything, so it compiles the runtime's own
sources beside the design, as under `--lto`, and links its own sanitizer's runtime, and the user's
foreign sources are instrumented with them. The shipped recipe of an emitted project does the same.

The release's symbolizer ships beside its sanitizer runtime, as clang ships it, and a program `run`
starts for a checked build is told where it is, as a test runner tells the programs it starts,
unless the person running Lyra already named one. Lyra both supplies the instrumenting release and
starts that program, where clang does only the first and the test runner only the second, so it does
what each does. A program `build` wrote and a person starts finds a symbolizer as clang's and
rustc's do, on `PATH`.

### D3. The runtime library is checked as the design is

Half the run-time defects so far were reads inside the runtime, so the runtime is compiled for the
sanitizer whenever the design is, rather than left as rustc leaves its standard library. The library
compiled for it carries machine code alone: the release refuses to instrument a fat LTO object,
whose pipeline would instrument each module twice, so under `--lto` a checked program on the
execution backend optimizes the design's modules together and links the runtime as it links any
member carrying no bitcode.

### D4. The runtime tells the sanitizer of every switch that runs code on a foreign call's stack

A foreign task call runs on a stack of its own, so that it can wait, and the sanitizer keeps its own
account of which stack is running: a throw on one it was not told of is unwound against the wrong
bounds, and it warns that false reports may follow. So the runtime tells it of each switch that runs
code on that stack once the call has begun, from both sides, as the sanitizer's interface asks of a
fiber library (`compiler-rt/include/sanitizer/common_interface_defs.h`,
`__sanitizer_start_switch_fiber`), including the two the fiber library makes on its own: leaving a
call's stack for good, and unwinding a waiting call when it is destroyed. One source serves both
builds of the runtime, because the interface is reached through weak references that a program
linking no sanitizer leaves empty.

The stack is not reported as left for good when the call ends. Reporting it frees at once the frames
the sanitizer keeps for that stack to catch a return's storage being used later, and the fiber
library's own frames still run on them before its last switch, which a build checking returns can
fault on, as the one GCC instrumented did. They are kept until the call's fiber is destroyed, then
taken over by the stack destroying it and freed, as SystemC's coroutine package frees them
(`sysc/kernel/sc_cor_qt.cpp`, `~sc_cor_qt`).

### D5. The corpus runs that way on the schedule that checks Lyra's own memory

Where Lyra's own build is instrumented, the corpus target builds every program it runs with
`--sanitize address`, so one scheduled run answers whether anything reads storage that has ended, in
the compiler or in what it built. A report fails the case that made it through the exit status,
which is how a case passes or fails at all. What the sanitizer finds that is not fixed at once is
held in a record of its own beside the path's defect record, since the same case passes in the run
that builds it as usual.

## Rejected

| Alternative                                                                                          | The requirement it fails                                                                                                                                        |
| ---------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Annotating every accessor that hands out a view so that the C++ compiler checks lifetimes statically | Generated code. It is in no source file, so no annotation reaches it; and every accessor would carry the annotation.                                            |
| Refusing a view of storage to an object about to end, accessor by accessor                           | The same, and it refused only sound uses where it was tried.                                                                                                    |
| Linking the host compiler's sanitizer runtime on the execution backend                               | D2. The runtime's version is named in the instrumented code, and the host's need not be the release that instrumented it.                                       |
| Switching the fiber library to its annotated form for the sanitizer                                  | A checked run would then test a different context switch from the one every other build runs, since the annotated form exists only over the operating system's. |

## What was measured

valgrind on a program whose exported function hands C a freed string named the free and the read in
one run, built as usual, while the program printed the right text. With `--sanitize address` the
same kind of read, a freed string the runtime copies, ends the run with `heap-use-after-free` on
both backends and through an emitted project's recipe, after a foreign call that waited on a stack
of its own. On the execution backend that report gave addresses alone where no symbolizer is on
`PATH`, and started by `run` it names the foreign function that freed the text. The C++ backend's,
under GCC's sanitizer runtime, names it either way, since that runtime symbolizes in process.

The first checked run of the corpus failed eight of its 814 cases. Six were one read of freed
storage in the runtime: a program's last initial procedure ending ended the program, which released
that procedure while the scheduler was still finishing it. One was a foreign call disabled while it
waited, the throw on its stack that D4 answers. One was a pair of class objects that name each
other, which a reference count never reclaims (LRM 8.29). Once D4 was in place, a ninth showed: a
landing on the execution backend began handling whatever unwound into it and never ended that when
it carried the unwinding on. The exception that unwinds a waiting foreign call's stack, when the
call is destroyed, passes such a landing, so it was never freed.
