# Dev Ergonomics

Tracks gaps in the developer feedback loop -- the things that slow down "I see weird behavior in one
.sv file, I want to understand why". The pipeline today is HIR -> MIR -> C++ emit -> compile -> run,
and the last two stages are buried inside the bazel test harness, so isolated debugging falls back
on test-shaped YAML files plus a missing manual build recipe. Each item below is a single PR-sized
change that pulls one of those hidden seams out into a first-class tool.

The work is "done" when a developer can take a single `.sv` file, observe a divergence between Lyra
and Verilator, and pinpoint the divergent IR layer without writing a YAML stanza.

## Sub-Steps

- [ ] D1 -- Restore `lyra run` with a backend selector. The archive's `lyra run` was the canonical
      "go from .sv to executed simulation" command (`archived/docs/cli-design.md`), defaulting to
      AOT and accepting `--backend=jit` for the dev path. The C++ pipeline that exists today on this
      branch is exposed only as `emit cpp`, which leaves the user to compile + run the artifact
      themselves. Re-introduce `run` as the project's primary execute command and make the existing
      C++ pipeline a `--backend=cpp` choice (alongside the AOT / JIT backends that will return when
      LLVM work resumes -- see `architecture-reset.md` R3 / R4). `lyra run` should accept the same
      project / file inputs the dump and emit commands already take, and should stream stdout /
      stderr through. The deliverable removes the "no command runs a .sv file" gap that pushes
      developers toward ad-hoc YAML test cases.

- [ ] D4 -- Self-contained build recipe for `emit cpp` output. `playground/out/main.cpp` is not
      directly buildable today: `CMakePresets.json` was removed, and a hand-written `clang++`
      invocation against `include/` fails because the emitted code transitively includes abseil
      headers that are only reachable via bazel. The cleanest fix is for `emit cpp` itself to emit
      the build system alongside the C++ -- a minimal `CMakeLists.txt` (or `Makefile`) that lists
      the runtime include roots, links against the runtime static library, and produces a runnable
      binary. The user runs one documented command (`cmake --build` or `make`) and gets
      `playground/out/program`. The emitter is the right home for this because only it knows which
      runtime headers the artifact actually pulls in. Once D1 lands and `lyra run` internalizes the
      same recipe, D4's value is mostly "the C++ artifact is portable on its own" -- a developer can
      hand the `out/` directory to someone else and they can build it.

## Out of Scope

- New SystemVerilog feature coverage. This file tracks the developer feedback loop, not language
  features.
- Verilator-side tooling. Comparison workflows that drive both Lyra and Verilator are a separate
  workstream.
- Performance instrumentation. `docs/profiling.md` already documents the callgrind workflow.
