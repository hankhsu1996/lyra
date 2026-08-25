# CLAUDE.md

Layer contracts live under `docs/architecture/`, settled choices under `docs/decisions/`, and gaps
against those contracts under `docs/progress/`. `docs/README.md` indexes them; `docs/style.md`
governs writing them. The `/archived` directory is read-only reference from earlier iterations --
never import from it.

## Commands

```bash
npm ci
bazel build //...
bazel test //... --test_output=errors    # same target set CI runs
clang-format -i <files>
npm run format
buildifier -r .
```

## Build configuration

Three layers, and a setting belongs to exactly one:

| File            | Holds                                              | Tracked |
| --------------- | -------------------------------------------------- | ------- |
| `.bazelrc`      | build facts, and the definition of each `--config` | yes     |
| `.bazelrc.user` | your credentials and your default config           | no      |
| `~/.bazelrc`    | this machine: resource limits, disk cache location | no      |

`git clone` then `bazel build //...` works with no account and no local setup. `--config=ci` and
`--config=rbe` are opt-in and each needs a BuildBuddy key from `.bazelrc.user`; nobody is required
to have one. `.bazelrc.user.example` lists what may go there.

**The compiler differs between configs.** `--action_env=CC=clang` names the local compiler by
environment, but a config setting `--platforms` resolves a registered toolchain instead, so
`--config=rbe` builds under the remote image's system GCC. Cache entries are keyed by toolchain, so
local and `rbe` builds share nothing, and code compiling under one can fail under the other --
usually through the standard library, whose version travels with the compiler.

## Lyra CLI

```bash
lyra check [files...]                 # Elaborate and report diagnostics; no lowering
lyra dump ast [files...]              # Dump slang's elaborated AST as JSON
lyra dump hir|mir|lir|llvm [files...] # Dump the named intermediate form
lyra emit cpp -o <dir> [files...]     # Write a self-contained C++ project
lyra compile -o <dir> [files...]      # Emit that project and build it -> <dir>/program
lyra run [files...]                   # Emit, build, and execute
lyra cache clear                      # Drop the precompiled-header cache
```

Command words are positional, and everything after them is one command line shared with the slang
driver: every front-end option slang accepts -- `--top`, `-I`, `-D`, `-G`, `--single-unit`, `-y`,
`--libext`, `-f` / `-F` filelists, `-W` warnings -- reaches Lyra unchanged. A standalone `--` ends
Lyra's command line; what follows is the simulation's own argv, where LRM 21.6 plusargs go.
`lyra --help` prints the authoritative option list.

**`--release` trades build time for simulation speed.** By default the design's translation unit is
compiled unoptimized, because iterating pays that compile on every edit; `--release` optimizes it
for a run long enough to earn the compile back. The runtime library the program links is prebuilt
and always optimized, so it is not on this axis and costs nothing either way.

## SystemVerilog version

Lyra targets **IEEE 1800-2023**, defaulting the front end to `--std 1800-2023` and slang's VCS
compatibility mode. Both are defaults a caller may override. Testing a 2023 feature against slang
directly needs `--std 1800-2023`.

## Architecture

```
SV ---> slang AST ---> HIR ---> MIR ---> backend::cpp ---> C++ source + runtime
```

The pipeline is HIR -> MIR -> LIR -> LLVM IR; `docs/architecture/compiler_overview.md` holds the
binding contract.

- Semantic modeling lives in HIR and MIR; execution modeling lives in LIR and below.
- A compilation unit is the top-level semantic boundary (module, package, interface).
- Compile time produces class-level artifacts; runtime constructs objects and installs relations.

Headers in `include/lyra/`, implementations in `src/lyra/`.

## Testing

Nearly every test is an end-to-end case under `tests/cases/` driven by SystemVerilog input: one
`case.yaml` per case, grouped into suites by `tests/suites.yaml`. `expect.variables` asserts a
variable's final value and accepts `0x` hex and SV-sized literals.

A case carries a tag per backend that runs it, and each backend that claims a case is held to the
expectations the case already states -- so a construct a backend has not reached is simply left
unclaimed, and coverage is measured rather than assumed. The suites read those tags: `cpp_tests`
runs everything tagged for the C++ backend, `jit_tests` everything tagged for the execution backend.

To iterate on one failing case, filter by its gtest name -- the case `id` with dots, prefixed by the
backend that ran it:

```bash
bazel test //tests:cpp_tests --test_filter='Cpp.errors.nets_multi_driver'
```

CI's test job passes `--test_tag_filters=-requires-host-cxx`, which excludes `cpp_tests`,
`run_tests`, and `pch_audit_test` -- everything that spawns the host C++ compiler. The C++ backend
is therefore verified before a commit and not again on merge, so a full local `bazel test //...` is
the only gate it gets.

## Code style

C++23, Google style, clang-tidy warning-free. `CamelCase` classes and functions, `lower_case_`
members, `kCamelCase` enums. Use IEEE 1800 LRM terminology for SystemVerilog concepts, and prefer
the modern idiom -- `std::format`, `std::span` / `std::string_view`, `std::array`, `std::optional` /
`std::expected`, structured bindings. Comments follow `docs/code-comments.md`.

- **A semantic id, index, or offset is a strong wrapper type**, never a raw integer outside a C ABI
  boundary -- including as a return value a caller then uses as an id.
- **A closed set of alternatives is a `std::variant` of per-kind structs**, not a tag enum beside
  always-present spare fields, so an invalid combination cannot be spelled. No arm is added without
  a complete lowering path in the same change.
- **One namespace per directory**; a sub-namespace with no corresponding folder is not one.
- **ASCII only in source and docs**, enforced by `tools/policy/check_ascii.py`. In markdown,
  backtick underscored SV keywords (`always_comb` and friends) or Prettier mangles them and the
  format job fails.
- **No `/*param=*/value` at call sites**, and no inline comment on a struct field or variable
  declaration -- put it on the line above, where the formatter cannot wrap it badly.
- **`TODO(<owner>)`** carries whose it is.
- The zero-warning rule governs Lyra's own source. Emitted C++ is not built under it.

## Error handling

| Error type        | When                                                |
| ----------------- | --------------------------------------------------- |
| `diag::Result<T>` | Recoverable lowering / backend failures, with codes |
| `InternalError`   | Compiler bugs (invariant violations)                |
| `SimulationError` | Failures of the simulated design at run time        |

These three are the only exception types anyone may throw; `std::` exception types are banned
outside their own definitions. The dividing question is whether the condition depends on a value the
simulated program computes: a negative `new[N]` size, a tagged-union access inconsistent with its
tag, or a malformed run-time format string is the design's failure and gets `SimulationError`, while
a width, plane, or arena invariant the compiler itself established gets `InternalError` and tells
the reader to report a bug. An operation a legal program requests that Lyra does not yet carry out
is also `SimulationError` -- the reader's next step is to ask for support, not to file a bug.

Avoid `assert()` and `<cassert>`; `catch(...)` is allowed only in `src/lyra/driver/`. A control
effect -- leaving a disabled scope (LRM 9.6.2) -- is not an error and is thrown by the runtime that
defines it; nothing else may add a thrown type.

## Approach to changes

Adding a feature: find how the neighbours already do it, and extend the existing abstraction at the
right level rather than building a parallel one. The result should read as a natural extension, not
a bolt-on.

Fixing a bug: after finding the immediate cause, ask what allowed it. Fix that, not the symptom -- a
control branch that hides the condition leaves the codebase weaker than before.
