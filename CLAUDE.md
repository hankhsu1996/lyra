# CLAUDE.md

Layer contracts live under `docs/architecture/`, settled choices under `docs/decisions/`, and gaps
against those contracts under `docs/progress/`. `docs/README.md` indexes them; `docs/style.md`
governs writing them.

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

| File            | Holds                                                      | Tracked |
| --------------- | ---------------------------------------------------------- | ------- |
| `.bazelrc`      | build facts, and the definition of each `--config`         | yes     |
| `.bazelrc.user` | this checkout: its default config, and keys only it uses   | no      |
| `~/.bazelrc`    | this machine: resource limits, cache location, shared keys | no      |

`git clone` then `bazel build //...` works with no account and no local setup. `--config=ci`,
`--config=rbe`, and `--config=dev` are opt-in and each needs a BuildBuddy key from one of the two
untracked layers; nobody is required to have one. `.bazelrc.user.example` lists what may go there.

`ci` shares a cache while actions stay local; `rbe` runs the actions remotely for a gate that reads
none of what it produces; `dev` is `rbe` for the edit loop, where top-level outputs come back so the
binary just built can be run. Under either remote-execution config the concurrency of several
sessions at once needs no local setting, because the remote scheduler is one queue across every
invocation -- the coordination that separate Bazel servers on one machine cannot have. A test that
drives the host C++ compiler cannot run remotely at all and is tagged to stay local.

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

Nearly every test is a conformance case under `tests/conformance/`: a self-checking SystemVerilog
program stating what IEEE 1800 requires of it. The outer directory is the LRM clause, the inner one
is the subject, and `main.sv` is the entry. `tests/conformance/README.md` is the contract every case
answers to, and `docs/decisions/conformance-case-shape.md` records why it has that shape.

A case names no path. What a path currently refuses is recorded once for the path in
`tests/paths/<path>.yaml`, and a case that starts passing fails until its entry is dropped, so the
file only shrinks and is therefore the coverage report.

To iterate on one case, filter by its path under the corpus with `/` written as `.`; the target
already names the path that runs it:

```bash
bazel test //tests:llvm_tests --test_filter='12_statements.case_default_item'
```

CI's test job passes `--test_tag_filters=-requires-host-cxx`, which excludes everything that spawns
a host compiler: `cpp_tests`, `llvm_dpi_tests`, `cli_tests`, and `pch_audit_test`. What is left is
`llvm_tests`, the corpus minus the cases carrying foreign sources, which is the merge gate and the
one that grows as the execution backend fills in.

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
- **Whatever shape it takes, that set is consumed by a `switch` or a `std::visit`, never by `==`**,
  so gaining an alternative breaks the build until every consumer says what it means. Enforced by
  `tools/policy/check_architecture.py` A013.
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
