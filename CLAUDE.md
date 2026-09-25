# CLAUDE.md

Layer contracts live under `docs/architecture/`, settled choices under `docs/decisions/`, and gaps
against those contracts under `docs/progress/`. `docs/README.md` indexes them; `docs/style.md`
governs writing them.

## Commands

```bash
npm ci
bazel build //...
bazel test //...                         # the set a remote executor can take
bazel test //... --config=full           # that set plus the host-compile group
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
drives the host C++ compiler finds whichever one the machine executing it has; the remote image has
the platform's own and no clang, so only a test about clang itself is tagged to stay local.

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
lyra build [-o <file>] [files...]     # Build the program; ./<design name or top> by default
lyra run [files...]                   # Build the program and execute it; writes nothing here
lyra cache clear                      # Empty the store of kept programs, units and headers
```

`--backend cpp|llvm` picks which backend builds the program; either way it is one program, linked
with the host C++ compiler (`--cxx`, else `clang++`, else `c++`). **A built program is kept in a
store in the user's cache directory under a name computed from what the build read**, so building or
running an unchanged design again copies the kept program instead of compiling, several checkouts
share what they have in common, and nothing is written into the project. On the LLVM backend each
unit's object is kept the same way, so a build after an edit compiles only the units whose generated
code changed. `--rebuild` builds as though nothing were kept; `--cache-dir` names the store.
`docs/decisions/a-program-is-kept-by-what-built-it.md` holds why.

`-j` says how many of the design's units are lowered and compiled at once, on either backend, and
one is the default. What a build produces is the same however many ran at once.
`docs/decisions/a-build-is-told-how-wide-to-run.md` holds why.

Command words are positional, and everything after them is one command line shared with the slang
driver: every front-end option slang accepts -- `--top`, `-I`, `-D`, `-G`, `--single-unit`, `-y`,
`--libext`, `-f` / `-F` filelists, `-W` warnings -- reaches Lyra unchanged. A standalone `--` ends
Lyra's command line; what follows is the simulation's own argv, where LRM 21.6 plusargs go.
`lyra --help` prints the authoritative option list.

**A design declares itself in a `lyra.toml` beside it, so it is not respelled at every invocation.**
The file names the design's sources, search paths, defines, parameter overrides, tops, and the
native sources DPI-C resolves against; it is found by walking up from the working directory, and
every path in it resolves against the file's own directory. A command line naming sources uses no
declaration at all, and `--config <file>` names one outright. What is true of one invocation or one
machine -- `-o`, `--release`, `--backend`, `--cxx`, `--cache-dir` -- is refused by name, because the
file is committed and shared. `docs/decisions/project-file.md` holds the schema and the precedence
rule: material accumulates, selection is replaced.

**`--release` trades build time for simulation speed.** By default the design's own code is compiled
unoptimized on either backend, because iterating pays that compile on every edit; `--release`
optimizes it for a run long enough to earn the compile back. The runtime library the program links
is prebuilt and always optimized, so it is not on this axis and costs nothing either way.

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

**Before writing or changing anything that consumes MIR -- a backend render, the MIR-to-LIR lowering
-- read `docs/architecture/backend_contract.md`.** It holds the test that separates a spelling a
backend may choose from an operation only MIR may state, and the difference is not visible from the
code you are editing.

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

`bazel test //...` is the merge gate's own set, so a green run before committing is what says "this
lands green". Do not widen or narrow it: the answer holds only while the two are the same command.

Two targets are out of it, and they are one subject: whether an emitted C++ project still builds and
runs. `cpp_tests` asks that over the whole corpus, once per case, and `emitted_project_tests` asks
what such a project does once built. Both carry `nightly` and are reached by `--config=nightly`;
`--config=full` is both sets. Run them whenever the change touches what the C++ backend emits or how
an emitted project is built -- which is not the same as editing `backend/cpp`, since the renderer is
a function of MIR and LIR node shapes. A filtered run names only the tests it ran, so reach for
`--config=full` whenever a result has to stand as evidence. `docs/ci/README.md` holds the whole
strategy: which moment answers which question, and what a change selects.

A run someone is waiting on stops at the first case that fails; a scheduled one reports every
failure instead. So a red local run names one case and not the list, and
`--test_arg=--gtest_fail_fast=false` is how to ask for the list.

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
  so gaining an alternative breaks the build until every consumer says what it means. That last
  clause is what a catch-all switches off, so it takes three rules in
  `tools/policy/check_architecture.py`: A013 for the comparison, A020 for a `default:` arm, and A021
  for a visit arm that names no alternative.
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

Avoid `assert()` and `<cassert>`; `catch(...)` is allowed only in the command's own top frame, where
an escaped throw becomes an exit status instead of an abort (`src/lyra/cli/`). A control effect --
leaving a disabled scope (LRM 9.6.2) -- is not an error and is thrown by the runtime that defines
it; nothing else may add a thrown type.

## Approach to changes

Adding a feature: find how the neighbours already do it, and extend the existing abstraction at the
right level rather than building a parallel one. The result should read as a natural extension, not
a bolt-on.

Fixing a bug: after finding the immediate cause, ask what allowed it. Fix that, not the symptom -- a
control branch that hides the condition leaves the codebase weaker than before.
