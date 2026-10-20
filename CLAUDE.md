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
`--config=rbe`, `--config=dev` and `--config=gcc` are opt-in and each needs a BuildBuddy key from
one of the two untracked layers; nobody is required to have one. `.bazelrc.user.example` lists what
may go there.

`ci` shares a cache while actions stay local; `rbe` runs the actions remotely for a gate that reads
none of what it produces; `dev` is `rbe` for the edit loop, where top-level outputs come back so the
binary just built can be run. Under either remote-execution config the concurrency of several
sessions at once needs no local setting, because the remote scheduler is one queue across every
invocation -- the coordination that separate Bazel servers on one machine cannot have.

**Which compiler builds Lyra is a setting of its own, apart from where the actions run.** It is the
clang release `MODULE.bazel` pins, fetched by the build, wherever they run. `--config=gcc` names the
remote image's GCC instead, and runs the actions there because that is the only place it exists.
Either one compiles against the libstdc++ of the machine running the actions, because a program Lyra
builds is linked by the user's compiler against the runtime library built here. Every warning is
fatal and each compiler warns of things the other does not, so the merge gate builds under both. A
test that drives the host C++ compiler finds whichever one the machine executing it has on its PATH;
the remote image has GCC and no clang, so only a test about clang itself is tagged to stay local.

**The runtime library a program links is compiled by the clang of the LLVM release Lyra links,
whichever compiler builds Lyra**, and one number in `MODULE.bazel` names both. A build asked for
`--lto` optimizes a design together with that library's bitcode, which only that release reads
reliably, so moving Lyra's LLVM moves this clang with it
(`docs/decisions/the-design-is-optimized-with-the-runtime-at-the-link.md`).

## Lyra CLI

```bash
lyra check [files...]                 # Elaborate and report diagnostics; no lowering
lyra dump ast [files...]              # Dump slang's elaborated AST as JSON
lyra dump hir|mir|lir|llvm [files...] # Dump the named intermediate form
lyra emit cpp -o <dir> [files...]     # Write a self-contained C++ project
lyra build [-o <file>] [files...]     # Build the program; ./<library name or top> by default
lyra run [files...]                   # Build the program and execute it; writes nothing here
lyra cache clear                      # Empty the store of kept build results
```

A design is named by `--top` and the files on the command line, or by a `lyra.toml` found by walking
up from the working directory. Everything after the command word is one command line shared with the
slang driver, and a standalone `--` ends it; what follows is the simulation's own argv, where LRM
21.6 plusargs go. `--backend cpp|llvm` picks the backend, `-O0` to `-O3` trade build time for
simulation speed as a C++ compiler's do, `--lto` optimizes the design together with the runtime, and
`--time-trace` and `--stats-file` read where a run's cost went off the run itself. A build is kept
in the user's cache directory by what it read, so `--rebuild` is how to build as though nothing were
kept. `-j` says how many units are lowered and compiled at once, one by default
(`docs/decisions/a-build-is-told-how-wide-to-run.md`), and every text Lyra reads finds `__lyra__`
defined as 1 beside the front end's `__slang__` (`docs/decisions/project-file.md` D10).
`lyra --help` is the authoritative option list, and why each behaves as it does is in
`docs/decisions/`.

## SystemVerilog version

Lyra targets **IEEE 1800-2023**, defaulting the front end to `--std 1800-2023` and to its own
reading of the standard. Language analysis is the front end's alone. Lyra takes the text it accepts
at the severity it gives, except where the standard leaves the choice to the tool. Lowering never
judges legality again, and an analysis error is corrected in the front end, never in Lyra's
settings. A caller may select a tolerant reading by name (`--compat all`).
`docs/architecture/language_conformance.md` holds the rule and every disputed text ruled on so far.
Testing a 2023 feature against slang directly needs `--std 1800-2023`.

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
The gate asks two things more: that the whole graph still builds optimized, which
`bazel build -c opt //...` answers here, and that it builds under GCC, which
`bazel build //... --config=gcc` answers.

A test asserts only what one tree answers the same on every run -- a count, a value, a text -- and
never a duration or a peak memory, which say as much about the machine running it as about the tree.

Two targets are out of it, and they are one subject: whether an emitted C++ project still builds and
runs. `cpp_tests` asks that over the whole corpus, once per case, and `emitted_project_tests` asks
what such a project does once built. Both carry `nightly` and are reached by `--config=nightly`;
`--config=full` is both sets. Run them whenever the change touches what the C++ backend emits or how
an emitted project is built -- which is not the same as editing `backend/cpp`, since the renderer is
a function of MIR and LIR node shapes. A filtered run names only the tests it ran, so reach for
`--config=full` whenever a result has to stand as evidence. `docs/ci/README.md` holds the whole
strategy: which moment answers which question, and what a change selects.

One more target is in neither set and is run only by naming it: `external_design_tests` builds and
runs designs written elsewhere for other simulators -- today those of RTLMeter, Verilator's
benchmark suite -- and holds each to what `tests/paths/llvm.external_designs.yaml` records of how
far it gets. A case is minutes and the suite is half a gigabyte to fetch, so it is `manual`, and one
case is the usual thing to ask for:

```bash
bazel test //tests:external_design_tests --test_arg=--gtest_filter='*VeeR_EL2*' --test_output=streamed
```

A run someone is waiting on stops at the first case that fails; a scheduled one reports every
failure instead. So a red local run names one case and not the list, and
`--test_arg=--gtest_fail_fast=false` is how to ask for the list.

## Code style

C++23, Google style, clang-tidy warning-free. `CamelCase` classes and functions, `lower_case_`
members, `kCamelCase` enums. Use IEEE 1800 LRM terminology for SystemVerilog concepts, and prefer
the modern idiom -- `std::format`, `std::span` / `std::string_view`, `std::array`, `std::optional` /
`std::expected`, structured bindings. Comments follow `docs/code-comments.md`, and a text a user
reads follows `docs/messages.md`.

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
