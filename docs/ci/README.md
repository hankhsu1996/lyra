# CI Design Notes

## Strategy

Build and test by asking for the whole graph -- `bazel build //...`, `bazel test //...` -- rather
than naming targets, so a new target is covered the moment it exists and no list has to be kept in
step with the `BUILD` files.

One split cuts across that, on cost, and it is one target wide. `cpp_tests` host-compiles the whole
corpus once per case, which no per-PR budget holds; it carries `nightly`, `.bazelrc` makes the
default set its complement, and `host-cxx-nightly.yml` takes `--config=nightly`. `--config=full` is
both. So a developer running the tests locally runs what the gate runs without naming anything, and
the expensive target is asked for rather than avoided.

Needing a host compiler is a separate question from being too expensive to gate on, and four targets
need one: `cpp_tests`, `llvm_dpi_tests`, `cli_tests`, and `pch_audit_test`. All four carry
`no-remote-exec`, because a compiler spawned from inside a test is not a Bazel action and remote
execution cannot provision it. Only the first is excluded from the gate. The other three cost a
fraction of a minute between them, and a target kept out of the gate should be kept out for its own
reason -- holding the DPI cases back because they share a compiler with the expensive one is how
foreign-boundary regressions reach `main` and wait a day to be found.

The two sides of `nightly` are complements, so every test target is covered once and none twice.
That is the property to preserve: a new target joins whichever side its tag puts it on, and neither
list is maintained by hand.

## How the corpus divides into targets

The conformance corpus is one set of cases, and a target is a pair: which path runs the case, and
what the case needs to run at all. The second half is a property of the case rather than of the path
-- a case carrying foreign sources builds them with the host C compiler wherever it runs, because a
path changes how the design is translated and not how a foreign symbol is produced. Keeping the two
apart is what stops a handful of DPI cases holding the whole corpus to a machine that has a C
compiler.

| Target           | Runs                           | Where  | At merge time |
| ---------------- | ------------------------------ | ------ | ------------- |
| `llvm_tests`     | the corpus minus foreign cases | remote | yes           |
| `llvm_dpi_tests` | the foreign cases              | local  | yes           |
| `cpp_tests`      | the whole corpus               | local  | no            |

`llvm_tests` is therefore the gate, and it grows on its own: `tests/paths/llvm.yaml` records what
that path still refuses, only ever shrinks, and is the measure of how much of the corpus the merge
gate actually covers.

Which column a target sits in has a direction. Remote execution scales out, so what `llvm_tests`
covers costs the gate almost nothing however far it grows -- every case the LLVM path gains is
coverage the gate gets for free. `cpp_tests` is local and its cost grows with the corpus. Needing a
host compiler for the C++ _path_ is therefore transitional and shrinks as that path stops being the
one under test; needing one for a _case_ that carries foreign sources is permanent, because nothing
about a path changes how a foreign symbol is produced. The two look alike today and have opposite
futures, which is why they are not one tag.

## When to run what

Four moments, each answering a different question, and the answers are not interchangeable.

| Moment     | Question                      | Command                                      |
| ---------- | ----------------------------- | -------------------------------------------- |
| edit loop  | did that edit do what I meant | one case, or the binary on one file directly |
| pre-commit | will this land green          | `bazel test //...`                           |
| merge gate | is `main` still correct       | `bazel test //...`                           |
| nightly    | is the C++ path still correct | `bazel test //... --config=nightly`          |

Pre-commit and the merge gate are the same command, and that is the whole point: a green run before
committing means "this lands green" only while the two sets are identical. Any change that makes the
local command wider or narrower than the gate turns its answer back into a guess.

On top of the default set, what a change touches selects what else to run:

- **What the C++ backend emits** -- `--config=nightly`, the only thing that compiles emitted text.
  Read this one carefully: it is not "did I edit `backend/cpp`". The renderer is a function of MIR
  and LIR node shapes, so changing one of those shapes changes the emit without touching the
  backend, and a green default run says nothing about whether the result compiles. Emitting one case
  and reading the file catches most of that class in seconds.
- **The foreign boundary** -- `llvm_dpi_tests`, which the gate already runs.
- **The driver, the CLI, or the prelude PCH** -- `cli_tests` and `pch_audit_test`, likewise already
  in the gate.
- **Anything else** -- HIR, MIR, LIR, the execution backend, the runtime value library -- the
  default set is the whole answer.

As the LLVM path fills in, this shortens rather than grows: `cpp_tests` stops being the backend
under test, and what remains is one permanent local target for the cases that carry foreign sources.

## Why one way of running the module is enough

The LLVM module can be run by the JIT, compiled ahead of time, or interpreted. One module has one
meaning, so running it three ways and comparing measures LLVM's execution engines rather than
anything Lyra decided, and the corpus runs it once.

The exception is not a conformance question. Compiling ahead of time optimizes, and an optimizer is
free to do anything at all with a module that contains undefined behaviour -- so the one way the two
can disagree is when the emitted module is already wrong in a way the JIT happens not to expose.
That, plus linking the runtime as a static library rather than resolving it in process, is what an
ahead-of-time run is worth testing for, and neither is a statement about IEEE 1800. It belongs in
the same place the C++ path is heading: a small set of designs, run for the artifact rather than for
the claim.

Interpreting is not worth running at all while it is not a shipping mode. Where it diverges, it
diverges because the interpreter is incomplete, which is a fact about that interpreter.

The C++ path is on its way out of this table. Its subject is not what IEEE 1800 requires -- the
execution backend answers that -- but whether an emitted project still builds and runs under a host
toolchain, which makes it a tool test rather than a conformance path. When the execution backend
covers enough of the language, it stops running the corpus and becomes a small, explicitly chosen
set of designs, sitting beside `cli_tests` rather than beside `llvm_tests`. Nothing in the corpus
has to change for that, because no case names a path.

## Gating workflows

Each runs on push to `main` and on pull requests.

| Workflow               | What it enforces                                               |
| ---------------------- | -------------------------------------------------------------- |
| `bazel-build.yml`      | `bazel build //...`, then the default test set                 |
| `cpp-style.yml`        | `clang-format` over `src include tests`, plus C++ style policy |
| `bazel-lint.yml`       | `buildifier` formatting and lint warnings                      |
| `md-format.yml`        | Prettier over every markdown file                              |
| `ascii-policy.yml`     | ASCII-only, on the diff against `origin/main`                  |
| `exception-policy.yml` | The thrown-type policy, on the same diff                       |
| `architecture.yml`     | Layer boundaries between the IRs and the backends              |
| `docs-policy.yml`      | The doc claims a machine can settle (paths, links, indexes)    |

`bazel-build.yml` runs its two commands as steps of one job rather than as two jobs. The separate
durations are worth having, but a second runner would not inherit the first's analysis: Bazel holds
that in its server's memory, no cache moves it between machines, and it is the dominant cost here --
around fifty seconds against a critical path of one, because both commands find every action already
cached remotely. One job keeps the two durations honest, since the first pays for analysis once and
the second then reports what running the tests costs. A compile failure still shows as the build
step going red.

## Nightly workflows

Two run on a schedule and on demand rather than per merge, for the same reason and with opposite
severity.

`cpp-tidy.yml` re-analyzes every translation unit from scratch. It **reports rather than gates**, so
its findings are a backlog to pay down and a red run is not an alarm.

`host-cxx-nightly.yml` runs the `nightly` target. It **gates its own run**: a corpus case that stops
passing is a regression, and a red nightly is the thing to act on that day.

## Jobs waiting on the execution backend

These carry `if: false`. Their triggers and setup steps are intact so each returns by deleting that
one line.

| Workflow                | Job         | Waiting on                                           |
| ----------------------- | ----------- | ---------------------------------------------------- |
| `smoke-test.yml`        | `smoke`     | A design running end to end on the execution backend |
| `benchmark.yml`         | `benchmark` | The same, plus a stable number to compare against    |
| `benchmark-nightly.yml` | `benchmark` | The same                                             |
| `sigill-diagnosis.yml`  | `diagnose`  | The LLVM path it diagnoses                           |

What they need is not a CLI surface -- `run`, `compile`, and `dump llvm` all exist, and the runtime
ships as a static library -- but a design that survives the whole execution-backend path. Until it
does, these jobs would measure a failure rather than a result.

## Remote execution

Every flow that can run remotely takes its flags from the `BUILDBUDDY_API_KEY` repository secret.
With the secret unset -- a fork, or a dispatch without secret access -- the same commands run
locally. Nothing requires an account.

A flow running one bazel command builds them inline:

```bash
RBE_FLAGS=""
if [[ -n "$BUILDBUDDY_API_KEY" ]]; then
  RBE_FLAGS="--config=rbe --remote_header=x-buildbuddy-api-key=$BUILDBUDDY_API_KEY"
fi
bazel build -c opt //:lyra $RBE_FLAGS
```

`bazel-build.yml` runs two, so it appends them to `.bazelrc.user` instead -- the untracked layer the
build configuration already reserves for one checkout's own key. Two command lines can drift apart;
one rc file cannot, and flags that differ between the two invocations would throw away the analysis
that keeping them in one job exists to share.

Remote execution resolves the toolchain registered for the remote platform, which is that image's
system GCC rather than the clang a local build picks up. A change can therefore compile in one place
and fail in the other, usually through the standard library; `CLAUDE.md` states the boundary.

## LLVM toolchain

A job that needs a specific LLVM gets it from the `setup-clang` composite action, naming only the
extra tools it wants:

```yaml
- name: Setup Clang
  uses: ./.github/actions/setup-clang
  with:
    tools: llvm
```

The action also points the unsuffixed names at that release, and every call site uses them: a job
runs `clang-format` or `run-clang-tidy`, never a version-suffixed binary. So the release is written
once, inside the action, and a bump is one edit with nothing else to keep in step.

Pointing a name at a release and that name resolving there are different facts, and the action
verifies the second one before any job proceeds. It has to: `update-alternatives` governs `/usr/bin`
alone, so anything earlier on PATH still wins, and it declines a path that is already a plain file
rather than a symlink. Both failures are silent, and a job that installs a toolchain and then
compiles with the runner's own would go green while testing something nobody asked for. The check
prints each tool's full `--version` output and fails the step on a major-version mismatch, so the
wrong toolchain surfaces at setup rather than as a puzzling result much later.

It checks only the names the job installed a release binary for. The runner image ships older LLVM
tools at unsuffixed paths -- a `clang-tidy` several majors behind is there whether or not anything
asked for it -- and failing a job over a tool it never invokes would be a false alarm rather than
the hazard. A job that wants a tool covered asks for it in `tools`.

`bazel-build.yml` deliberately installs nothing. It compiles under the runner image's own clang, and
under the remote image's GCC when the RBE key is set, so the merge gate sees two standard libraries
rather than one pinned toolchain. That divergence is coverage, not an oversight, and unifying it
away would delete the only place a standard-library incompatibility surfaces before release.
