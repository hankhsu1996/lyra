# CI Design Notes

## Strategy

Build and test by asking for the whole graph -- `bazel build //...`, `bazel test //...` -- rather
than naming targets, so a new target is covered the moment it exists and no list has to be kept in
step with the `BUILD` files.

One split cuts across that, on cost. Three targets spawn the host C++ compiler once per case, which
dominates their runtime and does not fit a merge-time budget: `cpp_tests`, `run_tests`, and
`pch_audit_test`, all tagged `requires-host-cxx`. The merge gate filters them out with
`--test_tag_filters=-requires-host-cxx`, and `host-cxx-nightly.yml` runs exactly them with the
positive form of the same filter.

The two filters are complements, so every test target is covered once and none twice. That is the
property to preserve: a new target joins whichever side its tag puts it on, and neither list is
maintained by hand. What it costs is latency rather than coverage. The C++ backend accepts more of
the language than the execution backend does, and a regression in it surfaces within a day rather
than at the merge that caused it.

## Gating workflows

Each runs on push to `main` and on pull requests.

| Workflow               | What it enforces                                               |
| ---------------------- | -------------------------------------------------------------- |
| `bazel-build.yml`      | `bazel build //...`, then the tests minus `requires-host-cxx`  |
| `cpp-style.yml`        | `clang-format` over `src include tests`, plus C++ style policy |
| `bazel-lint.yml`       | `buildifier` formatting and lint warnings                      |
| `md-format.yml`        | Prettier over every markdown file                              |
| `ascii-policy.yml`     | ASCII-only, on the diff against `origin/main`                  |
| `exception-policy.yml` | The thrown-type policy, on the same diff                       |
| `architecture.yml`     | Layer boundaries between the IRs and the backends              |
| `docs-policy.yml`      | The doc claims a machine can settle (paths, links, indexes)    |

## Nightly workflows

Two run on a schedule and on demand rather than per merge, for the same reason and with opposite
severity.

`cpp-tidy.yml` re-analyzes every translation unit from scratch. It **reports rather than gates**, so
its findings are a backlog to pay down and a red run is not an alarm.

`host-cxx-nightly.yml` runs the three `requires-host-cxx` targets. It **gates its own run**: a
corpus case that stops passing is a regression, and a red nightly is the thing to act on that day.

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

`bazel-build.yml` and the benchmark and smoke flows build their remote flags from the
`BUILDBUDDY_API_KEY` repository secret:

```bash
RBE_FLAGS=""
if [[ -n "$BUILDBUDDY_API_KEY" ]]; then
  RBE_FLAGS="--config=rbe --remote_header=x-buildbuddy-api-key=$BUILDBUDDY_API_KEY"
fi
bazel build //... $RBE_FLAGS
```

With the secret unset -- a fork, or a dispatch without secret access -- the same commands run
locally. Nothing requires an account.

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
