# CI Design Notes

## Strategy

Build and test by asking for the whole graph -- `bazel build //...`, `bazel test //...` -- rather
than naming targets, so a new target is covered the moment it exists and no list has to be kept in
step with the `BUILD` files.

One filter cuts across that. The test job passes `--test_tag_filters=-requires-host-cxx`, and three
targets carry the tag: `cpp_tests`, `run_tests`, and `pch_audit_test`. Each spawns the host C++
compiler once per case, which is the dominant cost in all three. What that filter excludes is the
whole of the C++ backend's corpus, so what CI gates today is the execution backend's claimed set
plus the unit tests; the C++ backend is verified locally, before a commit, and not again on merge.
This is a real hole rather than a rule about scope, and the ratio makes it worse over time: the C++
backend claims roughly twice the corpus the execution backend does.

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

`cpp-tidy.yml` runs nightly and on demand rather than per merge: clang-tidy re-analyzes every
translation unit from scratch, which does not fit a merge-time budget. It reports rather than gates,
so its findings are a backlog to pay down.

## Jobs waiting on the execution backend

Five jobs carry `if: false`. Their triggers and setup steps are intact so each returns by deleting
that one line.

| Workflow                | Job         | Waiting on                                           |
| ----------------------- | ----------- | ---------------------------------------------------- |
| `bazel-build.yml`       | `aot-full`  | A test target that does not exist yet                |
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

The jobs that need a specific clang install it themselves:

```yaml
- name: Install LLVM 20
  run: |
    wget -qO /tmp/llvm.key https://apt.llvm.org/llvm-snapshot.gpg.key
    sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/apt.llvm.org.gpg /tmp/llvm.key
    . /etc/lsb-release
    echo "deb http://apt.llvm.org/${DISTRIB_CODENAME}/ llvm-toolchain-${DISTRIB_CODENAME}-20 main" | sudo tee /etc/apt/sources.list.d/llvm.list
    sudo apt-get update
    sudo apt-get install -y clang-20 llvm-20
    sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-20 100
    sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-20 100
    sudo update-alternatives --install /usr/bin/lli lli /usr/bin/lli-20 100
    sudo update-alternatives --set clang++ /usr/bin/clang++-20
    sudo update-alternatives --set clang /usr/bin/clang-20
    sudo update-alternatives --set lli /usr/bin/lli-20
```

`cpp-style.yml` installs only `clang-format-20`, since that is all it runs.
