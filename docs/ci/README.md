# CI Design Notes (Architecture Reset Context)

## Bazel CI Strategy

- Build: `bazel build //...`
- Test: `bazel test //...`
- Avoid hardcoding targets; rely on the Bazel graph so new targets are picked up automatically.

## Active Workflows

- `bazel-build.yml` -- builds and tests `//...`. The historical `aot-full` job is preserved with
  `if: false` for future re-enablement.
- `cpp-format.yml` -- runs `clang-format-20 --dry-run --Werror` over `src include tests`.
- `bazel-lint.yml` -- runs `buildifier -mode=check -r .` plus a warning pass.
- `md-format.yml` -- runs `prettier --check "**/*.md"`.
- `ascii-policy.yml` -- runs `tools/policy/check_ascii.py` on the diff vs `origin/main`.
- `exception-policy.yml` -- runs `tools/policy/check_exceptions.py` on the diff vs `origin/main`.
- `architecture-boundaries.yml` -- runs `tools/policy/check_architecture_boundaries.py` (rules
  described in that script).
- `cpp-tidy.yml` -- placeholder (steps commented out until a new pipeline lands).

## Disabled Workflows

These workflows depend on runtime artifacts and CLI shapes that the architecture reset removed
(`-C`, `run`, `compile`, `dump llvm`, `liblyra_runtime_static.a`). Triggers are intact but the main
job carries `if: false`:

- `bazel-build.yml` -> `aot-full` job
- `smoke-test.yml` -> `smoke` job
- `benchmark.yml` -> `benchmark` job
- `benchmark-nightly.yml` -> `benchmark` job
- `sigill-diagnosis.yml` -> `diagnose` job

Re-enable a job by removing its `if: false` line once the runtime layer it depends on is rebuilt.

## Historical Test Matrix (Disabled)

The previous `bazel-build.yml` test job ran a 3-target matrix on every push/PR plus a push-only
AOT-full job:

- JIT: `//tests:jit_dev_tests`
- JIT-2S: `//tests:jit_two_state_tests`
- AOT Smoke: `//tests:aot_smoke_tests`
- AOT Full (push only): `//tests:aot_dev_tests`

All four targets were deleted by the reset. The current single `bazel test //...` step subsumes them
via the Bazel graph; per-target enumeration is intentionally not reintroduced.

## LLVM Toolchain Setup (CI)

The installation snippet used by AOT/JIT-bearing jobs:

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

The `cpp-format.yml` workflow uses a smaller variant that only installs `clang-format-20`.

## RBE / BuildBuddy

Build and test steps in `bazel-build.yml` (and benchmark/smoke flows) construct RBE flags from the
`BUILDBUDDY_API_KEY` repository secret:

```bash
RBE_FLAGS=""
if [[ -n "$BUILDBUDDY_API_KEY" ]]; then
  RBE_FLAGS="--config=rbe --remote_header=x-buildbuddy-api-key=$BUILDBUDDY_API_KEY"
fi
bazel build //... $RBE_FLAGS
```

When the secret is unset (forks, dispatch from PRs without secret access) the commands fall back to
local execution.

## PR vs Push Strategy

- PR: lighter test coverage (currently `bazel test //...`).
- Push to `main`: previously also ran the AOT-full suite; that path is disabled until the runtime
  layer is rebuilt.

The split exists so AOT toolchain costs only land on push. While the runtime is offline the strategy
is simplified to a single `bazel test //...` invocation.

## Benchmark / Smoke / Sigill Workflows

These depend on `lyra -C <path> run|compile`, `dump llvm`, and the runtime static library. All three
are removed on the reset branch, so the workflows are kept disabled via `if: false`. Their LLVM
install steps and Verilator setup remain in place so the recipes can be restored verbatim once the
runtime returns.
