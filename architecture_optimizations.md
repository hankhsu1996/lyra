# Lyra Architectural Optimization Notes

This is a quick scan for architectural improvements that could reduce build,
run, or test time. The items below are actionable changes rather than micro-
optimizations.

Latest scan: No items below appear fully implemented yet, so the list remains
unchanged.

## Build/Emit Time

1) Cache/skip codegen and compile when sources are unchanged
- Today `lyra run` always parses, lowers, emits, and compiles. A content-hash
  cache in `out/` keyed by source files + `lyra.toml` + Lyra version would
  avoid recompiling when nothing changed.
- Entry point is `src/bin/lyra.cpp` (emit/build/run orchestration).
- Implementation tasks:
  - Add a build fingerprint (hash) that includes: sorted source file paths,
    file contents, `lyra.toml`, Lyra version/build ID, and CLI flags.
  - Store the fingerprint in `out/.lyra_build.json` and skip emit/build when
    unchanged.
  - Teach `lyra run` to reuse existing `out/` build products when the hash
    matches, and invalidate on mismatch.

2) Reduce “always emit everything” headers in codegen
- `src/lyra/compiler/codegen.cpp` currently emits all SDK type aliases even when
  unused (see comments around always-emitting aliases).
- A feature-driven emission (similar to existing `used_features_`) would shrink
  generated C++ and speed compile times for large designs.
- Implementation tasks:
  - Track SDK alias usage in codegen (`used_features_` or a new alias bitset).
  - Emit only the aliases used by the current design.
  - Add a regression test that validates generated headers for a small module
    don’t include unused aliases.

3) Decouple embedded SDK regeneration from every core build
- The embedded SDK is generated as raw strings in a Bazel genrule
  (`docs/architecture.md`). Any SDK header change forces a full rebuild of the
  `lyra` binary.
- Consider building an optional external SDK bundle for `lyra emit`, or only
  regenerating the embedded header when `include/lyra/sdk/*` changes (hash-based
  or separate target), to keep incremental builds smaller.
- Implementation tasks:
  - Split the embedded SDK genrule into its own Bazel target with precise
    inputs (`include/lyra/sdk/*` only).
  - Optionally add a CLI flag to use an external SDK bundle for `lyra emit`
    (skip embedding in the main binary).
  - Update CI to ensure both embedded and external SDK paths are tested.

4) Interpreter include directories
- Interpreter mode bypasses toolchain checks but still lacks include-dir support
  (`src/bin/lyra.cpp` TODO). Once implemented, you can run more tests under the
  interpreter without a full C++ compile, improving dev iteration speed.
- Implementation tasks:
  - Extend `lyra.toml` parsing to provide `incdir` to interpreter runs.
  - Thread include dirs into `Interpreter::RunFromFiles`.
  - Add tests that rely on `incdir` in interpreter mode.

5) Avoid rewriting unchanged generated files
- `src/bin/lyra.cpp` writes generated files (`main.cpp`, CMake files, SDK
  headers) unconditionally, which can trigger rebuilds even when content is
  identical.
- Implementation tasks:
  - Change `WriteFile` to compare existing file contents and skip writes when
    unchanged.
  - Add a “no-op emit” test that verifies timestamps do not change when input
    is identical.
  - Ensure the SDK embedding path preserves deterministic output (stable order).

6) Skip redundant CMake configure steps
- `lyra run` and `lyra build` always run `cmake --preset default`, even when
  the build directory is already configured and inputs are unchanged.
- Implementation tasks:
  - Store a config hash (CMake preset + compiler path + toolchain) in
    `out/build/.lyra_cmake.json`.
  - Only re-run configure when the hash changes or `CMakeCache.txt` is missing.
  - Keep the stale-cache cleanup logic in `src/bin/lyra.cpp` as a fallback.

7) Prefer Ninja generator when available
- Build times depend heavily on the generator; Ninja usually outperforms Make.
- Implementation tasks:
  - Update `GenerateCMakePresets` to prefer Ninja if found, otherwise fallback.
  - Add a CLI flag to override generator (`--generator=ninja|make`).
  - Add a toolchain check to ensure Ninja is present when selected.

## Runtime (Simulation) Performance

1) Reduce O(N^2) trigger checks in scheduler
- `include/lyra/sdk/scheduler.hpp` uses `std::vector` waiters, then deduplicates
  with `std::ranges::any_of` (O(N^2) in `CollectTriggeredHandles`).
- Use an `unordered_set` of handle addresses or a stamp-based dedupe to make
  trigger collection O(N).
- Implementation tasks:
  - Add a fast dedupe structure (e.g., `unordered_set<void*>`) during trigger
    collection.
  - Replace the `any_of`-based dedupe in `CollectTriggeredHandles`.
  - Add a microbenchmark (or synthetic test) to validate reduced overhead.

2) Replace `std::function` in waiters with lighter alternatives
- `Waiter` stores `std::function<bool()>`, which adds heap allocation and
  virtual dispatch in hot paths. Consider a small function buffer or typed
  trigger objects to lower overhead.
- Implementation tasks:
  - Introduce a lightweight function wrapper (small buffer optimization) or
    store trigger structs with inline call operators.
  - Update `RegisterWait` to accept the new trigger type.
  - Verify no extra allocations in the hot path (perf test or logging).

3) Delay queue data structure
- Delay scheduling uses `std::map<uint64_t, vector<handle>>`, which is fine but
  adds logN overhead per schedule. A binary heap of (time, handle) or a min-heap
  of buckets could reduce overhead for dense workloads.
- Implementation tasks:
  - Prototype a heap-based delay queue and compare with `std::map` in a
    synthetic workload.
  - Keep stable ordering at equal times if required (vector bucket per time).
  - Switch implementation behind a small interface for easy rollback.

4) Interpreter trigger checks
- The interpreter’s wake/check path mirrors SDK scheduling logic. A shared or
  optimized trigger index (by variable) could reduce per-time-slot scanning.
- Implementation tasks:
  - Build a trigger index keyed by variable symbol and edge type.
  - Update wake logic to consult the index rather than scan all waiters.
  - Add a stress test to validate correctness under heavy event loads.

5) Interpreter trigger cleanup complexity
- `TriggerManager::CheckTriggers` scans `wait_set_` to remove triggered entries,
  which is O(N) over all waits and can dominate for large designs.
- Implementation tasks:
  - Add a reverse index from process instance to its waiting entries.
  - Remove triggered waits via that index instead of scanning `wait_set_`.
  - Add a microbenchmark to track trigger cleanup overhead.

6) Reduce per-waiter state in SDK triggers
- `include/lyra/sdk/wait_event.hpp` allocates per-waiter lambdas that capture
  previous values; this can be heavy for large sensitivity lists.
- Implementation tasks:
  - Move previous-value tracking into `Scheduler` or variable storage.
  - Replace `std::function` with a lightweight trigger object that can query
    scheduler state.
  - Validate edge detection correctness with a targeted test.

## Test Time

1) Avoid double parsing in tests
- `tests/framework/sv_feature_tests.cpp` runs interpreter and codegen
  separately per test case, re-parsing each time. Sharing AST/MIR across both
  paths (or caching per test case) would reduce test time.
- Status: Implemented in `feature/test-mir-cache` (shared Slang compilation per
  test case and reused for interpreter + codegen).
- Implementation tasks:
  - Add a shared compile/cache layer for test cases (AST or MIR cache). ✅
  - Reuse the cached artifact for both interpreter and codegen in the test
    harness. ✅
  - Add test assertions to ensure cache invalidates on file changes. ✅

2) Add a “fast” test profile
- Add a test mode that runs interpreter-only for most tests, and a smaller
  subset for codegen. This reduces total runtime while preserving coverage.
- Implementation tasks:
  - Add a `--fast` (or env var) to gate which tests run codegen vs interpreter.
  - Maintain a curated list of codegen-critical tests.
  - Add CI job(s) for full mode, and keep fast mode for local dev.

3) Filter YAML loading using gtest filters
- `sv_feature_tests` currently loads all YAML files even if a narrow
  `--gtest_filter` is used, which can waste time for targeted runs.
- Implementation tasks:
  - Read `::testing::GTEST_FLAG(filter)` or `GTEST_FILTER` env var early.
  - Skip YAML files whose category prefix can’t match the filter.
  - Add a unit test that verifies only matching YAML files are parsed.

4) Reduce temp file churn in tests
- Multi-file tests write to a shared temp directory on each test, causing IO
  overhead and possible contention.
- Implementation tasks:
  - Use per-test temp directories (e.g., include test name in path).
  - Cache file contents across interpreter/codegen runs within the same test.
  - Clean temp dirs after test completion to avoid buildup.

## Notes / References

- Build/emit pipeline: `src/bin/lyra.cpp`, `docs/architecture.md`
- Codegen behavior: `src/lyra/compiler/codegen.cpp`
- Scheduler hot path: `include/lyra/sdk/scheduler.hpp`
- Test harness: `tests/framework/sv_feature_tests.cpp`
