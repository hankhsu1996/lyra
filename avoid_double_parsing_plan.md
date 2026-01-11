# Plan: Avoid Double Parsing in SV Feature Tests

## Goal
Eliminate redundant Slang parsing when `sv_feature_tests` runs both interpreter
and codegen for the same test case by sharing a cached AST/MIR artifact.

## Proposed Approach
Cache a parsed/ lowered representation per test case (prefer MIR) and reuse it
for both interpreter and compiler runs inside the test harness.

## Steps
1) **Define a test-level cache API** (implemented)
   - Add a small cache struct in `tests/framework` that maps a stable key
     (source content + file list + include dirs) to a cached artifact.
   - Start with MIR as the cached artifact; keep `slang::ast::Compilation`
     ownership if needed for error reporting.

2) **Expose interpreter/compiler entry points that accept MIR** (implemented)
   - Add new overloads to `Interpreter` and `Compiler` that accept pre-lowered
     MIR modules/packages (or a small wrapper type that owns them).
   - Keep existing public APIs unchanged for non-test usage.

3) **Integrate cache into `sv_feature_tests.cpp`** (implemented)
   - For each `TestCase`, compute a cache key (single-file: `sv_code`; multi-
     file: list of file names + contents).
   - On cache hit, reuse the MIR for both interpreter and codegen paths.
   - On miss, parse once, lower once, populate cache, then run both.

4) **Handle invalidation and correctness** (implemented: content-based key)
   - Ensure the cache key changes when any file content or include directory
     changes.
   - Add a small unit test that mutates input and confirms cache invalidation.

5) **Validation** (pending: run tests)
   - Run `bazel test //tests:sv_feature_tests` and confirm output matches.
   - Compare runtime before/after to verify reduced total test time.

## Risks / Notes
- MIR ownership/lifetimes: ensure cached data outlives both interpreter and
  compiler calls.
- Slang diagnostics: if you rely on `Compilation` for source ranges, store it
  in the cached bundle or reparse only on errors.
