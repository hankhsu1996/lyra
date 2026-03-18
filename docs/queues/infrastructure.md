# Infrastructure

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Correctness, code quality, and tooling gaps that are not performance issues.

## Progress

- [x] I1 immediate fixes: canonical storage contract bug fixes
- [x] I2 double-sharding fix: test framework sharding collision
- [x] I2 temporary exclusion: gtest_filter in BUILD.bazel for known-failing cases
- [x] I3 immediate fixes: four-state query bug fix + policy check enforcement
- [ ] I1: canonical storage contract enforcement (authority collapse, domain-typed APIs, policy checker)
- [ ] I2: test expectations overlay (case-level xfail/skip/quarantine metadata)
- [ ] I3: four-state query layer cleanup (intrinsic rename, wrapper removal, audit)

## I1: Canonical storage contract enforcement

The system had two competing layout authorities for packed storage -- the backend and the runtime computed storage byte sizes differently, and multiple code paths used LLVM struct aggregate layout to address canonical arena storage. Immediate bug fixes applied, but the structural problem remains: canonical storage is exposed as raw pointers, allowing anyone to derive layout math independently.

Target direction: single shared contract module that owns all storage byte size, plane offset, and addressing arithmetic. Domain-typed APIs that prevent raw pointer access to canonical storage. Policy checker that rejects forbidden patterns.

Stages remaining: authority collapse into shared module, domain-typed APIs with explicit materialization boundaries, forbidden-pattern policy checker, debug-mode cross-component layout tripwire.

## I2: Test expectations overlay

Test cases should stay in their feature YAML files. CI exclusion and expected-failure state should be a separate metadata layer, not file reorganization or gtest_filter hacks.

Target direction: centralized expectations overlay with per-case metadata: status (unsupported / xfail / quarantine / skip), reason, issue ID, scope. Framework reads the overlay at test registration and applies GTEST_SKIP or xfail. Ratchet checks prevent stale entries.

Current workaround: gtest_filter exclusion in BUILD.bazel. This is not the target structure.
