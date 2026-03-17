# Infrastructure Gaps

Correctness, code quality, and tooling gaps that are not performance issues.

A recurring infrastructure smell across these gaps is **multiple implicit authorities over the same contract**. In I1, multiple components silently became authorities over storage layout. In I2, the framework and gtest both became authorities over test execution partitioning. Future infrastructure work should prefer single-owner contracts with narrow APIs and policy enforcement. When two components can independently derive the same fact, that is a structural defect, not a style issue.

## I1: Canonical storage contract enforcement

### The problem

The system had two competing layout authorities for packed storage. The backend uses `GetStorageByteSize` (byte-aligned `ceil(width/8)` for >64 bits), while the runtime IO path had its own `GetIntegralStorageBytes` (word-aligned `ceil(width/64)*8`). Additionally, code in `default_init.cpp` and `context_place.cpp` used LLVM struct aggregate layout (`CreateStructGEP`, `load {iN, iN}`) to address canonical arena storage. LLVM's alignment rules diverge from the flat canonical layout for non-power-of-2 storage sizes (e.g., 68 bits: LLVM places element 1 at offset 16, canonical layout at offset 9).

**Root cause:** Two distinct failure modes, same underlying problem:

1. **LLVM aggregate layout vs canonical layout**: code treats canonical arena storage as an LLVM `{iN, iN}` struct object.
2. **Duplicated layout math**: backend and runtime independently implement storage byte size computation with different formulas.

The real bug is not one wrong offset. It is that the codebase allows two competing layout authorities. As long as canonical storage is exposed as a raw `llvm::Value*` pointer, anyone can `CreateStructGEP` or aggregate-store to it. And as long as layout math can be re-derived locally, someone will re-derive it differently.

### Invariant

**Canonical storage is its own domain.** It is Lyra-defined memory, not an LLVM typed object. No code outside the canonical storage layer may derive addressing, plane offsets, or storage size from any source other than the canonical storage contract API. This applies to every consumer of stored values: backend codegen, runtime, dump paths, tracing, test harnesses, serializers, debug printers, future waveform code -- everything.

**Layout math has exactly one authority.** The canonical storage contract module owns all storage byte size computation, plane offsets, and addressing arithmetic. No other module may independently compute these. The formula `(width + 7) / 8` or `FourStateUnknownLaneOffset(width)` must not appear outside the contract module. If somebody writes a fresh storage size formula anywhere else, that is a structural violation, not just a style issue.

### Immediate fixes applied

- `default_init.cpp`: replaced all `CreateStructGEP` / aggregate store with byte GEP using `FourStateUnknownLaneOffset` (5 sites)
- `context_place.cpp`: `LoadPlaceValue` / `LoadPlaceBaseValue` load wide 4-state planes separately
- `runtime/io.cpp`: `GetIntegralStorageBytes` fixed to match `GetStorageByteSize`

### Target direction

#### 1. Authority collapse

The layout contract must physically live in one shared module with one API surface. All consumers (backend, runtime, tooling) call into it. The goal is not only to remove existing duplicated formulas, but to make it impossible to add new duplicated formulas casually. The shared contract module is the only place where storage byte size, plane offsets, and addressing arithmetic may be computed.

#### 2. Domain-typed APIs

Canonical storage pointers are wrapped in a dedicated type (`CanonicalStorageRef`) that does not expose raw `llvm::Value*`. There is also a distinct SSA-side carrier (`PackedRValue`). Crossings between the two are explicit materialization / store-back boundaries. Callsites cannot write `CreateStructGEP` because they never get a raw pointer to canonical storage.

#### 3. Forbidden representation patterns

Canonical storage must never be represented as LLVM aggregate memory. Whole-value packed loads/stores from canonical memory must always decompose into plane-aware canonical helpers. This prevents regressions on both the read side (`load {iN, iN}`) and the write side (`store {iN, iN}`, `CreateStructGEP`).

#### 4. Policy enforcement

Layer boundary checkers that prohibit dangerous patterns:

- `CreateStructGEP` / aggregate typed load-store on canonical storage
- Independent storage byte size computation outside the contract module (e.g., local `(width + 7) / 8` or `((width + 63) / 64) * 8`)
- Raw `llvm::Value*` canonical pointer usage outside the emitter facade
- Layout arithmetic re-derived locally instead of calling the contract API

#### 5. Debug-mode tripwire

A debug-mode self-check that compares backend/runtime/tooling interpretations of representative widths and storage kinds. Widths: 1, 8, 32, 64, 65, 68, 127, 128. Not the primary guarantee, but a cheap tripwire for silent divergence.

### Stages

- Immediate bug fixes -- done
- Authority collapse: shared contract module, eliminate all duplicated layout math
- Domain-typed APIs: `CanonicalStorageRef` + `PackedRValue` with explicit boundaries
- Forbidden patterns: migrate all canonical access to plane-aware helpers
- Policy checker for layer boundary enforcement
- Debug-mode tripwire for cross-component layout consistency

## I2: Test infrastructure known-issue management

### The problem

Test cases should stay in their feature YAML files. CI exclusion / expected-failure / quarantine should be a separate metadata layer, not file reorganization.

No mechanism exists to skip or xfail individual test cases within a YAML file. The suite `exclude_regex` operates on file paths only. When a case fails, the only options are: exclude the entire file (losing other passing cases), move the case to a separate file (fragmenting feature coverage), or leave it failing.

Additionally, a double-sharding bug in `tests/main.cpp` was silently dropping test cases when Bazel's gtest sharding overlapped with the framework's YAML-path sharding. Fixed by `unsetenv("GTEST_TOTAL_SHARDS")` after framework sharding.

**Temporary workaround:** `--gtest_filter` exclusion in `tests/BUILD.bazel` for known-failing cases. This is not the target structure.

### Invariant

**Feature corpus expresses semantic coverage. Expectations overlay expresses current product health. Suite selection expresses execution policy.** These three must stay separate. Mixing them (e.g., moving test cases to different files for CI reasons) conflates coverage structure with transient product state.

The expectations overlay is **controlled debt**, not a convenience mechanism. Every entry must have a reason, issue ID, scope, and review/expiry expectation. Otherwise it decays into a permanent dumping ground.

### Known failing cases (as of this writing)

- `datatypes/unpacked/assoc_arrays/four_state.yaml::subfield_write` -- unsupported feature
- `system_tf/effect/display_string_packed/default.yaml::packed_wide` -- flaky (full-suite only)
- `optimization/bounds_check_elimination/default.yaml::stress_array_pattern` -- flaky (full-suite only)
- `scheduling/edge_refresh_reconcile/default.yaml::nba_only_generated_processes` -- flaky (full-suite only)
- `scheduling/edge_refresh_reconcile/default.yaml::mixed_direct_write_and_nba` -- flaky (full-suite only)

### Target direction: three-layer test architecture

#### 1. Feature corpus

YAML files describe functionality and expected values. No CI policy in test files. Cases are never moved out of feature files for CI reasons.

#### 2. Suite selection

Suite configs decide which features/tiers to run per backend. This is execution policy, separate from health state.

#### 3. Expectations overlay

Centralized metadata (`expected_failures.yaml` or similar) that marks individual cases by canonical test ID (`path::case_name`).

**Status categories** (not interchangeable):

- `unsupported`: known unimplemented functionality; not expected to pass yet
- `xfail`: known product bug; currently expected to fail deterministically
- `quarantine`: nondeterministic or infra-sensitive case; not trusted in gating
- `skip`: intentionally not run in this suite/backend for policy reasons

**Required fields per entry:** status, reason, issue ID, backend/suite scope.

**Case-level identity** is a first-class concept. A stable canonical test ID (`feature_path::case_name`) is foundational for overlay metadata, ratchets, history, and tooling.

### Ratchets

- Overlay entry for a passing test triggers warning or failure (prevents stale entries)
- New overlay entries should be rare and visible (CI summary shows overlay count by category)
- Overlay entry without issue ID is rejected
- Overlay entry pointing to nonexistent test ID is rejected

### Stages

- Double-sharding fix -- done
- Temporary gtest_filter exclusion in BUILD.bazel -- done (not target structure)
- Define `expected_failures.yaml` schema with required fields
- Add framework support: read overlay at test registration, apply GTEST_SKIP / xfail
- Migrate current gtest_filter exclusions to overlay
- Add ratchet checks
