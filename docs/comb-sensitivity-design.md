# Comb Sensitivity: Must-Def Analysis

Design for proper write-before-read exclusion in `always_comb` implicit sensitivity lists.

## Problem

IEEE 1800 says variables that are always written before being read in an `always_comb` block should be excluded from the implicit sensitivity list. Lyra's `CollectSensitivity()` currently includes ALL RHS reads without write-before-read exclusion. This creates false self-edges in the comb trigger map.

Pattern that triggers the bug:

```systemverilog
always_comb begin
  data_comb = data_in ^ (data_in >> 1);  // write data_comb
  data_comb = data_comb + data_in[15:0]; // read data_comb (but always written above)
end
```

`data_comb` appears in both the write set and the read set, but the read on line 2 is always preceded by the write on line 1. IEEE 1800 requires excluding `data_comb` from the sensitivity list. Without this, the comb kernel has a self-edge: it triggers on `data_comb` changes, then writes `data_comb`, which triggers itself.

The runtime currently mitigates this with pre-comb snapshot comparison (suppresses net-zero self-triggers). This document describes the compiler-level fix that removes the false edges at the source.

## Design

### 1. Access-path model

An access path identifies a signal target for must-def analysis. The full model is a base signal (scope + slot_id) plus zero or more path segments (field, index, slice). Two paths alias if they refer to overlapping storage.

Phase 1 implementation uses only whole-variable paths (no segments). The `AccessPath` struct and `MustDefSet` API are path-shaped to avoid rewriting when Phase 2 adds projections, but no projection logic exists yet.

### 2. Covers relation

The core question: is this read fully covered by some prior must-def write?

Phase 1: whole-variable exact match only. `MustDefSet::Covers(read_path)` returns true iff the exact path was previously inserted. This is correct but incomplete -- it cannot reason about sub-path relationships.

Phase 2 will implement the full coverage relation:

- Write whole `v` covers read `v`, `v.f`, `v[3]`, `v[7:4]`
- Write `v.f` covers read `v.f` and sub-paths of `v.f`
- Write `v.f` does NOT cover read `v` (partial)
- Write `v[3]` covers read `v[3]` but NOT `v[4]` or `v`
- Write `v[7:4]` covers read `v[5]` but NOT `v[2]` or `v`

### 3. Must-def analysis over statements

The core abstraction is a forward dataflow analysis:

```
AnalyzeBlock(block, incoming_must_def) -> { outgoing_must_def, sensitivity_reads }
```

Where:

- `incoming_must_def` = paths definitely written on all paths before this block
- For each statement, in order:
  1. Extract reads from RHS -- if covered by current must_def, exclude from sensitivity
  2. Extract writes from LHS -- if unconditional write to signal, add to must_def
- `outgoing_must_def` = must_def after all statements

Three layers keep concerns separated:

- `CollectReadsFrom*(...)` -- extract signal reads, filtered through current must-def
- `TryExtractWholeSignalPath(dest)` -- extract whole-signal write target (Phase 1 only)
- `TransferStatement(stmt, must_def, obs)` -- orchestrate reads then writes per statement

### 4. Path-sensitive control flow

Must-def analysis operates on the MIR CFG with a conservative lattice:

- **Domain:** `MustDefSet` = set of definitely-defined access paths, with a Top element (everything defined, optimistic initial state for non-entry blocks)
- **Meet:** intersection -- a path is must-defined at a join only if must-defined on ALL incoming paths
- **Transfer:** sequential extension -- a write adds to the set, a read checks against it

Control-flow rules:

- Sequential: `write(v); read(v)` -- v is must-defined at the read
- If-else (Branch): must-defined after merge only if BOTH branches must-define v
- Case (Switch/QualifiedDispatch): must-defined only if ALL reachable targets must-define v; no default means conservative drop
- Loop back-edges: do not carry body writes out as must-def (conservative -- loops may execute zero times)
- Repeat terminator: ignored for must-def (represents next activation, not control flow within this activation)

Algorithm: forward dataflow in reverse postorder, iterate to fixpoint. For acyclic CFGs (the common case for `always_comb`), one RPO pass is exact.

### 5. Conservative fallback for hard cases

Treat these as NOT must-def:

- Dynamic index: `v[i] = ...` (unknown at compile time)
- Partial writes with projections (Phase 1 only tracks whole variables)
- `GuardedAssign` (conditional -- write might not happen)
- `DeferredAssign` (NBA -- happens in a later region, not before subsequent reads)
- Function/task calls with side effects
- `disable` / `event` / `wait` inside `always_comb`

Conservative means we may miss some exclusions (sensitivity list may be larger than necessary) but we never become unsound (never exclude a variable that should be sensitive).

### 6. Runtime resilience (keep the bucket fix)

Even with perfect compiler analysis, keep the runtime's fixpoint iteration cap and net-zero self-trigger suppression as defense-in-depth. The compiler fix reduces false edges; the runtime fix prevents divergence if any slip through.

## Scope

### Phase 1: whole-variable must-def with CFG sensitivity (done)

The minimum correct unit. Even simple variables need control-flow sensitivity (e.g. `if (cond) a = x; y = a;` must NOT exclude `a`).

Implementation:

- `AccessPath` = scope + slot_id (no projection segments yet)
- `MustDefSet::Covers()` = exact whole-variable match
- `TryExtractWholeSignalPath()` = returns path only for unprojected signal roots
- CFG-based forward dataflow with RPO iteration and fixpoint convergence
- Observations accumulated monotonically across iterations (safe because later iterations only make must-def more conservative, adding reads, never removing them)

What Phase 1 covers:

- Whole-variable writes and reads only (no projections)
- Path-sensitive must-def across sequential, if-else, case
- Forward dataflow on CFG with fixpoint iteration
- Conservative on loops, calls, guarded/deferred assigns

What Phase 1 does NOT cover:

- Field paths, constant indices, constant slices
- The `Covers()` relation for sub-path coverage

Covers the clock-pipeline fixture pattern (simple variable write-then-read). Result: 737M -> 634M Ir (-14%) on clock-pipeline benchmark.

### Phase 2: projections + covers relation

Add field, constant index, and constant slice segments to access paths. Implement the `covers()` relation for projections. A write to a projection must-defines only that projection and sub-projections; a read of a parent requires all children to be must-defined.

### Phase 3: precision improvements

Better control-flow precision (selected loop precision, maybe summarized pure helper calls). Only if worth it based on real design patterns.

## Common SystemVerilog patterns

Patterns the analysis must handle correctly (not necessarily all in Phase 1):

```systemverilog
// Write-then-read (Phase 1)
a = f(x); a = a + g(x);

// Partial projections (Phase 2)
a[3] = x; y = a[3];
a[3] = x; y = a[4];  // NOT covered

// Control-flow partial (Phase 1)
if (cond) a = x; y = a;  // NOT must-defined

// Control-flow full (Phase 1)
if (cond) a = x; else a = y; z = a;  // must-defined

// Dynamic index (conservative)
a[i] = x; y = a[j];  // NOT must-defined

// Case without default (conservative)
case (sel) 2'b00: a = x; 2'b01: a = y; endcase  // NOT must-defined

// Struct mixed (Phase 2)
s.arr[3].f = x; y = s.arr[3].f;
```

## Current Mitigation

Runtime: per-iteration worklist dedup + pre-comb snapshot comparison in `FlushAndPropagateConnections()`. This suppresses net-zero self-triggers at the slot-byte level. It is correct but coarse -- it reasons at slot granularity, not kernel-output granularity. See `docs/queues/performance.md` G7 section.
