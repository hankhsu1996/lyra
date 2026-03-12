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

Define a canonical representation for assignable targets:

- Simple variable: `v`
- Field access: `v.field`
- Constant index: `v[3]`
- Constant slice: `v[7:4]`

Two access paths alias if they refer to overlapping storage. The analysis reasons about symbolic access paths with a conservative overlap relation. The mapping to concrete slot-byte ranges is a lowering detail, not the primary abstraction.

### 2. Projection-aware must-def

A write to `v.field` does NOT must-define all of `v`. A write to `v` DOES must-define `v.field`.

Rules:

- Write to whole variable -> must-defines all projections
- Write to field/index/slice -> must-defines only that projection and sub-projections
- Read of any projection -> adds the read's byte range to the sensitivity set unless already must-defined

### 3. Path-sensitive control flow

Must-def analysis must be path-sensitive through control flow:

- Sequential: `write(v); read(v)` -> v is must-defined at the read
- If-else: v is must-defined after if-else only if BOTH branches must-define v
- Loop: conservative -- treat loop body reads as potentially first (loops may execute zero times unless guaranteed)
- Case: must-defined only if all reachable branches (including default/implicit) must-define v

### 4. Conservative fallback for hard cases

- Dynamic index: `v[i] = ...` does NOT must-define any projection (i is unknown at compile time)
- Aliased pointers: if two paths may alias, neither must-defines the other
- Function calls with side effects: conservative -- assume no must-def
- `disable` / `event` / `wait` inside `always_comb`: treat as control-flow barrier

### 5. Implementation location

The analysis runs in `CollectSensitivity()` (`src/lyra/mir/sensitivity.cpp`). Current implementation only calls `CollectFromStatement()` which processes RHS reads. The fix adds a must-def set that tracks which byte ranges have been written, and filters reads against it.

The must-def set is per-statement-list (sequential), with join operations at control-flow merge points.

### 6. Runtime resilience (keep the bucket fix)

Even with perfect compiler analysis, keep the runtime's fixpoint iteration cap and net-zero self-trigger suppression as defense-in-depth. The compiler fix reduces false edges; the runtime fix prevents divergence if any slip through.

## Scope

Phase 1: simple variables with path-sensitive must-def (sequential, if-else, case). This is the minimum correct unit -- even simple variables need control-flow sensitivity (e.g. `if (cond) a = x; y = a;` must NOT exclude `a`). Covers the pipeline fixture pattern.

Phase 2: projections (field, constant index, constant slice) with conservative alias model. A write to a projection must-defines only that projection and sub-projections; a read of a parent requires all children to be must-defined.

Phase 3: precision improvements for harder cases (loop iteration bounds, dynamic index partial coverage, cross-function analysis).

## Current Mitigation

Runtime: per-iteration worklist dedup + pre-comb snapshot comparison in `FlushAndPropagateConnections()`. This suppresses net-zero self-triggers at the slot-byte level. It is correct but coarse -- it reasons at slot granularity, not kernel-output granularity. See `docs/queues/performance.md` G7 section.
