# Change Propagation

How a write to a design variable wakes up a process that observes it.

## Signal/Slot Model

Every design variable (reg, wire, int, struct, array) maps to a **slot** in the flat DesignState allocation. Each slot has a numeric `slot_id` and byte-level metadata (offset, size, storage kind) in SlotMetaRegistry. SignalId and slot_id are the same value.

## Pipeline

```
Mutation              UpdateSet             FlushSignalUpdates
(write to slot)  ->  (dirty facts)    ->  (filter + snapshot + wakeup)
```

1. **Mutation** writes bytes to DesignState and records a dirty fact
2. **UpdateSet** accumulates dirty facts per slot, per delta
3. **FlushSignalUpdates** walks dirty slots, checks subscriptions, wakes processes

These are orthogonal: mutation sites produce facts without knowing who observes; the scheduler consumes facts without knowing who wrote.

## UpdateSet

Tracks which slots changed and what byte ranges within them.

Two levels of accumulation:

| Level           | Lifetime                                   | Consumer                       |
| --------------- | ------------------------------------------ | ------------------------------ |
| Delta dirty     | Cleared after each FlushSignalUpdates call | Scheduler (wakeup filtering)   |
| Time-slot dirty | Cleared at end of time slot                | Trace (value-change snapshots) |

Dirty ranges within a slot are kept sorted and merged on insertion. A full-slot mark produces `[0, slot_size)`, which overlaps any observation range.

## Subscriptions

A subscription binds a process to a signal with an edge type and an observation range.

| Field                  | Purpose                                                 |
| ---------------------- | ------------------------------------------------------- |
| signal                 | Which slot to watch                                     |
| edge                   | kAnyChange, kPosedge, kNegedge                          |
| byte_offset, byte_size | Observed byte range within the slot                     |
| snapshot               | Previous value of observed bytes (for change detection) |

Subscriptions are doubly-linked per signal (fast iteration during flush) and singly-linked per process (fast cleanup on resume).

**Snapshot storage**: Inline buffer (16 bytes) for small observations, heap-allocated for larger. kPosedge/kNegedge store a single bit instead.

## Wakeup Logic (FlushSignalUpdates)

Called twice per delta iteration inside a time slot: once after Active/Inactive, once after NBA.
If wakeups schedule more Active work in the same time slot, the loop repeats and both flush points run again.

For each delta-dirty slot:

1. **Range filter** -- skip subscriptions whose observation range does not overlap any dirty range
2. **Snapshot comparison** -- memcmp observed bytes against stored snapshot (kAnyChange) or compare single bit (kPosedge/kNegedge)
3. **Enqueue** -- if changed, add process to next-delta queue (de-duplicated per process)

Range filtering is a performance optimization. Snapshot comparison is the correctness backstop. A wider-than-necessary dirty range causes extra comparisons but never misses a real change.

Process enqueue order is an implementation detail. The runtime de-duplicates per process, but does not define fairness/stable ordering guarantees across different signals.

## Byte Range Contract

A byte range is a half-open interval `[offset, offset + size)` within a slot.

**By-layer representation**:

- Runtime canonical form (`UpdateSet`, `Engine::Subscribe`) uses concrete non-zero sizes only.
- ABI sentinel form uses size `0` to mean full-slot:
  - `WaitTriggerRecord`: `byte_size == 0` means "observe full slot".
  - `LyraStorePacked`: `dirty_size == 0` means "dirty full slot".

**Overlap rule**: full-slot (materialized as `[0, slot_size)`) overlaps everything. Two precise ranges overlap when their intervals intersect.

**Conservatism invariant**: The system must never produce a range narrower than the actual mutation or observation. When in doubt, fall back to full-slot.

## Sub-Slot Precision

Both read-side (observation) and write-side (dirty) ranges are resolved by walking the MIR Place projection chain at compile time.

**Supported projections** (produce precise byte ranges):

| Projection      | Condition                                                                     |
| --------------- | ----------------------------------------------------------------------------- |
| FieldProjection | Always (struct layout offset)                                                 |
| IndexProjection | Constant index into unpacked array (or temp proven constant at LLVM lowering) |

**Unsupported projections** (fall back to full-slot):

| Projection                 | Reason                                 |
| -------------------------- | -------------------------------------- |
| Dynamic IndexProjection    | Runtime value, unknown at compile time |
| BitRange / SliceProjection | Sub-word, not byte-aligned             |
| DerefProjection            | Pointer indirection                    |
| UnionMemberProjection      | Overlapping storage                    |

Multi-step chains work (e.g., `structs[1].x`) as long as every step is supported. Any unsupported step causes the entire chain to fall back to full-slot.

### Write-side precision by mutation path

| Path                                     | Precision                        | Notes                                     |
| ---------------------------------------- | -------------------------------- | ----------------------------------------- |
| LyraStorePacked (blocking assign)        | Precise when projection resolves | Most common path                          |
| NBA commit                               | Precise via pointer subtraction  | Stores slot root pointer at schedule time |
| LyraNotifySignal (aggregate notify)      | Full-slot                        | Aggregate updates                         |
| LyraReadmem / LyraFread (memory variant) | Full-slot                        | Bulk I/O                                  |
| LyraStoreString / LyraStoreDynArray      | Full-slot                        | Handle-based types                        |
| LyraNotifyContainerMutation (element)    | Precise external range           | Container element writes                  |
| LyraNotifyContainerMutation (structural) | Full-external `(0,0)`            | Container push/pop/insert/delete/resize   |
| Union memcpy                             | Full-slot                        | Overlapping storage                       |

### NBA pointer subtraction

NBA entries store `notify_base_ptr` (slot root) at schedule time. At commit time:

```
dirty_offset = write_ptr - notify_base_ptr
dirty_size   = entry.byte_size
```

Invariant: `notify_base_ptr` must be the slot root, not the projected write address. Validated at schedule time against SlotMetaRegistry. Violation is an InternalError (fast-fail).

## Time-Slot Stratification

```
Time Slot N:
  +-> Active    (blocking assignments, $display)
  |   Inactive  (#0 delays)
  +-- NBA       (nonblocking assignment commits)
  FlushSignalUpdates after Active/Inactive
  FlushSignalUpdates after NBA
  if next-delta queue not empty: repeat Active/Inactive/NBA within Time Slot N
  Postponed ($strobe, $monitor)
  Trace flush (time-slot level dirty -> value-change snapshots)

Advance to Time Slot N+1
```

FlushSignalUpdates runs at two points per delta iteration to separate blocking-side and NBA-side wakeups. Delta dirty ranges are cleared after each flush; time-slot dirty sets persist until end-of-slot trace flush.

## Interactions

**Trace**: Uses time-slot-level dirty sets (which slots changed), not byte-range dirty sets. Sub-slot precision only affects wakeup filtering.

**Edge subscriptions**: kPosedge/kNegedge require `byte_size=1` at subscription time. The runtime samples the bit at `bit_index` (0-7) within the observation byte at `byte_offset`. For root-signal edges this is byte 0, bit 0. For packed sub-expressions -- constant bit/element-selects (e.g., `@(posedge bus[11])`, `@(posedge bus[1])` on multi-dimensional packed arrays), packed struct fields (e.g., `@(posedge s.flag)`, `@(posedge s.b)` where `b` is multi-bit), and constant range/part-selects (e.g., `@(posedge bus[7:4])`, `@(posedge bus[3 +: 4])`) -- `byte_offset` and `bit_index` are derived from the LSB bit position within the packed storage. For unpacked sub-expressions -- constant-index unpacked array elements (e.g., `@(posedge arr[0])` where elements can be any width) and unpacked struct fields (e.g., `@(posedge s.flag)`, `@(posedge s.data)` where `data` is multi-bit) -- the projection chain includes IndexProjection/FieldProjection followed by BitRangeProjection{bit_offset=0, width=1} to target bit 0 (LSB) of the unpacked leaf. All multi-bit sub-expressions (packed or unpacked) use `expr[0]` (the LSB) for edge detection. All edge-triggered forms produce a `BitRangeProjection` with `width=1`. Level-sensitive waits (`@(expr)` with no edge) use the full element/field width, which may fall back to full-slot observation for multi-bit ranges.

**Late-bound edge subscriptions**: Dynamic bit-selects, indexed part-selects, and unpacked array element accesses with variable indices (e.g., `@(posedge bus[i])`, `@(posedge bus[i+j])`, `@(posedge bus[i +: 4])`, `@(posedge arr[i])` on unpacked arrays) use late-bound subscription rebinding. At suspend time, codegen evaluates the index expression and computes `byte_offset`/`bit_index` via an affine mapping (`BitTargetMapping`). For design-state index variables, runtime creates rebind subscriptions on each dependency slot. The index expression is compiled to a stack-based bytecode plan (`IndexPlanOp`) that supports reads, constants, and arithmetic (`+`, `-`, `*`, `&`, `|`, `^`, `<<`, `>>>`). When any dependency changes, `FlushSignalUpdates` runs a two-pass flush: Pass 1 evaluates rebind subscriptions (evaluates the expression plan, recomputes the bit target via `RebindSubscription`), Pass 2 evaluates edge subscriptions at their (potentially updated) targets. An epoch guard ensures each target is re-evaluated at most once per flush even when multiple dependencies change in the same delta. OOB indices deactivate the edge subscription (`is_active=false`); when the index returns in-range, the rebind reactivates it. X/Z in any dependency also deactivates the subscription. Local-variable indices skip rebinding (the index cannot change while the process is suspended). Same-delta rebinding uses snapshot-based old-bit extraction to preserve correct edge detection semantics.

**Coordinate systems for late-bound targets**: Three coordinate systems map an SV index to a storage position:

1. **SV index**: The programmer's value (e.g., `i` in `arr[i]` or `bus[i]`). IndexPlan evaluates to this (int64).
2. **Logical bit offset**: `BitTargetMapping` converts SV index to a non-negative bit position via `logical_bit = (sv_index - index_base) * index_step`. For packed types, `index_step` is 1 (descending range) or -1 (ascending range), and `index_base` is the range bound closest to bit 0. For unpacked arrays, `index_step = element_bit_stride` (ascending) or `-element_bit_stride` (descending), and `index_base = range.left` (the declared left bound). Element stride is computed from LLVM DataLayout at codegen time.
3. **Storage (byte_offset, bit_index)**: Runtime divides logical bit offset by 8. For unpacked array elements, `bit_index` is always 0 (byte-aligned elements).

**Unpacked element LSB sampling**: Edge triggers on unpacked array elements sample bit 0 of the element's packed storage (`expr[0]`). This is the first byte at the element's base address with `bit_index = 0`. For multi-bit elements (e.g., `logic [7:0]`), only a transition on the LSB triggers the edge; changes to higher bits do not.

**Container element subscriptions**: Dynamic arrays and queues store a `void*` handle in DesignState pointing to a heap-allocated `DynArrayData` structure. Edge triggers on container elements (e.g., `@(posedge d[i])`, `@(posedge q[i])`) use container-mode subscriptions that chase the handle to read element data from the heap.

Container subscriptions use **external dirty ranges** -- heap-relative byte ranges tracked separately from DesignState ranges in UpdateSet. Element writes produce precise external ranges (`off = index * elem_stride, size = elem_stride`). Structural mutations (push/pop/insert/delete/resize) produce full-external-dirty `(off=0, size=0)`. The `(0,0)` sentinel overlaps all external subscriptions on that slot, analogous to full-slot dirty for DesignState.

`DynArrayData` carries a `magic` field (validated on every handle chase) and an `epoch` counter (incremented by runtime on structural mutations). Magic prevents UAF on freed handles; epoch is stored on subscription nodes for future staleness detection.

Container subscription flush: Pass 1 rebinds always read from the heap (element content may change even if byte_offset is unchanged). Pass 2 filters active subs via external range overlap before chasing the handle. Inactive subs (OOB index) attempt reactivation only when external dirty ranges overlap their candidate offset. Reactivation captures a snapshot but does not trigger an edge.

## Worked Example

| Case                                      | Dirty range emitted | Observation range  | Range overlap | Snapshot/edge check   | Wake?                       |
| ----------------------------------------- | ------------------- | ------------------ | ------------- | --------------------- | --------------------------- |
| `arr[2]` write, process observes `arr[2]` | Precise sub-slot    | Same sub-slot      | Yes           | Value changed         | Yes                         |
| `arr[0]` write, process observes `arr[2]` | Precise sub-slot    | Different sub-slot | No            | Skipped               | No                          |
| Dynamic index write (`arr[idx]`)          | Full-slot           | `arr[2]` sub-slot  | Yes           | Value compare decides | Conservative (never missed) |

## Not In Scope

- Multi-bit sub-byte observation ranges (multi-bit packed edge triggers observe only the LSB bit)
- 4-state plane-aware byte mapping
- Multi-threaded scheduling
- Dynamic dirty range compaction
