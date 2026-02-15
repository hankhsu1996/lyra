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

**Edge subscriptions**: kPosedge/kNegedge require `byte_size=1` at subscription time. The runtime samples the bit at `bit_index` (0-7) within the observation byte at `byte_offset`. For root-signal edges this is byte 0, bit 0. For constant bit-selects (e.g., `@(posedge bus[11])`), `byte_offset` and `bit_index` are derived from the bit position within the packed storage.

## Worked Example

| Case                                      | Dirty range emitted | Observation range  | Range overlap | Snapshot/edge check   | Wake?                       |
| ----------------------------------------- | ------------------- | ------------------ | ------------- | --------------------- | --------------------------- |
| `arr[2]` write, process observes `arr[2]` | Precise sub-slot    | Same sub-slot      | Yes           | Value changed         | Yes                         |
| `arr[0]` write, process observes `arr[2]` | Precise sub-slot    | Different sub-slot | No            | Skipped               | No                          |
| Dynamic index write (`arr[idx]`)          | Full-slot           | `arr[2]` sub-slot  | Yes           | Value compare decides | Conservative (never missed) |

## Not In Scope

- Part-select / packed range-select (sub-byte granularity beyond single-bit)
- 4-state plane-aware byte mapping
- Multi-threaded scheduling
- Dynamic dirty range compaction
