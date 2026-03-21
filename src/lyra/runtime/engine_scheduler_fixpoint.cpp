#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"

namespace lyra::runtime {

void Engine::InitConnectionBatch(std::span<const ConnectionDescriptor> descs) {
  if (descs.empty()) return;

  // Sort by trigger_slot_id so each group is contiguous
  struct IndexedConn {
    uint32_t trigger_slot_id;
    BatchedConnection conn;
  };
  std::vector<IndexedConn> sorted;
  sorted.reserve(descs.size());
  for (const auto& d : descs) {
    if (d.trigger_byte_size > 0) {
      ++conn_narrow_count_;
    } else {
      ++conn_full_slot_count_;
    }
    sorted.push_back(
        {d.trigger_slot_id, BatchedConnection{
                                .src_byte_offset = d.src_byte_offset,
                                .dst_byte_offset = d.dst_byte_offset,
                                .byte_size = d.byte_size,
                                .dst_slot_id = d.dst_slot_id}});
  }
  std::ranges::sort(sorted, {}, &IndexedConn::trigger_slot_id);

  all_connections_.reserve(sorted.size());
  for (const auto& s : sorted) {
    all_connections_.push_back(s.conn);
  }

  // Build dense trigger map: trigger_slot_id -> {start, count}
  // Sized from authoritative slot count (same universe as signal_waiters_).
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::InitConnectionBatch",
        "InitConnectionBatch before InitSlotMeta");
  }
  conn_trigger_map_.resize(slot_meta_registry_.Size());

  uint32_t i = 0;
  while (i < sorted.size()) {
    uint32_t trigger = sorted[i].trigger_slot_id;
    uint32_t start = i;
    while (i < sorted.size() && sorted[i].trigger_slot_id == trigger) {
      ++i;
    }
    conn_trigger_map_[trigger] = {.start = start, .count = i - start};
  }
}

void Engine::EvaluateAllConnections() {
  if (all_connections_.empty() || design_state_base_ == nullptr) return;
  auto design_state = std::span(
      static_cast<uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());
  for (const auto& conn : all_connections_) {
    auto* src = &design_state[conn.src_byte_offset];
    auto* dst = &design_state[conn.dst_byte_offset];
    if (std::memcmp(dst, src, conn.byte_size) != 0) {
      std::memcpy(dst, src, conn.byte_size);
      MarkSlotDirty(conn.dst_slot_id);
    }
  }
}

void Engine::InitCombKernels(
    std::span<const uint32_t> words,
    std::span<const ProcessDescriptorEntry> descriptors,
    uint32_t num_connection, void** states) {
  if (words.empty()) return;

  // Word table format:
  // [num_comb, (proc_idx, flags, num_triggers, (slot, byte_off, byte_size)..)*]
  uint32_t pos = 0;
  uint32_t num_comb = words[pos++];

  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::InitCombKernels", "InitCombKernels before InitSlotMeta");
  }

  auto proc_states = std::span(states, num_processes_);

  comb_kernel_flags_.resize(num_processes_, 0);

  struct ParsedTrigger {
    uint32_t slot_id;
    uint32_t kernel_idx;
    uint32_t byte_offset;
    uint32_t byte_size;
    bool has_self_edge;
  };
  std::vector<ParsedTrigger> entries;

  for (uint32_t ki = 0; ki < num_comb; ++ki) {
    if (pos + 3 > words.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels", "word table truncated");
    }
    uint32_t proc_idx = words[pos++];
    uint32_t flags = words[pos++];
    uint32_t num_triggers = words[pos++];
    if (pos + num_triggers * 3 > words.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels", "trigger list truncated");
    }

    if (proc_idx >= num_processes_) {
      throw common::InternalError(
          "Engine::InitCombKernels",
          std::format(
              "comb kernel proc_idx {} exceeds num_processes {}", proc_idx,
              num_processes_));
    }

    // Comb kernels are always module processes.
    if (proc_idx < num_connection) {
      throw common::InternalError(
          "Engine::InitCombKernels",
          std::format(
              "comb kernel proc_idx {} is below connection boundary {}",
              proc_idx, num_connection));
    }

    uint32_t desc_idx = proc_idx - num_connection;
    if (desc_idx >= descriptors.size()) {
      throw common::InternalError(
          "Engine::InitCombKernels",
          std::format(
              "descriptor index {} exceeds descriptor count {}", desc_idx,
              descriptors.size()));
    }

    if ((flags & CombKernel::kSelfEdge) != 0) {
      has_any_self_edge_comb_ = true;
    }

    // Resolve body pointer from descriptor table.
    auto body =
        reinterpret_cast<SharedBodyFn>(descriptors[desc_idx].shared_body);

    auto comb_idx = static_cast<uint32_t>(comb_kernels_.size());
    comb_kernels_.push_back(
        CombKernel{
            .body = body,
            .frame = proc_states[proc_idx],
            .process_index = proc_idx,
            .flags = flags,
        });

    comb_kernel_flags_[proc_idx] = 1;

    bool kernel_self_edge = (flags & CombKernel::kSelfEdge) != 0;
    for (uint32_t ti = 0; ti < num_triggers; ++ti) {
      uint32_t trigger_slot = words[pos++];
      uint32_t byte_offset = words[pos++];
      uint32_t byte_size = words[pos++];
      entries.push_back(
          {trigger_slot, comb_idx, byte_offset, byte_size, kernel_self_edge});
      if (byte_size > 0) {
        ++comb_narrow_count_;
      } else {
        ++comb_full_slot_count_;
      }
    }
  }

  if (entries.empty()) return;

  // Allocate persistent scratch storage for FlushAndPropagateConnections.
  // Sized once here; lazy-clear pattern in the hot loop resets only touched
  // elements per iteration.
  uint32_t slot_count = slot_meta_registry_.Size();
  fp_work_.pending_seen.resize(slot_count, 0);
  if (has_any_self_edge_comb_) {
    fp_work_.snapshot_index.assign(slot_count, UINT32_MAX);
  }

  // Sort by slot_id for contiguous grouping.
  std::ranges::sort(entries, {}, &ParsedTrigger::slot_id);

  // Build flat backing array and dense range table.
  // Sized from authoritative slot count (same universe as signal_waiters_).
  comb_trigger_backing_.reserve(entries.size());
  comb_trigger_map_.resize(slot_meta_registry_.Size());

  uint32_t i = 0;
  while (i < entries.size()) {
    uint32_t slot = entries[i].slot_id;
    auto start = static_cast<uint32_t>(comb_trigger_backing_.size());
    while (i < entries.size() && entries[i].slot_id == slot) {
      comb_trigger_backing_.push_back({
          .kernel_idx = entries[i].kernel_idx,
          .byte_offset = entries[i].byte_offset,
          .byte_size = entries[i].byte_size,
          .has_self_edge = entries[i].has_self_edge,
      });
      ++i;
    }
    uint32_t count =
        static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
    comb_trigger_map_[slot] = {.start = start, .count = count};
    comb_trigger_slots_.push_back(slot);
  }
}

void Engine::SeedCombKernelDirtyMarks() {
  for (uint32_t trigger_slot : comb_trigger_slots_) {
    MarkSlotDirty(trigger_slot);
  }
}

void Engine::FlushAndPropagateConnections() {
  if (detailed_stats_enabled_) {
    auto pending = update_set_.DeltaDirtySlots().size();
    ++stats_.detailed.prop_calls_total;
    stats_.detailed.prop_pending_slots_total += pending;
    if (pending > 0) {
      ++stats_.detailed.prop_calls_with_work;
    } else {
      ++stats_.detailed.prop_calls_without_work;
    }
  }
  if (update_set_.DeltaDirtySlots().empty()) {
    return;
  }
  bool has_conns = !all_connections_.empty();
  bool has_combs = !comb_kernels_.empty();
  if (!has_conns && !has_combs) {
    FlushSignalUpdates();
    update_set_.ClearDelta();
    return;
  }

  // Fixed-point propagation of connections and comb kernels.
  //
  // Two tracking channels serve different purposes:
  //   update_set_ (delta_dirty_/delta_seen_): cross-phase dirty tracking for
  //     scheduler wakeup and trace snapshots. Uses dedup (delta_seen_) so each
  //     slot appears at most once per delta.
  //   pending/next_pending (local work list): fixed-point propagation
  //     scheduling within this function only. Per-iteration dedup via
  //     pending_seen[] ensures each slot appears at most once per iteration,
  //     preventing exponential blowup from comb kernel intermediate writes.
  //
  // The local work list exists because delta_seen_ dedup is wrong for
  // convergence: if a comb kernel writes an intermediate value then re-writes
  // the correct value in a later iteration, the corrected write would be
  // invisible to delta_dirty_ (already deduped). The local list captures
  // comb writes via comb_write_capture_ which bypasses delta_seen_.
  //
  // Using a local vector also avoids span invalidation: iterating
  // delta_dirty_ while MarkSlotDirty pushes to it would be UB.
  constexpr uint32_t kMaxIterations = 100;
  const bool detailed = detailed_stats_enabled_;
  uint32_t iterations_used = 0;
  auto design_state = std::span(
      static_cast<uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  // Seed work list from current delta dirty slots.
  auto initial = update_set_.DeltaDirtySlots();
  fp_work_.pending.clear();
  fp_work_.pending.insert(
      fp_work_.pending.end(), initial.begin(), initial.end());

  // Per-iteration dedup bitvector. Prevents exponential blowup when comb
  // kernels write intermediate values (e.g. a = f(x); a = a + g(x)) that
  // differ from the final value. Lazy-clear resets only touched slots each
  // iteration. Allocated on first call (covers both conn-only and comb cases).
  uint32_t slot_count = slot_meta_registry_.Size();
  if (fp_work_.pending_seen.size() < slot_count) {
    fp_work_.pending_seen.resize(slot_count, 0);
  }
  if (has_any_self_edge_comb_ && fp_work_.snapshot_index.size() < slot_count) {
    fp_work_.snapshot_index.assign(slot_count, UINT32_MAX);
  }

  // Helper: enqueue a slot into next_pending with dedup.
  auto enqueue_pending = [&](uint32_t slot_id) {
    if (detailed) ++stats_.detailed.prop_enqueue_attempts;
    if (slot_id < slot_count && fp_work_.pending_seen[slot_id] == 0) {
      fp_work_.pending_seen[slot_id] = 1;
      fp_work_.next_pending.push_back(slot_id);
    } else {
      if (detailed) ++stats_.detailed.prop_enqueue_deduped;
    }
  };

  // Pre-comb snapshot state for self-trigger suppression. Only allocated when
  // at least one kernel has self-edge risk (has_any_self_edge_comb_). A kernel
  // triggered by slot S may write intermediate values to S that differ from the
  // final value. Both intermediate and final writes pass LyraStorePacked's
  // memcmp, but the NET change (pre-comb vs post-comb) is zero. Without
  // snapshot comparison, the slot oscillates indefinitely.
  //
  // For kernels WITHOUT self-edges: skipping snapshot is safe because those
  // kernels cannot trigger themselves, so multi-write intermediate values at
  // most cause one bounded extra fixpoint iteration (store-level memcmp catches
  // the no-op on the next pass).

  for (uint32_t iter = 0; iter < kMaxIterations; ++iter) {
    if (fp_work_.pending.empty()) break;
    ++iterations_used;
    fp_work_.next_pending.clear();

    // Reset seen bits for slots in the current work list.
    for (uint32_t s : fp_work_.pending) fp_work_.pending_seen[s] = 0;

    if (detailed) stats_.detailed.prop_pending_slots += fp_work_.pending.size();

    // Phase 1: connection propagation (memcmp guards actual change).
    if (has_conns) {
      for (uint32_t slot_id : fp_work_.pending) {
        if (slot_id >= conn_trigger_map_.size()) continue;
        if (detailed) ++stats_.detailed.prop_conn_trigger_lookups;
        auto [start, count] = conn_trigger_map_[slot_id];
        if (count == 0) continue;
        if (detailed) ++stats_.detailed.prop_conn_trigger_hits;
        for (uint32_t ci = start; ci < start + count; ++ci) {
          if (detailed) ++stats_.detailed.conn_considered;
          const auto& conn = all_connections_[ci];
          auto* src = &design_state[conn.src_byte_offset];
          auto* dst = &design_state[conn.dst_byte_offset];
          if (detailed) ++stats_.detailed.conn_memcmp_executed;
          if (std::memcmp(dst, src, conn.byte_size) != 0) {
            if (detailed) ++stats_.detailed.conn_memcpy_executed;
            std::memcpy(dst, src, conn.byte_size);
            MarkSlotDirty(conn.dst_slot_id);
            enqueue_pending(conn.dst_slot_id);
          }
        }
      }
    }

    // Phase 2: comb kernel evaluation.
    // Install capture so comb writes feed back into the work list.
    if (has_combs) {
      // Snapshot pending slots that have self-edge comb triggers (pre-comb
      // state). Only slots where at least one trigger entry has has_self_edge
      // need snapshot protection; others are safe without it.
      if (has_any_self_edge_comb_) {
        fp_work_.snapshot_buf.clear();
        fp_work_.snapshots.clear();
        fp_work_.snapshotted_slots.clear();
        for (uint32_t slot_id : fp_work_.pending) {
          if (slot_id >= comb_trigger_map_.size()) continue;
          auto [start, count] = comb_trigger_map_[slot_id];
          if (count == 0) continue;
          bool needs_snapshot = false;
          for (uint32_t ci = start; ci < start + count; ++ci) {
            if (comb_trigger_backing_[ci].has_self_edge) {
              needs_snapshot = true;
              break;
            }
          }
          if (!needs_snapshot) continue;
          const auto& meta = slot_meta_registry_.Get(slot_id);
          auto buf_off = static_cast<uint32_t>(fp_work_.snapshot_buf.size());
          fp_work_.snapshot_buf.resize(buf_off + meta.total_bytes);
          std::memcpy(
              &fp_work_.snapshot_buf[buf_off], &design_state[meta.base_off],
              meta.total_bytes);
          fp_work_.snapshot_index[slot_id] =
              static_cast<uint32_t>(fp_work_.snapshots.size());
          fp_work_.snapshots.push_back(
              {buf_off, meta.base_off, meta.total_bytes});
          fp_work_.snapshotted_slots.push_back(slot_id);
        }
      }

      // Install capture so comb writes feed into the work list.
      // Invariant: comb_write_capture_ must be nullptr on every exit from
      // this function. The pointer targets persistent workspace storage, so
      // a stale pointer would silently corrupt future calls.
      fp_work_.comb_writes.clear();
      comb_write_capture_ = &fp_work_.comb_writes;

      for (uint32_t slot_id : fp_work_.pending) {
        if (slot_id >= comb_trigger_map_.size()) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_lookups;
        auto [cstart, ccount] = comb_trigger_map_[slot_id];
        if (ccount == 0) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_hits;

        const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);

        for (uint32_t ci = cstart; ci < cstart + ccount; ++ci) {
          if (detailed) ++stats_.detailed.comb_considered;
          const auto& entry = comb_trigger_backing_[ci];

          if (entry.byte_size > 0 &&
              !dirty_ranges.Overlaps(entry.byte_offset, entry.byte_size)) {
            if (detailed) ++stats_.detailed.comb_skipped_range;
            continue;
          }

          if (detailed) ++stats_.detailed.comb_executed;
          const auto& ck = comb_kernels_[entry.kernel_idx];
          ck.body(ck.frame, 0);
        }
      }

      comb_write_capture_ = nullptr;

      // Enqueue comb writes, suppressing net-zero self-triggers.
      // For non-snapshotted slots (no self-edge risk), writes are enqueued
      // directly. This may cause at most one bounded extra fixpoint iteration
      // per multi-write output slot: the kernel re-evaluates, store-level
      // memcmp catches the no-op, convergence is reached.
      for (uint32_t s : fp_work_.comb_writes) {
        if (s >= slot_count || fp_work_.pending_seen[s] != 0) continue;

        if (has_any_self_edge_comb_ &&
            fp_work_.snapshot_index[s] != UINT32_MAX) {
          const auto& snap = fp_work_.snapshots[fp_work_.snapshot_index[s]];
          if (std::memcmp(
                  &design_state[snap.base_off],
                  &fp_work_.snapshot_buf[snap.buf_off],
                  snap.total_bytes) == 0) {
            continue;
          }
        }

        enqueue_pending(s);
      }

      if (has_any_self_edge_comb_) {
        for (uint32_t slot_id : fp_work_.snapshotted_slots) {
          fp_work_.snapshot_index[slot_id] = UINT32_MAX;
        }
      }
    }

    // Swap preserves both vectors' capacity. Clear after swap (not before)
    // so next_pending is empty before any enqueue in the next iteration.
    std::swap(fp_work_.pending, fp_work_.next_pending);
    fp_work_.next_pending.clear();
  }

  if (!fp_work_.pending.empty()) {
    throw common::InternalError(
        "Engine::FlushAndPropagateConnections",
        std::format(
            "convergence not reached after {} iterations "
            "({} slots still pending)",
            kMaxIterations, fp_work_.pending.size()));
  }

  ++stats_.core.propagation_calls;
  stats_.core.propagation_iterations += iterations_used;
  stats_.core.propagation_max_iterations = std::max(
      stats_.core.propagation_max_iterations,
      static_cast<uint64_t>(iterations_used));

  // Flush subscriptions with all accumulated dirty marks (process writes +
  // connection propagation + comb kernels), then clear the delta.
  FlushSignalUpdates();
  update_set_.ClearDelta();
}

}  // namespace lyra::runtime
