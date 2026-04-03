#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/process_trigger_abi.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"

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
                                .src_slot_id = d.src_slot_id,
                                .dst_slot_id = d.dst_slot_id,
                                .byte_size = d.byte_size}});
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
  if (all_connections_.empty()) return;
  for (const auto& conn : all_connections_) {
    const auto* src = ResolveSlotBytes(conn.src_slot_id);
    auto* dst = ResolveSlotBytesMut(conn.dst_slot_id);
    if (std::memcmp(dst, src, conn.byte_size) != 0) {
      std::memcpy(dst, src, conn.byte_size);
      MarkSlotDirty(conn.dst_slot_id);
    }
  }
}

void Engine::InitCombKernels(
    std::span<const uint32_t> words, uint32_t num_connection, void** states) {
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
    if (pos + num_triggers * 4 > words.size()) {
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

    if ((flags & CombKernel::kSelfEdge) != 0) {
      has_any_self_edge_comb_ = true;
    }

    // Resolve body pointer from the frame header. After H2, process
    // binding is constructor-owned and the body field is already set.
    auto* header =
        static_cast<const ProcessFrameHeader*>(proc_states[proc_idx]);
    auto body = header->body;

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
      uint32_t trigger_flags = words[pos++];
      // Relocate body-local trigger slot ids using the owning instance's
      // dense coordination base.
      if ((trigger_flags & kCombTriggerFlagBodyLocal) != 0) {
        if (header->instance == nullptr) {
          throw common::InternalError(
              "Engine::InitCombKernels",
              std::format(
                  "body-local comb trigger has no owning instance for "
                  "process {}",
                  proc_idx));
        }
        trigger_slot += header->instance->observability.flat_coord_base;
      }
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
  // Check whether there is any work to do across both domains.
  bool has_global_dirty = !update_set_.DeltaDirtySlots().empty();
  bool has_local_dirty = false;
  for (auto* inst : instances_) {
    if (inst->observability.local_signal_count > 0 &&
        !inst->observability.local_updates.DeltaDirtySignals().empty()) {
      has_local_dirty = true;
      break;
    }
  }

  if (detailed_stats_enabled_) {
    auto pending = update_set_.DeltaDirtySlots().size();
    ++stats_.detailed.prop_calls_total;
    stats_.detailed.prop_pending_slots_total += pending;
    if (pending > 0 || has_local_dirty) {
      ++stats_.detailed.prop_calls_with_work;
    } else {
      ++stats_.detailed.prop_calls_without_work;
    }
  }
  if (!has_global_dirty && !has_local_dirty) {
    return;
  }
  bool has_conns = !all_connections_.empty();
  bool has_combs = !comb_kernels_.empty();
  if (!has_conns && !has_combs) {
    FlushSignalUpdates();
    update_set_.ClearDelta();
    ClearLocalUpdatesDelta();
    return;
  }

  // Fixed-point propagation of connections and comb kernels.
  //
  // Two tracking channels serve different purposes:
  //   update_set_ (delta_dirty_/delta_seen_): dirty tracking for global slots.
  //   per-instance local_updates: dirty tracking for instance-owned slots.
  //   pending/next_pending (local work list): fixed-point propagation
  //     scheduling within this function only. Uses flat slot_ids for the
  //     trigger lookup (comb_trigger_map_ is flat-indexed). Per-iteration
  //     dedup via pending_seen[] ensures each slot appears at most once.
  //
  // Global comb writes (from MarkSlotDirty(uint32_t)) are captured via
  // comb_write_capture_ which bypasses delta_seen_ dedup. Local comb
  // writes go to local_updates and are collected after comb eval by
  // scanning instance delta dirty lists.
  constexpr uint32_t kMaxIterations = 100;
  const bool detailed = detailed_stats_enabled_;
  uint32_t iterations_used = 0;

  // Seed work list from both global and local delta dirty slots.
  fp_work_.pending.clear();
  for (uint32_t s : update_set_.DeltaDirtySlots()) {
    fp_work_.pending.push_back(s);
  }
  // Convert local delta dirty to flat for trigger lookup.
  for (auto* inst : instances_) {
    auto& obs = inst->observability;
    if (obs.local_signal_count == 0) continue;
    uint32_t base = obs.flat_coord_base;
    for (LocalSignalId lid : obs.local_updates.DeltaDirtySignals()) {
      fp_work_.pending.push_back(base + lid.value);
    }
  }
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
          const auto* src = ResolveSlotBytes(conn.src_slot_id);
          auto* dst = ResolveSlotBytesMut(conn.dst_slot_id);
          if (detailed) ++stats_.detailed.conn_memcmp_executed;
          if (std::memcmp(dst, src, conn.byte_size) != 0) {
            if (detailed) ++stats_.detailed.conn_memcpy_executed;
            std::memcpy(dst, src, conn.byte_size);
            MarkSlotDirty(conn.dst_slot_id);
            // Also mark local_updates for instance-owned destinations
            // so local subscription dispatch sees the connection write.
            if (conn.dst_slot_id >= global_slot_count_) {
              const auto& meta = slot_meta_registry_.Get(conn.dst_slot_id);
              if (meta.domain == SlotStorageDomain::kInstanceOwned &&
                  meta.owner_instance_id < instances_.size()) {
                auto* inst = instances_[meta.owner_instance_id];
                uint32_t local_id =
                    conn.dst_slot_id - inst->observability.flat_coord_base;
                inst->observability.local_updates.MarkSlotDirty(
                    LocalSignalId{local_id});
              }
            }
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
          const auto* slot_base =
              ResolveSlotBase(meta, design_state_base_, const_instances_);
          std::memcpy(
              &fp_work_.snapshot_buf[buf_off], slot_base, meta.total_bytes);
          fp_work_.snapshot_index[slot_id] =
              static_cast<uint32_t>(fp_work_.snapshots.size());
          fp_work_.snapshots.push_back({buf_off, slot_id, meta.total_bytes});
          fp_work_.snapshotted_slots.push_back(slot_id);
        }
      }

      // Install capture for global comb writes. Local comb writes go to
      // local_updates and are collected after comb eval by scanning deltas.
      // Invariant: comb_write_capture_ must be nullptr on every exit from
      // this function.
      fp_work_.comb_writes.clear();
      comb_write_capture_ = &fp_work_.comb_writes;

      // Record per-instance local delta sizes before comb eval so we can
      // identify newly-dirty local signals written by comb kernels.
      fp_work_.local_delta_pre.clear();
      for (auto* inst : instances_) {
        auto& obs = inst->observability;
        fp_work_.local_delta_pre.push_back(
            obs.local_signal_count > 0
                ? static_cast<uint32_t>(
                      obs.local_updates.DeltaDirtySignals().size())
                : 0);
      }

      for (uint32_t slot_id : fp_work_.pending) {
        if (slot_id >= comb_trigger_map_.size()) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_lookups;
        auto [cstart, ccount] = comb_trigger_map_[slot_id];
        if (ccount == 0) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_hits;

        // Dirty range check: for global slots use update_set_ ranges;
        // for instance-owned slots (beyond global_slot_count_) treat as
        // full-extent (conservative but correct -- range filtering is an
        // optimization, not a correctness requirement).
        bool is_global = slot_id < global_slot_count_;
        const common::RangeSet* dirty_ranges_ptr = nullptr;
        if (is_global) {
          dirty_ranges_ptr = &update_set_.DeltaRangesFor(slot_id);
        }

        for (uint32_t ci = cstart; ci < cstart + ccount; ++ci) {
          if (detailed) ++stats_.detailed.comb_considered;
          const auto& entry = comb_trigger_backing_[ci];

          if (entry.byte_size > 0 && dirty_ranges_ptr != nullptr &&
              !dirty_ranges_ptr->Overlaps(entry.byte_offset, entry.byte_size)) {
            if (detailed) ++stats_.detailed.comb_skipped_range;
            continue;
          }

          if (detailed) ++stats_.detailed.comb_executed;
          const auto& ck = comb_kernels_[entry.kernel_idx];
          ck.body(ck.frame, 0);
        }
      }

      comb_write_capture_ = nullptr;

      // Collect local comb writes: scan instances for newly-dirty local
      // signals that appeared after comb eval. Convert to flat for pending.
      for (size_t i = 0; i < instances_.size(); ++i) {
        auto& obs = instances_[i]->observability;
        if (obs.local_signal_count == 0) continue;
        auto delta = obs.local_updates.DeltaDirtySignals();
        auto pre_size = fp_work_.local_delta_pre[i];
        if (delta.size() <= pre_size) continue;
        uint32_t base = obs.flat_coord_base;
        for (size_t j = pre_size; j < delta.size(); ++j) {
          fp_work_.comb_writes.push_back(base + delta[j].value);
        }
      }

      // Enqueue comb writes (global + local), suppressing net-zero
      // self-triggers.
      for (uint32_t s : fp_work_.comb_writes) {
        if (s >= slot_count || fp_work_.pending_seen[s] != 0) continue;

        if (has_any_self_edge_comb_ &&
            fp_work_.snapshot_index[s] != UINT32_MAX) {
          const auto& snap = fp_work_.snapshots[fp_work_.snapshot_index[s]];
          const auto& snap_meta = slot_meta_registry_.Get(snap.slot_id);
          const auto* snap_base =
              ResolveSlotBase(snap_meta, design_state_base_, const_instances_);
          if (std::memcmp(
                  snap_base, &fp_work_.snapshot_buf[snap.buf_off],
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
  // FlushSignalUpdates internally dispatches both global and local subs.
  FlushSignalUpdates();
  update_set_.ClearDelta();
  ClearLocalUpdatesDelta();
}

}  // namespace lyra::runtime
