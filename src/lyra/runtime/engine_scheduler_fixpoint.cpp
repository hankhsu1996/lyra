#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/process_trigger_abi.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"

namespace lyra::runtime {

void Engine::InitConnectionBatch(std::span<const ConnectionDescriptor> descs) {
  if (descs.empty()) return;

  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::InitConnectionBatch",
        "InitConnectionBatch before InitSlotMeta");
  }

  // Build typed connections with domain-aware destinations and triggers.
  // Each connection's trigger and destination domain is pre-classified
  // at codegen time via typed descriptor fields.
  struct IndexedConn {
    bool trigger_is_local;
    InstanceId trigger_instance_id;  // valid when trigger_is_local
    uint32_t trigger_local_id;       // valid when trigger_is_local
    uint32_t trigger_global_id;      // valid when !trigger_is_local
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

    // Decode typed destination from descriptor fields.
    ConnectionTarget dst;
    if (d.dst_is_local != 0) {
      auto iid = InstanceId{d.dst_instance_id};
      auto lid = LocalSignalId{d.dst_local_id};
      auto* inst = FindInstanceMut(iid);
      if (inst == nullptr) {
        throw common::InternalError(
            "Engine::InitConnectionBatch",
            std::format("dst instance_id {} not found", iid));
      }
      if (lid.value >= inst->observability.local_signal_count) {
        throw common::InternalError(
            "Engine::InitConnectionBatch",
            std::format(
                "dst local_id {} >= local_signal_count {} for instance {}",
                lid.value, inst->observability.local_signal_count, iid));
      }
      dst = LocalConnectionTarget{.instance_id = iid, .signal = lid};
    } else {
      dst = GlobalConnectionTarget{GlobalSignalId{d.dst_slot_id}};
    }

    // Decode typed trigger from descriptor fields.
    auto trigger_is_local = (d.trigger_is_local != 0);
    auto trigger_instance_id = InstanceId{d.trigger_instance_id};
    uint32_t trigger_local_id = d.trigger_local_id;
    uint32_t trigger_global_id = d.trigger_slot_id;
    if (trigger_is_local) {
      auto* tinst = FindInstanceMut(trigger_instance_id);
      if (tinst == nullptr) {
        throw common::InternalError(
            "Engine::InitConnectionBatch",
            std::format(
                "trigger instance_id {} not found", trigger_instance_id));
      }
      if (trigger_local_id >= tinst->observability.local_signal_count) {
        throw common::InternalError(
            "Engine::InitConnectionBatch",
            std::format(
                "trigger local_id {} >= local_signal_count {} for "
                "instance {}",
                trigger_local_id, tinst->observability.local_signal_count,
                trigger_instance_id));
      }
    }

    sorted.push_back(
        IndexedConn{
            .trigger_is_local = trigger_is_local,
            .trigger_instance_id = trigger_instance_id,
            .trigger_local_id = trigger_local_id,
            .trigger_global_id = trigger_global_id,
            .conn =
                BatchedConnection{
                    .src_slot_id = d.src_slot_id,
                    .byte_size = d.byte_size,
                    .dst = dst,
                },
        });
  }

  // Separate global and local trigger entries, sort each group, and build
  // domain-split trigger maps.
  std::vector<IndexedConn> global_entries;
  std::vector<IndexedConn> local_entries;
  for (auto& ic : sorted) {
    if (ic.trigger_is_local) {
      local_entries.push_back(std::move(ic));
    } else {
      global_entries.push_back(std::move(ic));
    }
  }

  // Sort global entries by trigger_global_id for contiguous grouping.
  std::ranges::sort(global_entries, {}, &IndexedConn::trigger_global_id);

  // Build global connection trigger map.
  global_conn_trigger_map_.resize(global_slot_count_);
  auto conn_base = static_cast<uint32_t>(all_connections_.size());
  for (const auto& ge : global_entries) {
    all_connections_.push_back(ge.conn);
  }
  {
    uint32_t i = 0;
    while (i < global_entries.size()) {
      uint32_t trigger = global_entries[i].trigger_global_id;
      uint32_t start = conn_base + i;
      while (i < global_entries.size() &&
             global_entries[i].trigger_global_id == trigger) {
        ++i;
      }
      if (trigger < global_conn_trigger_map_.size()) {
        global_conn_trigger_map_[trigger] = {
            .start = start, .count = conn_base + i - start};
      }
    }
  }

  // Build per-instance local connection trigger maps.
  // Group local_entries by instance_id, then by trigger_local_id.
  std::ranges::sort(local_entries, [](const auto& a, const auto& b) {
    if (a.trigger_instance_id != b.trigger_instance_id)
      return a.trigger_instance_id < b.trigger_instance_id;
    return a.trigger_local_id < b.trigger_local_id;
  });

  conn_base = static_cast<uint32_t>(all_connections_.size());
  for (const auto& le : local_entries) {
    all_connections_.push_back(le.conn);
  }
  {
    uint32_t i = 0;
    while (i < local_entries.size()) {
      auto iid = local_entries[i].trigger_instance_id;
      auto* inst = instance_trace_resolver_.FindInstanceMut(iid);
      if (inst == nullptr) {
        throw common::InternalError(
            "Engine::InitConnectionBatch",
            std::format("trigger instance_id {} not found", iid));
      }
      auto& obs = inst->observability;
      if (obs.local_conn_trigger_map.empty() && obs.local_signal_count > 0) {
        obs.local_conn_trigger_map.resize(obs.local_signal_count);
      }
      while (i < local_entries.size() &&
             local_entries[i].trigger_instance_id == iid) {
        uint32_t local_id = local_entries[i].trigger_local_id;
        uint32_t start = conn_base + i;
        while (i < local_entries.size() &&
               local_entries[i].trigger_instance_id == iid &&
               local_entries[i].trigger_local_id == local_id) {
          ++i;
        }
        if (local_id >= obs.local_conn_trigger_map.size()) {
          throw common::InternalError(
              "Engine::InitConnectionBatch",
              std::format(
                  "trigger local_id {} >= local_conn_trigger_map size {} "
                  "for instance {}",
                  local_id, obs.local_conn_trigger_map.size(), iid));
        }
        obs.local_conn_trigger_map[local_id] = {
            .start = start, .count = conn_base + i - start};
        if (local_id < obs.local_has_observers.size()) {
          obs.local_has_observers[local_id] = 1;
        }
      }
    }
  }
}

void Engine::EvaluateAllConnections() {
  if (all_connections_.empty()) return;
  for (const auto& conn : all_connections_) {
    const auto* src = ResolveSlotBytes(conn.src_slot_id);
    auto* dst = ResolveConnectionDstMut(conn.dst);
    if (std::memcmp(dst, src, conn.byte_size) != 0) {
      std::memcpy(dst, src, conn.byte_size);
      std::visit(
          [this](const auto& t) {
            using T = std::decay_t<decltype(t)>;
            if constexpr (std::is_same_v<T, GlobalConnectionTarget>) {
              MarkSlotDirty(t.signal.value);
            } else {
              auto* inst =
                  instance_trace_resolver_.FindInstanceMut(t.instance_id);
              MarkLocalSignalDirty(*inst, t.signal);
            }
          },
          conn.dst);
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
    bool is_local;
    InstanceId instance_id;  // valid when is_local
    uint32_t local_id;       // valid when is_local
    uint32_t global_id;      // valid when !is_local
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

    const auto* header =
        static_cast<const ProcessFrameHeader*>(proc_states[proc_idx]);

    auto comb_idx = static_cast<uint32_t>(comb_kernels_.size());
    comb_kernels_.push_back(
        BuildCombKernel(proc_idx, proc_states[proc_idx], flags));

    comb_kernel_flags_[proc_idx] = 1;

    bool kernel_self_edge = (flags & CombKernel::kSelfEdge) != 0;
    for (uint32_t ti = 0; ti < num_triggers; ++ti) {
      uint32_t trigger_slot = words[pos++];
      uint32_t byte_offset = words[pos++];
      uint32_t byte_size = words[pos++];
      uint32_t trigger_flags = words[pos++];

      // Classify trigger domain.
      bool is_local = (trigger_flags & kCombTriggerFlagBodyLocal) != 0;
      auto trigger_iid = InstanceId{0};
      uint32_t local_id = 0;
      uint32_t global_id = trigger_slot;
      if (is_local) {
        if (header->instance == nullptr) {
          throw common::InternalError(
              "Engine::InitCombKernels",
              std::format(
                  "body-local comb trigger has no owning instance for "
                  "process {}",
                  proc_idx));
        }
        trigger_iid = header->instance->instance_id;
        local_id = trigger_slot;
      }

      entries.push_back(
          ParsedTrigger{
              .is_local = is_local,
              .instance_id = trigger_iid,
              .local_id = local_id,
              .global_id = global_id,
              .kernel_idx = comb_idx,
              .byte_offset = byte_offset,
              .byte_size = byte_size,
              .has_self_edge = kernel_self_edge,
          });
      if (byte_size > 0) {
        ++comb_narrow_count_;
      } else {
        ++comb_full_slot_count_;
      }
    }
  }

  if (entries.empty()) return;

  // Separate global and local triggers.
  std::vector<ParsedTrigger> global_triggers;
  std::vector<ParsedTrigger> local_triggers;
  for (auto& e : entries) {
    if (e.is_local) {
      local_triggers.push_back(std::move(e));
    } else {
      global_triggers.push_back(std::move(e));
    }
  }

  // Build global comb trigger map.
  fp_work_.global_pending_seen.resize(global_slot_count_, 0);
  if (has_any_self_edge_comb_) {
    fp_work_.global_snapshot_index.assign(global_slot_count_, UINT32_MAX);
  }

  if (!global_triggers.empty()) {
    std::ranges::sort(global_triggers, {}, &ParsedTrigger::global_id);

    global_comb_trigger_map_.resize(global_slot_count_);
    uint32_t i = 0;
    while (i < global_triggers.size()) {
      uint32_t slot = global_triggers[i].global_id;
      auto start = static_cast<uint32_t>(comb_trigger_backing_.size());
      while (i < global_triggers.size() &&
             global_triggers[i].global_id == slot) {
        comb_trigger_backing_.push_back({
            .kernel_idx = global_triggers[i].kernel_idx,
            .byte_offset = global_triggers[i].byte_offset,
            .byte_size = global_triggers[i].byte_size,
            .has_self_edge = global_triggers[i].has_self_edge,
        });
        ++i;
      }
      uint32_t count =
          static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
      global_comb_trigger_map_[slot] = {.start = start, .count = count};
      global_comb_trigger_slots_.push_back(GlobalSignalId{slot});
    }
  }

  // Build per-instance local comb trigger maps.
  if (!local_triggers.empty()) {
    std::ranges::sort(local_triggers, [](const auto& a, const auto& b) {
      if (a.instance_id != b.instance_id) return a.instance_id < b.instance_id;
      return a.local_id < b.local_id;
    });

    uint32_t i = 0;
    while (i < local_triggers.size()) {
      auto iid = local_triggers[i].instance_id;
      auto* inst = instance_trace_resolver_.FindInstanceMut(iid);
      if (inst == nullptr) {
        throw common::InternalError(
            "Engine::InitCombKernels",
            std::format("comb trigger instance_id {} not found", iid));
      }
      auto& obs = inst->observability;
      if (obs.local_comb_trigger_map.empty() && obs.local_signal_count > 0) {
        obs.local_comb_trigger_map.resize(obs.local_signal_count);
      }
      while (i < local_triggers.size() &&
             local_triggers[i].instance_id == iid) {
        uint32_t local_id = local_triggers[i].local_id;
        auto start = static_cast<uint32_t>(comb_trigger_backing_.size());
        while (i < local_triggers.size() &&
               local_triggers[i].instance_id == iid &&
               local_triggers[i].local_id == local_id) {
          comb_trigger_backing_.push_back({
              .kernel_idx = local_triggers[i].kernel_idx,
              .byte_offset = local_triggers[i].byte_offset,
              .byte_size = local_triggers[i].byte_size,
              .has_self_edge = local_triggers[i].has_self_edge,
          });
          ++i;
        }
        uint32_t count =
            static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
        if (local_id >= obs.local_comb_trigger_map.size()) {
          throw common::InternalError(
              "Engine::InitCombKernels",
              std::format(
                  "local comb trigger id {} >= local_comb_trigger_map "
                  "size {} for instance {}",
                  local_id, obs.local_comb_trigger_map.size(), iid));
        }
        obs.local_comb_trigger_map[local_id] = {.start = start, .count = count};
        obs.local_comb_trigger_slots.push_back(LocalSignalId{local_id});
        if (local_id < obs.local_has_observers.size()) {
          obs.local_has_observers[local_id] = 1;
        }
      }
    }
  }
}

void Engine::SeedCombKernelDirtyMarks() {
  // Seed global comb trigger slots.
  for (GlobalSignalId gid : global_comb_trigger_slots_) {
    MarkSlotDirty(gid.value);
  }
  // Seed local comb trigger slots.
  for (uint32_t i = 0; i < instances_.size(); ++i) {
    auto* inst = instances_[i];
    for (LocalSignalId lid : inst->observability.local_comb_trigger_slots) {
      MarkLocalSignalDirty(*inst, lid, i);
    }
  }
}

auto Engine::BuildCombKernel(uint32_t proc_idx, void* frame, uint32_t flags)
    -> CombKernel {
  if (frame == nullptr) {
    throw common::InternalError(
        "Engine::BuildCombKernel",
        std::format("null proc state for comb proc_idx {}", proc_idx));
  }
  const auto* header = static_cast<const ProcessFrameHeader*>(frame);
  if (header->body == nullptr) {
    throw common::InternalError(
        "Engine::BuildCombKernel",
        std::format(
            "comb kernel proc_idx {} has null body function pointer "
            "(shared_body_fn not set by constructor)",
            proc_idx));
  }
  if (header->instance == nullptr) {
    throw common::InternalError(
        "Engine::BuildCombKernel",
        std::format(
            "comb kernel proc_idx {} has null instance pointer", proc_idx));
  }
  return CombKernel{
      .body = header->body,
      .frame = frame,
      .process_index = proc_idx,
      .flags = flags,
      .instance_idx = GetInstanceIndex(*header->instance),
  };
}

void Engine::PromoteLocalFrontier() {
  // Retire old current frontier: clear consumed pending.
  for (uint32_t idx : fp_work_.current_instances) {
    fp_work_.locals[idx].pending.clear();
  }

  // Promote next frontier: swap next->pending, clear dedup, clear membership.
  for (uint32_t idx : fp_work_.next_instances) {
    auto& lps = fp_work_.locals[idx];
    std::swap(lps.pending, lps.next);
    lps.next.clear();
    for (LocalSignalId lid : lps.pending) {
      lps.seen[lid.value] = 0;
    }
    fp_work_.in_next[idx] = 0;
  }

  std::swap(fp_work_.current_instances, fp_work_.next_instances);
  fp_work_.next_instances.clear();
}

void Engine::PromoteGlobalFrontier() {
  for (GlobalSignalId gid : fp_work_.next_globals) {
    fp_work_.global_pending_seen[gid.value] = 0;
  }
  std::swap(fp_work_.pending_globals, fp_work_.next_globals);
  fp_work_.next_globals.clear();
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

  constexpr uint32_t kMaxIterations = 100;
  const bool detailed = detailed_stats_enabled_;
  uint32_t iterations_used = 0;
  auto num_instances = static_cast<uint32_t>(instances_.size());

  // Ensure workspace is sized (lazy init, once per engine lifetime).
  if (fp_work_.global_pending_seen.size() < global_slot_count_) {
    fp_work_.global_pending_seen.resize(global_slot_count_, 0);
  }
  if (has_any_self_edge_comb_ &&
      fp_work_.global_snapshot_index.size() < global_slot_count_) {
    fp_work_.global_snapshot_index.assign(global_slot_count_, UINT32_MAX);
  }
  if (fp_work_.locals.size() != instances_.size()) {
    fp_work_.locals.resize(instances_.size());
    for (size_t i = 0; i < instances_.size(); ++i) {
      auto& lps = fp_work_.locals[i];
      lps.instance = instances_[i];
      auto lsc = instances_[i]->observability.local_signal_count;
      if (lsc > 0 && lps.seen.size() < lsc) {
        lps.seen.resize(lsc, 0);
      }
    }
  }
  if (fp_work_.in_next.size() < num_instances) {
    fp_work_.in_next.assign(num_instances, 0);
  }
  if (fp_work_.comb_touched_seen.size() < num_instances) {
    fp_work_.comb_touched_seen.assign(num_instances, 0);
  }
  if (fp_work_.delta_pre.size() < num_instances) {
    fp_work_.delta_pre.resize(num_instances, 0);
  }

  // Seed global frontier from delta dirty slots.
  fp_work_.pending_globals.clear();
  for (uint32_t s : update_set_.DeltaDirtySlots()) {
    if (s < global_slot_count_) {
      fp_work_.pending_globals.push_back(GlobalSignalId{s});
    } else {
      throw common::InternalError(
          "Engine::FlushAndPropagateConnections",
          std::format(
              "instance-owned slot {} in update_set_ delta "
              "(global_slot_count={})",
              s, global_slot_count_));
    }
  }

  // Seed local frontier from delta_dirty_instances_ (the canonical
  // dirty-instance index). Only visits instances that actually have
  // delta-dirty local signals -- no full-instance sweep.
  fp_work_.current_instances.clear();
  for (uint32_t i : delta_dirty_instances_) {
    auto& obs = instances_[i]->observability;
    if (obs.local_signal_count == 0) continue;
    auto& lps = fp_work_.locals[i];
    lps.pending.clear();
    for (LocalSignalId lid : obs.local_updates.DeltaDirtySignals()) {
      lps.pending.push_back(lid);
    }
    if (!lps.pending.empty()) {
      fp_work_.current_instances.push_back(i);
    }
  }

  // Helper: enqueue into global next with dedup.
  auto enqueue_global = [&](GlobalSignalId gid) {
    if (detailed) ++stats_.detailed.prop_enqueue_attempts;
    if (gid.value < global_slot_count_ &&
        fp_work_.global_pending_seen[gid.value] == 0) {
      fp_work_.global_pending_seen[gid.value] = 1;
      fp_work_.next_globals.push_back(gid);
    } else {
      if (detailed) ++stats_.detailed.prop_enqueue_deduped;
    }
  };

  // Helper: enqueue into local next with dedup.
  // When a signal is enqueued, ensures instance is in next_instances.
  auto enqueue_local = [&](uint32_t inst_idx, LocalSignalId lid) {
    if (detailed) ++stats_.detailed.prop_enqueue_attempts;
    auto& lps = fp_work_.locals[inst_idx];
    if (lid.value < lps.seen.size() && lps.seen[lid.value] == 0) {
      lps.seen[lid.value] = 1;
      lps.next.push_back(lid);
      if (fp_work_.in_next[inst_idx] == 0) {
        fp_work_.in_next[inst_idx] = 1;
        fp_work_.next_instances.push_back(inst_idx);
      }
    } else {
      if (detailed) ++stats_.detailed.prop_enqueue_deduped;
    }
  };

  // Helper: connection destination enqueue (handles global/local dispatch).
  auto enqueue_conn_dst = [&](const ConnectionTarget& dst) {
    std::visit(
        [&](const auto& t) {
          using T = std::decay_t<decltype(t)>;
          if constexpr (std::is_same_v<T, GlobalConnectionTarget>) {
            MarkSlotDirty(t.signal.value);
            enqueue_global(t.signal);
          } else {
            auto inst_idx = GetInstanceIndex(t.instance_id);
            auto* local_inst = instances_[inst_idx];
            MarkLocalSignalDirty(*local_inst, t.signal, inst_idx);
            enqueue_local(inst_idx, t.signal);
          }
        },
        dst);
  };

  // Helper: mark a comb kernel's owning instance as touched this iteration.
  // Records delta_pre exactly once per instance, before first comb execution.
  auto mark_comb_touched = [&](uint32_t inst_idx) {
    if (fp_work_.comb_touched_seen[inst_idx] != 0) return;
    fp_work_.comb_touched_seen[inst_idx] = 1;
    fp_work_.comb_touched.push_back(inst_idx);
    auto& obs = instances_[inst_idx]->observability;
    fp_work_.delta_pre[inst_idx] =
        (obs.local_signal_count > 0)
            ? static_cast<uint32_t>(
                  obs.local_updates.DeltaDirtySignals().size())
            : 0;
  };

  for (uint32_t iter = 0; iter < kMaxIterations; ++iter) {
    if (fp_work_.pending_globals.empty() &&
        fp_work_.current_instances.empty()) {
      break;
    }
    ++iterations_used;
    fp_work_.next_globals.clear();

    if (detailed) {
      auto total_pending =
          static_cast<uint32_t>(fp_work_.pending_globals.size());
      for (uint32_t idx : fp_work_.current_instances) {
        total_pending +=
            static_cast<uint32_t>(fp_work_.locals[idx].pending.size());
      }
      stats_.detailed.prop_pending_slots += total_pending;
    }

    // Phase 1: connection propagation.
    if (has_conns) {
      // 1a. Global triggers.
      for (GlobalSignalId gid : fp_work_.pending_globals) {
        if (gid.value >= global_conn_trigger_map_.size()) continue;
        if (detailed) ++stats_.detailed.prop_conn_trigger_lookups;
        auto [start, count] = global_conn_trigger_map_[gid.value];
        if (count == 0) continue;
        if (detailed) ++stats_.detailed.prop_conn_trigger_hits;
        for (uint32_t ci = start; ci < start + count; ++ci) {
          if (detailed) ++stats_.detailed.conn_considered;
          const auto& conn = all_connections_[ci];
          const auto* src = ResolveSlotBytes(conn.src_slot_id);
          auto* dst = ResolveConnectionDstMut(conn.dst);
          if (detailed) ++stats_.detailed.conn_memcmp_executed;
          if (std::memcmp(dst, src, conn.byte_size) != 0) {
            if (detailed) ++stats_.detailed.conn_memcpy_executed;
            std::memcpy(dst, src, conn.byte_size);
            enqueue_conn_dst(conn.dst);
          }
        }
      }

      // 1b. Local triggers (current frontier only).
      for (uint32_t idx : fp_work_.current_instances) {
        auto& lps = fp_work_.locals[idx];
        auto& obs = lps.instance->observability;
        for (LocalSignalId lid : lps.pending) {
          if (lid.value >= obs.local_conn_trigger_map.size()) continue;
          if (detailed) ++stats_.detailed.prop_conn_trigger_lookups;
          auto [start, count] = obs.local_conn_trigger_map[lid.value];
          if (count == 0) continue;
          if (detailed) ++stats_.detailed.prop_conn_trigger_hits;
          for (uint32_t ci = start; ci < start + count; ++ci) {
            if (detailed) ++stats_.detailed.conn_considered;
            const auto& conn = all_connections_[ci];
            const auto* src = ResolveSlotBytes(conn.src_slot_id);
            auto* dst = ResolveConnectionDstMut(conn.dst);
            if (detailed) ++stats_.detailed.conn_memcmp_executed;
            if (std::memcmp(dst, src, conn.byte_size) != 0) {
              if (detailed) ++stats_.detailed.conn_memcpy_executed;
              std::memcpy(dst, src, conn.byte_size);
              enqueue_conn_dst(conn.dst);
            }
          }
        }
      }
    }

    // Phase 2: comb kernel evaluation.
    if (has_combs) {
      // Snapshot global pending slots with self-edge comb triggers.
      if (has_any_self_edge_comb_) {
        fp_work_.snapshot_buf.clear();
        fp_work_.snapshots.clear();
        fp_work_.snapshotted_slots.clear();
        for (GlobalSignalId gid : fp_work_.pending_globals) {
          if (gid.value >= global_comb_trigger_map_.size()) continue;
          auto [start, count] = global_comb_trigger_map_[gid.value];
          if (count == 0) continue;
          bool needs_snapshot = false;
          for (uint32_t ci = start; ci < start + count; ++ci) {
            if (comb_trigger_backing_[ci].has_self_edge) {
              needs_snapshot = true;
              break;
            }
          }
          if (!needs_snapshot) continue;
          const auto& meta = slot_meta_registry_.Get(gid.value);
          auto buf_off = static_cast<uint32_t>(fp_work_.snapshot_buf.size());
          fp_work_.snapshot_buf.resize(buf_off + meta.total_bytes);
          const auto* slot_base =
              ResolveSlotBase(meta, design_state_base_, const_instances_);
          std::memcpy(
              &fp_work_.snapshot_buf[buf_off], slot_base, meta.total_bytes);
          fp_work_.global_snapshot_index[gid.value] =
              static_cast<uint32_t>(fp_work_.snapshots.size());
          fp_work_.snapshots.push_back({buf_off, gid.value, meta.total_bytes});
          fp_work_.snapshotted_slots.push_back(gid);
        }
      }

      // Install capture for global comb writes.
      fp_work_.comb_writes_global.clear();
      fp_work_.comb_writes_local.clear();
      std::vector<uint32_t> flat_comb_writes;
      comb_write_capture_ = &flat_comb_writes;

      // 2a. Evaluate comb kernels from global pending.
      for (GlobalSignalId gid : fp_work_.pending_globals) {
        if (gid.value >= global_comb_trigger_map_.size()) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_lookups;
        auto [cstart, ccount] = global_comb_trigger_map_[gid.value];
        if (ccount == 0) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_hits;

        const auto& dirty_ranges = update_set_.DeltaRangesFor(gid.value);

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
          mark_comb_touched(ck.instance_idx);
          FlushDeferredAssertionsForProcess(
              ProcessId::FromIndex(ck.process_index));
          ck.body(ck.frame, 0);
        }
      }

      // 2b. Evaluate comb kernels from local pending (current frontier only).
      for (uint32_t idx : fp_work_.current_instances) {
        auto& lps = fp_work_.locals[idx];
        auto& obs = lps.instance->observability;
        for (LocalSignalId lid : lps.pending) {
          if (lid.value >= obs.local_comb_trigger_map.size()) continue;
          if (detailed) ++stats_.detailed.prop_comb_trigger_lookups;
          auto [cstart, ccount] = obs.local_comb_trigger_map[lid.value];
          if (ccount == 0) continue;
          if (detailed) ++stats_.detailed.prop_comb_trigger_hits;

          for (uint32_t ci = cstart; ci < cstart + ccount; ++ci) {
            if (detailed) ++stats_.detailed.comb_considered;
            if (detailed) ++stats_.detailed.comb_executed;
            const auto& ck =
                comb_kernels_[comb_trigger_backing_[ci].kernel_idx];
            mark_comb_touched(ck.instance_idx);
            FlushDeferredAssertionsForProcess(
                ProcessId::FromIndex(ck.process_index));
            ck.body(ck.frame, 0);
          }
        }
      }

      comb_write_capture_ = nullptr;

      // 2c. Classify flat comb writes into global domain.
      for (uint32_t s : flat_comb_writes) {
        if (s < global_slot_count_) {
          fp_work_.comb_writes_global.push_back(GlobalSignalId{s});
        }
      }

      // 2d. Collect local comb writes (scan comb_touched only).
      for (uint32_t tidx : fp_work_.comb_touched) {
        auto& obs = instances_[tidx]->observability;
        if (obs.local_signal_count == 0) continue;
        auto delta = obs.local_updates.DeltaDirtySignals();
        auto pre_size = fp_work_.delta_pre[tidx];
        if (delta.size() <= pre_size) continue;
        for (size_t j = pre_size; j < delta.size(); ++j) {
          fp_work_.comb_writes_local.push_back(
              FixpointWorkspace::LocalCombWrite{
                  .instance_idx = tidx,
                  .signal = delta[j],
              });
        }
      }

      // Enqueue global comb writes, suppressing net-zero self-triggers.
      for (GlobalSignalId gid : fp_work_.comb_writes_global) {
        if (gid.value >= global_slot_count_ ||
            fp_work_.global_pending_seen[gid.value] != 0)
          continue;

        if (has_any_self_edge_comb_ &&
            fp_work_.global_snapshot_index[gid.value] != UINT32_MAX) {
          const auto& snap =
              fp_work_.snapshots[fp_work_.global_snapshot_index[gid.value]];
          const auto& snap_meta = slot_meta_registry_.Get(snap.slot_id);
          const auto* snap_base =
              ResolveSlotBase(snap_meta, design_state_base_, const_instances_);
          if (std::memcmp(
                  snap_base, &fp_work_.snapshot_buf[snap.buf_off],
                  snap.total_bytes) == 0) {
            continue;
          }
        }

        enqueue_global(gid);
      }

      // Enqueue local comb writes.
      for (const auto& lw : fp_work_.comb_writes_local) {
        enqueue_local(lw.instance_idx, lw.signal);
      }

      if (has_any_self_edge_comb_) {
        for (GlobalSignalId gid : fp_work_.snapshotted_slots) {
          fp_work_.global_snapshot_index[gid.value] = UINT32_MAX;
        }
      }

      // 2e. Reset comb_touched for next iteration.
      for (uint32_t tidx : fp_work_.comb_touched) {
        fp_work_.comb_touched_seen[tidx] = 0;
      }
      fp_work_.comb_touched.clear();
    }

    // Phase 3: Promote frontiers.
    PromoteLocalFrontier();
    PromoteGlobalFrontier();
  }

  if (!fp_work_.pending_globals.empty() ||
      !fp_work_.current_instances.empty()) {
    throw common::InternalError(
        "Engine::FlushAndPropagateConnections",
        std::format(
            "convergence not reached after {} iterations", kMaxIterations));
  }

  ++stats_.core.propagation_calls;
  stats_.core.propagation_iterations += iterations_used;
  stats_.core.propagation_max_iterations = std::max(
      stats_.core.propagation_max_iterations,
      static_cast<uint64_t>(iterations_used));

  FlushSignalUpdates();
  update_set_.ClearDelta();
  ClearLocalUpdatesDelta();
}

}  // namespace lyra::runtime
