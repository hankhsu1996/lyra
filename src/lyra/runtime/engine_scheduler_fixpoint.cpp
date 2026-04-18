#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <functional>
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

void Engine::InitConnectionBatch(
    std::span<const RuntimeConnectionDescriptor> descs) {
  if (descs.empty()) return;

  // Build batched connections from already-materialized runtime
  // descriptors. All endpoints carry direct RuntimeInstance* -- no
  // numeric identity lookup needed.
  struct IndexedConn {
    bool trigger_is_local;
    RuntimeInstance* trigger_instance;
    uint32_t trigger_local_id;
    uint32_t trigger_global_id;
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

    const auto* src_ptr = ResolveInstanceStorageOffset(
        *d.src_instance, d.src_byte_offset, d.byte_size,
        "Engine::InitConnectionBatch[src]");

    auto* dst_ptr = ResolveInstanceStorageOffset(
        *d.dst_instance, d.dst_byte_offset, d.byte_size,
        "Engine::InitConnectionBatch[dst]");

    BatchedConnectionDst dst = LocalConnectionDst{
        .instance = d.dst_instance, .signal = d.dst_local_signal};

    sorted.push_back(
        IndexedConn{
            .trigger_is_local = true,
            .trigger_instance = d.trigger_instance,
            .trigger_local_id = d.trigger_local_id.value,
            .trigger_global_id = 0,
            .conn =
                BatchedConnection{
                    .src_ptr = src_ptr,
                    .dst_ptr = dst_ptr,
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
  // Group local_entries by trigger_instance, then by trigger_local_id.
  std::ranges::sort(local_entries, [](const auto& a, const auto& b) {
    if (a.trigger_instance != b.trigger_instance)
      return std::less<RuntimeInstance*>{}(
          a.trigger_instance, b.trigger_instance);
    return a.trigger_local_id < b.trigger_local_id;
  });

  conn_base = static_cast<uint32_t>(all_connections_.size());
  for (const auto& le : local_entries) {
    all_connections_.push_back(le.conn);
  }
  {
    uint32_t i = 0;
    while (i < local_entries.size()) {
      auto* inst = local_entries[i].trigger_instance;
      if (inst == nullptr) {
        throw common::InternalError(
            "Engine::InitConnectionBatch",
            "null trigger instance in local connection entry");
      }
      auto& obs = inst->observability;
      if (obs.local_conn_trigger_map.empty() && obs.local_signal_count > 0) {
        obs.local_conn_trigger_map.resize(obs.local_signal_count);
      }
      while (i < local_entries.size() &&
             local_entries[i].trigger_instance == inst) {
        uint32_t local_id = local_entries[i].trigger_local_id;
        uint32_t start = conn_base + i;
        while (i < local_entries.size() &&
               local_entries[i].trigger_instance == inst &&
               local_entries[i].trigger_local_id == local_id) {
          ++i;
        }
        if (local_id >= obs.local_conn_trigger_map.size()) {
          throw common::InternalError(
              "Engine::InitConnectionBatch",
              std::format(
                  "trigger local_id {} >= local_conn_trigger_map size {} "
                  "for instance '{}'",
                  local_id, obs.local_conn_trigger_map.size(),
                  inst->scope.path_c_str));
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
    const auto* src = conn.src_ptr;
    auto* dst = conn.dst_ptr;
    if (std::memcmp(dst, src, conn.byte_size) != 0) {
      std::memcpy(dst, src, conn.byte_size);
      std::visit(
          [this](const auto& t) {
            using T = std::decay_t<decltype(t)>;
            if constexpr (std::is_same_v<T, GlobalConnectionDst>) {
              MarkSlotDirty(t.signal.value);
            } else {
              MarkLocalSignalDirty(*t.instance, t.signal);
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

  struct ParsedTrigger {
    bool is_local;
    RuntimeInstance* instance;  // valid when is_local
    uint32_t local_id;          // valid when is_local
    uint32_t global_id;         // valid when !is_local
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

    processes_[proc_idx].is_comb_kernel = true;

    bool kernel_self_edge = (flags & CombKernel::kSelfEdge) != 0;
    for (uint32_t ti = 0; ti < num_triggers; ++ti) {
      uint32_t trigger_slot = words[pos++];
      uint32_t byte_offset = words[pos++];
      uint32_t byte_size = words[pos++];
      uint32_t trigger_flags = words[pos++];

      // Classify trigger domain.
      bool is_local = (trigger_flags & kCombTriggerFlagBodyLocal) != 0;
      RuntimeInstance* trigger_inst = nullptr;
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
        trigger_inst = header->instance;
        local_id = trigger_slot;
      }

      entries.push_back(
          ParsedTrigger{
              .is_local = is_local,
              .instance = trigger_inst,
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

    global_reactive_trigger_map_.resize(global_slot_count_);
    uint32_t i = 0;
    while (i < global_triggers.size()) {
      uint32_t slot = global_triggers[i].global_id;
      auto start = static_cast<uint32_t>(reactive_trigger_backing_.size());
      while (i < global_triggers.size() &&
             global_triggers[i].global_id == slot) {
        reactive_trigger_backing_.push_back(
            ReactiveTriggerEntry{
                .kind = ReactiveTriggerEntry::Kind::kCombKernel,
                .byte_offset = global_triggers[i].byte_offset,
                .byte_size = global_triggers[i].byte_size,
                .has_self_edge = global_triggers[i].has_self_edge,
                .owner_idx = global_triggers[i].kernel_idx,
            });
        ++i;
      }
      uint32_t count =
          static_cast<uint32_t>(reactive_trigger_backing_.size()) - start;
      global_reactive_trigger_map_[slot] = {.start = start, .count = count};
      global_reactive_trigger_slots_.push_back(GlobalSignalId{slot});
    }
  }

  // Build per-instance local comb trigger maps.
  if (!local_triggers.empty()) {
    std::ranges::sort(local_triggers, [](const auto& a, const auto& b) {
      if (a.instance != b.instance)
        return std::less<RuntimeInstance*>{}(a.instance, b.instance);
      return a.local_id < b.local_id;
    });

    uint32_t i = 0;
    while (i < local_triggers.size()) {
      auto* inst = local_triggers[i].instance;
      if (inst == nullptr) {
        throw common::InternalError(
            "Engine::InitCombKernels",
            "null trigger instance in local comb trigger entry");
      }
      auto& obs = inst->observability;
      if (obs.local_reactive_trigger_map.empty() &&
          obs.local_signal_count > 0) {
        obs.local_reactive_trigger_map.resize(obs.local_signal_count);
      }
      while (i < local_triggers.size() && local_triggers[i].instance == inst) {
        uint32_t local_id = local_triggers[i].local_id;
        auto start = static_cast<uint32_t>(reactive_trigger_backing_.size());
        while (i < local_triggers.size() &&
               local_triggers[i].instance == inst &&
               local_triggers[i].local_id == local_id) {
          reactive_trigger_backing_.push_back(
              ReactiveTriggerEntry{
                  .kind = ReactiveTriggerEntry::Kind::kCombKernel,
                  .byte_offset = local_triggers[i].byte_offset,
                  .byte_size = local_triggers[i].byte_size,
                  .has_self_edge = local_triggers[i].has_self_edge,
                  .owner_idx = local_triggers[i].kernel_idx,
              });
          ++i;
        }
        uint32_t count =
            static_cast<uint32_t>(reactive_trigger_backing_.size()) - start;
        if (local_id >= obs.local_reactive_trigger_map.size()) {
          throw common::InternalError(
              "Engine::InitCombKernels",
              std::format(
                  "local comb trigger id {} >= local_reactive_trigger_map "
                  "size {} for instance '{}'",
                  local_id, obs.local_reactive_trigger_map.size(),
                  inst->scope.path_c_str));
        }
        obs.local_reactive_trigger_map[local_id] = {
            .start = start, .count = count};
        obs.local_reactive_trigger_slots.push_back(LocalSignalId{local_id});
        if (local_id < obs.local_has_observers.size()) {
          obs.local_has_observers[local_id] = 1;
        }
      }
    }
  }
}

void Engine::SeedCombKernelDirtyMarks() {
  // Seed global comb trigger slots.
  for (GlobalSignalId gid : global_reactive_trigger_slots_) {
    MarkSlotDirty(gid.value);
  }
  // Seed local comb trigger slots.
  for (auto* inst : instances_) {
    for (LocalSignalId lid : inst->observability.local_reactive_trigger_slots) {
      MarkLocalSignalDirty(*inst, lid);
    }
  }
}

void Engine::LoadInstalledComputations(
    std::span<const InstalledComputationLoadEntry> entries) {
  installed_computations_.clear();
  installed_computations_.reserve(entries.size());
  for (const auto& e : entries) {
    installed_computations_.push_back(
        InstalledComputation{
            .callable = e.callable,
            .owner_instance = e.owner_instance,
            .dep_body_local_slots = e.dep_body_local_slots,
        });
  }

  // Register each IC's dependency slots into the shared reactive trigger
  // model. ICs with a non-empty dep set appear alongside comb kernels in
  // the same reactive_trigger_backing_ and map; fixpoint dispatch fires
  // them through the kind-tagged switch in Phase 2.
  //
  // For each dep slot that already has a comb range, the existing entries
  // are moved to a new contiguous region at the end of the backing and
  // the IC entries are appended. The map entry is updated to point at the
  // new region. This preserves the one-range-per-slot invariant of the
  // shared map.
  struct PerOwnerGroup {
    RuntimeInstance* owner = nullptr;
    // Map from local dep slot to the list of IC indices that observe it.
    std::unordered_map<uint32_t, std::vector<uint32_t>> per_slot_ics;
  };
  std::unordered_map<RuntimeInstance*, PerOwnerGroup> by_owner;
  for (uint32_t ic_idx = 0;
       ic_idx < static_cast<uint32_t>(installed_computations_.size());
       ++ic_idx) {
    const auto& ic = installed_computations_[ic_idx];
    if (ic.owner_instance == nullptr) continue;
    if (ic.dep_body_local_slots.empty()) continue;
    auto& group = by_owner[ic.owner_instance];
    group.owner = ic.owner_instance;
    for (uint32_t slot : ic.dep_body_local_slots) {
      group.per_slot_ics[slot].push_back(ic_idx);
    }
  }

  for (auto& [owner_ptr, group] : by_owner) {
    auto& obs = owner_ptr->observability;
    if (obs.local_reactive_trigger_map.size() < obs.local_signal_count) {
      obs.local_reactive_trigger_map.resize(obs.local_signal_count);
    }
    for (auto& [slot, ic_idxs] : group.per_slot_ics) {
      if (slot >= obs.local_reactive_trigger_map.size()) {
        throw common::InternalError(
            "Engine::LoadInstalledComputations",
            std::format(
                "ic dep slot {} >= local_reactive_trigger_map size {} on "
                "instance '{}'",
                slot, obs.local_reactive_trigger_map.size(),
                owner_ptr->scope.path_c_str));
      }
      auto& map_entry = obs.local_reactive_trigger_map[slot];
      uint32_t old_start = map_entry.start;
      uint32_t old_count = map_entry.count;
      auto new_start = static_cast<uint32_t>(reactive_trigger_backing_.size());

      if (old_count > 0) {
        // Snapshot then re-append existing entries to keep the range
        // contiguous with the new IC entries.
        std::vector<ReactiveTriggerEntry> saved(
            reactive_trigger_backing_.begin() + old_start,
            reactive_trigger_backing_.begin() + old_start + old_count);
        for (auto& e : saved) {
          reactive_trigger_backing_.push_back(e);
        }
      } else {
        obs.local_reactive_trigger_slots.push_back(LocalSignalId{slot});
        if (slot < obs.local_has_observers.size()) {
          obs.local_has_observers[slot] = 1;
        }
      }

      for (uint32_t ic_idx : ic_idxs) {
        reactive_trigger_backing_.push_back(
            ReactiveTriggerEntry{
                .kind = ReactiveTriggerEntry::Kind::kInstallableComputation,
                .byte_offset = 0,
                .byte_size = 0,
                .has_self_edge = false,
                .owner_idx = ic_idx,
            });
      }

      map_entry.start = new_start;
      map_entry.count = old_count + static_cast<uint32_t>(ic_idxs.size());
    }
  }
}

void Engine::EvaluateInstalledComputations() {
  // Each installable computation is a void-returning writeback body.
  // The body reads parent slots, evaluates its expression, and writes
  // the child target through its owner's ext_ref_bindings; the emitted
  // store already marks the child slot dirty via LyraMarkDirtyExtRef.
  // Runtime just invokes it with the ordinary body-function ABI.
  for (const auto& ic : installed_computations_) {
    if (ic.callable == nullptr) continue;
    if (ic.owner_instance == nullptr) continue;
    ic.callable(
        design_state_base_, this, ic.owner_instance->storage.inline_base,
        ic.owner_instance);
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
      .instance = header->instance,
  };
}

void Engine::PromoteLocalFrontier() {
  // Retire old current frontier: clear consumed pending.
  for (auto* inst : fp_work_.current_instances) {
    inst->local_fixpoint.pending.clear();
  }

  // Promote next frontier: swap next->pending, clear dedup, clear membership.
  for (auto* inst : fp_work_.next_instances) {
    auto& ws = inst->local_fixpoint;
    std::swap(ws.pending, ws.next);
    ws.next.clear();
    for (LocalSignalId lid : ws.pending) {
      ws.seen[lid.value] = 0;
    }
    inst->fixpoint_scratch.in_next = false;
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
  bool has_local_dirty = !delta_dirty_instances_.empty();

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
  // Unified reactive dispatch: Phase 2 iterates the shared
  // reactive_trigger_backing_ and fires every registered consumer kind
  // (comb kernels, installable computations, ...). "Has any reactive
  // consumer" is the honest gate, not "has comb kernels".
  bool has_reactive =
      !comb_kernels_.empty() || !installed_computations_.empty();
  if (!has_conns && !has_reactive) {
    FlushSignalUpdates();
    update_set_.ClearDelta();
    ClearLocalUpdatesDelta();
    return;
  }

  constexpr uint32_t kMaxIterations = 100;
  const bool detailed = detailed_stats_enabled_;
  uint32_t iterations_used = 0;

  // Ensure workspace is sized (lazy init, once per engine lifetime).
  if (fp_work_.global_pending_seen.size() < global_slot_count_) {
    fp_work_.global_pending_seen.resize(global_slot_count_, 0);
  }
  if (has_any_self_edge_comb_ &&
      fp_work_.global_snapshot_index.size() < global_slot_count_) {
    fp_work_.global_snapshot_index.assign(global_slot_count_, UINT32_MAX);
  }
  // Per-instance local fixpoint workspace (pending, next, seen) lives on
  // RuntimeInstance::local_fixpoint. Lazy-init seen vectors once per engine
  // lifetime; scratch scalars are reset during sparse-list-driven cleanup.
  if (!fp_work_.locals_initialized) {
    for (auto* inst : instances_) {
      auto lsc = inst->observability.local_signal_count;
      if (lsc > 0 && inst->local_fixpoint.seen.size() < lsc) {
        inst->local_fixpoint.seen.resize(lsc, 0);
      }
    }
    fp_work_.locals_initialized = true;
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
  for (auto* inst : delta_dirty_instances_) {
    auto& obs = inst->observability;
    if (obs.local_signal_count == 0) continue;
    auto& ws = inst->local_fixpoint;
    ws.pending.clear();
    for (LocalSignalId lid : obs.local_updates.DeltaDirtySignals()) {
      ws.pending.push_back(lid);
    }
    if (!ws.pending.empty()) {
      fp_work_.current_instances.push_back(inst);
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
  auto enqueue_local = [&](RuntimeInstance& inst, LocalSignalId lid) {
    if (detailed) ++stats_.detailed.prop_enqueue_attempts;
    auto& ws = inst.local_fixpoint;
    if (lid.value < ws.seen.size() && ws.seen[lid.value] == 0) {
      ws.seen[lid.value] = 1;
      ws.next.push_back(lid);
      if (!inst.fixpoint_scratch.in_next) {
        inst.fixpoint_scratch.in_next = true;
        fp_work_.next_instances.push_back(&inst);
      }
    } else {
      if (detailed) ++stats_.detailed.prop_enqueue_deduped;
    }
  };

  // Helper: connection destination enqueue (handles global/local dispatch).
  // Destinations are pre-resolved: local targets carry RuntimeInstance*.
  auto enqueue_conn_dst = [&](const BatchedConnectionDst& dst) {
    std::visit(
        [&](const auto& t) {
          using T = std::decay_t<decltype(t)>;
          if constexpr (std::is_same_v<T, GlobalConnectionDst>) {
            MarkSlotDirty(t.signal.value);
            enqueue_global(t.signal);
          } else {
            MarkLocalSignalDirty(*t.instance, t.signal);
            enqueue_local(*t.instance, t.signal);
          }
        },
        dst);
  };

  // Helper: enqueue signals newly dirtied on `inst` during phase-2 firing.
  // `pre_size` is the DeltaDirtySignals size captured at phase-2 entry
  // (0 for instances first dirtied during phase-2). Used by the
  // delta_dirty_instances_-driven phase-2d scan.
  auto scan_phase2_new_dirty_for_instance = [&](RuntimeInstance& inst,
                                                uint32_t pre_size) {
    auto& obs = inst.observability;
    if (obs.local_signal_count == 0) return;
    auto delta = obs.local_updates.DeltaDirtySignals();
    if (delta.size() <= pre_size) return;
    for (size_t j = pre_size; j < delta.size(); ++j) {
      fp_work_.comb_writes_local.push_back(
          FixpointWorkspace::LocalCombWrite{
              .instance = &inst,
              .signal = delta[j],
          });
    }
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
      for (auto* inst : fp_work_.current_instances) {
        total_pending +=
            static_cast<uint32_t>(inst->local_fixpoint.pending.size());
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
          const auto* src = conn.src_ptr;
          auto* dst = conn.dst_ptr;
          if (detailed) ++stats_.detailed.conn_memcmp_executed;
          if (std::memcmp(dst, src, conn.byte_size) != 0) {
            if (detailed) ++stats_.detailed.conn_memcpy_executed;
            std::memcpy(dst, src, conn.byte_size);
            enqueue_conn_dst(conn.dst);
          }
        }
      }

      // 1b. Local triggers (current frontier only).
      for (auto* inst : fp_work_.current_instances) {
        auto& obs = inst->observability;
        for (LocalSignalId lid : inst->local_fixpoint.pending) {
          if (lid.value >= obs.local_conn_trigger_map.size()) continue;
          if (detailed) ++stats_.detailed.prop_conn_trigger_lookups;
          auto [start, count] = obs.local_conn_trigger_map[lid.value];
          if (count == 0) continue;
          if (detailed) ++stats_.detailed.prop_conn_trigger_hits;
          for (uint32_t ci = start; ci < start + count; ++ci) {
            if (detailed) ++stats_.detailed.conn_considered;
            const auto& conn = all_connections_[ci];
            const auto* src = conn.src_ptr;
            auto* dst = conn.dst_ptr;
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

    // Phase 2: shared reactive trigger dispatch.
    // Iterates the unified reactive_trigger_backing_ keyed through the
    // global + per-instance local reactive trigger maps. Each entry is
    // kind-tagged (comb kernel / installable computation) and dispatched
    // through the kind switch below. Gated on any reactive consumer.
    //
    // Coverage: all comb kernels and all installable computations --
    // which includes every reactive parent->child port binding, since
    // BuildInstallableComputations produces one installable computation
    // per binding regardless of source shape.
    // Not covered here: kDriveChildToParent memcpy connections,
    // handled by phase-1 above.
    if (has_reactive) {
      // Phase-2 entry snapshot for dirty-instance discovery.
      //
      // Source of truth for "instances dirtied during phase-2 firing" is
      // `delta_dirty_instances_` -- populated by MarkInstanceDeltaDirty
      // on every local dirty-mark regardless of origin (self writes,
      // cross-object writes via ExternalRefId, user hierarchical-ref
      // writes). For instances already dirty at phase-2 entry, record
      // their current DeltaDirtySignals size so phase-2d can enqueue
      // only the suffix newly appended during phase-2. Instances first
      // dirtied during phase-2 have implicit baseline 0.
      const size_t delta_dirty_pre_size = delta_dirty_instances_.size();
      for (size_t i = 0; i < delta_dirty_pre_size; ++i) {
        auto* inst = delta_dirty_instances_[i];
        auto& obs = inst->observability;
        inst->fixpoint_scratch.delta_pre =
            (obs.local_signal_count > 0)
                ? static_cast<uint32_t>(
                      obs.local_updates.DeltaDirtySignals().size())
                : 0;
      }

      // Snapshot global pending slots with self-edge comb triggers.
      if (has_any_self_edge_comb_) {
        fp_work_.snapshot_buf.clear();
        fp_work_.snapshots.clear();
        fp_work_.snapshotted_slots.clear();
        for (GlobalSignalId gid : fp_work_.pending_globals) {
          if (gid.value >= global_reactive_trigger_map_.size()) continue;
          auto [start, count] = global_reactive_trigger_map_[gid.value];
          if (count == 0) continue;
          bool needs_snapshot = false;
          for (uint32_t ci = start; ci < start + count; ++ci) {
            if (reactive_trigger_backing_[ci].has_self_edge) {
              needs_snapshot = true;
              break;
            }
          }
          if (!needs_snapshot) continue;
          const auto& meta = slot_meta_registry_.Get(gid.value);
          auto buf_off = static_cast<uint32_t>(fp_work_.snapshot_buf.size());
          fp_work_.snapshot_buf.resize(buf_off + meta.total_bytes);
          const auto* slot_base =
              runtime::ResolveGlobalSlotBase(meta, design_state_base_);
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

      // 2a. Fire reactive triggers from global pending.
      //     Each entry is kind-tagged: comb kernel or installable computation.
      for (GlobalSignalId gid : fp_work_.pending_globals) {
        if (gid.value >= global_reactive_trigger_map_.size()) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_lookups;
        auto [cstart, ccount] = global_reactive_trigger_map_[gid.value];
        if (ccount == 0) continue;
        if (detailed) ++stats_.detailed.prop_comb_trigger_hits;

        const auto& dirty_ranges = update_set_.DeltaRangesFor(gid.value);

        for (uint32_t ci = cstart; ci < cstart + ccount; ++ci) {
          if (detailed) ++stats_.detailed.comb_considered;
          const auto& entry = reactive_trigger_backing_[ci];

          if (entry.byte_size > 0 &&
              !dirty_ranges.Overlaps(entry.byte_offset, entry.byte_size)) {
            if (detailed) ++stats_.detailed.comb_skipped_range;
            continue;
          }

          switch (entry.kind) {
            case ReactiveTriggerEntry::Kind::kCombKernel: {
              if (detailed) ++stats_.detailed.comb_executed;
              const auto& ck = comb_kernels_[entry.owner_idx];
              FlushDeferredAssertionsForProcess(
                  ProcessId::FromIndex(ck.process_index));
              ck.body(ck.frame, 0);
              break;
            }
            case ReactiveTriggerEntry::Kind::kInstallableComputation: {
              auto& ic = installed_computations_[entry.owner_idx];
              if (ic.callable == nullptr) break;
              if (ic.owner_instance == nullptr) break;
              ic.callable(
                  design_state_base_, this,
                  ic.owner_instance->storage.inline_base, ic.owner_instance);
              break;
            }
          }
        }
      }

      // 2b. Fire reactive triggers from local pending (current frontier).
      for (auto* inst : fp_work_.current_instances) {
        auto& obs = inst->observability;
        for (LocalSignalId lid : inst->local_fixpoint.pending) {
          if (lid.value >= obs.local_reactive_trigger_map.size()) continue;
          if (detailed) ++stats_.detailed.prop_comb_trigger_lookups;
          auto [cstart, ccount] = obs.local_reactive_trigger_map[lid.value];
          if (ccount == 0) continue;
          if (detailed) ++stats_.detailed.prop_comb_trigger_hits;

          for (uint32_t ci = cstart; ci < cstart + ccount; ++ci) {
            if (detailed) ++stats_.detailed.comb_considered;
            const auto& entry = reactive_trigger_backing_[ci];
            switch (entry.kind) {
              case ReactiveTriggerEntry::Kind::kCombKernel: {
                if (detailed) ++stats_.detailed.comb_executed;
                const auto& ck = comb_kernels_[entry.owner_idx];
                FlushDeferredAssertionsForProcess(
                    ProcessId::FromIndex(ck.process_index));
                ck.body(ck.frame, 0);
                break;
              }
              case ReactiveTriggerEntry::Kind::kInstallableComputation: {
                auto& ic = installed_computations_[entry.owner_idx];
                if (ic.callable == nullptr) break;
                if (ic.owner_instance == nullptr) break;
                ic.callable(
                    design_state_base_, this,
                    ic.owner_instance->storage.inline_base, ic.owner_instance);
                break;
              }
            }
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

      // 2d. Collect local writes newly produced during phase-2 firing.
      //
      // Source of truth is delta_dirty_instances_: every local dirty
      // mark -- self writes from a comb body, cross-object writes via
      // ExternalRefId from an installable-computation body, user
      // hierarchical-ref writes -- routes through MarkLocalSignalDirty
      // and appears here. For pre-existing entries (index <
      // delta_dirty_pre_size) the per-instance delta_pre snapshot is
      // the baseline; for entries appended during phase-2 (index >=
      // delta_dirty_pre_size) the instance had no dirty signals before
      // phase-2, so the baseline is 0. delta_dirty_instances_ is
      // internally deduped via dedup_state.in_delta_dirty, so no extra
      // dedup is needed here.
      for (size_t i = 0; i < delta_dirty_instances_.size(); ++i) {
        auto* inst = delta_dirty_instances_[i];
        uint32_t pre_size =
            (i < delta_dirty_pre_size) ? inst->fixpoint_scratch.delta_pre : 0;
        scan_phase2_new_dirty_for_instance(*inst, pre_size);
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
              runtime::ResolveGlobalSlotBase(snap_meta, design_state_base_);
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
        enqueue_local(*lw.instance, lw.signal);
      }

      if (has_any_self_edge_comb_) {
        for (GlobalSignalId gid : fp_work_.snapshotted_slots) {
          fp_work_.global_snapshot_index[gid.value] = UINT32_MAX;
        }
      }
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
