#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/byte_offset.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/slot_id.hpp"
#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/constructor_.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/instance_metadata.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/process_trigger_registry.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"

namespace lyra::runtime {

namespace {

// Relocate a body-local observable index to design-global slot_id.
auto RelocateBodyLocalSlot(
    const RuntimeInstance& instance, uint32_t body_local_index)
    -> common::SlotId {
  return common::SlotId{instance.local_signal_coord_base + body_local_index};
}

// Relocate an observable descriptor's storage_owner_ref to design-global.
auto RelocateObservableOwnerSlot(
    const RuntimeInstance& instance, const ObservableDescriptorEntry& entry)
    -> common::SlotId {
  if ((entry.flags & kObservableFlagOwnerAbsolute) != 0) {
    return common::SlotId{entry.storage_owner_ref};
  }
  return common::SlotId{
      instance.local_signal_coord_base + entry.storage_owner_ref};
}

// Compose hierarchical trace signal name from instance path and local name.
auto ComposeHierarchicalTraceName(
    std::string_view instance_path, std::string_view local_name)
    -> std::string {
  if (instance_path.empty()) return std::string(local_name);
  if (local_name.empty()) return std::string(instance_path);
  return std::format("{}.{}", instance_path, local_name);
}

// Storage-facing facts for one relocated observable entry.
struct ObservableStorageFact {
  common::SlotId slot_id;
  common::SlotId owner_slot_id;
  SlotStorageDomain domain;
  uint32_t instance_id;
  common::InstanceByteOffset instance_rel_off;
  uint32_t total_bytes;
  SlotStorageKind storage_kind;
  PackedPlanes planes;
};

// Trace-facing facts for one relocated observable entry.
struct ObservableTraceFact {
  std::string hierarchical_name;
  uint32_t bit_width;
  TraceSignalKind trace_kind;
};

// Combined pre-resolved fact bundle.
struct ObservableRuntimeFact {
  ObservableStorageFact storage;
  std::optional<ObservableTraceFact> trace;
};

// Read a pool string from an observable descriptor template.
auto ReadObservablePoolString(
    const ObservableDescriptorTemplateView& obs, uint32_t pool_off)
    -> std::string_view {
  if (pool_off == 0 || pool_off >= obs.pool_size) return {};
  return std::span(obs.pool, obs.pool_size).subspan(pool_off).data();
}

// Walk one bundle's observable descriptors, relocate, decode names,
// and call back with pre-resolved facts.
template <typename Fn>
void ForEachBundleObservable(
    const InstanceMetadataBundle& bundle, const Fn& callback) {
  const auto& obs = bundle.body_desc->observable_descriptors;
  for (uint32_t local = 0; local < obs.entries.size(); ++local) {
    const auto& entry = obs.entries[local];

    ObservableStorageFact storage{
        .slot_id = RelocateBodyLocalSlot(*bundle.instance, local),
        .owner_slot_id = RelocateObservableOwnerSlot(*bundle.instance, entry),
        .domain = static_cast<SlotStorageDomain>(entry.storage_domain),
        .instance_id = bundle.instance_id,
        .instance_rel_off =
            common::InstanceByteOffset{entry.storage_byte_offset},
        .total_bytes = entry.total_bytes,
        .storage_kind = static_cast<SlotStorageKind>(entry.storage_kind),
        .planes =
            PackedPlanes{
                .value_off = entry.value_lane_offset,
                .value_bytes = entry.value_lane_bytes,
                .unk_off = entry.unk_lane_offset,
                .unk_bytes = entry.unk_lane_bytes,
            },
    };

    auto local_name = ReadObservablePoolString(obs, entry.local_name_pool_off);
    std::string_view instance_path =
        bundle.instance_path != nullptr ? bundle.instance_path : "";
    auto hierarchical_name =
        ComposeHierarchicalTraceName(instance_path, local_name);

    ObservableRuntimeFact fact{
        .storage = storage,
        .trace =
            ObservableTraceFact{
                .hierarchical_name = std::move(hierarchical_name),
                .bit_width = entry.bit_width,
                .trace_kind = static_cast<TraceSignalKind>(entry.trace_kind),
            },
    };

    callback(fact);
  }
}

auto BuildModuleTriggerDescriptors(
    std::span<const InstanceMetadataBundle> bundles, uint32_t total_slot_count)
    -> std::vector<ProcessTriggerDescriptor> {
  std::vector<ProcessTriggerDescriptor> descriptors;

  for (const InstanceMetadataBundle& bundle : bundles) {
    const auto& triggers = bundle.body_desc->triggers;
    if (triggers.proc_ranges.empty()) continue;

    uint32_t slot_base = bundle.instance->local_signal_coord_base;

    for (uint32_t pwb = 0; pwb < triggers.proc_ranges.size(); ++pwb) {
      uint32_t proc_idx = bundle.module_proc_base + pwb;
      const auto& range = triggers.proc_ranges[pwb];
      bool groupable = triggers.proc_groupable[pwb] != 0;

      for (uint32_t t = 0; t < range.count; ++t) {
        const auto& te = triggers.entries[range.start + t];
        uint32_t slot_id = (te.flags & kTriggerTemplateFlagDesignGlobal) != 0
                               ? te.slot_id
                               : slot_base + te.slot_id;

        if (slot_id >= total_slot_count) {
          throw common::InternalError(
              "BuildModuleTriggerDescriptors",
              std::format(
                  "relocated slot_id {} exceeds total {} for instance {}",
                  slot_id, total_slot_count, bundle.instance_id));
        }

        descriptors.push_back(
            ProcessTriggerDescriptor{
                .scheduled_process_index = proc_idx,
                .slot_id = slot_id,
                .edge = static_cast<common::EdgeKind>(te.edge),
                .is_groupable = groupable,
            });
      }
    }
  }

  return descriptors;
}

}  // namespace

auto Engine::InitModuleInstancesFromBundles(
    std::span<const InstanceMetadataBundle> bundles,
    std::span<const uint32_t> design_global_slot_meta_words, void** states)
    -> TraceSignalMetaRegistry {
  // Reset all module-owned outputs for clean initialization.
  pending_module_trigger_descs_.clear();
  pending_module_process_meta_.clear();
  slot_meta_registry_ = SlotMetaRegistry{};
  comb_kernels_.clear();
  comb_kernel_flags_.clear();
  comb_trigger_backing_.clear();
  comb_trigger_map_.clear();
  comb_trigger_slots_.clear();
  comb_narrow_count_ = 0;
  comb_full_slot_count_ = 0;
  has_any_self_edge_comb_ = false;
  fp_work_.pending_seen.clear();
  fp_work_.snapshot_index.clear();
  signal_subs_.clear();
  activation_slot_gen_.clear();

  if (bundles.empty() && design_global_slot_meta_words.empty()) return {};

  // Step B: Build slot meta registry.
  // Design-global slots from constructor flat transport (structural debt).
  // Instance-owned slots from bundles via shared observable walk.
  auto global_slot_count =
      static_cast<uint32_t>(design_global_slot_meta_words.size());
  if (global_slot_count > 0) {
    slot_meta_registry_ = SlotMetaRegistry(
        design_global_slot_meta_words.data(), global_slot_count);
  }

  // Assign dense coordination bases before building instance-owned metadata.
  // This sets local_signal_coord_base on each instance, which relocation
  // helpers depend on.
  AssignDenseCoordinationBasesFromBundles(bundles);

  // Shared observable walk: build instance-owned slot meta and trace meta
  // from the same pre-resolved facts in the same order. Instance trace
  // signals are returned to the caller for merging with constructor-built
  // trace entries before InitTraceSignalMeta.
  TraceSignalMetaRegistry instance_trace;
  for (const auto& bundle : bundles) {
    ForEachBundleObservable(bundle, [&](const ObservableRuntimeFact& fact) {
      if (fact.storage.domain != SlotStorageDomain::kInstanceOwned) return;

      slot_meta_registry_.AppendSlot(
          SlotMeta{
              .domain = SlotStorageDomain::kInstanceOwned,
              .owner_instance_id = fact.storage.instance_id,
              .instance_rel_off =
                  static_cast<uint32_t>(fact.storage.instance_rel_off.value),
              .total_bytes = fact.storage.total_bytes,
              .kind = fact.storage.storage_kind,
              .planes = fact.storage.planes,
              .storage_owner_slot_id = fact.storage.owner_slot_id.value,
          });

      if (fact.trace.has_value()) {
        instance_trace.AppendSignal(
            fact.trace->hierarchical_name, fact.trace->bit_width,
            fact.trace->trace_kind, fact.storage.owner_slot_id.value);
      }
    });
  }

  uint32_t total_slots = slot_meta_registry_.Size();

  // Size coordination arrays.
  {
    std::vector<uint32_t> sizes;
    sizes.reserve(total_slots);
    for (uint32_t i = 0; i < total_slots; ++i) {
      sizes.push_back(slot_meta_registry_.Get(i).total_bytes);
    }
    update_set_.Init(total_slots, sizes);
  }
  signal_subs_.resize(total_slots);
  activation_slot_gen_.resize(total_slots, 0);

  ValidateInstanceOwnedSlotMeta();

  // Step C: Build module trigger descriptors as pending intermediate data.
  pending_module_trigger_descs_ =
      BuildModuleTriggerDescriptors(bundles, total_slots);

  // Step D: Build comb kernels from body templates.
  auto proc_states = std::span(states, num_processes_);
  comb_kernel_flags_.resize(num_processes_, 0);

  struct ParsedCombTrigger {
    uint32_t slot_id;
    uint32_t kernel_idx;
    uint32_t byte_offset;
    uint32_t byte_size;
    bool has_self_edge;
  };
  std::vector<ParsedCombTrigger> comb_entries;

  for (const InstanceMetadataBundle& bundle : bundles) {
    const auto& comb = bundle.body_desc->comb;
    if (comb.kernels.empty()) continue;

    uint32_t slot_base = bundle.instance->local_signal_coord_base;

    for (const auto& kernel : comb.kernels) {
      uint32_t proc_idx = bundle.module_proc_base + kernel.proc_within_body;
      uint32_t flags = (kernel.has_self_edge != 0) ? CombKernel::kSelfEdge : 0U;

      if ((flags & CombKernel::kSelfEdge) != 0) {
        has_any_self_edge_comb_ = true;
      }

      const auto* header =
          static_cast<const ProcessFrameHeader*>(proc_states[proc_idx]);

      auto comb_idx = static_cast<uint32_t>(comb_kernels_.size());
      comb_kernels_.push_back(
          CombKernel{
              .body = header->body,
              .frame = proc_states[proc_idx],
              .process_index = proc_idx,
              .flags = flags,
          });

      comb_kernel_flags_[proc_idx] = 1;

      bool kernel_self_edge = (flags & CombKernel::kSelfEdge) != 0;
      for (uint32_t ti = 0; ti < kernel.trigger_count; ++ti) {
        const auto& trig = comb.entries[kernel.trigger_start + ti];
        uint32_t trigger_slot =
            (trig.flags & kCombTemplateFlagDesignGlobal) != 0
                ? trig.slot_id
                : slot_base + trig.slot_id;

        comb_entries.push_back(
            ParsedCombTrigger{
                .slot_id = trigger_slot,
                .kernel_idx = comb_idx,
                .byte_offset = trig.byte_offset,
                .byte_size = trig.byte_size,
                .has_self_edge = kernel_self_edge,
            });
        if (trig.byte_size > 0) {
          ++comb_narrow_count_;
        } else {
          ++comb_full_slot_count_;
        }
      }
    }
  }

  if (!comb_entries.empty()) {
    fp_work_.pending_seen.resize(total_slots, 0);
    if (has_any_self_edge_comb_) {
      fp_work_.snapshot_index.assign(total_slots, UINT32_MAX);
    }

    std::ranges::sort(comb_entries, {}, &ParsedCombTrigger::slot_id);

    comb_trigger_backing_.reserve(comb_entries.size());
    comb_trigger_map_.resize(total_slots);

    uint32_t i = 0;
    while (i < comb_entries.size()) {
      uint32_t slot = comb_entries[i].slot_id;
      auto start = static_cast<uint32_t>(comb_trigger_backing_.size());
      while (i < comb_entries.size() && comb_entries[i].slot_id == slot) {
        comb_trigger_backing_.push_back({
            .kernel_idx = comb_entries[i].kernel_idx,
            .byte_offset = comb_entries[i].byte_offset,
            .byte_size = comb_entries[i].byte_size,
            .has_self_edge = comb_entries[i].has_self_edge,
        });
        ++i;
      }
      uint32_t count =
          static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
      comb_trigger_map_[slot] = {.start = start, .count = count};
      comb_trigger_slots_.push_back(slot);
    }
  }

  // Step E: Store narrow process-meta-specific pending data.
  pending_module_process_meta_.clear();
  pending_module_process_meta_.reserve(bundles.size());
  for (const InstanceMetadataBundle& bundle : bundles) {
    pending_module_process_meta_.push_back(
        PendingModuleProcessMeta{
            .body_desc = bundle.body_desc,
            .instance_path = bundle.instance_path,
            .module_proc_base = bundle.module_proc_base,
        });
  }

  // Step F: Register immutable decision metadata tables per process.
  for (const InstanceMetadataBundle& bundle : bundles) {
    const auto& dtables = bundle.body_desc->decision_tables;
    for (uint32_t local = 0; local < dtables.size(); ++local) {
      const auto pid = ProcessId::FromIndex(bundle.module_proc_base + local);
      if (pid.Index() >= process_decision_tables_.size()) continue;
      const auto& desc = dtables[local];
      if (desc.count > 0 && desc.metas == nullptr) {
        throw common::InternalError(
            "Engine::InitModuleInstancesFromBundles",
            std::format(
                "decision table for pid {} has count {} but null metas",
                pid.Index(), desc.count));
      }
      process_decision_tables_[pid.Index()] = ProcessBodyDecisionTable{
          .count = DecisionSiteCount::FromCount(desc.count),
          .metas = desc.metas,
      };
      if (desc.count > 0) {
        auto& state = decision_states_[pid.Index()];
        state.slots.resize(desc.count);
        state.dirty_seen.resize(desc.count, 0);
      }
    }
  }

  return instance_trace;
}

void Engine::AssignDenseCoordinationBasesFromBundles(
    std::span<const InstanceMetadataBundle> bundles) {
  // Global slot count = design-global slots already in the registry.
  uint32_t global_slot_count = slot_meta_registry_.Size();

  // Count instance-owned slots per bundle from observable descriptors.
  uint32_t next_base = global_slot_count;
  for (const InstanceMetadataBundle& bundle : bundles) {
    bundle.instance->local_signal_coord_base = next_base;
    uint32_t instance_owned_count = 0;
    for (const auto& entry : bundle.body_desc->observable_descriptors.entries) {
      if (entry.storage_domain == 1) {
        ++instance_owned_count;
      }
    }
    next_base += instance_owned_count;
  }
}

void Engine::InitProcessMeta(ProcessMetaRegistry connection_registry) {
  if (pending_module_process_meta_.empty()) {
    process_meta_ = std::move(connection_registry);
    return;
  }

  // Build process meta by proc_idx, not by append order.
  // Each slot is filled exactly once; validates completeness.
  struct PendingEntry {
    std::string instance_path;
    uint32_t kind_packed = 0;
    std::string file_path;
    uint32_t line = 0;
    uint32_t col = 0;
    bool filled = false;
  };
  std::vector<PendingEntry> entries(num_processes_);

  // Fill connection entries by proc_idx.
  for (uint32_t i = 0; i < connection_registry.Size(); ++i) {
    if (i >= num_processes_) {
      throw common::InternalError(
          "Engine::InitProcessMeta",
          std::format("connection proc_idx {} out of range", i));
    }
    const auto& meta = connection_registry.Get(i);
    auto& dst = entries[i];
    if (dst.filled) {
      throw common::InternalError(
          "Engine::InitProcessMeta",
          std::format("duplicate process meta for proc_idx {}", i));
    }
    dst.instance_path =
        connection_registry.GetPoolString(meta.instance_path_str_off);
    dst.kind_packed = static_cast<uint32_t>(meta.kind) |
                      (meta.reserved0 << 8U) |
                      (static_cast<uint32_t>(meta.reserved1) << 16U);
    dst.file_path = connection_registry.GetPoolString(meta.loc.file_str_off);
    dst.line = meta.loc.line;
    dst.col = meta.loc.col;
    dst.filled = true;
  }

  // Fill module entries by proc_idx from pending data.
  for (const auto& pending : pending_module_process_meta_) {
    const auto& meta = pending.body_desc->meta;
    for (uint32_t local = 0; local < meta.entries.size(); ++local) {
      uint32_t proc_idx = pending.module_proc_base + local;
      if (proc_idx >= num_processes_) {
        throw common::InternalError(
            "Engine::InitProcessMeta",
            std::format("module proc_idx {} out of range", proc_idx));
      }
      auto& dst = entries[proc_idx];
      if (dst.filled) {
        throw common::InternalError(
            "Engine::InitProcessMeta",
            std::format("duplicate process meta for proc_idx {}", proc_idx));
      }
      const auto& src = meta.entries[local];
      dst.instance_path =
          pending.instance_path != nullptr ? pending.instance_path : "";
      if (src.file_pool_off > 0 && src.file_pool_off < meta.pool_size) {
        dst.file_path = std::span(meta.pool, meta.pool_size)
                            .subspan(src.file_pool_off)
                            .data();
      }
      dst.kind_packed = src.kind_packed;
      dst.line = src.line;
      dst.col = src.col;
      dst.filled = true;
    }
  }

  // Serialize in proc_idx order into merged word table + pool.
  std::vector<uint32_t> merged_words;
  std::vector<char> merged_pool;
  merged_pool.push_back('\0');

  auto append_string = [&](std::string_view s) -> uint32_t {
    if (s.empty()) return 0;
    auto off = static_cast<uint32_t>(merged_pool.size());
    merged_pool.insert(merged_pool.end(), s.begin(), s.end());
    merged_pool.push_back('\0');
    return off;
  };

  merged_words.reserve(num_processes_ * process_meta_abi::kStride);
  for (uint32_t proc_idx = 0; proc_idx < num_processes_; ++proc_idx) {
    const auto& entry = entries[proc_idx];
    if (!entry.filled) {
      throw common::InternalError(
          "Engine::InitProcessMeta",
          std::format("missing process meta for proc_idx {}", proc_idx));
    }
    merged_words.push_back(append_string(entry.instance_path));
    merged_words.push_back(entry.kind_packed);
    merged_words.push_back(append_string(entry.file_path));
    merged_words.push_back(entry.line);
    merged_words.push_back(entry.col);
  }

  auto total_count =
      static_cast<uint32_t>(merged_words.size() / process_meta_abi::kStride);
  process_meta_ = ProcessMetaRegistry(
      merged_words.data(), total_count, merged_pool.data(),
      static_cast<uint32_t>(merged_pool.size()));

  pending_module_process_meta_.clear();
}

void Engine::ReleaseStringSlots() {
  if (!slot_meta_registry_.IsPopulated()) return;

  for (uint32_t slot_id = 0; slot_id < slot_meta_registry_.Size(); ++slot_id) {
    const auto& meta = slot_meta_registry_.Get(slot_id);
    if (meta.kind != SlotStorageKind::kString) continue;
    if (meta.storage_owner_slot_id != slot_id) continue;

    if (meta.total_bytes != sizeof(LyraStringHandle)) {
      throw common::InternalError(
          "Engine::ReleaseStringSlots",
          std::format(
              "string slot {} has total_bytes {} != expected {}", slot_id,
              meta.total_bytes, sizeof(LyraStringHandle)));
    }

    auto* bytes = ResolveSlotBytesMut(slot_id);
    LyraStringHandle handle{};
    std::memcpy(&handle, bytes, sizeof(handle));
    LyraStringRelease(handle);
    std::memset(bytes, 0, sizeof(handle));
  }
}

}  // namespace lyra::runtime
