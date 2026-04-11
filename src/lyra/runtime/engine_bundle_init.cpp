#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/byte_offset.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/slot_id.hpp"
#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/constructor_.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/instance_metadata.hpp"
#include "lyra/runtime/instance_observability.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/string.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"

namespace lyra::runtime {

namespace {

// Relocate a body-local observable index to design-global slot_id.
auto RelocateBodyLocalSlot(uint32_t flat_coord_base, uint32_t body_local_index)
    -> common::SlotId {
  return common::SlotId{flat_coord_base + body_local_index};
}

// Relocate an observable descriptor's storage_owner_ref to design-global.
auto RelocateObservableOwnerSlot(
    uint32_t flat_coord_base, const ObservableDescriptorEntry& entry)
    -> common::SlotId {
  if ((entry.flags & kObservableFlagOwnerAbsolute) != 0) {
    return common::SlotId{entry.storage_owner_ref};
  }
  return common::SlotId{flat_coord_base + entry.storage_owner_ref};
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
  common::SlotId slot_id = {};
  common::SlotId owner_slot_id = {};
  SlotStorageDomain domain = {};
  InstanceId instance_id = {};
  common::InstanceByteOffset instance_rel_off = {};
  uint32_t total_bytes = 0;
  SlotStorageKind storage_kind = {};
  PackedPlanes planes = {};
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
// flat_coord_base: design-global slot_id base for this instance's
// body-local observables. Computed from a running counter at the call site.
template <typename Fn>
void ForEachBundleObservable(
    const InstanceMetadataBundle& bundle, uint32_t flat_coord_base,
    Fn callback) {
  const auto& obs = bundle.body_desc->observable_descriptors;
  for (uint32_t local = 0; local < obs.entries.size(); ++local) {
    const auto& entry = obs.entries[local];

    ObservableStorageFact storage{
        .slot_id = RelocateBodyLocalSlot(flat_coord_base, local),
        .owner_slot_id = RelocateObservableOwnerSlot(flat_coord_base, entry),
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

    for (uint32_t pwb = 0; pwb < triggers.proc_ranges.size(); ++pwb) {
      uint32_t proc_idx = bundle.module_proc_base + pwb;
      const auto& range = triggers.proc_ranges[pwb];
      bool groupable = triggers.proc_groupable[pwb] != 0;

      for (uint32_t t = 0; t < range.count; ++t) {
        const auto& te = triggers.entries[range.start + t];
        bool is_ext_ref = (te.flags & kTriggerTemplateFlagExternalRef) != 0;
        bool is_global = (te.flags & kTriggerTemplateFlagDesignGlobal) != 0;

        if (is_ext_ref) {
          // External-ref trigger: resolve to object-local identity on
          // the target instance via the binding record.
          auto bindings = std::span(
              bundle.instance->ext_ref_bindings,
              bundle.instance->ext_ref_binding_count);
          if (te.slot_id >= bindings.size()) {
            throw common::InternalError(
                "BuildModuleTriggerDescriptors",
                std::format(
                    "instance {} ext-ref trigger index {} >= binding "
                    "count {}",
                    bundle.instance_id, te.slot_id, bindings.size()));
          }
          const auto& binding = bindings[te.slot_id];
          descriptors.push_back(
              ProcessTriggerDescriptor{
                  .scheduled_process_index = proc_idx,
                  .slot_id = binding.target_local_signal.value,
                  .edge = static_cast<common::EdgeKind>(te.edge),
                  .is_groupable = groupable,
                  .is_local = true,
                  .instance_id = InstanceId{binding.target_instance_id},
              });
        } else if (is_global) {
          if (te.slot_id >= total_slot_count) {
            throw common::InternalError(
                "BuildModuleTriggerDescriptors",
                std::format(
                    "global slot_id {} exceeds total {} for instance {}",
                    te.slot_id, total_slot_count, bundle.instance_id));
          }
          descriptors.push_back(
              ProcessTriggerDescriptor{
                  .scheduled_process_index = proc_idx,
                  .slot_id = te.slot_id,
                  .edge = static_cast<common::EdgeKind>(te.edge),
                  .is_groupable = groupable,
                  .is_local = false,
                  .instance_id = InstanceId{0},
              });
        } else {
          if (te.slot_id >= bundle.instance->observability.local_signal_count) {
            throw common::InternalError(
                "BuildModuleTriggerDescriptors",
                std::format(
                    "local slot_id {} exceeds local_signal_count {} "
                    "for instance {}",
                    te.slot_id,
                    bundle.instance->observability.local_signal_count,
                    bundle.instance_id));
          }
          descriptors.push_back(
              ProcessTriggerDescriptor{
                  .scheduled_process_index = proc_idx,
                  .slot_id = te.slot_id,
                  .edge = static_cast<common::EdgeKind>(te.edge),
                  .is_groupable = groupable,
                  .is_local = true,
                  .instance_id = bundle.instance_id,
              });
        }
      }
    }
  }

  return descriptors;
}

}  // namespace

void Engine::InitModuleInstancesFromBundles(
    std::span<const InstanceMetadataBundle> bundles,
    std::span<const uint32_t> design_global_slot_meta_words, void** states) {
  // Reset all module-owned outputs for clean initialization.
  pending_module_trigger_descs_.clear();
  pending_module_process_meta_.clear();
  process_instance_map_.assign(num_processes_, InstanceId{.value = 0});
  slot_meta_registry_ = SlotMetaRegistry{};
  comb_kernels_.clear();
  comb_kernel_flags_.clear();
  comb_trigger_backing_.clear();
  global_comb_trigger_map_.clear();
  global_comb_trigger_slots_.clear();
  global_conn_trigger_map_.clear();
  comb_narrow_count_ = 0;
  comb_full_slot_count_ = 0;
  has_any_self_edge_comb_ = false;
  fp_work_.global_pending_seen.clear();
  fp_work_.global_snapshot_index.clear();
  fp_work_.locals.clear();
  signal_subs_.clear();
  activation_slot_gen_.clear();
  body_observable_layouts_.clear();

  if (bundles.empty() && design_global_slot_meta_words.empty()) return;

  // Step B: Build slot meta registry.
  // Design-global slots from constructor flat transport (structural debt).
  // Instance-owned slots from bundles via shared observable walk.
  auto global_word_count =
      static_cast<uint32_t>(design_global_slot_meta_words.size());
  if (global_word_count > 0) {
    slot_meta_registry_ = SlotMetaRegistry(
        design_global_slot_meta_words.data(), global_word_count);
  }
  global_slot_count_ = slot_meta_registry_.Size();

  // Validate all bundle pointers before any dereference.
  for (size_t bi = 0; bi < bundles.size(); ++bi) {
    const auto& b = bundles[bi];
    if (b.instance == nullptr) {
      throw common::InternalError(
          "Engine::InitModuleInstancesFromBundles",
          std::format(
              "bundle {} (instance_id {}) has null instance", bi,
              b.instance_id));
    }
    if (b.body_desc == nullptr) {
      throw common::InternalError(
          "Engine::InitModuleInstancesFromBundles",
          std::format(
              "bundle {} (instance_id {}) has null body_desc", bi,
              b.instance_id));
    }
  }

  // D6d: Populate per-instance time metadata from body descriptors.
  InitInstanceTimeMetadata(bundles);

  // Compute per-instance flat coordinate bases inline from observable
  // descriptor counts. This replaces AssignDenseCoordinationBasesFromBundles.
  uint32_t next_flat_base = global_slot_count_;
  std::vector<uint32_t> bundle_flat_bases(bundles.size());
  for (size_t bi = 0; bi < bundles.size(); ++bi) {
    bundle_flat_bases[bi] = next_flat_base;
    uint32_t instance_owned_count = 0;
    for (const auto& entry :
         bundles[bi].body_desc->observable_descriptors.entries) {
      if (entry.storage_domain == 1) {
        ++instance_owned_count;
      }
    }
    next_flat_base += instance_owned_count;
  }

  // Shared observable walk: build instance-owned slot meta from the
  // pre-resolved facts. Instance-owned trace metadata lives in
  // BodyObservableLayout (built per-body below), not in a flat merged
  // registry.
  for (size_t bi = 0; bi < bundles.size(); ++bi) {
    const auto& bundle = bundles[bi];
    ForEachBundleObservable(
        bundle, bundle_flat_bases[bi], [&](const ObservableRuntimeFact& fact) {
          if (fact.storage.domain != SlotStorageDomain::kInstanceOwned) return;

          slot_meta_registry_.AppendSlot(
              SlotMeta{
                  .domain = SlotStorageDomain::kInstanceOwned,
                  .owner_instance_id = fact.storage.instance_id,
                  .instance_rel_off = static_cast<uint32_t>(
                      fact.storage.instance_rel_off.value),
                  .total_bytes = fact.storage.total_bytes,
                  .kind = fact.storage.storage_kind,
                  .planes = fact.storage.planes,
                  .storage_owner_slot_id = fact.storage.owner_slot_id.value,
              });
        });
  }

  // R5: Build per-body observable layouts and per-instance observability.
  // One BodyObservableLayout per distinct body_key (shared across instances
  // of the same specialization). Each instance's observability gets a
  // non-owning pointer to its body's layout.
  //
  // Temporary Cut 1 build-time map key. Final ownership is by shared-body
  // runtime metadata object.
  body_observable_layouts_.clear();
  auto get_or_create_layout =
      [this](
          const InstanceMetadataBundle& bundle,
          absl::flat_hash_map<const void*, BodyObservableLayout*>& map)
      -> BodyObservableLayout& {
    auto [it, inserted] = map.try_emplace(bundle.body_key, nullptr);
    if (!inserted) return *it->second;

    body_observable_layouts_.emplace_back();
    it->second = &body_observable_layouts_.back();
    auto& layout = *it->second;

    // Seed trace name pool with sentinel '\0' at offset 0.
    // Offset 0 means "no name". Real names start at offset >= 1.
    layout.trace_name_pool.push_back('\0');

    // Count instance-owned descriptors to size vectors upfront.
    const auto& obs = bundle.body_desc->observable_descriptors;
    uint32_t local_count = 0;
    for (const auto& entry : obs.entries) {
      if (entry.storage_domain == 1) ++local_count;
    }

    layout.slot_meta.resize(local_count);
    layout.trace_meta.resize(local_count);
    // Preserve existing behavior: all instance-owned signals are
    // trace-selected by default until a later trace-selection policy cut
    // changes this explicitly.
    layout.trace_select_default.assign(local_count, 1);

    // Populate by explicit local_signal_id, not encounter order.
    // Each instance-owned descriptor carries a stable body-local signal id
    // set at codegen time. Validates every id is populated exactly once.
    std::vector<uint8_t> populated(local_count, 0);
    for (const auto& entry : obs.entries) {
      if (entry.storage_domain != 1) continue;

      if (entry.local_signal_id >= local_count) {
        throw common::InternalError(
            "get_or_create_layout",
            std::format(
                "local_signal_id {} out of range (local_count {})",
                entry.local_signal_id, local_count));
      }
      if (populated[entry.local_signal_id] != 0) {
        throw common::InternalError(
            "get_or_create_layout",
            std::format("duplicate local_signal_id {}", entry.local_signal_id));
      }
      populated[entry.local_signal_id] = 1;

      auto lid = entry.local_signal_id;
      bool is_container = entry.backing_rel_off != 0;
      layout.slot_meta[lid] = InstanceSlotMeta{
          .instance_rel_off = entry.storage_byte_offset,
          .total_bytes = entry.total_bytes,
          .kind = static_cast<SlotStorageKind>(entry.storage_kind),
          .planes =
              PackedPlanes{
                  .value_off = entry.value_lane_offset,
                  .value_bytes = entry.value_lane_bytes,
                  .unk_off = entry.unk_lane_offset,
                  .unk_bytes = entry.unk_lane_bytes,
              },
          .is_container = is_container,
          .backing_rel_off = entry.backing_rel_off,
          .backing_bytes = is_container ? entry.total_bytes : 0U,
      };

      auto local_name =
          ReadObservablePoolString(obs, entry.local_name_pool_off);
      uint32_t name_off = 0;
      if (!local_name.empty()) {
        name_off = static_cast<uint32_t>(layout.trace_name_pool.size());
        layout.trace_name_pool.insert(
            layout.trace_name_pool.end(), local_name.begin(), local_name.end());
        layout.trace_name_pool.push_back('\0');
      }

      layout.trace_meta[lid] = BodyTraceMeta{
          .local_name_offset = name_off,
          .bit_width = entry.bit_width,
          .kind = static_cast<TraceSignalKind>(entry.trace_kind),
      };
    }

    // Validate every local signal id is populated.
    for (uint32_t i = 0; i < local_count; ++i) {
      if (populated[i] == 0) {
        throw common::InternalError(
            "get_or_create_layout",
            std::format("local_signal_id {} not populated", i));
      }
    }

    return layout;
  };

  absl::flat_hash_map<const void*, BodyObservableLayout*> body_layout_map;
  for (const auto& bundle : bundles) {
    if (bundle.instance == nullptr) {
      throw common::InternalError(
          "Engine::InitModuleInstancesFromBundles",
          std::format(
              "bundle instance_id {} has null instance pointer",
              bundle.instance_id));
    }
    // L8a: Initialize per-instance event state from body descriptor.
    bundle.instance->event_state.Init(bundle.body_desc->desc->event_count);

    auto& layout = get_or_create_layout(bundle, body_layout_map);
    auto local_count = static_cast<uint32_t>(layout.slot_meta.size());
    bundle.instance->observability.layout = &layout;
    bundle.instance->observability.local_signal_count = local_count;
    if (local_count > 0) {
      bundle.instance->observability.Init();
    }

    // Initialize deferred-NBA pending set sized to local signal count.
    bundle.instance->nba_pending.Init(
        local_count, GetInstanceIndex(bundle.instance_id));

    // R5: populate process-to-instance mapping for subscription routing.
    for (uint32_t p = 0; p < bundle.num_module_processes; ++p) {
      uint32_t proc_idx = bundle.module_proc_base + p;
      if (proc_idx < process_instance_map_.size()) {
        process_instance_map_[proc_idx] = bundle.instance_id;
      }
    }
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
  global_has_observers_.assign(total_slots, 0);
  activation_slot_gen_.resize(total_slots, 0);

  ValidateInstanceOwnedSlotMeta();

  // Step C: Build module trigger descriptors as pending intermediate data.
  pending_module_trigger_descs_ =
      BuildModuleTriggerDescriptors(bundles, total_slots);

  // Step D: Build comb kernels from body templates.
  // R5: Domain-split comb trigger maps. Global triggers go to
  // global_comb_trigger_map_. Local triggers go to per-instance
  // local_comb_trigger_map on observability.
  auto proc_states = std::span(states, num_processes_);
  comb_kernel_flags_.resize(num_processes_, 0);

  // Validated decode of CombTemplateEntry into exactly one of three modes.
  enum class CombTriggerMode { kBodyLocal, kGlobal, kCrossInstance };

  struct DecodedCombTriggerIdentity {
    CombTriggerMode mode = CombTriggerMode::kBodyLocal;
    GlobalSignalId global = GlobalSignalId{0};
    InstanceId instance_id = InstanceId{0};
    LocalSignalId local = LocalSignalId{0};
  };

  auto decode_comb_trigger =
      [](const CombTemplateEntry& trig) -> DecodedCombTriggerIdentity {
    const bool is_global = (trig.flags & kCombTemplateFlagDesignGlobal) != 0;
    const bool is_cross = (trig.flags & kCombTemplateFlagCrossInstance) != 0;

    if (is_global && is_cross) {
      throw common::InternalError(
          "DecodeCombTriggerIdentity",
          std::format(
              "comb trigger slot_id={} has both global and "
              "cross-instance flags",
              trig.slot_id));
    }

    if (is_global) {
      return {
          .mode = CombTriggerMode::kGlobal,
          .global = GlobalSignalId{trig.slot_id},
      };
    }

    if (is_cross) {
      return {
          .mode = CombTriggerMode::kCrossInstance,
          .instance_id = InstanceId{trig.owner_instance_id},
          .local = LocalSignalId{trig.local_signal_id},
      };
    }

    return {
        .mode = CombTriggerMode::kBodyLocal,
        .local = LocalSignalId{trig.slot_id},
    };
  };

  struct ParsedCombTrigger {
    bool is_local;
    RuntimeInstance* instance;  // valid when is_local
    uint32_t local_id;          // valid when is_local
    uint32_t global_id;         // valid when !is_local
    uint32_t kernel_idx;
    uint32_t byte_offset;
    uint32_t byte_size;
    bool has_self_edge;
  };
  std::vector<ParsedCombTrigger> comb_entries;

  for (const InstanceMetadataBundle& bundle : bundles) {
    const auto& comb = bundle.body_desc->comb;
    if (comb.kernels.empty()) continue;

    // Validate comb descriptor span consistency before dereferencing.
    for (const auto& kernel : comb.kernels) {
      uint32_t end = kernel.trigger_start + kernel.trigger_count;
      if (end < kernel.trigger_start || end > comb.entries.size()) {
        throw common::InternalError(
            "InitModuleInstancesFromBundles",
            std::format(
                "comb kernel proc_within_body {} has trigger range "
                "[{}, {}) out of bounds (entries.size={})",
                kernel.proc_within_body, kernel.trigger_start, end,
                comb.entries.size()));
      }
    }

    for (const auto& kernel : comb.kernels) {
      uint32_t proc_idx = bundle.module_proc_base + kernel.proc_within_body;
      if (proc_idx >= num_processes_) {
        throw common::InternalError(
            "InitModuleInstancesFromBundles",
            std::format(
                "comb proc_idx {} >= num_processes {}", proc_idx,
                num_processes_));
      }
      uint32_t flags = (kernel.has_self_edge != 0) ? CombKernel::kSelfEdge : 0U;

      if ((flags & CombKernel::kSelfEdge) != 0) {
        has_any_self_edge_comb_ = true;
      }

      auto* state_ptr = proc_states[proc_idx];
      auto comb_idx = static_cast<uint32_t>(comb_kernels_.size());
      comb_kernels_.push_back(BuildCombKernel(proc_idx, state_ptr, flags));

      comb_kernel_flags_[proc_idx] = 1;
      bool kernel_self_edge = (flags & CombKernel::kSelfEdge) != 0;
      for (uint32_t ti = 0; ti < kernel.trigger_count; ++ti) {
        const auto& trig = comb.entries[kernel.trigger_start + ti];
        auto decoded = decode_comb_trigger(trig);

        RuntimeInstance* trigger_inst = nullptr;
        uint32_t local_id = 0;
        uint32_t global_id = 0;
        bool is_local = true;

        switch (decoded.mode) {
          case CombTriggerMode::kGlobal:
            is_local = false;
            global_id = decoded.global.value;
            if (global_id >= global_slot_count_) {
              throw common::InternalError(
                  "InitModuleInstancesFromBundles",
                  std::format(
                      "comb global trigger {} >= global_slot_count {}",
                      global_id, global_slot_count_));
            }
            break;

          case CombTriggerMode::kCrossInstance:
            trigger_inst = FindInstanceMut(decoded.instance_id);
            if (trigger_inst == nullptr) {
              throw common::InternalError(
                  "InitModuleInstancesFromBundles",
                  std::format(
                      "comb cross-instance trigger instance_id {} "
                      "not found",
                      decoded.instance_id));
            }
            local_id = decoded.local.value;
            if (local_id >= trigger_inst->observability.local_signal_count) {
              throw common::InternalError(
                  "InitModuleInstancesFromBundles",
                  std::format(
                      "comb cross-instance trigger local_id {} >= "
                      "local_signal_count {} for instance {}",
                      local_id, trigger_inst->observability.local_signal_count,
                      decoded.instance_id));
            }
            break;

          case CombTriggerMode::kBodyLocal:
            trigger_inst = const_cast<RuntimeInstance*>(bundle.instance);
            if (trigger_inst == nullptr) {
              throw common::InternalError(
                  "InitModuleInstancesFromBundles",
                  std::format(
                      "comb body-local trigger has null bundle "
                      "instance for instance_id {}",
                      bundle.instance_id));
            }
            local_id = decoded.local.value;
            if (local_id >= trigger_inst->observability.local_signal_count) {
              throw common::InternalError(
                  "InitModuleInstancesFromBundles",
                  std::format(
                      "comb body-local trigger local_id {} >= "
                      "local_signal_count {} for instance {}",
                      local_id, trigger_inst->observability.local_signal_count,
                      bundle.instance_id));
            }
            break;
        }

        comb_entries.push_back(
            ParsedCombTrigger{
                .is_local = is_local,
                .instance = trigger_inst,
                .local_id = local_id,
                .global_id = global_id,
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

  // Debug dump: print comb metadata snapshot when env var is set.
  // Captures function pointer addresses, trigger counts, and span
  // bounds to aid CI artifact comparison between passing/failing builds.
  // NOLINTBEGIN(cppcoreguidelines-pro-type-reinterpret-cast)
  if (std::getenv("LYRA_DUMP_COMB_META") != nullptr) {
    auto msg = std::format(
        "=== COMB META DUMP ({} kernels, {} triggers) ===\n",
        comb_kernels_.size(), comb_entries.size());
    for (size_t ki = 0; ki < comb_kernels_.size() && ki < 8; ++ki) {
      const auto& ck = comb_kernels_[ki];
      msg += std::format(
          "  kernel[{}]: body={} frame={} proc={} inst_idx={} "
          "flags={:#x}\n",
          ki, reinterpret_cast<const void*>(ck.body), ck.frame,
          ck.process_index, ck.instance_idx, ck.flags);
    }
    for (const auto& bundle : bundles) {
      const auto& comb = bundle.body_desc->comb;
      if (comb.kernels.empty()) continue;
      msg += std::format(
          "  body instance_id={}: {} kernels, {} entries\n", bundle.instance_id,
          comb.kernels.size(), comb.entries.size());
      for (size_t ki = 0; ki < comb.kernels.size() && ki < 4; ++ki) {
        const auto& k = comb.kernels[ki];
        msg += std::format(
            "    desc[{}]: proc_within_body={} trigger=[{},+{}) "
            "self_edge={}\n",
            ki, k.proc_within_body, k.trigger_start, k.trigger_count,
            k.has_self_edge);
      }
    }
    msg += "=== END COMB META DUMP ===\n";
    fputs(msg.c_str(), stderr);
  }
  // NOLINTEND(cppcoreguidelines-pro-type-reinterpret-cast)

  if (!comb_entries.empty()) {
    fp_work_.global_pending_seen.resize(global_slot_count_, 0);
    if (has_any_self_edge_comb_) {
      fp_work_.global_snapshot_index.assign(global_slot_count_, UINT32_MAX);
    }

    // Separate global and local triggers.
    std::vector<ParsedCombTrigger> global_triggers;
    std::vector<ParsedCombTrigger> local_triggers;
    for (auto& e : comb_entries) {
      if (e.is_local) {
        local_triggers.push_back(std::move(e));
      } else {
        global_triggers.push_back(std::move(e));
      }
    }

    // Build global comb trigger map.
    if (!global_triggers.empty()) {
      std::ranges::sort(global_triggers, {}, &ParsedCombTrigger::global_id);
      comb_trigger_backing_.reserve(global_triggers.size());
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
        auto count =
            static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
        if (slot >= global_comb_trigger_map_.size()) {
          throw common::InternalError(
              "InitModuleInstancesFromBundles",
              std::format(
                  "global comb trigger slot {} >= global_comb_trigger_map "
                  "size {} (global_slot_count={})",
                  slot, global_comb_trigger_map_.size(), global_slot_count_));
        }
        global_comb_trigger_map_[slot] = {.start = start, .count = count};
        global_comb_trigger_slots_.push_back(GlobalSignalId{slot});
      }
    }

    // Build per-instance local comb trigger maps.
    if (!local_triggers.empty()) {
      std::ranges::sort(local_triggers, [](const auto& a, const auto& b) {
        if (a.instance != b.instance) return a.instance < b.instance;
        return a.local_id < b.local_id;
      });

      uint32_t i = 0;
      while (i < local_triggers.size()) {
        auto* inst = local_triggers[i].instance;
        auto& obs = inst->observability;
        if (obs.local_comb_trigger_map.empty() && obs.local_signal_count > 0) {
          obs.local_comb_trigger_map.resize(obs.local_signal_count);
        }
        while (i < local_triggers.size() &&
               local_triggers[i].instance == inst) {
          uint32_t local_id = local_triggers[i].local_id;
          auto start = static_cast<uint32_t>(comb_trigger_backing_.size());
          while (i < local_triggers.size() &&
                 local_triggers[i].instance == inst &&
                 local_triggers[i].local_id == local_id) {
            comb_trigger_backing_.push_back({
                .kernel_idx = local_triggers[i].kernel_idx,
                .byte_offset = local_triggers[i].byte_offset,
                .byte_size = local_triggers[i].byte_size,
                .has_self_edge = local_triggers[i].has_self_edge,
            });
            ++i;
          }
          auto count =
              static_cast<uint32_t>(comb_trigger_backing_.size()) - start;
          if (local_id >= obs.local_comb_trigger_map.size()) {
            throw common::InternalError(
                "InitModuleInstancesFromBundles",
                std::format(
                    "local comb trigger id {} >= local_comb_trigger_map "
                    "size {} for instance {}",
                    local_id, obs.local_comb_trigger_map.size(),
                    inst->instance_id));
          }
          obs.local_comb_trigger_map[local_id] = {
              .start = start, .count = count};
          obs.local_comb_trigger_slots.push_back(LocalSignalId{local_id});
          if (local_id < obs.local_has_observers.size()) {
            obs.local_has_observers[local_id] = 1;
          }
        }
      }
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

  // Step F: Register immutable decision metadata tables per owner.
  // First cut: 1:1 mapping from process to decision owner.
  for (const InstanceMetadataBundle& bundle : bundles) {
    const auto& dtables = bundle.body_desc->decision_tables;
    for (uint32_t local = 0; local < dtables.size(); ++local) {
      const auto oid =
          DecisionOwnerId::FromIndex(bundle.module_proc_base + local);
      if (oid.Index() >= decision_owner_tables_.size()) {
        throw common::InternalError(
            "Engine::InitModuleInstancesFromBundles",
            std::format(
                "decision owner {} out of range (tables size {}, "
                "bundle base {} local {})",
                oid.Index(), decision_owner_tables_.size(),
                bundle.module_proc_base, local));
      }
      if (oid.Index() >= decision_owner_states_.size()) {
        throw common::InternalError(
            "Engine::InitModuleInstancesFromBundles",
            std::format(
                "decision owner {} out of range (states size {}, "
                "bundle base {} local {})",
                oid.Index(), decision_owner_states_.size(),
                bundle.module_proc_base, local));
      }
      const auto& desc = dtables[local];
      if (desc.count > 0 && desc.metas == nullptr) {
        throw common::InternalError(
            "Engine::InitModuleInstancesFromBundles",
            std::format(
                "decision table for owner {} has count {} but null metas",
                oid.Index(), desc.count));
      }
      decision_owner_tables_[oid.Index()] = DecisionOwnerTable{
          .count = DecisionSiteCount::FromCount(desc.count),
          .metas = desc.metas,
      };
      if (desc.count > 0) {
        auto& state = decision_owner_states_[oid.Index()];
        state.slots.resize(desc.count);
        state.dirty_seen.resize(desc.count, 0);
      }
    }
  }
}

void Engine::InitInstanceTimeMetadata(
    std::span<const InstanceMetadataBundle> bundles) {
  if (bundles.empty()) {
    instance_time_metadata_.clear();
    return;
  }
  uint32_t max_id = 0;
  for (const auto& b : bundles) {
    max_id = std::max(max_id, b.instance_id.value);
  }
  instance_time_metadata_.assign(
      static_cast<size_t>(max_id) + 1, ScopeTimeMetadata{
                                           .time_unit_power = 0,
                                           .time_precision_power = 0,
                                           .initialized = false,
                                       });
  for (const auto& b : bundles) {
    if (b.body_desc == nullptr) {
      throw common::InternalError(
          "InitInstanceTimeMetadata",
          std::format("instance {} has null body_desc", b.instance_id.value));
    }
    if (b.body_desc->desc == nullptr) {
      throw common::InternalError(
          "InitInstanceTimeMetadata",
          std::format(
              "instance {} has null body realization desc",
              b.instance_id.value));
    }
    auto& slot = instance_time_metadata_.at(b.instance_id.value);
    if (slot.initialized) {
      throw common::InternalError(
          "InitInstanceTimeMetadata",
          std::format("duplicate instance_id {}", b.instance_id.value));
    }
    slot = ScopeTimeMetadata{
        .time_unit_power = b.body_desc->desc->time_unit_power,
        .time_precision_power = b.body_desc->desc->time_precision_power,
        .initialized = true,
    };
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
