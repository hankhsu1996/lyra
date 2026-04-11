#include "lyra/llvm_backend/runtime_data_extraction.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <limits>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/init_descriptor_utils.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/llvm_backend/observable_descriptor_utils.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/llvm_backend/storage_construction_recipe_builder.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Observable descriptor field helpers.

struct ObservableDescriptorOwnerRefFields {
  uint32_t storage_owner_ref = 0;
  uint32_t flags = 0;
};

auto BuildBodyObservableDescriptorOwnerRefFields(
    ObservableOwnerSlotId owner, uint32_t base_slot, uint32_t slot_count)
    -> ObservableDescriptorOwnerRefFields {
  const uint32_t raw_owner = owner.Raw();
  const bool owner_is_in_body =
      raw_owner >= base_slot && raw_owner < base_slot + slot_count;
  if (owner_is_in_body) {
    return {.storage_owner_ref = raw_owner - base_slot, .flags = 0};
  }
  return {
      .storage_owner_ref = raw_owner,
      .flags = runtime::kObservableFlagOwnerAbsolute};
}

auto BuildPackageObservableDescriptorOwnerRefFields(ObservableOwnerSlotId owner)
    -> ObservableDescriptorOwnerRefFields {
  return {
      .storage_owner_ref = owner.Raw(),
      .flags = runtime::kObservableFlagOwnerAbsolute |
               runtime::kObservableFlagPackageGlobal};
}

struct ObservableDescriptorShapeFields {
  uint32_t total_bytes = 0;
  uint32_t storage_kind = 0;
  uint32_t value_lane_offset = 0;
  uint32_t value_lane_bytes = 0;
  uint32_t unk_lane_offset = 0;
  uint32_t unk_lane_bytes = 0;
  uint32_t bit_width = 0;
  uint32_t trace_kind = 0;
};

auto BuildObservableDescriptorShapeFields(const CanonicalObservableShape& shape)
    -> ObservableDescriptorShapeFields {
  ObservableDescriptorShapeFields out{
      .total_bytes = shape.storage.total_bytes,
      .storage_kind = static_cast<uint32_t>(shape.storage.storage_kind),
      .bit_width = shape.trace.bit_width,
      .trace_kind = static_cast<uint32_t>(shape.trace.trace_kind),
  };
  if (const auto& lanes = shape.storage.packed4_lanes; lanes.has_value()) {
    out.value_lane_offset = lanes->value_lane_byte_offset;
    out.value_lane_bytes = lanes->value_lane_byte_size;
    out.unk_lane_offset = lanes->unk_lane_byte_offset;
    out.unk_lane_bytes = lanes->unk_lane_byte_size;
  }
  return out;
}

auto MakeObservableDescriptorEntry(
    uint32_t storage_byte_offset, uint32_t local_name_pool_off,
    const ObservableDescriptorOwnerRefFields& refs,
    const ObservableDescriptorShapeFields& sf, uint32_t storage_domain,
    uint32_t local_signal_id = UINT32_MAX, uint32_t backing_rel_off = 0)
    -> runtime::ObservableDescriptorEntry {
  return runtime::ObservableDescriptorEntry{
      .storage_byte_offset = storage_byte_offset,
      .total_bytes = sf.total_bytes,
      .storage_kind = sf.storage_kind,
      .value_lane_offset = sf.value_lane_offset,
      .value_lane_bytes = sf.value_lane_bytes,
      .unk_lane_offset = sf.unk_lane_offset,
      .unk_lane_bytes = sf.unk_lane_bytes,
      .bit_width = sf.bit_width,
      .local_name_pool_off = local_name_pool_off,
      .trace_kind = sf.trace_kind,
      .storage_owner_ref = refs.storage_owner_ref,
      .flags = refs.flags,
      .storage_domain = storage_domain,
      .local_signal_id = local_signal_id,
      .backing_rel_off = backing_rel_off,
  };
}

// Param slot template types and helpers.

struct ParamSlotTemplateEntry {
  uint32_t body_local_slot;
  runtime::ParamInitSlotEntry slot;
};

struct ParamSlotTemplate {
  std::vector<ParamSlotTemplateEntry> entries;
};

using BodyParamTemplateMap =
    std::unordered_map<mir::ModuleBodyId, ParamSlotTemplate>;

struct SortedParamInitRef {
  uint32_t body_local_slot;
  const mir::ConstSlotInit* init;
};

auto ValidateAndSortParamInits(
    const mir::InstanceConstBlock& const_block, size_t inst_idx)
    -> std::vector<SortedParamInitRef> {
  std::vector<SortedParamInitRef> refs;
  refs.reserve(const_block.slot_inits.size());
  for (const auto& si : const_block.slot_inits) {
    refs.push_back(
        SortedParamInitRef{
            .body_local_slot = si.body_local_slot,
            .init = &si,
        });
  }
  std::ranges::sort(refs, {}, &SortedParamInitRef::body_local_slot);
  for (size_t k = 1; k < refs.size(); ++k) {
    if (refs[k].body_local_slot == refs[k - 1].body_local_slot) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} const block has duplicate param init "
              "for body_local_slot {}",
              inst_idx, refs[k].body_local_slot));
    }
  }
  return refs;
}

void CheckNoStrayInitBefore(
    size_t inst_idx, std::span<const SortedParamInitRef> sorted_inits,
    size_t init_idx, uint32_t body_local_slot_limit) {
  if (init_idx < sorted_inits.size() &&
      sorted_inits[init_idx].body_local_slot < body_local_slot_limit) {
    throw common::InternalError(
        "BuildParamPayloads",
        std::format(
            "instance {} const block has param init for "
            "body_local_slot {} but body template does not "
            "contain that slot",
            inst_idx, sorted_inits[init_idx].body_local_slot));
  }
}

auto BuildParamPayloads(
    std::span<const uint32_t> instance_body_group,
    std::span<const uint32_t> design_base_slots,
    std::span<const mir::InstanceConstBlock> const_blocks, const Layout& layout,
    const BodyParamTemplateMap& body_param_templates)
    -> std::vector<std::vector<uint8_t>> {
  if (instance_body_group.size() != const_blocks.size()) {
    throw common::InternalError(
        "BuildParamPayloads",
        std::format(
            "instance_body_group size {} != const_blocks size {}",
            instance_body_group.size(), const_blocks.size()));
  }
  if (design_base_slots.size() != const_blocks.size()) {
    throw common::InternalError(
        "BuildParamPayloads",
        std::format(
            "design_base_slots size {} != const_blocks size {}",
            design_base_slots.size(), const_blocks.size()));
  }

  std::vector<std::vector<uint8_t>> payloads;
  payloads.resize(const_blocks.size());
  for (size_t inst_idx = 0; inst_idx < const_blocks.size(); ++inst_idx) {
    const auto& const_block = const_blocks[inst_idx];
    auto& payload = payloads[inst_idx];

    if (const_block.slot_inits.empty()) continue;
    uint32_t bg = instance_body_group[inst_idx];
    if (bg >= layout.body_realization_infos.size()) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} body_group {} >= body_realization_infos size {}",
              inst_idx, bg, layout.body_realization_infos.size()));
    }
    auto body_id = layout.body_realization_infos[bg].body_id;
    auto tmpl_it = body_param_templates.find(body_id);
    if (tmpl_it == body_param_templates.end()) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} has param inits but no body param template",
              inst_idx));
    }
    const auto& tmpl = tmpl_it->second.entries;

    auto sorted_inits = ValidateAndSortParamInits(const_block, inst_idx);
    uint32_t design_base_slot = design_base_slots[inst_idx];

    size_t init_idx = 0;
    for (const auto& te : tmpl) {
      CheckNoStrayInitBefore(
          inst_idx, sorted_inits, init_idx, te.body_local_slot);
      if (init_idx < sorted_inits.size() &&
          sorted_inits[init_idx].body_local_slot == te.body_local_slot) {
        const auto& init = *sorted_inits[init_idx].init;
        uint32_t abs_slot = design_base_slot + init.body_local_slot;
        const auto& spec = layout.design.slot_storage_specs[abs_slot];
        LowerIntegralConstantToCanonicalBytes(init.value, spec, payload);
        ++init_idx;
      } else {
        payload.resize(payload.size() + te.slot.byte_size, 0);
      }
    }
    if (init_idx != sorted_inits.size()) {
      throw common::InternalError(
          "BuildParamPayloads",
          std::format(
              "instance {} const block has param init for "
              "body_local_slot {} but body template does not "
              "contain that slot",
              inst_idx, sorted_inits[init_idx].body_local_slot));
    }
  }
  return payloads;
}

auto BuildConstructionProgram(
    std::span<const uint32_t> instance_body_group, const Layout& layout,
    const mir::InstanceTable& instance_table,
    std::span<const std::vector<uint8_t>> param_payloads,
    const std::vector<std::vector<common::ResolvedExtRefBinding>>&
        instance_ext_ref_bindings) -> ConstructionProgramData {
  auto instance_count = static_cast<uint32_t>(instance_body_group.size());
  ConstructionProgramData prog;
  prog.entries.reserve(instance_count);
  prog.ext_ref_binding_offsets.reserve(instance_count);
  prog.ext_ref_binding_counts.reserve(instance_count);

  for (uint32_t mi = 0; mi < instance_count; ++mi) {
    runtime::ConstructionProgramEntry entry{};

    entry.body_group = instance_body_group[mi];

    const auto& path = instance_table.entries[mi].full_path;
    entry.path_offset = static_cast<uint32_t>(prog.path_pool.size());
    prog.path_pool.insert(prog.path_pool.end(), path.begin(), path.end());
    prog.path_pool.push_back(0);

    const auto& payload = param_payloads[mi];
    if (!payload.empty()) {
      entry.param_offset = static_cast<uint32_t>(prog.param_pool.size());
      entry.param_size = static_cast<uint32_t>(payload.size());
      prog.param_pool.insert(
          prog.param_pool.end(), payload.begin(), payload.end());
    }

    const auto& sizes = layout.instance_storage_sizes[mi];
    entry.realized_inline_size = sizes.inline_bytes;
    entry.realized_appendix_size = sizes.appendix_bytes;

    // Pack per-instance ext-ref binding records into flat pool.
    if (mi < instance_ext_ref_bindings.size() &&
        !instance_ext_ref_bindings[mi].empty()) {
      auto offset = static_cast<uint32_t>(prog.ext_ref_binding_pool.size());
      prog.ext_ref_binding_offsets.push_back(offset);
      prog.ext_ref_binding_counts.push_back(
          static_cast<uint32_t>(instance_ext_ref_bindings[mi].size()));
      const auto& bindings = instance_ext_ref_bindings[mi];
      prog.ext_ref_binding_pool.insert(
          prog.ext_ref_binding_pool.end(), bindings.begin(), bindings.end());
    } else {
      prog.ext_ref_binding_offsets.push_back(UINT32_MAX);
      prog.ext_ref_binding_counts.push_back(0);
    }

    prog.entries.push_back(entry);
  }

  return prog;
}

// Comb template extraction helpers.

struct CombSlotAccum {
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  bool is_full_slot = false;
  bool is_design_global = false;
  uint32_t final_global_id = 0;
};

auto CompareCombSlotsByFinalObservableOrder(
    const std::pair<ScopedSignalKey, CombSlotAccum>& a,
    const std::pair<ScopedSignalKey, CombSlotAccum>& b) -> bool {
  if (a.second.final_global_id != b.second.final_global_id) {
    return a.second.final_global_id < b.second.final_global_id;
  }
  if (a.first.scope != b.first.scope) {
    return static_cast<uint8_t>(a.first.scope) <
           static_cast<uint8_t>(b.first.scope);
  }
  return a.first.id < b.first.id;
}

// Sub-extraction functions for each runtime data category.

void ExtractBodyMetadata(
    const LoweringInput& input, const Layout& layout,
    std::vector<BodyRuntimeProducts>& body_products) {
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    const auto& body = *info.body;
    const auto ordinal_map = BuildBodyProcessOrdinalMap(body);
    auto& bp = body_products[gi];
    bp.meta.pool.push_back('\0');

    std::optional<lowering::BodyLocalOriginResolver> body_resolver;
    std::optional<lowering::DiagnosticContext> body_diag_ctx;
    if (input.origin_provenance != nullptr) {
      const auto* prov = input.origin_provenance->Find(info.body);
      if (prov != nullptr && prov->arena != nullptr) {
        body_resolver.emplace(prov->origins, *prov->arena);
        body_diag_ctx.emplace(*body_resolver);
      }
    }
    const lowering::DiagnosticContext* diag =
        body_diag_ctx.has_value() ? &*body_diag_ctx : input.diag_ctx;

    auto add_pool_string = [&](const std::string& s) -> uint32_t {
      if (s.empty()) return 0;
      auto off = static_cast<uint32_t>(bp.meta.pool.size());
      bp.meta.pool.insert(bp.meta.pool.end(), s.begin(), s.end());
      bp.meta.pool.push_back('\0');
      return off;
    };

    auto num_entries =
        static_cast<uint32_t>(info.process_schema_indices.size());
    if (ordinal_map.nonfinal_processes.size() != num_entries) {
      throw common::InternalError(
          "ExtractBodyMetadata",
          std::format(
              "body {} non-final process count {} != schema count {}",
              info.body_id.value, ordinal_map.nonfinal_processes.size(),
              num_entries));
    }
    bp.meta.entries.resize(num_entries);

    ForEachNonFinalProcess(
        body, ordinal_map,
        [&](uint32_t nonfinal_proc_ordinal, mir::ProcessId /*pid*/,
            const mir::Process& proc) {
          auto kind = MapProcessKind(proc.kind);
          auto loc =
              ResolveProcessOrigin(proc.origin, diag, input.source_manager);

          uint32_t file_off = add_pool_string(loc.file);
          bp.meta.entries[nonfinal_proc_ordinal] =
              runtime::ProcessMetaTemplateEntry{
                  .kind_packed = static_cast<uint32_t>(kind),
                  .file_pool_off = file_off,
                  .line = loc.line,
                  .col = loc.col,
              };
        });

    // Decision site metadata.
    {
      uint32_t table_size = body.total_decision_sites;

      std::vector<runtime::DecisionMetaEntry> body_wide_metas(table_size);
      std::vector<std::string> body_wide_files(table_size);
      std::vector<uint8_t> filled(table_size, 0);

      auto place_record = [&](const mir::Process::MirDecisionSiteRecord& rec) {
        auto idx = rec.id.Index();
        if (idx >= table_size) {
          throw common::InternalError(
              "ExtractBodyMetadata",
              std::format(
                  "decision site id {} out of range in body {} "
                  "(allocator total {})",
                  idx, info.body_id.value, table_size));
        }
        if (filled[idx] != 0) {
          throw common::InternalError(
              "ExtractBodyMetadata",
              std::format(
                  "decision site id {} duplicated in body {}", idx,
                  info.body_id.value));
        }
        filled[idx] = 1;
        const auto& site = rec.site;
        auto site_loc =
            ResolveProcessOrigin(site.origin, diag, input.source_manager);
        uint32_t packed =
            static_cast<uint8_t>(site.qualifier) |
            (static_cast<uint8_t>(site.kind) << 8) |
            (static_cast<uint8_t>(site.has_fallback ? 1 : 0) << 16);
        body_wide_metas[idx] = runtime::DecisionMetaEntry{
            .qualifier_kind_packed = packed,
            .arm_count = static_cast<uint32_t>(site.arm_count.raw),
            .line = site_loc.line,
            .col = site_loc.col,
        };
        body_wide_files[idx] = site_loc.file;
      };

      for (mir::FunctionId fid : body.functions) {
        for (const auto& rec : body.arena[fid].decision_sites) {
          place_record(rec);
        }
      }
      ForEachNonFinalProcess(
          body, ordinal_map,
          [&](uint32_t, mir::ProcessId, const mir::Process& proc) {
            for (const auto& rec : proc.decision_sites) {
              place_record(rec);
            }
          });

      for (uint32_t i = 0; i < table_size; ++i) {
        if (filled[i] == 0) {
          throw common::InternalError(
              "ExtractBodyMetadata",
              std::format(
                  "decision site id {} missing in body {} "
                  "(allocator total {})",
                  i, info.body_id.value, table_size));
        }
      }

      bp.decision_metas.resize(num_entries);
      bp.decision_meta_files.resize(num_entries);
      for (uint32_t p = 0; p < num_entries; ++p) {
        bp.decision_metas[p] = body_wide_metas;
        bp.decision_meta_files[p] = body_wide_files;
      }
    }
  }
}

void ExtractConnectionMetadata(
    const LoweringInput& input, const Layout& layout,
    Layout::ConnectionTemplates& conn_templates) {
  // Build module_index -> body pointer mapping.
  std::vector<const mir::ModuleBody*> module_body_ptrs;
  for (const auto& elem : input.design->elements) {
    if (const auto* mod = std::get_if<mir::Module>(&elem)) {
      module_body_ptrs.push_back(mod->body);
    }
  }

  conn_templates.meta.pool.push_back('\0');
  auto add_conn_pool_string = [&](const std::string& s) -> uint32_t {
    if (s.empty()) return 0;
    auto off = static_cast<uint32_t>(conn_templates.meta.pool.size());
    conn_templates.meta.pool.insert(
        conn_templates.meta.pool.end(), s.begin(), s.end());
    conn_templates.meta.pool.push_back('\0');
    return off;
  };

  for (size_t ci = layout.num_init_processes;
       ci < layout.num_module_process_base; ++ci) {
    const auto& sp = layout.scheduled_processes[ci];
    bool is_body_local =
        sp.module_index && sp.module_index.value < module_body_ptrs.size();
    const mir::Arena& proc_arena =
        is_body_local ? module_body_ptrs[sp.module_index.value]->arena
                      : *input.mir_arena;
    const auto& proc = proc_arena[sp.process_id];
    auto kind = MapProcessKind(proc.kind);

    std::optional<lowering::BodyLocalOriginResolver> conn_resolver;
    std::optional<lowering::DiagnosticContext> conn_diag;
    const lowering::DiagnosticContext* conn_diag_ptr = input.diag_ctx;
    if (is_body_local && input.origin_provenance != nullptr) {
      const auto* prov = input.origin_provenance->Find(
          module_body_ptrs[sp.module_index.value]);
      if (prov != nullptr && prov->arena != nullptr) {
        conn_resolver.emplace(prov->origins, *prov->arena);
        conn_diag.emplace(*conn_resolver);
        conn_diag_ptr = &*conn_diag;
      }
    }
    auto loc =
        ResolveProcessOrigin(proc.origin, conn_diag_ptr, input.source_manager);

    uint32_t file_off = add_conn_pool_string(loc.file);
    conn_templates.meta.entries.push_back(
        runtime::ProcessMetaTemplateEntry{
            .kind_packed = static_cast<uint32_t>(kind),
            .file_pool_off = file_off,
            .line = loc.line,
            .col = loc.col,
        });
  }

  if (conn_templates.meta.entries.size() !=
      layout.connection_realization_infos.size()) {
    throw common::InternalError(
        "ExtractConnectionMetadata",
        std::format(
            "connection_templates.meta entries {} != "
            "connection_realization_infos {}",
            conn_templates.meta.entries.size(),
            layout.connection_realization_infos.size()));
  }
}

void ExtractBodyTriggerTemplates(
    const Layout& layout,
    const std::unordered_map<
        const mir::ModuleBody*,
        std::vector<std::optional<ProcessTriggerEntry>>>&
        body_to_process_triggers,
    std::vector<BodyRuntimeProducts>& body_products) {
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    const auto& body = *info.body;
    const auto ordinal_map = BuildBodyProcessOrdinalMap(body);
    auto nonfinal_count =
        static_cast<uint32_t>(ordinal_map.nonfinal_processes.size());

    auto num_procs = static_cast<uint32_t>(info.process_schema_indices.size());
    if (num_procs != nonfinal_count) {
      throw common::InternalError(
          "ExtractBodyTriggerTemplates",
          std::format(
              "body {} schema count {} != non-final process count {}",
              info.body_id.value, num_procs, nonfinal_count));
    }

    auto& bp = body_products[gi];
    bp.triggers.proc_ranges.resize(nonfinal_count);
    bp.triggers.proc_shapes.resize(
        nonfinal_count, static_cast<uint8_t>(runtime::WaitShapeKind::kStatic));
    bp.triggers.proc_groupable.resize(
        nonfinal_count, runtime::kProcNotGroupable);

    auto triggers_it = body_to_process_triggers.find(info.body);
    if (triggers_it == body_to_process_triggers.end()) continue;
    const auto& body_triggers = triggers_it->second;

    if (body_triggers.size() != nonfinal_count) {
      throw common::InternalError(
          "ExtractBodyTriggerTemplates",
          std::format(
              "body {} trigger vector size {} != non-final process "
              "count {}",
              info.body_id.value, body_triggers.size(), nonfinal_count));
    }

    for (uint32_t nonfinal_proc_ordinal = 0;
         nonfinal_proc_ordinal < nonfinal_count; ++nonfinal_proc_ordinal) {
      if (!body_triggers[nonfinal_proc_ordinal].has_value()) {
        bp.triggers.proc_ranges[nonfinal_proc_ordinal] = {
            .start = 0, .count = 0};
        continue;
      }
      const auto& entry = *body_triggers[nonfinal_proc_ordinal];

      auto range_start = static_cast<uint32_t>(bp.triggers.entries.size());
      for (const auto& fact : entry.triggers) {
        uint32_t flags = 0;
        if (fact.has_observed_place) {
          flags |= runtime::kTriggerTemplateFlagHasObservedPlace;
        }
        if (fact.external_ref_index.has_value()) {
          flags |= runtime::kTriggerTemplateFlagExternalRef;
          bp.triggers.entries.push_back(
              runtime::TriggerTemplateEntry{
                  .slot_id = *fact.external_ref_index,
                  .edge = static_cast<uint32_t>(fact.edge),
                  .flags = flags,
              });
        } else if (fact.signal.scope == mir::SignalRef::Scope::kDesignGlobal) {
          flags |= runtime::kTriggerTemplateFlagDesignGlobal;
          bp.triggers.entries.push_back(
              runtime::TriggerTemplateEntry{
                  .slot_id = fact.signal.id,
                  .edge = static_cast<uint32_t>(fact.edge),
                  .flags = flags,
              });
        } else {
          if (fact.signal.id >= info.slot_count) {
            throw common::InternalError(
                "ExtractBodyTriggerTemplates",
                std::format(
                    "body {} proc {} trigger slot_id {} >= slot_count {}",
                    info.body_id.value, nonfinal_proc_ordinal, fact.signal.id,
                    info.slot_count));
          }
          bp.triggers.entries.push_back(
              runtime::TriggerTemplateEntry{
                  .slot_id = fact.signal.id,
                  .edge = static_cast<uint32_t>(fact.edge),
                  .flags = flags,
              });
        }
      }
      auto range_count =
          static_cast<uint32_t>(bp.triggers.entries.size() - range_start);
      bp.triggers.proc_ranges[nonfinal_proc_ordinal] = {
          .start = range_start, .count = range_count};
      bp.triggers.proc_shapes[nonfinal_proc_ordinal] =
          static_cast<uint8_t>(entry.shape);

      auto proc_entries =
          std::span(bp.triggers.entries).subspan(range_start, range_count);
      bp.triggers.proc_groupable[nonfinal_proc_ordinal] =
          IsBodyGroupable(proc_entries, entry.shape)
              ? runtime::kProcGroupable
              : runtime::kProcNotGroupable;
    }
  }
}

void ExtractConnectionTriggerTemplates(
    const Layout& layout, Layout::ConnectionTemplates& conn_templates) {
  auto num_conn =
      static_cast<uint32_t>(layout.connection_realization_infos.size());

  auto& conn_trig = conn_templates.triggers;
  conn_trig.proc_ranges.resize(num_conn);
  conn_trig.proc_shapes.resize(
      num_conn, static_cast<uint8_t>(runtime::WaitShapeKind::kStatic));
  conn_trig.proc_groupable.resize(num_conn, runtime::kProcNotGroupable);

  for (uint32_t ci = 0; ci < num_conn; ++ci) {
    const auto& conn_info = layout.connection_realization_infos[ci];
    if (!conn_info.trigger) {
      conn_trig.proc_ranges[ci] = {.start = 0, .count = 0};
      continue;
    }
    const auto& result = *conn_info.trigger;

    auto range_start = static_cast<uint32_t>(conn_trig.entries.size());
    for (const auto& fact : result.triggers) {
      uint32_t flags = runtime::kTriggerTemplateFlagDesignGlobal;
      if (fact.has_observed_place) {
        flags |= runtime::kTriggerTemplateFlagHasObservedPlace;
      }
      conn_trig.entries.push_back(
          runtime::TriggerTemplateEntry{
              .slot_id = fact.signal.id,
              .edge = static_cast<uint32_t>(fact.edge),
              .flags = flags,
          });
    }
    auto range_count =
        static_cast<uint32_t>(conn_trig.entries.size() - range_start);
    conn_trig.proc_ranges[ci] = {.start = range_start, .count = range_count};
    conn_trig.proc_shapes[ci] = static_cast<uint8_t>(result.shape);

    auto proc_entries =
        std::span(conn_trig.entries).subspan(range_start, range_count);
    conn_trig.proc_groupable[ci] = IsBodyGroupable(proc_entries, result.shape)
                                       ? runtime::kProcGroupable
                                       : runtime::kProcNotGroupable;
  }
}

void ExtractBodyCombTemplates(
    const LoweringInput& input, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    std::vector<BodyRuntimeProducts>& body_products) {
  // Precompute body_id -> any representative base_slot mapping.
  std::unordered_map<uint32_t, uint32_t> body_base_slots;
  for (const auto& plan : module_plans) {
    auto body_id_val =
        static_cast<uint32_t>(plan.body - input.design->module_bodies.data());
    body_base_slots.try_emplace(body_id_val, plan.design_state_base_slot);
  }

  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    const auto& body = *info.body;
    const auto ordinal_map = BuildBodyProcessOrdinalMap(body);

    auto base_it = body_base_slots.find(info.body_id.value);
    uint32_t base_slot =
        (base_it != body_base_slots.end()) ? base_it->second : 0;
    bool found_base = (base_it != body_base_slots.end());

    auto& bp = body_products[gi];

    ForEachNonFinalProcess(
        body, ordinal_map,
        [&](uint32_t nonfinal_proc_ordinal, mir::ProcessId /*pid*/,
            const mir::Process& proc) {
          auto result = AnalyzeCombKernel(proc, body.arena);
          if (!result) return;

          auto trigger_start = static_cast<uint32_t>(bp.comb.entries.size());

          std::unordered_map<
              ScopedSignalKey, CombSlotAccum, ScopedSignalKeyHash>
              per_slot;

          for (const auto& fact : result->triggers) {
            bool is_global =
                fact.signal.scope == mir::SignalRef::Scope::kDesignGlobal;

            uint32_t final_global_id = 0;
            if (is_global) {
              final_global_id = fact.signal.id;
            } else {
              if (!found_base) {
                throw common::InternalError(
                    "ExtractBodyCombTemplates",
                    std::format(
                        "body {} proc {} comb kernel has module-local "
                        "trigger but no instance base_slot found",
                        info.body_id.value, nonfinal_proc_ordinal));
              }
              final_global_id = base_slot + fact.signal.id;
            }

            std::optional<ResolvedObservation> obs;
            if (fact.observed_place) {
              if (is_global) {
                obs = ResolveObservation(
                    body.arena, layout.design, common::SlotId{fact.signal.id},
                    *fact.observed_place);
              } else {
                obs = ResolveObservation(
                    body.arena, layout.design, common::SlotId{final_global_id},
                    *fact.observed_place);
              }
            }

            ScopedSignalKey key = {
                .scope = fact.signal.scope, .id = fact.signal.id};
            auto [it, inserted] = per_slot.try_emplace(key);
            auto& accum = it->second;
            if (inserted) {
              accum.is_design_global = is_global;
              accum.final_global_id = final_global_id;
            } else {
              if (accum.is_design_global != is_global) {
                throw common::InternalError(
                    "ExtractBodyCombTemplates",
                    std::format(
                        "body {} proc {} comb slot ({},{}) scope "
                        "mismatch",
                        info.body_id.value, nonfinal_proc_ordinal,
                        static_cast<int>(key.scope), key.id));
              }
              if (accum.final_global_id != final_global_id) {
                throw common::InternalError(
                    "ExtractBodyCombTemplates",
                    std::format(
                        "body {} proc {} comb slot ({},{}) "
                        "final_global_id mismatch ({} vs {})",
                        info.body_id.value, nonfinal_proc_ordinal,
                        static_cast<int>(key.scope), key.id,
                        accum.final_global_id, final_global_id));
              }
            }
            if (accum.is_full_slot) continue;
            if (!obs) {
              accum.is_full_slot = true;
              accum.byte_offset = 0;
              accum.byte_size = 0;
            } else if (accum.byte_size == 0 && !accum.is_full_slot) {
              accum.byte_offset = obs->byte_offset;
              accum.byte_size = obs->byte_size;
            } else {
              uint32_t existing_end = accum.byte_offset + accum.byte_size;
              uint32_t new_end = obs->byte_offset + obs->byte_size;
              accum.byte_offset = std::min(accum.byte_offset, obs->byte_offset);
              accum.byte_size =
                  std::max(existing_end, new_end) - accum.byte_offset;
            }
          }

          std::vector<std::pair<ScopedSignalKey, CombSlotAccum>> sorted_slots(
              per_slot.begin(), per_slot.end());
          std::ranges::sort(
              sorted_slots, CompareCombSlotsByFinalObservableOrder);

          for (const auto& [key, accum] : sorted_slots) {
            uint32_t flags = 0;
            uint32_t owner_instance_id = 0;
            uint32_t local_signal_id = 0;
            if (accum.is_design_global) {
              if (accum.final_global_id < layout.num_package_slots) {
                flags |= runtime::kCombTemplateFlagDesignGlobal;
              } else {
                flags |= runtime::kCombTemplateFlagCrossInstance;
                auto owner =
                    ResolveInstanceOwnedFlatSlot(layout, accum.final_global_id);
                owner_instance_id = owner.instance_id.value;
                local_signal_id = owner.local_signal_id.value;
              }
            }
            bp.comb.entries.push_back(
                runtime::CombTemplateEntry{
                    .slot_id = key.id,
                    .byte_offset = accum.is_full_slot ? 0 : accum.byte_offset,
                    .byte_size = accum.is_full_slot ? 0 : accum.byte_size,
                    .flags = flags,
                    .owner_instance_id = owner_instance_id,
                    .local_signal_id = local_signal_id,
                });
          }

          auto trigger_count =
              static_cast<uint32_t>(bp.comb.entries.size() - trigger_start);
          bp.comb.kernels.push_back(
              runtime::CombKernelDesc{
                  .proc_within_body = nonfinal_proc_ordinal,
                  .trigger_start = trigger_start,
                  .trigger_count = trigger_count,
                  .has_self_edge = static_cast<uint8_t>(result->has_self_edge),
              });
        });
  }
}

void ExtractBodyObservableDescriptors(
    const LoweringInput& input, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    std::vector<BodyRuntimeProducts>& body_products) {
  std::unordered_map<uint32_t, uint32_t> body_base_slots;
  for (const auto& plan : module_plans) {
    auto body_id_val =
        static_cast<uint32_t>(plan.body - input.design->module_bodies.data());
    body_base_slots.try_emplace(body_id_val, plan.design_state_base_slot);
  }

  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    auto& bp = body_products[gi];

    auto base_it = body_base_slots.find(info.body_id.value);
    if (base_it == body_base_slots.end()) {
      if (info.slot_count == 0) {
        bp.observable_descriptors.pool.assign(1, '\0');
        continue;
      }
      throw common::InternalError(
          "ExtractBodyObservableDescriptors",
          std::format(
              "missing representative base slot for body {} with {} slots",
              info.body_id.value, info.slot_count));
    }
    uint32_t base_slot = base_it->second;

    auto& tmpl = bp.observable_descriptors;
    tmpl.pool.push_back('\0');
    tmpl.entries.reserve(info.slot_count);

    if (info.slot_count == 0) continue;

    auto owned_base =
        layout.design.GetStorageBaseForRange(base_slot, info.slot_count);

    auto narrow_u64_to_u32 = [](uint64_t value,
                                std::string_view what) -> uint32_t {
      if (value > std::numeric_limits<uint32_t>::max()) {
        throw common::InternalError(
            "ExtractBodyObservableDescriptors",
            std::format("{} {} exceeds uint32_t transport limit", what, value));
      }
      return static_cast<uint32_t>(value);
    };

    const auto& obs_body = *info.body;
    for (uint32_t i = 0; i < info.slot_count; ++i) {
      uint32_t gsi = base_slot + i;
      if (gsi >= layout.design.slots.size()) {
        throw common::InternalError(
            "ExtractBodyObservableDescriptors",
            std::format(
                "body {} slot {} (gsi {}) out of range (design slots {})",
                info.body_id.value, i, gsi, layout.design.slots.size()));
      }

      const ObservableOwnerSlotId owner =
          ObservableOwnerSlotId::Create(layout.design.slots[gsi].value);
      const CanonicalObservableShape shape = ComputeCanonicalObservableShape(
          owner, layout.design, obs_body.slots[i].type, obs_body.slots[i].kind,
          *input.type_arena);
      const ObservableDescriptorShapeFields sf =
          BuildObservableDescriptorShapeFields(shape);

      std::string_view local_name;
      uint32_t name_off = 0;
      if (!local_name.empty()) {
        name_off = static_cast<uint32_t>(tmpl.pool.size());
        tmpl.pool.insert(tmpl.pool.end(), local_name.begin(), local_name.end());
        tmpl.pool.push_back('\0');
      }

      auto body_offset = layout.design.GetBodyOffset(gsi, *owned_base);

      ObservableDescriptorOwnerRefFields refs =
          BuildBodyObservableDescriptorOwnerRefFields(
              owner, base_slot, info.slot_count);

      uint32_t domain = 1;
      uint32_t storage_offset =
          narrow_u64_to_u32(body_offset.value, "body-local byte offset");

      uint32_t body_local_signal_id = gsi - base_slot;
      uint32_t backing_off = 0;
      if (layout.design.owned_data_offsets[gsi].has_value()) {
        auto backing_body_off =
            *layout.design.owned_data_offsets[gsi] - owned_base->value;
        backing_off = narrow_u64_to_u32(
            backing_body_off, "container backing body offset");
      }
      tmpl.entries.push_back(MakeObservableDescriptorEntry(
          storage_offset, name_off, refs, sf, domain, body_local_signal_id,
          backing_off));
    }

    // Validate dense population.
    {
      std::vector<uint8_t> seen(info.slot_count, 0);
      for (const auto& entry : tmpl.entries) {
        if (entry.storage_domain != 1) continue;
        if (entry.local_signal_id >= info.slot_count ||
            seen[entry.local_signal_id]++ != 0) {
          throw common::InternalError(
              "ExtractBodyObservableDescriptors",
              std::format(
                  "body {}: invalid or duplicate local_signal_id {} "
                  "(slot_count {})",
                  info.body_id.value, entry.local_signal_id, info.slot_count));
        }
      }
    }
  }
}

void ExtractBodyInitDescriptors(
    const LoweringInput& input, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    std::span<const uint32_t> instance_body_group,
    std::vector<BodyRuntimeProducts>& body_products,
    ConstructionProgramData& construction_program) {
  std::unordered_map<uint32_t, uint32_t> body_base_slots;
  for (const auto& plan : module_plans) {
    auto body_id_val =
        static_cast<uint32_t>(plan.body - input.design->module_bodies.data());
    body_base_slots.try_emplace(body_id_val, plan.design_state_base_slot);
  }

  BodyParamTemplateMap body_param_templates;

  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    const auto& info = layout.body_realization_infos[gi];
    auto& bp = body_products[gi];

    if (info.slot_count == 0) continue;
    auto base_it = body_base_slots.find(info.body_id.value);
    if (base_it == body_base_slots.end()) {
      throw common::InternalError(
          "ExtractBodyInitDescriptors",
          std::format(
              "missing base slot for body {} in init descriptor extraction",
              info.body_id.value));
    }
    uint32_t base_slot = base_it->second;
    auto init_owned_base =
        layout.design.GetStorageBaseForRange(base_slot, info.slot_count);
    if (!init_owned_base.has_value()) continue;

    const auto& init_body = *info.body;
    std::vector<ParamSlotTemplateEntry> param_entries;

    for (uint32_t i = 0; i < info.slot_count; ++i) {
      uint32_t gsi = base_slot + i;

      uint64_t inst_rel_offset =
          layout.design.slot_byte_offsets[gsi] - init_owned_base->value;
      const auto& spec = layout.design.slot_storage_specs[gsi];

      if (init_body.slots[i].kind == mir::SlotKind::kParamConst) {
        param_entries.push_back(
            ParamSlotTemplateEntry{
                .body_local_slot = i,
                .slot =
                    runtime::ParamInitSlotEntry{
                        .rel_byte_offset = NarrowToU32(
                            inst_rel_offset, "init param rel offset"),
                        .byte_size = spec.TotalByteSize(),
                    },
            });
      }

      bool is_owned = layout.design.owned_data_offsets[gsi].has_value();
      if (is_owned) {
        uint64_t backing_rel =
            *layout.design.owned_data_offsets[gsi] - init_owned_base->value;
        if (auto root = BuildStorageConstructionRecipeForSlot(
                spec, layout.design.storage_spec_arena,
                NarrowToU32(inst_rel_offset, "init rel offset"), true,
                NarrowToU32(inst_rel_offset, "init handle rel offset"),
                NarrowToU32(backing_rel, "init backing rel offset"),
                bp.init.storage_recipe, bp.init.recipe_child_indices)) {
          bp.init.recipe_root_indices.push_back(*root);
        }
      } else {
        if (auto root = BuildStorageConstructionRecipeForSlot(
                spec, layout.design.storage_spec_arena,
                NarrowToU32(inst_rel_offset, "init rel offset"), false, 0, 0,
                bp.init.storage_recipe, bp.init.recipe_child_indices)) {
          bp.init.recipe_root_indices.push_back(*root);
        }
      }
    }

    std::ranges::sort(
        param_entries, {}, &ParamSlotTemplateEntry::body_local_slot);
    bp.init.param_slots.reserve(param_entries.size());
    for (const auto& pe : param_entries) {
      bp.init.param_slots.push_back(pe.slot);
    }
    body_param_templates[info.body_id] =
        ParamSlotTemplate{.entries = std::move(param_entries)};
  }

  // Build param payloads and construction program.
  std::vector<uint32_t> design_base_slots;
  design_base_slots.reserve(input.construction->objects.size());
  for (const auto& obj : input.construction->objects) {
    design_base_slots.push_back(obj.design_state_base_slot);
  }

  auto param_payloads = BuildParamPayloads(
      instance_body_group, design_base_slots, input.construction->const_blocks,
      layout, body_param_templates);

  // Validate parallel structure invariants.
  {
    auto instance_count = instance_body_group.size();
    if (instance_count != input.construction->instance_table.entries.size()) {
      throw common::InternalError(
          "ExtractBodyInitDescriptors",
          std::format(
              "instance_body_group size {} != instance_table size {}",
              instance_count,
              input.construction->instance_table.entries.size()));
    }
    if (instance_count != layout.instance_storage_bases.size()) {
      throw common::InternalError(
          "ExtractBodyInitDescriptors",
          std::format(
              "instance_body_group size {} != instance_storage_bases size {}",
              instance_count, layout.instance_storage_bases.size()));
    }
    if (instance_count != param_payloads.size()) {
      throw common::InternalError(
          "ExtractBodyInitDescriptors",
          std::format(
              "instance_body_group size {} != param_payloads size {}",
              instance_count, param_payloads.size()));
    }
    auto num_body_groups = body_products.size();
    if (num_body_groups != layout.body_realization_infos.size()) {
      throw common::InternalError(
          "ExtractBodyInitDescriptors",
          std::format(
              "body_products size {} != body_realization_infos size {}",
              num_body_groups, layout.body_realization_infos.size()));
    }
    for (size_t mi = 0; mi < instance_count; ++mi) {
      if (instance_body_group[mi] >= num_body_groups) {
        throw common::InternalError(
            "ExtractBodyInitDescriptors",
            std::format(
                "instance {} body_group {} >= num_body_groups {}", mi,
                instance_body_group[mi], num_body_groups));
      }
    }
  }

  construction_program = BuildConstructionProgram(
      instance_body_group, layout, input.construction->instance_table,
      param_payloads, input.construction->instance_ext_ref_bindings);

  // Validate construction program.
  for (size_t i = 0; i < construction_program.entries.size(); ++i) {
    const auto& e = construction_program.entries[i];
    if (e.body_group >= layout.body_realization_infos.size()) {
      throw common::InternalError(
          "ExtractBodyInitDescriptors",
          std::format(
              "construction entry {} body_group {} >= "
              "body_realization_infos size {}",
              i, e.body_group, layout.body_realization_infos.size()));
    }
  }
}

void ExtractPackageInitDescriptor(
    const Layout& layout, PackageInitDescriptor& pkg_init) {
  uint32_t num_pkg = layout.num_package_slots;
  const auto& spec_arena = layout.design.storage_spec_arena;

  for (uint32_t gsi = 0; gsi < num_pkg; ++gsi) {
    if (gsi >= layout.design.slots.size()) break;
    const auto& spec = layout.design.slot_storage_specs[gsi];
    uint64_t abs_offset = layout.design.slot_byte_offsets[gsi];

    bool is_owned = layout.design.owned_data_offsets[gsi].has_value();
    if (is_owned) {
      uint64_t backing_abs = *layout.design.owned_data_offsets[gsi];
      if (auto root = BuildStorageConstructionRecipeForSlot(
              spec, spec_arena, NarrowToU32(abs_offset, "pkg init rel offset"),
              true, NarrowToU32(abs_offset, "pkg init handle offset"),
              NarrowToU32(backing_abs, "pkg init backing offset"),
              pkg_init.storage_recipe, pkg_init.recipe_child_indices)) {
        pkg_init.recipe_root_indices.push_back(*root);
      }
    } else {
      if (auto root = BuildStorageConstructionRecipeForSlot(
              spec, spec_arena, NarrowToU32(abs_offset, "pkg init rel offset"),
              false, 0, 0, pkg_init.storage_recipe,
              pkg_init.recipe_child_indices)) {
        pkg_init.recipe_root_indices.push_back(*root);
      }
    }
  }
}

void ExtractPackageObservableDescriptors(
    const LoweringInput& input, const Layout& layout,
    OwnedObservableDescriptorTemplate& pkg) {
  pkg.pool.push_back('\0');
  uint32_t num_pkg = layout.num_package_slots;
  pkg.entries.reserve(num_pkg);

  auto read_pkg_trace_pool = [&](uint32_t offset) -> std::string_view {
    const auto& pool = input.design->slot_trace_string_pool;
    if (offset >= pool.size()) {
      throw common::InternalError(
          "ExtractPackageObservableDescriptors",
          std::format(
              "trace pool offset {} out of range (pool size {})", offset,
              pool.size()));
    }
    return {&pool[offset]};
  };

  auto append_to_pool = [&](std::string_view name) -> uint32_t {
    if (name.empty()) return 0;
    auto off = static_cast<uint32_t>(pkg.pool.size());
    pkg.pool.insert(pkg.pool.end(), name.begin(), name.end());
    pkg.pool.push_back('\0');
    return off;
  };

  auto narrow_u64_to_u32 = [](uint64_t value,
                              std::string_view what) -> uint32_t {
    if (value > std::numeric_limits<uint32_t>::max()) {
      throw common::InternalError(
          "ExtractPackageObservableDescriptors",
          std::format("{} {} exceeds uint32_t transport limit", what, value));
    }
    return static_cast<uint32_t>(value);
  };

  for (uint32_t gsi = 0; gsi < num_pkg; ++gsi) {
    if (gsi >= layout.design.slots.size()) break;

    const auto& pkg_slot = input.design->slots[gsi];
    const ObservableOwnerSlotId owner =
        ObservableOwnerSlotId::Create(layout.design.slots[gsi].value);
    const CanonicalObservableShape shape = ComputeCanonicalObservableShape(
        owner, layout.design, pkg_slot.type, pkg_slot.kind, *input.type_arena);
    const ObservableDescriptorShapeFields sf =
        BuildObservableDescriptorShapeFields(shape);

    const auto& prov = input.design->slot_trace_provenance[gsi];
    auto local_name = read_pkg_trace_pool(prov.local_name_str_off);
    std::string qualified_name;
    if (prov.scope_kind == mir::SlotScopeKind::kPackage) {
      auto pkg_name = read_pkg_trace_pool(prov.scope_ref);
      qualified_name = std::format("{}.{}", pkg_name, local_name);
    } else {
      qualified_name = std::string(local_name);
    }
    uint32_t name_off = append_to_pool(qualified_name);

    uint32_t storage_offset = narrow_u64_to_u32(
        layout.design.slot_byte_offsets[gsi], "package/global byte offset");
    const ObservableDescriptorOwnerRefFields refs =
        BuildPackageObservableDescriptorOwnerRefFields(owner);

    pkg.entries.push_back(
        MakeObservableDescriptorEntry(storage_offset, name_off, refs, sf, 0));
  }
}

}  // namespace

auto ExtractRuntimeData(
    const LoweringInput& input, const Layout& layout,
    std::span<const LayoutModulePlan> module_plans,
    const std::unordered_map<
        const mir::ModuleBody*,
        std::vector<std::optional<ProcessTriggerEntry>>>&
        body_to_process_triggers,
    std::span<const uint32_t> instance_body_group)
    -> RuntimeExtractionProducts {
  RuntimeExtractionProducts products;
  products.body_products.resize(layout.body_realization_infos.size());

  ExtractBodyMetadata(input, layout, products.body_products);
  ExtractConnectionMetadata(input, layout, products.connection_templates);
  ExtractBodyTriggerTemplates(
      layout, body_to_process_triggers, products.body_products);
  ExtractConnectionTriggerTemplates(layout, products.connection_templates);
  ExtractBodyCombTemplates(input, layout, module_plans, products.body_products);
  ExtractBodyObservableDescriptors(
      input, layout, module_plans, products.body_products);
  ExtractBodyInitDescriptors(
      input, layout, module_plans, instance_body_group, products.body_products,
      products.construction_program);
  ExtractPackageInitDescriptor(layout, products.package_init_descriptor);
  ExtractPackageObservableDescriptors(
      input, layout, products.package_observable_descriptors);

  return products;
}

void ApplyRuntimeDataToLayout(
    RuntimeExtractionProducts products, Layout& layout,
    RealizationData& realization) {
  // Body runtime data.
  for (size_t gi = 0; gi < products.body_products.size(); ++gi) {
    auto& bp = products.body_products[gi];
    auto& info = layout.body_realization_infos[gi];
    info.meta = std::move(bp.meta);
    info.triggers = std::move(bp.triggers);
    info.comb = std::move(bp.comb);
    info.observable_descriptors = std::move(bp.observable_descriptors);
    info.init = std::move(bp.init);
    info.decision_metas = std::move(bp.decision_metas);
    info.decision_meta_files = std::move(bp.decision_meta_files);
  }

  // Connection templates.
  layout.connection_templates = std::move(products.connection_templates);

  // Package runtime data.
  layout.package_observable_descriptors =
      std::move(products.package_observable_descriptors);
  layout.package_init_descriptor = std::move(products.package_init_descriptor);

  // Construction program.
  realization.construction_program = std::move(products.construction_program);
}

}  // namespace lyra::lowering::mir_to_llvm
