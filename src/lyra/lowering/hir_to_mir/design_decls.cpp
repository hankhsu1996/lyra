#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>

#include "lyra/common/symbol.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Conservative storage-shape classifier for direct unpacked array roots
// in module instances. Scoped to direct roots only -- does not attempt
// aggregate-root or transitive containment classification.
//
// Current approximation: if the module has any promoted parameters AND the
// slot type is a direct unpacked array root, classify as kOwnedContainer.
// This over-classifies literal-dimensioned arrays in parameterized modules
// (e.g., int arr[4] in a module with an unrelated value-only parameter).
// Direct unpacked array roots whose extent varies via promoted parameters
// will classify as kOwnedContainer under this rule. The cost of
// over-classification is 16 bytes of handle overhead per false positive.
//
// param_slots contains value-only/elaboration-time params that are
// transmitted per-instance. Structural-only params create different
// specializations and cannot cause within-spec variation, so modules
// with only structural params correctly return kInlineValue.
auto ClassifySlotStorageShape(
    const hir::Module& mod, TypeId type, const TypeArena& types)
    -> mir::StorageShape {
  if (mod.param_slots.empty()) return mir::StorageShape::kInlineValue;
  if (types[type].Kind() == TypeKind::kUnpackedArray) {
    return mir::StorageShape::kOwnedContainer;
  }
  return mir::StorageShape::kInlineValue;
}

}  // namespace

auto CollectBodyLocalDecls(
    const hir::Module& module, const SymbolTable& symbol_table,
    mir::Arena& mir_arena) -> BodyLocalDecls {
  BodyLocalDecls body_decls;
  uint32_t next_body_slot = 0;

  for (SymbolId var : module.variables) {
    const Symbol& sym = symbol_table[var];
    mir::Place body_place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kModuleSlot,
                .id = static_cast<int>(next_body_slot++),
                .type = sym.type,
            },
        .projections = {},
    };
    body_decls.places[var] = mir_arena.AddPlace(std::move(body_place));
    body_decls.slots.push_back({sym.type, mir::SlotKind::kVariable});
  }

  for (SymbolId net : module.nets) {
    const Symbol& sym = symbol_table[net];
    mir::Place body_place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kModuleSlot,
                .id = static_cast<int>(next_body_slot++),
                .type = sym.type,
            },
        .projections = {},
    };
    body_decls.places[net] = mir_arena.AddPlace(std::move(body_place));
    body_decls.slots.push_back({sym.type, mir::SlotKind::kNet});
  }

  for (SymbolId param : module.param_slots) {
    const Symbol& sym = symbol_table[param];
    mir::Place body_place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kModuleSlot,
                .id = static_cast<int>(next_body_slot++),
                .type = sym.type,
            },
        .projections = {},
    };
    body_decls.places[param] = mir_arena.AddPlace(std::move(body_place));
    body_decls.slots.push_back({sym.type, mir::SlotKind::kParamConst});
  }

  return body_decls;
}

namespace {

// Intern a string into the pool: deduplicates identical strings by returning
// the existing offset if the string was already interned.
class StringPoolIntern {
 public:
  auto Intern(std::vector<char>& pool, const std::string& name) -> uint32_t {
    if (name.empty()) return 0;
    auto [it, inserted] = index_.emplace(name, 0u);
    if (!inserted) return it->second;
    auto off = static_cast<uint32_t>(pool.size());
    pool.insert(pool.end(), name.begin(), name.end());
    pool.push_back('\0');
    it->second = off;
    return off;
  }

 private:
  std::unordered_map<std::string, uint32_t> index_;
};

}  // namespace

auto CollectDesignDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations {
  DesignDeclarations decls;
  int next_slot = 0;

  // Initialize trace string pool with empty string at offset 0.
  decls.slot_trace_string_pool.push_back('\0');

  StringPoolIntern intern;

  // Ordering contract: packages first (in element order), then all module
  // instances (in BFS elaboration order from LowerDesign). This order is ABI -
  // do not change without updating all consumers (LLVM layout, dump).

  // Allocate package variable design places + pre-allocate function IDs
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      const Symbol& pkg_sym = (*input.symbol_table)[pkg->symbol];
      auto pkg_name_off =
          intern.Intern(decls.slot_trace_string_pool, pkg_sym.name);

      for (SymbolId var : pkg->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesignGlobal,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slots.push_back({sym.type, mir::SlotKind::kVariable});
        decls.slot_trace_provenance.push_back(
            {.local_name_str_off =
                 intern.Intern(decls.slot_trace_string_pool, sym.name),
             .scope_kind = mir::SlotScopeKind::kPackage,
             .scope_ref = pkg_name_off});
      }

      // Pre-allocate MIR function IDs with frozen signatures
      for (hir::FunctionId hir_func_id : pkg->functions) {
        const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
        mir::FunctionSignature sig = BuildFunctionSignature(
            hir_func, *input.symbol_table, *input.type_arena);
        mir::FunctionId mir_func_id =
            mir_arena.ReserveFunction(std::move(sig), hir_func.symbol);
        decls.functions[hir_func.symbol] = mir_func_id;
      }
    }
  }

  decls.num_package_slots = static_cast<uint32_t>(next_slot);

  // Allocate module variable and net design-global places.
  // Design-global kDesignGlobal places for connections, layout, and runtime
  // placement. Body-local kModuleSlot places are collected separately by
  // CollectBodyLocalDecls() per specialization group.
  for (const auto& element : design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      auto instance_slot_begin = static_cast<uint32_t>(next_slot);
      auto instance_idx =
          static_cast<uint32_t>(decls.instance_slot_ranges.size());

      for (SymbolId var : mod->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place global_place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesignGlobal,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(global_place));
        decls.slots.push_back(
            mir::SlotDesc{
                .type = sym.type,
                .kind = mir::SlotKind::kVariable,
                .storage_shape =
                    ClassifySlotStorageShape(*mod, sym.type, *input.type_arena),
            });
        decls.slot_trace_provenance.push_back(
            {.local_name_str_off =
                 intern.Intern(decls.slot_trace_string_pool, sym.name),
             .scope_kind = mir::SlotScopeKind::kInstance,
             .scope_ref = instance_idx});
      }

      for (SymbolId net : mod->nets) {
        const Symbol& sym = (*input.symbol_table)[net];
        mir::Place global_place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesignGlobal,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[net] = mir_arena.AddPlace(std::move(global_place));
        decls.slots.push_back(
            mir::SlotDesc{
                .type = sym.type,
                .kind = mir::SlotKind::kNet,
                .storage_shape =
                    ClassifySlotStorageShape(*mod, sym.type, *input.type_arena),
            });
        decls.slot_trace_provenance.push_back(
            {.local_name_str_off =
                 intern.Intern(decls.slot_trace_string_pool, sym.name),
             .scope_kind = mir::SlotScopeKind::kInstance,
             .scope_ref = instance_idx});
      }

      std::vector<mir::ParamInitEntry> param_inits;
      for (size_t pi = 0; pi < mod->param_slots.size(); ++pi) {
        SymbolId param = mod->param_slots[pi];
        const Symbol& sym = (*input.symbol_table)[param];
        auto slot_id = static_cast<uint32_t>(next_slot);
        mir::Place global_place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesignGlobal,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[param] =
            mir_arena.AddPlace(std::move(global_place));
        decls.slots.push_back({sym.type, mir::SlotKind::kParamConst});
        decls.slot_trace_provenance.push_back(
            {.local_name_str_off =
                 intern.Intern(decls.slot_trace_string_pool, sym.name),
             .scope_kind = mir::SlotScopeKind::kInstance,
             .scope_ref = instance_idx});

        if (pi < mod->param_init_values.size()) {
          param_inits.push_back(
              mir::ParamInitEntry{
                  .slot_id = slot_id,
                  .value = mod->param_init_values[pi],
              });
        }
      }

      auto instance_slot_count =
          static_cast<uint32_t>(next_slot) - instance_slot_begin;
      decls.instance_slot_ranges.push_back(
          {instance_slot_begin, instance_slot_count});
      decls.module_def_ids.push_back(mod->module_def_id);
      decls.instance_param_inits.push_back(std::move(param_inits));
    }
  }

  decls.num_design_slots = static_cast<size_t>(next_slot);

  // Build reverse lookup: instance symbol -> instance table index (for %m)
  if (input.instance_table != nullptr) {
    for (uint32_t i = 0; i < input.instance_table->entries.size(); ++i) {
      decls.instance_indices[input.instance_table->entries[i].instance_sym] = i;
    }
  }

  return decls;
}

}  // namespace lyra::lowering::hir_to_mir
