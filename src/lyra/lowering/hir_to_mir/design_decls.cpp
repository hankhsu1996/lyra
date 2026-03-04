#include <cstddef>
#include <cstdint>
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

auto CollectDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations {
  DesignDeclarations decls;
  int next_slot = 0;

  // Ordering contract: packages first (in element order), then all module
  // instances (in BFS elaboration order from LowerDesign). This order is ABI -
  // do not change without updating all consumers (LLVM layout, MIR interpreter,
  // dump).

  // Allocate package variable design places + pre-allocate function IDs
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      for (SymbolId var : pkg->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesign,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slots.push_back({sym.type, mir::SlotKind::kVariable});
      }

      // Pre-allocate MIR function IDs with frozen signatures
      for (hir::FunctionId hir_func_id : pkg->functions) {
        const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
        mir::FunctionSignature sig = BuildFunctionSignature(
            hir_func, *input.symbol_table, *input.type_arena);
        mir::FunctionId mir_func_id = mir_arena.ReserveFunction(std::move(sig));
        decls.functions[hir_func.symbol] = mir_func_id;
      }
    }
  }

  // Allocate module variable and net design places
  for (const auto& element : design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      auto instance_slot_begin = static_cast<uint32_t>(next_slot);

      for (SymbolId var : mod->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesign,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slots.push_back({sym.type, mir::SlotKind::kVariable});
      }

      for (SymbolId net : mod->nets) {
        const Symbol& sym = (*input.symbol_table)[net];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesign,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[net] = mir_arena.AddPlace(std::move(place));
        decls.slots.push_back({sym.type, mir::SlotKind::kNet});
      }

      std::vector<mir::ParamInitEntry> param_inits;
      for (size_t pi = 0; pi < mod->param_slots.size(); ++pi) {
        SymbolId param = mod->param_slots[pi];
        const Symbol& sym = (*input.symbol_table)[param];
        auto slot_id = static_cast<uint32_t>(next_slot);
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesign,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[param] = mir_arena.AddPlace(std::move(place));
        decls.slots.push_back({sym.type, mir::SlotKind::kParamConst});

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
      decls.module_def_keys.push_back(mod->module_def_key);
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
