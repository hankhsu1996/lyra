#include "lyra/lowering/hir_to_mir/design.hpp"

#include <cstddef>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/common/symbol.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {
template <class>
inline constexpr bool kAlwaysFalse = false;
}  // namespace

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> mir::Design {
  mir::Design result;

  // Build design-level PlaceMap with deterministic slot layout:
  //   [package vars...] [module vars...]
  PlaceMap design_places;
  int next_slot = 0;

  // Design-wide function map: all package functions are visible to all scopes.
  SymbolToMirFunctionMap design_functions;

  // Phase 1: Allocate package variable design places + pre-allocate function
  // IDs
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
        design_places[var] = mir_arena.AddPlace(std::move(place));
      }

      // Pre-allocate MIR function IDs with frozen signatures
      for (hir::FunctionId hir_func_id : pkg->functions) {
        const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
        mir::FunctionSignature sig =
            BuildFunctionSignature(hir_func, *input.symbol_table);
        mir::FunctionId mir_func_id = mir_arena.ReserveFunction(std::move(sig));
        design_functions[hir_func.symbol] = mir_func_id;
      }
    }
  }

  // Phase 2: Lower package init processes (can now reference package functions)
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        const hir::Process& proc = (*input.hir_arena)[pkg->init_process];
        mir::ProcessId mir_proc = LowerProcess(
            proc, input, mir_arena, design_places, design_functions,
            origin_map);
        result.init_processes.push_back(mir_proc);
      }
    }
  }

  // Phase 3: Allocate module variable design places
  for (const auto& element : design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
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
        design_places[var] = mir_arena.AddPlace(std::move(place));
      }
    }
  }

  result.num_design_slots = static_cast<size_t>(next_slot);

  // Phase 4: Lower design elements (modules and packages)
  for (const auto& element : design.elements) {
    std::visit(
        [&](const auto& e) {
          using T = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<T, hir::Module>) {
            result.elements.emplace_back(LowerModule(
                e, input, mir_arena, origin_map, design_places,
                design_functions));
          } else if constexpr (std::is_same_v<T, hir::Package>) {
            result.elements.emplace_back(LowerPackage(
                e, input, mir_arena, origin_map, design_places,
                design_functions));
          } else {
            static_assert(kAlwaysFalse<T>, "unhandled hir::DesignElement");
          }
        },
        element);
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
