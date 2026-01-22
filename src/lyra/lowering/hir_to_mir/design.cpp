#include "lyra/lowering/hir_to_mir/design.hpp"

#include <cstddef>
#include <type_traits>
#include <variant>

#include "lyra/common/symbol.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/mir/place.hpp"

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

  // Package variables first
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

      // Lower package init process
      if (pkg->init_process) {
        const hir::Process& proc = (*input.hir_arena)[pkg->init_process];
        // Packages currently have no functions. If this changes,
        // build a real SymbolToMirFunctionMap for package scope.
        SymbolToMirFunctionMap empty_func_map;
        mir::ProcessId mir_proc = LowerProcess(
            proc, input, mir_arena, design_places, empty_func_map, origin_map);
        result.init_processes.push_back(mir_proc);
      }
    }
  }

  // Module variables next
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

  // Lower design elements
  for (const auto& element : design.elements) {
    std::visit(
        [&](const auto& e) {
          using T = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<T, hir::Module>) {
            result.elements.emplace_back(
                LowerModule(e, input, mir_arena, origin_map, design_places));
          } else if constexpr (std::is_same_v<T, hir::Package>) {
            result.elements.emplace_back(
                LowerPackage(e, input, mir_arena, origin_map));
          } else {
            static_assert(kAlwaysFalse<T>, "unhandled hir::DesignElement");
          }
        },
        element);
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
