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

auto CollectDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations {
  DesignDeclarations decls;
  int next_slot = 0;

  // Ordering contract: packages first (in element order), then all module
  // instances (in BFS elaboration order from LowerDesign). This order is ABI —
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
        decls.slot_table.push_back(sym.type);
      }

      // Pre-allocate MIR function IDs with frozen signatures
      for (hir::FunctionId hir_func_id : pkg->functions) {
        const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
        mir::FunctionSignature sig =
            BuildFunctionSignature(hir_func, *input.symbol_table);
        mir::FunctionId mir_func_id = mir_arena.ReserveFunction(std::move(sig));
        decls.functions[hir_func.symbol] = mir_func_id;
      }
    }
  }

  // Allocate module variable design places
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
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slot_table.push_back(sym.type);
      }
    }
  }

  decls.num_design_slots = static_cast<size_t>(next_slot);
  return decls;
}

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> mir::Design {
  const DesignDeclarations decls =
      CollectDeclarations(design, input, mir_arena);

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slot_table = decls.slot_table;

  // Lower package init processes
  DeclView init_view{
      .places = &decls.design_places, .functions = &decls.functions};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        const hir::Process& proc = (*input.hir_arena)[pkg->init_process];
        mir::ProcessId mir_proc =
            LowerProcess(proc, input, mir_arena, init_view, origin_map);
        result.init_processes.push_back(mir_proc);
      }
    }
  }

  // Lower design elements (modules and packages)
  for (const auto& element : design.elements) {
    std::visit(
        [&](const auto& e) {
          using T = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<T, hir::Module>) {
            result.elements.emplace_back(
                LowerModule(e, input, mir_arena, origin_map, decls));
          } else if constexpr (std::is_same_v<T, hir::Package>) {
            result.elements.emplace_back(
                LowerPackage(e, input, mir_arena, origin_map, decls));
          } else {
            static_assert(kAlwaysFalse<T>, "unhandled hir::DesignElement");
          }
        },
        element);
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
