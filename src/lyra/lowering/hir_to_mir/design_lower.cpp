#include <type_traits>
#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {
template <class>
inline constexpr bool kAlwaysFalse = false;
}  // namespace

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> Result<mir::Design> {
  const DesignDeclarations decls =
      CollectDeclarations(design, input, mir_arena);

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slot_table = decls.slot_table;
  result.global_precision_power = input.global_precision_power;

  // Lower package init processes
  // Collect dynamically generated functions (e.g., strobe thunks)
  DeclView init_view{
      .places = &decls.design_places, .functions = &decls.functions};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        hir::ProcessId hir_proc_id = pkg->init_process;
        const hir::Process& proc = (*input.hir_arena)[hir_proc_id];
        mir::ProcessId mir_proc = LowerProcess(
            hir_proc_id, proc, input, mir_arena, init_view, origin_map,
            &result.generated_functions);
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

  // Apply port drive bindings (creates synthetic always_comb processes)
  if (input.binding_plan != nullptr) {
    auto binding_result =
        ApplyBindings(*input.binding_plan, decls, input, mir_arena, result);
    if (!binding_result) return std::unexpected(binding_result.error());
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
