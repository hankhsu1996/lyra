#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"
#include "lyra/lowering/hir_to_mir/lower_type.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModuleUnit(const hir::ModuleUnit& unit)
    -> diag::Result<mir::CompilationUnit> {
  mir::CompilationUnit out;
  UnitLoweringState unit_state(out);

  for (std::size_t i = 0; i < unit.types.size(); ++i) {
    const hir::TypeId hir_id{static_cast<std::uint32_t>(i)};
    auto mir_data = TranslateTypeData(unit.types[i].data, unit_state);
    const mir::TypeId mir_id = unit_state.AddType(std::move(mir_data));
    unit_state.MapType(hir_id, mir_id);
  }

  ScopeStack stack;
  auto top_r =
      LowerScopeAsClass(unit_state, nullptr, stack, unit.root_scope, unit.name);
  if (!top_r) return std::unexpected(std::move(top_r.error()));

  unit_state.AddClass(*std::move(top_r));
  return out;
}

}  // namespace lyra::lowering::hir_to_mir
