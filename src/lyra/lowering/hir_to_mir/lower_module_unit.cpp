#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>

#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"
#include "lyra/lowering/hir_to_mir/lower_process.hpp"
#include "lyra/lowering/hir_to_mir/lower_type.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModuleUnit(const hir::ModuleUnit& unit) -> mir::CompilationUnit {
  const UnitLoweringFacts unit_facts(unit, unit.RootScope());
  UnitLoweringState unit_state;

  mir::ClassDecl cls(unit.Name());

  for (std::size_t i = 0; i < unit.Types().size(); ++i) {
    const hir::TypeId hir_id{static_cast<std::uint32_t>(i)};
    const mir::TypeId mir_id = cls.AddType(LowerTypeData(unit.Types()[i].data));
    unit_state.SetType(hir_id, mir_id);
  }

  const auto& root = unit_facts.RootScope();
  for (std::size_t i = 0; i < root.VarDecls().size(); ++i) {
    const hir::VarDeclId hir_id{static_cast<std::uint32_t>(i)};
    const auto& d = root.VarDecls()[i];
    const mir::MemberId mir_id =
        cls.AddMember(d.name, unit_state.TranslateType(d.type));
    unit_state.SetRootVar(hir_id, mir_id);
  }

  for (const auto& p : root.Processes()) {
    cls.AddProcess(LowerProcess(unit_facts, unit_state, root, p));
  }

  LowerConstructorFromScope(unit_facts, unit_state, root, cls.Constructor());

  mir::CompilationUnit out;
  out.AddClass(std::move(cls));
  return out;
}

}  // namespace lyra::lowering::hir_to_mir
