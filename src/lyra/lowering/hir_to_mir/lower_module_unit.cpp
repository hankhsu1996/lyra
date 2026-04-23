#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"

#include <cstddef>

#include "lyra/hir/module_unit.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_process.hpp"
#include "lyra/lowering/hir_to_mir/lower_type.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/module_unit.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerAllTypes(const UnitLoweringFacts& facts, UnitLoweringState& state)
    -> void {
  const auto& hir_types = facts.HirUnit().Types();
  state.type_map.resize(hir_types.size());
  for (std::size_t i = 0; i < hir_types.size(); ++i) {
    state.type_map[i] =
        facts.MirUnit().AddType(LowerTypeData(hir_types[i].data));
  }
}

auto LowerAllVarDeclsToMembers(
    const UnitLoweringFacts& facts, UnitLoweringState& state) -> void {
  const auto& hir_decls = facts.HirUnit().VarDecls();
  state.var_map.resize(hir_decls.size());
  for (std::size_t i = 0; i < hir_decls.size(); ++i) {
    const auto& decl = hir_decls[i];
    state.var_map[i] =
        facts.MirUnit().AddMember(decl.name, state.type_map[decl.type.value]);
  }
}

auto LowerAllProcesses(
    const UnitLoweringFacts& facts, const UnitLoweringState& state) -> void {
  for (const auto& hir_process : facts.HirUnit().Processes()) {
    facts.MirUnit().AddProcess(LowerProcess(state, hir_process));
  }
}

}  // namespace

auto LowerModuleUnit(const hir::ModuleUnit& unit) -> mir::ModuleUnit {
  mir::ModuleUnit mir_unit{unit.Name()};

  const UnitLoweringFacts facts{unit, mir_unit};
  UnitLoweringState state;

  LowerAllTypes(facts, state);
  LowerAllVarDeclsToMembers(facts, state);
  LowerAllProcesses(facts, state);

  return mir_unit;
}

}  // namespace lyra::lowering::hir_to_mir
