#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"

#include <cstddef>
#include <cstdint>

#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_process.hpp"
#include "lyra/lowering/hir_to_mir/lower_type.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/module_unit.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto InstallTypes(const UnitLoweringFacts& facts, UnitLoweringState& state)
    -> void {
  const auto& hir_types = facts.HirUnit().Types();
  for (std::size_t i = 0; i < hir_types.size(); ++i) {
    state.AddType(
        hir::TypeId{static_cast<std::uint32_t>(i)},
        LowerTypeData(hir_types[i].data));
  }
}

auto InstallVarDeclsAsMembers(
    const UnitLoweringFacts& facts, UnitLoweringState& state) -> void {
  const auto& hir_decls = facts.HirUnit().VarDecls();
  for (std::size_t i = 0; i < hir_decls.size(); ++i) {
    const auto& decl = hir_decls[i];
    state.AddMember(
        hir::VarDeclId{static_cast<std::uint32_t>(i)}, decl.name,
        state.TranslateType(decl.type));
  }
}

auto InstallProcesses(const UnitLoweringFacts& facts, UnitLoweringState& state)
    -> void {
  for (const auto& hir_process : facts.HirUnit().Processes()) {
    state.AddProcess(LowerProcess(facts, state, hir_process));
  }
}

}  // namespace

auto LowerModuleUnit(const hir::ModuleUnit& unit) -> mir::ModuleUnit {
  const UnitLoweringFacts facts(unit);
  UnitLoweringState state(unit.Name());

  InstallTypes(facts, state);
  InstallVarDeclsAsMembers(facts, state);
  InstallProcesses(facts, state);

  return state.MoveMirUnit();
}

}  // namespace lyra::lowering::hir_to_mir
