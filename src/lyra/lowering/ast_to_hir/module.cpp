#include "module.hpp"

#include <expected>
#include <string>
#include <utility>

#include <slang/ast/symbols/InstanceSymbols.h>

#include "facts.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(
    const UnitLoweringFacts& unit_facts,
    const slang::ast::InstanceBodySymbol& body)
    -> diag::Result<hir::ModuleUnit> {
  UnitLoweringState unit_state{std::string{body.name}};
  ScopeStack stack;
  auto r = LowerScopeInto(
      unit_facts, unit_state, unit_state.HirUnit().root_scope, body, stack);
  if (!r) return std::unexpected(std::move(r.error()));
  return unit_state.MoveHirUnit();
}

}  // namespace lyra::lowering::ast_to_hir
