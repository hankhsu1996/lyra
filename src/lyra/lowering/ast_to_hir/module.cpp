#include "module.hpp"

#include <string>

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/hir/module_unit.hpp"
#include "scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(const slang::ast::InstanceBodySymbol& body)
    -> hir::ModuleUnit {
  UnitLoweringState unit_state{std::string{body.name}};
  ScopeStack stack;
  LowerScope(unit_state, unit_state.HirUnit().RootScope(), body, stack);
  return unit_state.MoveHirUnit();
}

}  // namespace lyra::lowering::ast_to_hir
