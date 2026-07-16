#include "lyra/lowering/hir_to_mir/completion_payload.hpp"

#include <cstddef>
#include <vector>

#include "lyra/hir/subroutine.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto CompletionComponentTypes(
    const UnitLowerer& unit_lowerer, const hir::SubroutineDecl& decl)
    -> std::vector<mir::TypeId> {
  std::vector<mir::TypeId> components;
  if (decl.result_var.has_value()) {
    components.push_back(unit_lowerer.TranslateType(decl.result_type));
  }
  for (const auto& param : decl.params) {
    if (param.direction == hir::ParamDirection::kOutput ||
        param.direction == hir::ParamDirection::kInOut) {
      components.push_back(unit_lowerer.TranslateType(
          decl.body.procedural_vars.Get(param.var).type));
    }
  }
  return components;
}

auto NormalizeCompletionPayload(
    mir::CompilationUnit& unit, const std::vector<mir::TypeId>& components)
    -> mir::TypeId {
  if (components.empty()) return unit.builtins.void_type;
  if (components.size() == 1) return components.front();
  return unit.types.Intern(mir::TupleType{.elements = components});
}

auto ProjectCompletionComponent(
    mir::Block& block, mir::LocalId completion, mir::TypeId payload_type,
    std::size_t component_count, std::size_t index, mir::TypeId component_type)
    -> mir::ExprId {
  if (component_count == 1) {
    return block.exprs.Add(mir::MakeLocalRefExpr(completion, component_type));
  }
  const mir::ExprId tuple_ref =
      block.exprs.Add(mir::MakeLocalRefExpr(completion, payload_type));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::TupleGetExpr{.tuple = tuple_ref, .index = index},
          .type = component_type});
}

}  // namespace lyra::lowering::hir_to_mir
