#include "lyra/lowering/hir_to_mir/completion_payload.hpp"

#include <cstddef>
#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

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
