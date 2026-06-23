#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"

#include <variant>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Yields the container access chain's base argument when `expr` is a
// container access (per `mir::IsContainerAccessCallee`); null otherwise.
auto AsContainerAccessBase(const mir::Expr& expr) -> const mir::ExprId* {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr) return nullptr;
  if (!mir::IsContainerAccessCallee(call->callee)) return nullptr;
  if (call->arguments.empty()) return nullptr;
  return &call->arguments.front();
}

}  // namespace

auto FindLhsRootId(const mir::Block& block, mir::ExprId lhs_id) -> mir::ExprId {
  while (true) {
    const auto& expr = block.exprs.Get(lhs_id);
    if (const auto* base = AsContainerAccessBase(expr)) {
      lhs_id = *base;
      continue;
    }
    return lhs_id;
  }
}

auto RewriteLhsRootWithMutate(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId services_id) -> mir::ExprId {
  const auto& expr = block.exprs.Get(lhs_id);
  if (AsContainerAccessBase(expr) != nullptr) {
    auto rewritten_call = std::get<mir::CallExpr>(expr.data);
    rewritten_call.arguments.front() = RewriteLhsRootWithMutate(
        unit, block, rewritten_call.arguments.front(), services_id);
    return block.exprs.Add(
        mir::Expr{.data = std::move(rewritten_call), .type = expr.type});
  }
  const mir::TypeId value_type =
      mir::ObservableInnerValueType(unit.GetType(expr.type));
  const mir::ExprId mutate_id = block.exprs.Add(
      mir::MakeObservableMutateCallExpr(lhs_id, services_id, value_type));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::DerefExpr{.pointer = mutate_id}, .type = value_type});
}

auto BuildObservableAssignExpr(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::ExprId services_id, mir::ExprId lhs_id, mir::ExprId rhs_id,
    std::optional<mir::BinaryOp> compound_op, mir::TypeId result_type,
    mir::TypeId void_type) -> mir::Expr {
  const mir::ExprId root_id = FindLhsRootId(block, lhs_id);
  const bool root_is_cell =
      mir::IsObservableCellType(unit.GetType(block.exprs.Get(root_id).type));
  if (!root_is_cell) {
    return mir::Expr{
        .data =
            mir::AssignExpr{
                .target = lhs_id, .compound_op = compound_op, .value = rhs_id},
        .type = result_type};
  }
  const bool whole_var_simple_write =
      (root_id == lhs_id) && !compound_op.has_value();
  if (whole_var_simple_write) {
    return mir::MakeObservableSetCallExpr(
        lhs_id, services_id, rhs_id, void_type);
  }
  const mir::ExprId rewritten =
      RewriteLhsRootWithMutate(unit, block, lhs_id, services_id);
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = rewritten, .compound_op = compound_op, .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
