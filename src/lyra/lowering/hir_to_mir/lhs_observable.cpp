#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"

#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto FindLhsRootId(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId lhs_id) -> mir::ExprId {
  while (true) {
    const auto& expr = block.exprs.Get(lhs_id);
    // A captured carrier -- a closure-record field holding a `Ref` (or other
    // observable cell) -- is itself the root cell, reached by a field access
    // over the closure receiver. Stop here rather than projecting through it as
    // if it were a struct member of an observable aggregate.
    if (mir::IsObservableCellType(unit.types.Get(expr.type))) {
      return lhs_id;
    }
    // A designated part of a value states its owner, so the root is reached
    // without walking a descent chain.
    if (const auto* projection =
            std::get_if<mir::ValueProjectionExpr>(&expr.data)) {
      lhs_id = projection->owner;
      continue;
    }
    if (const auto* m = std::get_if<mir::TaggedGetRefExpr>(&expr.data)) {
      lhs_id = m->union_value;
      continue;
    }
    return lhs_id;
  }
}

auto RewriteLhsRootWithMutate(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId runtime_id) -> mir::ExprId {
  const auto& expr = block.exprs.Get(lhs_id);
  // A captured carrier (cell-typed) is the chain's leaf root: mutate it
  // directly rather than projecting through it as a struct member.
  if (mir::IsObservableCellType(unit.types.Get(expr.type))) {
    const mir::TypeId value_type =
        mir::ObservableInnerValueType(unit.types.Get(expr.type));
    const mir::ExprId mutate_id = block.exprs.Add(
        mir::MakeObservableMutateCallExpr(lhs_id, runtime_id, value_type));
    return block.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = mutate_id}, .type = value_type});
  }
  if (const auto* projection =
          std::get_if<mir::ValueProjectionExpr>(&expr.data)) {
    mir::ValueProjectionExpr rewritten = *projection;
    const mir::TypeId result_ty = expr.type;
    rewritten.owner =
        RewriteLhsRootWithMutate(unit, block, rewritten.owner, runtime_id);
    return block.exprs.Add(
        mir::Expr{.data = std::move(rewritten), .type = result_ty});
  }
  if (const auto* m = std::get_if<mir::TaggedGetRefExpr>(&expr.data)) {
    mir::TaggedGetRefExpr rewritten = *m;
    const mir::TypeId result_ty = expr.type;
    rewritten.union_value = RewriteLhsRootWithMutate(
        unit, block, rewritten.union_value, runtime_id);
    return block.exprs.Add(mir::Expr{.data = rewritten, .type = result_ty});
  }
  throw InternalError(
      "RewriteLhsRootWithMutate: LHS root is neither an observable cell nor a "
      "projection over one");
}

auto LhsRootIsObservableCell(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId lhs_id) -> bool {
  const mir::ExprId root_id = FindLhsRootId(unit, block, lhs_id);
  return mir::IsObservableCellType(
      unit.types.Get(block.exprs.Get(root_id).type));
}

auto BuildObservableAssignExpr(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::optional<mir::ExprId> runtime_id, mir::ExprId lhs_id,
    mir::ExprId rhs_id, std::optional<mir::BinaryOp> compound_op,
    mir::TypeId result_type, mir::TypeId void_type) -> mir::Expr {
  // A plain store carries the right-hand side to the destination's full
  // declared representation before it reaches the cell (LRM 10.6.1), for every
  // value family. The front end already converts width, signedness, and state
  // domain; the dimension stack (and, for containers, the element
  // representation and bound) is the axis it leaves to assignment, so the value
  // coerces here to the destination's declared type. A compound store computes
  // its stored value through the operator, which already yields the destination
  // shape.
  if (!compound_op.has_value()) {
    const mir::Type& lhs_ty = unit.types.Get(block.exprs.Get(lhs_id).type);
    const mir::TypeId dst_value_type =
        mir::IsObservableCellType(lhs_ty)
            ? mir::ObservableInnerValueType(lhs_ty)
            : block.exprs.Get(lhs_id).type;
    rhs_id = ConvertToType(unit, block, rhs_id, dst_value_type);
  }
  const mir::ExprId root_id = FindLhsRootId(unit, block, lhs_id);
  if (!LhsRootIsObservableCell(unit, block, lhs_id)) {
    return mir::Expr{
        .data =
            mir::AssignExpr{
                .target = lhs_id, .compound_op = compound_op, .value = rhs_id},
        .type = result_type};
  }
  // The write is observable, so it notifies through a runtime handle the caller
  // must have supplied.
  if (!runtime_id.has_value()) {
    throw InternalError(
        "BuildObservableAssignExpr: an observable write requires a runtime "
        "value");
  }
  const bool whole_var_simple_write =
      (root_id == lhs_id) && !compound_op.has_value();
  if (whole_var_simple_write) {
    return mir::MakeObservableSetCallExpr(
        lhs_id, *runtime_id, rhs_id, void_type);
  }
  const mir::ExprId rewritten =
      RewriteLhsRootWithMutate(unit, block, lhs_id, *runtime_id);
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = rewritten, .compound_op = compound_op, .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
