#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"

#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
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
    // A struct member projects through a tuple component and a union member
    // through its union access; the observable root is the base, reached the
    // same way a container access reaches its receiver.
    if (const auto* g = std::get_if<mir::TupleGetExpr>(&expr.data)) {
      lhs_id = g->tuple;
      continue;
    }
    if (const auto* m = std::get_if<mir::UnionGetRefExpr>(&expr.data)) {
      lhs_id = m->union_value;
      continue;
    }
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
  // A captured carrier (cell-typed) is the chain's leaf root: mutate it
  // directly rather than projecting through it as a struct member.
  if (mir::IsObservableCellType(unit.types.Get(expr.type))) {
    const mir::TypeId value_type =
        mir::ObservableInnerValueType(unit.types.Get(expr.type));
    const mir::ExprId mutate_id = block.exprs.Add(
        mir::MakeObservableMutateCallExpr(lhs_id, services_id, value_type));
    return block.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = mutate_id}, .type = value_type});
  }
  if (const auto* g = std::get_if<mir::TupleGetExpr>(&expr.data)) {
    mir::TupleGetExpr rewritten = *g;
    const mir::TypeId result_ty = expr.type;
    rewritten.tuple =
        RewriteLhsRootWithMutate(unit, block, rewritten.tuple, services_id);
    return block.exprs.Add(mir::Expr{.data = rewritten, .type = result_ty});
  }
  if (const auto* m = std::get_if<mir::UnionGetRefExpr>(&expr.data)) {
    mir::UnionGetRefExpr rewritten = *m;
    const mir::TypeId result_ty = expr.type;
    rewritten.union_value = RewriteLhsRootWithMutate(
        unit, block, rewritten.union_value, services_id);
    return block.exprs.Add(mir::Expr{.data = rewritten, .type = result_ty});
  }
  if (AsContainerAccessBase(expr) != nullptr) {
    auto rewritten_call = std::get<mir::CallExpr>(expr.data);
    // Read the type out before the recursive rewrite below: that recursion
    // appends to the same expression arena, which invalidates the `expr`
    // reference.
    const mir::TypeId expr_type = expr.type;
    rewritten_call.arguments.front() = RewriteLhsRootWithMutate(
        unit, block, rewritten_call.arguments.front(), services_id);
    return block.exprs.Add(
        mir::Expr{.data = std::move(rewritten_call), .type = expr_type});
  }
  throw InternalError(
      "RewriteLhsRootWithMutate: LHS root is neither an observable cell nor a "
      "projection over one");
}

auto BuildObservableAssignExpr(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::ExprId services_id, mir::ExprId lhs_id, mir::ExprId rhs_id,
    std::optional<mir::BinaryOp> compound_op, mir::TypeId result_type,
    mir::TypeId void_type) -> mir::Expr {
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
  const bool root_is_cell =
      mir::IsObservableCellType(unit.types.Get(block.exprs.Get(root_id).type));
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
