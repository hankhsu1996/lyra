#include "lyra/lowering/hir_to_mir/lhs_store.hpp"

#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The operand a call yields unchanged, when `expr` is such a call. A walk that
// is following where storage lives passes through it rather than stopping.
auto GuardedOperand(const mir::Expr& expr) -> const mir::ExprId* {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr || call->arguments.empty()) return nullptr;
  if (!mir::IsPassThroughCallee(call->callee)) return nullptr;
  return &call->arguments.front();
}

}  // namespace

auto FindLhsRootId(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId lhs_id) -> mir::ExprId {
  while (true) {
    const auto& expr = block.exprs.Get(lhs_id);
    // A captured carrier -- a closure-record field holding a `Ref` (or other
    // capability wrapper) -- is itself the root cell, reached by a field access
    // over the closure receiver. Stop here rather than projecting through it as
    // if it were a struct member of an observable aggregate.
    if (unit.types.Get(expr.type).IsCapabilityWrapper()) {
      return lhs_id;
    }
    // A designated part of a value states its owner, so the root is reached
    // without walking a descent chain.
    if (const auto* projection =
            std::get_if<mir::ValueProjectionExpr>(&expr.data)) {
      lhs_id = projection->owner;
      continue;
    }
    // A guard yields the value it guards, so the storage a write reaches is
    // whatever the guarded expression reaches.
    if (const mir::ExprId* guarded = GuardedOperand(expr)) {
      lhs_id = *guarded;
      continue;
    }
    if (const auto* m = std::get_if<mir::TaggedGetRefExpr>(&expr.data)) {
      lhs_id = m->union_value;
      continue;
    }
    return lhs_id;
  }
}

auto ReplaceLhsRoot(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId root_id) -> mir::ExprId {
  const auto& expr = block.exprs.Get(lhs_id);
  // The descent stops where the root walk stops, so the two agree on which node
  // is the root: whatever that finds, this one substitutes.
  if (unit.types.Get(expr.type).IsCapabilityWrapper()) {
    return root_id;
  }
  if (const auto* projection =
          std::get_if<mir::ValueProjectionExpr>(&expr.data)) {
    mir::ValueProjectionExpr rebuilt = *projection;
    // Read the type out before the recursion below: it appends to the same
    // arena, which invalidates the `expr` reference.
    const mir::TypeId result_ty = expr.type;
    rebuilt.owner = ReplaceLhsRoot(unit, block, rebuilt.owner, root_id);
    return block.exprs.Add(
        mir::Expr{.data = std::move(rebuilt), .type = result_ty});
  }
  if (const auto* m = std::get_if<mir::TaggedGetRefExpr>(&expr.data)) {
    mir::TaggedGetRefExpr rebuilt = *m;
    const mir::TypeId result_ty = expr.type;
    rebuilt.union_value =
        ReplaceLhsRoot(unit, block, rebuilt.union_value, root_id);
    return block.exprs.Add(mir::Expr{.data = rebuilt, .type = result_ty});
  }
  if (GuardedOperand(expr) != nullptr) {
    auto rebuilt = std::get<mir::CallExpr>(expr.data);
    const mir::TypeId result_ty = expr.type;
    rebuilt.arguments.front() =
        ReplaceLhsRoot(unit, block, rebuilt.arguments.front(), root_id);
    return block.exprs.Add(
        mir::Expr{.data = std::move(rebuilt), .type = result_ty});
  }
  return root_id;
}

auto StoragePlaceOf(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id)
    -> mir::ExprId {
  const mir::ExprId root_id = FindLhsRootId(unit, block, lhs_id);
  const mir::Type& root_ty = unit.types.Get(block.exprs.Get(root_id).type);
  if (!root_ty.IsCapabilityWrapper()) {
    return lhs_id;
  }
  // A net's resolved cell is readable and observable, but its storage is not
  // reachable this way at all: a value gets into a net only through one of its
  // drivers (LRM 6.5), and a net is not a variable, so it is neither a store
  // destination (LRM 6.5) nor a legal `ref` actual (LRM 13.5.2). A producer
  // hands this the driver in the cell's place; arriving with the cell means it
  // did not.
  if (root_ty.Is<mir::ResolvedType>()) {
    throw InternalError(
        "StoragePlaceOf: a net's cell holds no storage a write or a reference "
        "may reach; the destination is one of its drivers");
  }
  const mir::ExprId storage_id =
      block.exprs.Add(mir::MakeDerefExpr(root_id, root_ty.WrappedValueType()));
  return ReplaceLhsRoot(unit, block, lhs_id, storage_id);
}

auto BuildStoreExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId rhs_id, std::optional<mir::BinaryOp> compound_op,
    mir::TypeId result_type) -> mir::Expr {
  // A plain store carries the right-hand side to the destination's full
  // declared representation before it reaches the cell (LRM 10.6.1), for every
  // value family. The front end already converts width, signedness, and state
  // domain; the dimension stack (and, for containers, the element
  // representation and bound) is the axis it leaves to assignment, so the value
  // coerces here to the destination's declared type. A compound store computes
  // its stored value through the operator, which already yields the destination
  // shape.
  if (!compound_op.has_value()) {
    const mir::TypeId lhs_type = block.exprs.Get(lhs_id).type;
    const mir::Type& lhs_ty = unit.types.Get(lhs_type);
    const mir::TypeId dst_value_type =
        lhs_ty.IsCapabilityWrapper() ? lhs_ty.WrappedValueType() : lhs_type;
    rhs_id = ConvertToType(unit, block, rhs_id, dst_value_type);
  }
  const mir::ExprId target_id = StoragePlaceOf(unit, block, lhs_id);
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = target_id, .compound_op = compound_op, .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
