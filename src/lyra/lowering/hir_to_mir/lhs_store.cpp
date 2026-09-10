#include "lyra/lowering/hir_to_mir/lhs_store.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto CallEntry(
    mir::Block& block, support::BuiltinFn fn,
    std::optional<base::ComponentIndex> position, mir::ExprId receiver,
    std::vector<mir::ExprId> operands, mir::TypeId type) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = fn,
                          .receiver = receiver,
                          .position = position},
                  .arguments = std::move(operands)},
          .type = type});
}

// A net's resolved cell is readable and observable, but no value gets into it
// this way at all: a net takes one only through a driver (LRM 6.5), and a net
// is not a variable, so it is neither a store destination nor a legal `ref`
// actual (LRM 13.5.2). A producer hands this the driver in the cell's place;
// arriving with the cell means it did not.
void RefuseNetCell(const mir::Type& place_ty) {
  if (place_ty.Is<mir::ResolvedType>()) {
    throw InternalError(
        "lhs_store: a net's cell takes no value a write may put there; the "
        "destination is one of its drivers");
  }
}

}  // namespace

auto DescendInto(WriteTarget base, DescentStep step) -> WriteTarget {
  base.descent.push_back(std::move(step));
  return base;
}

auto TargetValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const WriteTarget& target) -> mir::TypeId {
  if (!target.descent.empty()) {
    return target.descent.back().part_type;
  }
  const mir::TypeId owner_type = block.exprs.Get(target.owner).type;
  const mir::Type& owner_ty = unit.types.Get(owner_type);
  return owner_ty.IsCapabilityWrapper() ? owner_ty.WrappedValueType()
                                        : owner_type;
}

auto OpenedPlace(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId place)
    -> mir::ExprId {
  const mir::Type& place_ty = unit.types.Get(block.exprs.Get(place).type);
  if (!place_ty.IsCapabilityWrapper()) {
    return place;
  }
  RefuseNetCell(place_ty);
  const mir::TypeId value_type = place_ty.WrappedValueType();
  const mir::ExprId opened = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kOpenForWrite,
                          .receiver = place},
                  .arguments = {}},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = value_type,
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kMutable}})});
  return block.exprs.Add(mir::MakeDerefExpr(opened, value_type));
}

auto TargetPlace(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId {
  mir::ExprId reached = OpenedPlace(unit, block, target.owner);
  for (const DescentStep& step : target.descent) {
    reached = CallEntry(
        block, step.part_entry, step.position, reached, step.operands,
        step.part_type);
  }
  return reached;
}

auto ReadTargetValue(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId {
  const mir::Type& owner_ty =
      unit.types.Get(block.exprs.Get(target.owner).type);
  mir::ExprId reached = target.owner;
  if (owner_ty.IsCapabilityWrapper()) {
    reached = block.exprs.Add(
        mir::MakeCellLoadCallExpr(target.owner, owner_ty.WrappedValueType()));
  }
  for (const DescentStep& step : target.descent) {
    reached = CallEntry(
        block, step.value_entry, step.position, reached, step.operands,
        step.part_type);
  }
  return reached;
}

auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target,
    mir::ExprId rhs_id, std::optional<mir::BinaryOp> compound_op,
    mir::TypeId result_type) -> mir::Expr {
  // A store carries the right-hand side to the destination's declared
  // representation before it lands (LRM 10.6.1). The front end already converts
  // width, signedness, and state domain; the dimension stack -- and, for a
  // container, the element representation and bound -- is the axis it leaves to
  // assignment. A compound store computes its value through the operator, which
  // already yields the destination's shape.
  if (!compound_op.has_value()) {
    rhs_id = ConvertToType(
        unit, block, rhs_id, TargetValueType(unit, block, target));
  }
  // Replacing the whole of what a capability wrapper holds acts on the wrapper
  // -- the value lands in its storage and it reports the change to whatever is
  // watching -- so it is a call taking the wrapper as its destination. A
  // compound store reads before it writes, and a store that descends writes a
  // part; both reach storage the way a read does and assign through what they
  // reach.
  const mir::Type& owner_ty =
      unit.types.Get(block.exprs.Get(target.owner).type);
  if (target.descent.empty() && !compound_op.has_value() &&
      owner_ty.IsCapabilityWrapper() && !owner_ty.Is<mir::ResolvedType>()) {
    // The operands are the destination and the value, and nothing else: the
    // engine the wrapper reports through is the ambient one, which has the
    // standing of a stack pointer rather than of program data.
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kStore,
                        .receiver = target.owner},
                .arguments = {rhs_id}},
        .type = unit.builtins.void_type};
  }
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = TargetPlace(unit, block, target),
              .compound_op = compound_op,
              .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
