#include "lyra/lowering/hir_to_mir/lhs_store.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The operand a step in target position reaches its storage through, and
// nothing where `expr` is not a step at all. A field or member reach is taken
// from the value below it; so is a call that reaches through the object it
// dispatches on -- a guard, which yields the value it guards, and an access,
// whose receiver holds the part it reaches.
auto StepReceiver(const mir::Expr& expr) -> std::optional<mir::ExprId> {
  if (const auto* m = std::get_if<mir::FieldAccessExpr>(&expr.data)) {
    return m->receiver;
  }
  if (const auto* m = std::get_if<mir::UnionMemberExpr>(&expr.data)) {
    return m->union_value;
  }
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr || !mir::ReachesThroughReceiver(call->callee)) {
    return std::nullopt;
  }
  return mir::CalleeReceiver(call->callee);
}

// `expr`'s own form with the operand it steps from replaced. Defined exactly
// where a step is, so the two agree on which nodes have one.
auto WithStepReceiver(const mir::Expr& expr, mir::ExprId receiver)
    -> mir::ExprData {
  if (const auto* m = std::get_if<mir::FieldAccessExpr>(&expr.data)) {
    mir::FieldAccessExpr rebuilt = *m;
    rebuilt.receiver = receiver;
    return rebuilt;
  }
  if (const auto* m = std::get_if<mir::UnionMemberExpr>(&expr.data)) {
    mir::UnionMemberExpr rebuilt = *m;
    rebuilt.union_value = receiver;
    return rebuilt;
  }
  auto rebuilt = std::get<mir::CallExpr>(expr.data);
  std::get<mir::Direct>(rebuilt.callee).receiver = receiver;
  return rebuilt;
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
    // Every step above the root reaches its storage through the value it is
    // taken from, so the walk is one step per node until something is not a
    // step at all.
    const std::optional<mir::ExprId> receiver = StepReceiver(expr);
    if (!receiver) {
      return lhs_id;
    }
    lhs_id = *receiver;
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
  const std::optional<mir::ExprId> receiver = StepReceiver(expr);
  if (!receiver) {
    return root_id;
  }
  // Read the type out before the recursion: it appends to the same arena, which
  // invalidates the `expr` reference.
  const mir::TypeId result_ty = expr.type;
  const mir::ExprId rebuilt_receiver =
      ReplaceLhsRoot(unit, block, *receiver, root_id);
  return block.exprs.Add(
      mir::Expr{
          .data = WithStepReceiver(block.exprs.Get(lhs_id), rebuilt_receiver),
          .type = result_ty});
}

auto StoragePlaceOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id)
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
  // Asking a wrapper for its storage as somewhere to write is an operation on
  // the wrapper -- which storage it currently stands for is a fact about it --
  // so it is a call, and the answer is a pointer the ordinary dereference then
  // names the storage through.
  const mir::TypeId value_type = root_ty.WrappedValueType();
  const mir::ExprId opened = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kOpenForWrite,
                          .receiver = root_id},
                  .arguments = {}},
          .type = unit.types.Intern(
              mir::Type{mir::PointerType{
                  .pointee = value_type,
                  .ownership = mir::PointerOwnership::kBorrowed,
                  .mutability = mir::Mutability::kMutable}})});
  const mir::ExprId storage_id =
      block.exprs.Add(mir::MakeDerefExpr(opened, value_type));
  return ReplaceLhsRoot(unit, block, lhs_id, storage_id);
}

auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
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
  // Replacing the whole of what a capability wrapper holds acts on the wrapper
  // -- the value lands in its storage and it reports the change to whatever is
  // watching -- so the operation is a call taking the wrapper as its
  // destination. A compound store reads before it writes, and a store that
  // descends writes a part; both reach storage the way a read does and assign
  // through what they reach.
  const mir::ExprId root_id = FindLhsRootId(unit, block, lhs_id);
  const mir::Type& root_ty = unit.types.Get(block.exprs.Get(root_id).type);
  if (root_id == lhs_id && !compound_op.has_value() &&
      root_ty.IsCapabilityWrapper() && !root_ty.Is<mir::ResolvedType>()) {
    // The operands are the destination and the value, and nothing else: the
    // engine the wrapper reports through is the ambient one, which has the
    // standing of a stack pointer rather than of program data.
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kStore,
                        .receiver = root_id},
                .arguments = {rhs_id}},
        .type = unit.builtins.void_type};
  }

  const mir::ExprId target_id = StoragePlaceOf(unit, block, lhs_id);
  return mir::Expr{
      .data =
          mir::AssignExpr{
              .target = target_id, .compound_op = compound_op, .value = rhs_id},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
