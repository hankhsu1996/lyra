#include "lyra/mir_transform/loop_analysis.hpp"

#include <algorithm>
#include <cstddef>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

// The call `expr` makes to reach storage a write lands in: a write in
// progress opened on it, or the whole of what that write holds.
auto ReachesWritten(const mir::Expr& expr) -> const mir::CallExpr* {
  for (const support::BuiltinFn reaching :
       {support::BuiltinFn::kDesignateWhole,
        support::BuiltinFn::kOpenForWrite}) {
    if (const mir::CallExpr* call = BuiltinCallOf(expr, reaching)) {
      return call;
    }
  }
  return nullptr;
}

auto ReceiverOf(const mir::CallExpr& call) -> mir::ExprId {
  return *mir::CalleeReceiver(call.callee);
}

// Whether a member of a value of `aggregate` lies where every other member
// does: a member of a union (LRM 7.3).
auto OverlaysSiblings(const mir::CompilationUnit& unit, mir::TypeId aggregate)
    -> bool {
  const std::optional<mir::RecordKind> kind =
      mir::RecordKindOf(unit, aggregate);
  if (!kind.has_value()) {
    return false;
  }
  switch (*kind) {
    case mir::RecordKind::kStruct:
      return false;
    case mir::RecordKind::kUnion:
    case mir::RecordKind::kTaggedUnion:
      return true;
  }
  throw InternalError("mir_transform: unknown record kind");
}

// Whether converting from `from` to `to` keeps every value `from` holds: the
// same width read the same way, or a wider one that extends it as its own
// signedness says to.
auto KeepsEveryValue(const mir::IntegralType& from, const mir::IntegralType& to)
    -> bool {
  if (to.bit_width == from.bit_width) {
    return to.signedness == from.signedness;
  }
  return to.bit_width > from.bit_width &&
         (to.signedness == mir::Signedness::kSigned ||
          from.signedness == mir::Signedness::kUnsigned);
}

}  // namespace

auto BuiltinCallOf(const mir::Expr& expr, support::BuiltinFn entry)
    -> const mir::CallExpr* {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr || mir::DirectBuiltinFn(*call) != entry) {
    return nullptr;
  }
  return call;
}

auto UnderlyingObjectOf(const mir::CompilationUnit& unit, NodeRef place)
    -> std::optional<UnderlyingObject> {
  const mir::Expr& expr = place.Get();
  if (const std::optional<mir::LocalId> local =
          mir::ReferencedLocal(expr.data)) {
    return UnderlyingObject{
        .root = *local, .through_pointer = false, .members = {}};
  }
  if (const auto* field = std::get_if<mir::FieldAccessExpr>(&expr.data)) {
    const NodeRef receiver = place.At(field->receiver);
    std::optional<UnderlyingObject> within = UnderlyingObjectOf(unit, receiver);
    if (within.has_value()) {
      within->members.push_back(
          MemberStep{
              .field = field->field,
              .overlays_siblings =
                  OverlaysSiblings(unit, receiver.Get().type)});
    }
    return within;
  }
  if (const auto* bits = std::get_if<mir::SliceExpr>(&expr.data)) {
    return UnderlyingObjectOf(unit, place.At(bits->receiver));
  }
  if (const auto* deref = std::get_if<mir::DerefExpr>(&expr.data)) {
    const mir::Expr& pointer = place.block->exprs.Get(deref->pointer);
    if (const auto* read = std::get_if<mir::CopyExpr>(&pointer.data)) {
      const std::optional<mir::LocalId> holder =
          mir::ReferencedLocal(place.block->exprs.Get(read->place).data);
      if (!holder.has_value()) {
        return std::nullopt;
      }
      return UnderlyingObject{
          .root = *holder, .through_pointer = true, .members = {}};
    }
    if (const mir::CallExpr* contents =
            BuiltinCallOf(pointer, support::BuiltinFn::kGet)) {
      return UnderlyingObjectOf(unit, place.At(ReceiverOf(*contents)));
    }
    if (const mir::CallExpr* written = ReachesWritten(pointer)) {
      return UnderlyingObjectOf(unit, place.At(ReceiverOf(*written)));
    }
    return std::nullopt;
  }
  if (const mir::CallExpr* call = ReachesWritten(expr)) {
    return UnderlyingObjectOf(unit, place.At(ReceiverOf(*call)));
  }
  return std::nullopt;
}

auto MayAlias(
    const UnderlyingObject& a, const UnderlyingObject& b,
    std::optional<mir::LocalId> receiver) -> bool {
  if (a.through_pointer != b.through_pointer) {
    const UnderlyingObject& through = a.through_pointer ? a : b;
    return through.root != receiver;
  }
  if (a.root != b.root) {
    return a.through_pointer;
  }
  for (std::size_t i = 0; i < a.members.size() && i < b.members.size(); ++i) {
    if (a.members[i].field != b.members[i].field) {
      return a.members[i].overlays_siblings;
    }
  }
  return true;
}

auto LoopLocalRead(NodeRef node, const LoopLocals& locals)
    -> std::optional<NodeRef> {
  const auto* read = std::get_if<mir::CopyExpr>(&node.Get().data);
  if (read == nullptr) {
    return std::nullopt;
  }
  const std::optional<mir::LocalId> local =
      mir::ReferencedLocal(node.block->exprs.Get(read->place).data);
  const auto found = local.has_value() ? locals.find(*local) : locals.end();
  if (found == locals.end()) {
    return std::nullopt;
  }
  return found->second;
}

auto InductionVariableReadBy(const mir::CompilationUnit& unit, NodeRef node)
    -> std::optional<InductionVariable> {
  const auto* read = std::get_if<mir::CopyExpr>(&node.Get().data);
  if (read == nullptr) {
    return std::nullopt;
  }
  const NodeRef place = node.At(read->place);
  if (const auto* deref = std::get_if<mir::DerefExpr>(&place.Get().data)) {
    if (const mir::CallExpr* contents = BuiltinCallOf(
            place.block->exprs.Get(deref->pointer), support::BuiltinFn::kGet)) {
      std::optional<UnderlyingObject> cell =
          UnderlyingObjectOf(unit, place.At(ReceiverOf(*contents)));
      if (!cell.has_value()) {
        return std::nullopt;
      }
      return InductionVariable{.object = *std::move(cell), .in_cell = true};
    }
  }
  std::optional<UnderlyingObject> variable = UnderlyingObjectOf(unit, place);
  if (!variable.has_value()) {
    return std::nullopt;
  }
  return InductionVariable{.object = *std::move(variable), .in_cell = false};
}

LoopEvolution::LoopEvolution(
    const mir::CompilationUnit& unit, std::optional<mir::LocalId> receiver,
    const LoopLocals& locals, InductionVariable induction)
    : unit_(&unit),
      receiver_(receiver),
      locals_(&locals),
      induction_(std::move(induction)) {
}

auto LoopEvolution::ReadsInductionVariable(NodeRef node) const -> bool {
  return InductionVariableReadBy(*unit_, node) == induction_;
}

auto LoopEvolution::ReadsBesideInductionVariable(NodeRef place) const -> bool {
  const std::optional<UnderlyingObject> object =
      UnderlyingObjectOf(*unit_, place);
  return object.has_value() && !MayAlias(*object, induction_.object, receiver_);
}

auto LoopEvolution::IsLoopInvariant(NodeRef node) const -> bool {
  if (ReadsInductionVariable(node)) {
    return false;
  }
  if (const std::optional<NodeRef> init = LoopLocalRead(node, *locals_)) {
    return IsLoopInvariant(*init);
  }
  const mir::Expr& expr = node.Get();
  if (const auto* read = std::get_if<mir::CopyExpr>(&expr.data);
      read != nullptr && !ReadsBesideInductionVariable(node.At(read->place))) {
    return false;
  }
  if (const auto* block = std::get_if<mir::BlockExpr>(&expr.data)) {
    return IsInvariantIn(node.block->child_scopes.Get(block->scope));
  }
  bool invariant = true;
  mir::ForEachOperand(expr.data, [&](mir::ExprId operand, mir::OperandUse) {
    invariant = invariant && IsLoopInvariant(node.At(operand));
  });
  return invariant;
}

auto LoopEvolution::IsInvariantIn(const mir::Block& block) const -> bool {
  return std::ranges::all_of(
             block.exprs.Ids(),
             [&](mir::ExprId id) {
               return IsLoopInvariant(NodeRef{.block = &block, .id = id});
             }) &&
         std::ranges::all_of(block.child_scopes, [&](const mir::Block& nested) {
           return IsInvariantIn(nested);
         });
}

auto LoopEvolution::AddRecurrenceOf(NodeRef node) const
    -> std::optional<AddRecurrence> {
  if (ReadsInductionVariable(node)) {
    return AddRecurrence{};
  }
  if (const std::optional<NodeRef> init = LoopLocalRead(node, *locals_)) {
    return AddRecurrenceOf(*init);
  }
  const mir::Expr& expr = node.Get();
  const mir::Type& type = unit_->types.Get(expr.type);
  if (!type.IsIntegral()) {
    return std::nullopt;
  }
  const bool at_position = mir::IsPositionType(unit_->types, expr.type);
  if (const auto* binary = std::get_if<mir::BinaryExpr>(&expr.data)) {
    if (binary->op != mir::BinaryOp::kAdd &&
        binary->op != mir::BinaryOp::kSub) {
      return std::nullopt;
    }
    const NodeRef lhs = node.At(binary->lhs);
    const NodeRef rhs = node.At(binary->rhs);
    const bool lhs_moves = !IsLoopInvariant(lhs);
    if (lhs_moves == !IsLoopInvariant(rhs) ||
        (binary->op == mir::BinaryOp::kSub && !lhs_moves)) {
      return std::nullopt;
    }
    const NodeRef moving = lhs_moves ? lhs : rhs;
    std::optional<AddRecurrence> recurrence = AddRecurrenceOf(moving);
    if (!recurrence.has_value()) {
      return std::nullopt;
    }
    // A sum taken where it may wrap is checked as the position it becomes, so
    // no position is summed after it.
    if (at_position) {
      return recurrence->wraps_at.has_value() ? std::nullopt : recurrence;
    }
    if (moving.Get().type != expr.type) {
      return std::nullopt;
    }
    return AddRecurrence{.wraps_at = type.Integral()};
  }
  const mir::CallExpr* convert =
      BuiltinCallOf(expr, support::BuiltinFn::kToPosition);
  if (convert == nullptr) {
    convert = BuiltinCallOf(expr, support::BuiltinFn::kIntegralConvert);
  }
  if (convert == nullptr || convert->arguments.size() != 1) {
    return std::nullopt;
  }
  const NodeRef from = node.At(convert->arguments.front());
  const mir::Type& from_type = unit_->types.Get(from.Get().type);
  if (!from_type.IsIntegral() ||
      !KeepsEveryValue(from_type.Integral(), type.Integral())) {
    return std::nullopt;
  }
  return AddRecurrenceOf(from);
}

auto IsSafeToExpand(NodeRef node, const LoopLocals& locals) -> bool {
  if (const std::optional<NodeRef> init = LoopLocalRead(node, locals)) {
    return IsSafeToExpand(*init, locals);
  }
  const mir::ExprData& data = node.Get().data;
  if (std::holds_alternative<mir::BlockExpr>(data) ||
      std::holds_alternative<mir::ConditionalExpr>(data) ||
      std::holds_alternative<mir::ClosureExpr>(data)) {
    return false;
  }
  if (const std::optional<mir::LocalId> local = mir::ReferencedLocal(data);
      local.has_value() && locals.contains(*local)) {
    return false;
  }
  bool safe = true;
  mir::ForEachOperand(data, [&](mir::ExprId operand, mir::OperandUse) {
    safe = safe && IsSafeToExpand(node.At(operand), locals);
  });
  return safe;
}

auto ExpandCode(NodeRef node, const Into& into, const LoopLocals& locals)
    -> mir::ExprId {
  if (const std::optional<NodeRef> init = LoopLocalRead(node, locals)) {
    return ExpandCode(*init, into, locals);
  }
  const mir::Expr& expr = node.Get();
  mir::ExprData data = expr.data;
  mir::ForEachOperand(data, [&](mir::ExprId& operand, mir::OperandUse) {
    operand = ExpandCode(node.At(operand), into, locals);
  });
  return into(
      mir::Expr{.data = std::move(data), .type = expr.type, .span = expr.span});
}

}  // namespace lyra::mir_transform
