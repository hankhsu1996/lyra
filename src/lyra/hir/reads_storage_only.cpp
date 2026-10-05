#include "lyra/hir/reads_storage_only.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

auto ReadsStorageOnly(const base::Arena<Expr, ExprId>& exprs, ExprId id)
    -> bool {
  const auto reads = [&](ExprId operand) {
    return ReadsStorageOnly(exprs, operand);
  };
  const auto all_read = [&](std::span<const ExprId> operands) {
    return std::ranges::all_of(operands, reads);
  };
  const auto reads_if_present = [&](const std::optional<ExprId>& operand) {
    return !operand.has_value() || reads(*operand);
  };
  return std::visit(
      Overloaded{
          [](const PrimaryExpr& p) {
            // A property named bare is one of the object the method runs on,
            // which no elaboration sealed (LRM 8.4).
            return !std::holds_alternative<ClassPropertyRef>(p.data);
          },
          [&](const UnaryExpr& e) { return reads(e.operand); },
          [&](const BinaryExpr& e) { return reads(e.lhs) && reads(e.rhs); },
          [&](const ConditionalExpr& e) {
            return std::ranges::all_of(
                       e.conditions,
                       [&](const ConditionClause& clause) {
                         return reads(clause.expr);
                       }) &&
                   reads(e.then_value) && reads(e.else_value);
          },
          [&](const ConversionExpr& e) { return reads(e.operand); },
          [&](const ValueRangeExpr& e) { return reads(e.lo) && reads(e.hi); },
          [&](const InsideExpr& e) {
            return reads(e.lhs) && all_read(e.items);
          },
          [&](const ElementSelectExpr& e) {
            return reads(e.base_value) && reads(e.index);
          },
          [&](const RangeSelectExpr& e) {
            return reads(e.base_value) &&
                   std::visit(
                       Overloaded{
                           [&](const RangeConstantBounds& b) {
                             return reads(b.left_bound) && reads(b.right_bound);
                           },
                           [&](const RangeIndexedUpBounds& b) {
                             return reads(b.base_index) && reads(b.width);
                           },
                           [&](const RangeIndexedDownBounds& b) {
                             return reads(b.base_index) && reads(b.width);
                           }},
                       e.bounds);
          },
          [&](const MemberAccessExpr& e) { return reads(e.base_value); },
          [&](const ConcatExpr& e) { return all_read(e.operands); },
          [&](const StreamingConcatExpr& e) { return all_read(e.operands); },
          [&](const ReplicationExpr& e) {
            return reads(e.count) && reads(e.concat);
          },
          [&](const AssignmentPatternExpr& e) { return all_read(e.elements); },
          [&](const AssignmentPatternReplicationExpr& e) {
            return reads(e.count) && all_read(e.items);
          },
          [&](const AssociativeAssignmentPatternExpr& e) {
            return std::ranges::all_of(
                       e.entries,
                       [&](const AssociativeAssignmentPatternExpr::Entry&
                               entry) {
                         return reads(entry.key) && reads(entry.value);
                       }) &&
                   reads_if_present(e.default_value);
          },
          [&](const AssignmentPatternKeyedExpr& e) {
            return std::ranges::all_of(
                       e.entries,
                       [&](const AssignmentPatternKeyedExpr::Entry& entry) {
                         return reads(entry.index) && reads(entry.value);
                       }) &&
                   reads_if_present(e.default_value);
          },
          [&](const TaggedUnionExpr& e) { return reads_if_present(e.payload); },
          // A call may do anything; a write changes what is read; and what a
          // handle or a virtual interface reaches is no storage elaboration
          // sealed, and a null one fails the evaluation (LRM 8.4, 25.9).
          [](const CallExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const IncDecExpr&) { return false; },
          [](const ClassPropertyAccessExpr&) { return false; },
          [](const InterfaceMemberAccessExpr&) { return false; },
          [](const InterfaceInstanceAccessExpr&) { return false; },
          [](const DynamicArrayNewExpr&) { return false; },
          [](const ClassNewExpr&) { return false; },
          [](const DynamicCastExpr&) { return false; }},
      exprs.Get(id).data);
}

}  // namespace lyra::hir
