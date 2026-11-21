#include "lyra/mir/wide_integral_operator.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/integral_operation.hpp"

namespace lyra::mir {

namespace {

using support::IntegralOp;

// The operation of a type `applied` is a call of: of the type it makes where
// no operand settles its answer's type, and otherwise of its first integral
// operand's, taken at the type of any other integral operand it reads. Nothing
// where that type is no wider than a machine operates on at once, which the
// library carries the operator out over for every width of the other.
auto SourceOf(
    const CompilationUnit& unit, const Block& block, const Expr& expr,
    const IntegralOperator& applied) -> std::optional<TypeOperationSource> {
  const support::IntegralOperation& declared =
      support::IntegralOperationOf(applied.op);
  std::optional<TypeId> self;
  if (support::AnswersAtTheTypeOfTheCall(declared.answer)) {
    self = expr.type;
  }
  std::optional<TypeId> at;
  const std::span<const support::IntegralOperandKind> kinds =
      declared.operands.Kinds();
  for (std::size_t i = 0; i < kinds.size(); ++i) {
    const TypeId type = block.exprs.Get(applied.operands.at(i)).type;
    switch (kinds[i]) {
      case support::IntegralOperandKind::kBits:
      case support::IntegralOperandKind::kNumber:
        if (!self.has_value()) {
          self = type;
        } else if (!at.has_value()) {
          at = type;
        }
        break;
      case support::IntegralOperandKind::kSameType:
      case support::IntegralOperandKind::kMachineInt:
      case support::IntegralOperandKind::kMachineBool:
      case support::IntegralOperandKind::kSvLogic:
      case support::IntegralOperandKind::kText:
      case support::IntegralOperandKind::kCanonicalBits:
      case support::IntegralOperandKind::kCanonicalLogic:
      case support::IntegralOperandKind::kPositions:
        break;
    }
  }
  if (!self.has_value() || !IsExpandedIntegral(unit.types, *self)) {
    return std::nullopt;
  }
  TypeOperationSource source{.type = *self, .answers = applied.op};
  if (at.has_value()) {
    source.arguments.push_back(*at);
  }
  return source;
}

}  // namespace

auto IntegralOperatorOf(
    const CompilationUnit& unit, const Block& block, const Expr& expr)
    -> std::optional<IntegralOperator> {
  const auto integral = [&](ExprId operand) {
    return unit.types.Get(block.exprs.Get(operand).type).IsIntegral();
  };
  if (const auto* binary = std::get_if<BinaryExpr>(&expr.data)) {
    if (!integral(binary->lhs)) {
      return std::nullopt;
    }
    return IntegralOperator{
        .op = IntegralOpOf(binary->op), .operands = {binary->lhs, binary->rhs}};
  }
  if (const auto* unary = std::get_if<UnaryExpr>(&expr.data)) {
    if (!integral(unary->operand)) {
      return std::nullopt;
    }
    return IntegralOperator{
        .op = IntegralOpOf(unary->op), .operands = {unary->operand}};
  }
  // A value read as the machine boolean a condition tests is its truth (LRM
  // 12.4).
  if (const auto* cast = std::get_if<CastExpr>(&expr.data)) {
    if (!integral(cast->operand) ||
        !unit.types.Get(expr.type).Is<MachineBoolType>()) {
      return std::nullopt;
    }
    return IntegralOperator{
        .op = IntegralOp::kIsTrue, .operands = {cast->operand}};
  }
  const auto* call = std::get_if<CallExpr>(&expr.data);
  const auto* direct =
      call == nullptr ? nullptr : std::get_if<Direct>(&call->callee);
  const auto* fn = direct == nullptr
                       ? nullptr
                       : std::get_if<support::BuiltinFn>(&direct->target);
  if (fn == nullptr) {
    return std::nullopt;
  }
  const std::optional<IntegralOp> op = support::RuntimeEntryOf(*fn).integral;
  if (!op.has_value()) {
    return std::nullopt;
  }
  // A built-in method's receiver is the operator's first operand.
  IntegralOperator applied{.op = *op, .operands = {}};
  if (direct->receiver.has_value()) {
    applied.operands.push_back(*direct->receiver);
  }
  applied.operands.insert(
      applied.operands.end(), call->arguments.begin(), call->arguments.end());
  return applied;
}

auto WideOperatorExpansionOf(
    const CompilationUnit& unit, const Block& block, const Expr& expr,
    const IntegralOperator& applied) -> std::optional<WideOperatorExpansion> {
  const auto wide = [&](TypeId type) {
    return IsExpandedIntegral(unit.types, type);
  };
  if (!wide(expr.type) &&
      std::ranges::none_of(applied.operands, [&](ExprId operand) {
        return wide(block.exprs.Get(operand).type);
      })) {
    return std::nullopt;
  }
  const support::IntegralOperation& operation =
      support::IntegralOperationOf(applied.op);
  switch (operation.wide) {
    case support::WideAction::kExpand:
      break;
    case support::WideAction::kLibCall:
      return std::nullopt;
    // Its operands are truths, which HIR-to-MIR states (LRM 11.4.7).
    case support::WideAction::kNever:
      throw InternalError(
          std::format(
              "mir: the logical operator '{}' is applied to a value wider than "
              "a machine operates on at once rather than to its truth -- "
              "please report this as a bug",
              operation.name));
  }
  if (operation.narrows_to_its_low_part && !wide(expr.type)) {
    return Truncated{};
  }
  std::optional<TypeOperationSource> source =
      SourceOf(unit, block, expr, applied);
  if (!source.has_value()) {
    return std::nullopt;
  }
  return CallsItsTypesOperation{.source = std::move(*source)};
}

}  // namespace lyra::mir
