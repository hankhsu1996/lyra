#include "lyra/mir/integral_constant_folding.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_operation.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/string.hpp"

namespace lyra::mir {

namespace {

using support::IntegralOp;
using support::IntegralOperandKind;
using support::IntegralOperation;
using support::IntegralOperationOf;

auto SignednessOf(Signedness signedness) -> value::Signedness {
  switch (signedness) {
    case Signedness::kSigned:
      return value::Signedness::kSigned;
    case Signedness::kUnsigned:
      return value::Signedness::kUnsigned;
  }
  throw InternalError("FoldIntegral: unknown signedness");
}

auto DomainOf(IntegralStateKind state) -> value::StateDomain {
  switch (state) {
    case IntegralStateKind::kTwoState:
      return value::StateDomain::kTwoState;
    case IntegralStateKind::kFourState:
      return value::StateDomain::kFourState;
  }
  throw InternalError("FoldIntegral: unknown integral state kind");
}

auto ShapeOf(const IntegralType& type) -> value::IntegralShape {
  return value::IntegralShape{
      .width = type.bit_width,
      .signedness = SignednessOf(type.signedness),
      .domain = DomainOf(type.state_kind)};
}

auto ShapeOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<value::IntegralShape> {
  const Type& ty = unit.types.Get(type);
  return ty.IsIntegral() ? std::optional(ShapeOf(ty.Integral())) : std::nullopt;
}

// The constant an integral operand names, absent where it names none.
auto ConstantOf(const CompilationUnit& unit, const Expr& operand)
    -> const IntegralConstant* {
  const auto* reference = std::get_if<ReferenceExpr>(&operand.data);
  if (reference == nullptr) {
    return nullptr;
  }
  const auto* constant = std::get_if<IntegralConstantRef>(&reference->target);
  return constant == nullptr
             ? nullptr
             : &unit.integral_constants.Get(constant->constant).value;
}

// One operand as the operation is applied to it, absent where the operand is
// not a value fixed before the program runs.
auto AppliedOperand(
    const CompilationUnit& unit, const Expr& operand, IntegralOperandKind kind,
    const std::optional<value::IntegralShape>& shape)
    -> std::optional<value::IntegralOperand> {
  const auto planes = [&]() -> std::optional<value::ConstPlanes> {
    const IntegralConstant* constant = ConstantOf(unit, operand);
    if (constant == nullptr) {
      return std::nullopt;
    }
    return value::ConstPlanes{
        .value = constant->value_words, .unknown = constant->state_words};
  };
  const auto bits = [&]() -> std::optional<value::IntegralOperand> {
    const std::optional<value::ConstPlanes> held = planes();
    if (!held) {
      return std::nullopt;
    }
    return value::BitsOperand{.planes = *held, .width = shape->width};
  };
  const auto number = [&]() -> std::optional<value::IntegralOperand> {
    const std::optional<value::ConstPlanes> held = planes();
    if (!held) {
      return std::nullopt;
    }
    return value::NumberOperand{
        .planes = *held,
        .width = shape->width,
        .signedness = shape->signedness};
  };
  switch (kind) {
    case IntegralOperandKind::kBits:
      return bits();
    // An operand of the first one's type is read as the first one is, which the
    // operation's own arm does, so it is handed over with all it could be read
    // by.
    case IntegralOperandKind::kNumber:
    case IntegralOperandKind::kSameType:
      return number();
    case IntegralOperandKind::kMachineInt: {
      const auto* literal = std::get_if<MachineIntLiteral>(&operand.data);
      return literal == nullptr
                 ? std::nullopt
                 : std::optional<value::IntegralOperand>(literal->value);
    }
    case IntegralOperandKind::kMachineBool: {
      const auto* literal = std::get_if<MachineBoolLiteral>(&operand.data);
      return literal == nullptr
                 ? std::nullopt
                 : std::optional<value::IntegralOperand>(literal->value);
    }
    case IntegralOperandKind::kText: {
      const auto* literal = std::get_if<StringLiteral>(&operand.data);
      return literal == nullptr ? std::nullopt
                                : std::optional<value::IntegralOperand>(
                                      std::string_view(literal->value));
    }
    // What a foreign call leaves behind exists only once the call has run.
    case IntegralOperandKind::kSvLogic:
    case IntegralOperandKind::kCanonicalBits:
    case IntegralOperandKind::kCanonicalLogic:
      return std::nullopt;
  }
  throw InternalError("AppliedOperand: unknown integral operand kind");
}

[[noreturn]] void Misapplied(
    const IntegralOperation& operation, std::string_view what) {
  throw InternalError(
      std::format(
          "FoldIntegral: the integral operation {} {}", operation.name, what));
}

}  // namespace

auto IntegralOpOf(BinaryOp op) -> IntegralOp {
  switch (op) {
    case BinaryOp::kAdd:
      return IntegralOp::kAdd;
    case BinaryOp::kSub:
      return IntegralOp::kSubtract;
    case BinaryOp::kMul:
      return IntegralOp::kMultiply;
    case BinaryOp::kDiv:
      return IntegralOp::kDivide;
    case BinaryOp::kMod:
      return IntegralOp::kModulo;
    case BinaryOp::kBitwiseAnd:
      return IntegralOp::kBitwiseAnd;
    case BinaryOp::kBitwiseOr:
      return IntegralOp::kBitwiseOr;
    case BinaryOp::kBitwiseXor:
      return IntegralOp::kBitwiseXor;
    case BinaryOp::kEquality:
      return IntegralOp::kEqual;
    case BinaryOp::kInequality:
      return IntegralOp::kNotEqual;
    case BinaryOp::kGreaterEqual:
      return IntegralOp::kGreaterEqual;
    case BinaryOp::kGreaterThan:
      return IntegralOp::kGreater;
    case BinaryOp::kLessEqual:
      return IntegralOp::kLessEqual;
    case BinaryOp::kLessThan:
      return IntegralOp::kLess;
    case BinaryOp::kLogicalAnd:
      return IntegralOp::kLogicalAnd;
    case BinaryOp::kLogicalOr:
      return IntegralOp::kLogicalOr;
  }
  throw InternalError("IntegralOpOf: unknown mir::BinaryOp");
}

auto IntegralOpOf(UnaryOp op) -> IntegralOp {
  switch (op) {
    case UnaryOp::kMinus:
      return IntegralOp::kNegate;
    case UnaryOp::kBitwiseNot:
      return IntegralOp::kBitwiseNot;
    case UnaryOp::kLogicalNot:
      return IntegralOp::kLogicalNot;
  }
  throw InternalError("IntegralOpOf: unknown mir::UnaryOp");
}

auto FoldStringFromBits(const CompilationUnit& unit, const Expr& operand)
    -> std::optional<std::string> {
  const IntegralConstant* constant = ConstantOf(unit, operand);
  if (constant == nullptr) {
    return std::nullopt;
  }
  const value::IntegralShape shape =
      ShapeOf(unit.types.Get(operand.type).Integral());
  return std::string(
      value::String::FromIntegral(
          value::ConstIntegralView{
              .planes =
                  value::ConstPlanes{
                      .value = constant->value_words,
                      .unknown = constant->state_words},
              .width = shape.width,
              .signedness = shape.signedness})
          .View());
}

auto FoldFormattedText(
    const CompilationUnit& unit, const Expr& operand,
    const value::FormatSpec& spec) -> std::optional<std::string> {
  const IntegralConstant* constant = ConstantOf(unit, operand);
  if (constant == nullptr) {
    return std::nullopt;
  }
  const value::IntegralShape shape =
      ShapeOf(unit.types.Get(operand.type).Integral());
  return value::FormatIntegralOperand(
      spec,
      value::ConstIntegralView{
          .planes =
              value::ConstPlanes{
                  .value = constant->value_words,
                  .unknown = constant->state_words},
          .width = shape.width,
          .signedness = shape.signedness},
      value::FormatContext{});
}

auto FoldIntegral(
    const CompilationUnit& unit, const Block& block, IntegralOp op,
    std::span<const ExprId> operands, TypeId result)
    -> std::optional<FoldedIntegral> {
  const IntegralOperation& operation = IntegralOperationOf(op);
  const std::span<const IntegralOperandKind> kinds = operation.operands.Kinds();
  if (operands.size() != kinds.size()) {
    Misapplied(
        operation, "is built with a number of operands it does not take");
  }

  std::vector<value::IntegralShape> integral;
  std::vector<value::IntegralExtent> extents;
  std::vector<value::IntegralOperand> applied;
  for (std::size_t i = 0; i < kinds.size(); ++i) {
    const Expr& operand = block.exprs.Get(operands[i]);
    std::optional<value::IntegralShape> shape;
    if (support::IsIntegralOperand(kinds[i])) {
      shape = ShapeOf(unit, operand.type);
      if (!shape) {
        Misapplied(
            operation, "is built over no integral value where it takes one");
      }
      integral.push_back(*shape);
      extents.push_back(value::ExtentOf(*shape));
    }
    if (std::optional<value::IntegralOperand> fixed =
            AppliedOperand(unit, operand, kinds[i], shape)) {
      applied.push_back(*fixed);
    }
  }
  value::RequireOperandTypes(op, integral);

  const std::optional<value::IntegralShape> answer_shape =
      value::AnswerShapeOf(op, extents, ShapeOf(unit, result));

  if (applied.size() != kinds.size()) {
    return std::nullopt;
  }
  // An operation answering a machine value writes no planes.
  IntegralConstant answer =
      answer_shape ? BlankIntegralConstant(unit.types.Get(result).Integral())
                   : IntegralConstant{};
  const value::MachineAnswer machine = value::ApplyIntegralOperation(
      op, applied,
      value::Planes{.value = answer.value_words, .unknown = answer.state_words},
      answer_shape.value_or(value::IntegralShape{}).width);
  return std::visit(
      Overloaded{
          [&](std::monostate) -> FoldedIntegral { return std::move(answer); },
          [](bool predicate) -> FoldedIntegral { return predicate; },
          [](std::int64_t number) -> FoldedIntegral { return number; }},
      machine);
}

}  // namespace lyra::mir
