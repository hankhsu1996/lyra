#include "lyra/value/integral_operation.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

namespace {

using detail::ScalarOf;
using support::IntegralAnswer;
using support::IntegralOp;
using support::IntegralOperandKind;
using support::IntegralOperation;
using support::IntegralOperationOf;

[[noreturn]] void Misapplied(IntegralOp op, std::string_view what) {
  throw InternalError(
      std::format(
          "the integral operation {} {}", IntegralOperationOf(op).name, what));
}

// The operands of one application, each read as the operation's arm asks for
// it. An operand of another kind than the arm asks for is a call that was put
// together against another operation's declaration.
class Applied {
 public:
  Applied(IntegralOp op, std::span<const IntegralOperand> operands)
      : op_(op), operands_(operands) {
    if (operands.size() != IntegralOperationOf(op).operands.Kinds().size()) {
      Misapplied(op, "is applied to a number of operands it does not take");
    }
  }

  [[nodiscard]] auto At(std::size_t at) const -> const ConstIntegralView& {
    const auto* value = std::get_if<ConstIntegralView>(&operands_[at]);
    if (value == nullptr) {
      Misapplied(op_, "is handed no integral value where it takes one");
    }
    return *value;
  }

  template <typename T>
  [[nodiscard]] auto MachineAt(std::size_t at) const -> T {
    const T* value = std::get_if<T>(&operands_[at]);
    if (value == nullptr) {
      Misapplied(op_, "is handed an operand of another kind than it takes");
    }
    return *value;
  }

 private:
  IntegralOp op_;
  std::span<const IntegralOperand> operands_;
};

}  // namespace

auto AnswerTypeOf(IntegralOp op, std::span<const IntegralExtent> operands)
    -> IntegralAnswerType {
  const auto any_four_state = [&] {
    StateDomain domain = StateDomain::kTwoState;
    for (const IntegralExtent& operand : operands) {
      domain = detail::CombinedDomain(domain, operand.domain);
    }
    return domain;
  };
  const auto first = [&]() -> const IntegralExtent& {
    if (operands.empty()) {
      Misapplied(op, "answers at its first operand's type and is handed none");
    }
    return operands.front();
  };
  switch (IntegralOperationOf(op).answer) {
    case IntegralAnswer::kOfFirstOperand:
      return first();
    case IntegralAnswer::kOneBit:
      return IntegralExtent{.width = 1, .domain = any_four_state()};
    case IntegralAnswer::kTwoStateBit:
      return ExtentOf(kShapeOf<Bit>);
    case IntegralAnswer::kJoined: {
      std::uint64_t width = 0;
      for (const IntegralExtent& operand : operands) {
        width += operand.width;
      }
      return IntegralExtent{.width = width, .domain = any_four_state()};
    }
    case IntegralAnswer::kInt:
      return ExtentOf(kShapeOf<Int>);
    case IntegralAnswer::kInteger:
      return ExtentOf(kShapeOf<Integer>);
    case IntegralAnswer::kPosition:
      return ExtentOf(kShapeOf<Position>);
    case IntegralAnswer::kOfTheCall:
      return AnswerOfTheCall{};
    case IntegralAnswer::kMachineBool:
      return MachineBoolAnswer{};
    case IntegralAnswer::kMachineInt:
      return MachineIntAnswer{};
  }
  throw InternalError("unknown integral answer");
}

auto AnswerShapeOf(
    IntegralOp op, std::span<const IntegralExtent> integral_operands,
    std::optional<IntegralShape> stated) -> std::optional<IntegralShape> {
  using Shape = std::optional<IntegralShape>;
  return std::visit(
      Overloaded{
          [&](const IntegralExtent& fixed) -> Shape {
            if (!stated.has_value() || ExtentOf(*stated) != fixed) {
              Misapplied(
                  op,
                  "is stated at an answer type other than the one the "
                  "operation answers at");
            }
            return stated;
          },
          [&](const AnswerOfTheCall&) -> Shape {
            if (!stated.has_value()) {
              Misapplied(
                  op,
                  "answers an integral value and is stated at no integral "
                  "type");
            }
            return stated;
          },
          [](const MachineBoolAnswer&) -> Shape { return std::nullopt; },
          [](const MachineIntAnswer&) -> Shape { return std::nullopt; }},
      AnswerTypeOf(op, integral_operands));
}

void RequireOperandTypes(
    IntegralOp op, std::span<const IntegralShape> integral_operands) {
  const IntegralOperation& operation = IntegralOperationOf(op);
  std::size_t at = 0;
  for (const IntegralOperandKind kind : operation.operands.Kinds()) {
    if (!support::IsIntegralOperand(kind)) {
      continue;
    }
    if (at == integral_operands.size()) {
      Misapplied(op, "is handed fewer integral operands than it takes");
    }
    const IntegralShape& handed = integral_operands[at++];
    switch (kind) {
      case IntegralOperandKind::kSameType: {
        const IntegralShape& first = integral_operands.front();
        const bool read_as_a_number =
            support::IsNumberOperand(operation.operands.Kinds().front());
        if (ExtentOf(handed) != ExtentOf(first) ||
            (read_as_a_number && handed.signedness != first.signedness)) {
          Misapplied(
              op, "takes operands of one type and is handed two that differ");
        }
        break;
      }
      case IntegralOperandKind::kBits:
      case IntegralOperandKind::kNumber:
      case IntegralOperandKind::kMachineInt:
      case IntegralOperandKind::kMachineBool:
      case IntegralOperandKind::kSvLogic:
      case IntegralOperandKind::kText:
      case IntegralOperandKind::kCanonicalBits:
      case IntegralOperandKind::kCanonicalLogic:
        break;
    }
  }
  if (at != integral_operands.size()) {
    Misapplied(op, "is handed more integral operands than it takes");
  }
}

auto ApplyIntegralOperation(
    IntegralOp op, std::span<const IntegralOperand> operands,
    const IntegralView& answer) -> MachineAnswer {
  const Applied to(op, operands);
  const auto one_bit = [&](FourStateBit bit) -> MachineAnswer {
    FillScalar(answer, bit);
    return {};
  };
  const auto number = [&](std::int64_t value) -> MachineAnswer {
    FromInt(answer, value);
    return {};
  };
  const auto reduced = [&](ReductionOp reduction) {
    return one_bit(Reduce(to.At(0), reduction));
  };
  const auto resolved = [&](NetResolution fold) -> MachineAnswer {
    Resolve(answer, to.At(0), to.At(1), fold);
    return {};
  };
  switch (op) {
    case IntegralOp::kAdd:
      Add(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kSubtract:
      Subtract(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kMultiply:
      Multiply(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kDivide:
      Divide(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kModulo:
      Modulo(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kNegate:
      Negate(answer, to.At(0));
      return {};
    case IntegralOp::kPower:
      Power(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kBitwiseAnd:
      BitwiseAnd(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kBitwiseOr:
      BitwiseOr(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kBitwiseXor:
      BitwiseXor(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kBitwiseXnor:
      BitwiseXnor(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kBitwiseNot:
      BitwiseNot(answer, to.At(0));
      return {};
    case IntegralOp::kEqual:
      return one_bit(Equal(to.At(0), to.At(1)));
    case IntegralOp::kNotEqual:
      return one_bit(NotEqual(to.At(0), to.At(1)));
    case IntegralOp::kLess:
      return one_bit(Less(to.At(0), to.At(1)));
    case IntegralOp::kLessEqual:
      return one_bit(LessEqual(to.At(0), to.At(1)));
    case IntegralOp::kGreater:
      return one_bit(Greater(to.At(0), to.At(1)));
    case IntegralOp::kGreaterEqual:
      return one_bit(GreaterEqual(to.At(0), to.At(1)));
    case IntegralOp::kCaseEqual:
      return one_bit(ScalarOf(CaseEqual(to.At(0), to.At(1))));
    case IntegralOp::kWildcardEqual:
      return one_bit(WildcardEqual(to.At(0), to.At(1)));
    case IntegralOp::kCasezMatch:
      return one_bit(ScalarOf(CasezMatch(to.At(0), to.At(1))));
    case IntegralOp::kCasexMatch:
      return one_bit(ScalarOf(CasexMatch(to.At(0), to.At(1))));
    case IntegralOp::kLogicalAnd:
      return one_bit(LogicalAnd(to.At(0), to.At(1)));
    case IntegralOp::kLogicalOr:
      return one_bit(LogicalOr(to.At(0), to.At(1)));
    case IntegralOp::kLogicalNot:
      return one_bit(LogicalNot(to.At(0)));
    case IntegralOp::kLogicalEquivalence:
      return one_bit(LogicalEquivalence(to.At(0), to.At(1)));
    case IntegralOp::kIsTrue:
      return Truth(to.At(0)) == Truthiness::kKnownNonzero;
    case IntegralOp::kReductionAnd:
      return reduced(ReductionOp::kAnd);
    case IntegralOp::kReductionOr:
      return reduced(ReductionOp::kOr);
    case IntegralOp::kReductionXor:
      return reduced(ReductionOp::kXor);
    case IntegralOp::kReductionNand:
      return reduced(ReductionOp::kNand);
    case IntegralOp::kReductionNor:
      return reduced(ReductionOp::kNor);
    case IntegralOp::kReductionXnor:
      return reduced(ReductionOp::kXnor);
    case IntegralOp::kShiftLeft:
      ShiftLeft(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kLogicalShiftRight:
      LogicalShiftRight(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kArithmeticShiftRight:
      ArithmeticShiftRight(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kConcat:
      Concat(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kReplicate:
      Replicate(answer, to.At(0));
      return {};
    case IntegralOp::kSlice:
      Slice(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kWithSlice:
      detail::Copy(to.At(0).value, answer.value);
      detail::Copy(to.At(0).unknown, answer.unknown);
      WithSlice(answer, to.At(1), to.At(2));
      return {};
    case IntegralOp::kConvert:
      Convert(answer, to.At(0));
      return {};
    case IntegralOp::kFromInt:
      return number(to.MachineAt<std::int64_t>(0));
    case IntegralOp::kFromBool:
      return number(to.MachineAt<bool>(0) ? 1 : 0);
    case IntegralOp::kToInt64:
      return ToInt64(to.At(0));
    case IntegralOp::kToPosition:
      ToPosition(answer, to.At(0));
      return {};
    case IntegralOp::kIsUnknown:
      return one_bit(ScalarOf(HasUnknown(to.At(0))));
    case IntegralOp::kHasUnknown:
      return HasUnknown(to.At(0));
    case IntegralOp::kBitIdentical:
      return CaseEqual(to.At(0), to.At(1));
    case IntegralOp::kCountBits:
      return number(CountBits(to.At(0), to.At(1)));
    case IntegralOp::kCeilLog2:
      return number(CeilLog2(to.At(0)));
    case IntegralOp::kMergeConditional:
      MergeConditional(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kResolveTriState:
      return resolved(NetResolution::kTriState);
    case IntegralOp::kResolveWiredAnd:
      return resolved(NetResolution::kWiredAnd);
    case IntegralOp::kResolveWiredOr:
      return resolved(NetResolution::kWiredOr);
    case IntegralOp::kDominate:
      Dominate(answer, to.At(0), to.At(1));
      return {};
    case IntegralOp::kReverseBlocks: {
      const auto block = to.MachineAt<std::int64_t>(1);
      if (block <= 0) {
        Misapplied(
            op,
            "is handed a block size that is not positive, which the front "
            "end has already refused");
      }
      ReverseBlocks(answer, to.At(0), static_cast<std::uint64_t>(block));
      return {};
    }
    case IntegralOp::kFromText: {
      const auto text = to.MachineAt<std::string_view>(0);
      FromBytes(answer, std::span<const char>{text.data(), text.size()});
      return {};
    }
    case IntegralOp::kReadCanonicalBits:
      ReadCanonicalBitVec(to.MachineAt<const svBitVecVal*>(0), answer);
      return {};
    case IntegralOp::kReadCanonicalLogic:
      ReadCanonicalLogicVec(to.MachineAt<const svLogicVecVal*>(0), answer);
      return {};
    case IntegralOp::kFromSvLogic:
      FromSvLogic(to.MachineAt<std::uint8_t>(0), answer);
      return {};
  }
  throw InternalError("unknown integral operation");
}

}  // namespace lyra::value
