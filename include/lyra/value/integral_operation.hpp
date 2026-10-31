#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// How many positions a value of an integral type has and which states each can
// hold, which is all of a type that laying a value of it out reads.
struct IntegralExtent {
  std::uint64_t width = 0;
  StateDomain domain = StateDomain::kTwoState;

  auto operator==(const IntegralExtent&) const -> bool = default;
};

[[nodiscard]] constexpr auto ExtentOf(const IntegralShape& shape)
    -> IntegralExtent {
  return IntegralExtent{.width = shape.width, .domain = shape.domain};
}

// One operand as an operation is applied to it: an integral value, or the
// machine value, text or foreign buffer the operation is declared to take
// there.
using IntegralOperand = std::variant<
    ConstIntegralView, std::int64_t, bool, std::uint8_t, std::string_view,
    const svBitVecVal*, const svLogicVecVal*>;

// An integral answer of the type the call states, and an answer that is a
// machine predicate or number.
struct AnswerOfTheCall {};
struct MachineBoolAnswer {};
struct MachineIntAnswer {};

// The type `op` answers at: the extent the operation fixes, given its integral
// operands' extents in order, or which of the other three it is.
using IntegralAnswerType = std::variant<
    IntegralExtent, AnswerOfTheCall, MachineBoolAnswer, MachineIntAnswer>;

// Ends the compiler or the run over an application of `op` that whatever built
// it put together against another operation's declaration.
[[noreturn]] void MisappliedIntegralOperation(
    support::IntegralOp op, std::string_view what);

// The answer type of `op`, with the operation known where the caller is
// compiled, so only its own rule is compiled in.
template <support::IntegralOp op>
[[nodiscard]] auto AnswerTypeOf(std::span<const IntegralExtent> operands)
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
      MisappliedIntegralOperation(
          op, "answers at its first operand's type and is handed none");
    }
    return operands.front();
  };
  switch (support::IntegralOperationOf(op).answer) {
    case support::IntegralAnswer::kOfFirstOperand:
      return first();
    case support::IntegralAnswer::kOneBit:
      return IntegralExtent{.width = 1, .domain = any_four_state()};
    case support::IntegralAnswer::kTwoStateBit:
      return ExtentOf(kShapeOf<Bit>);
    case support::IntegralAnswer::kInt:
      return ExtentOf(kShapeOf<Int>);
    case support::IntegralAnswer::kInteger:
      return ExtentOf(kShapeOf<Integer>);
    case support::IntegralAnswer::kPosition:
      return ExtentOf(kShapeOf<Position>);
    case support::IntegralAnswer::kOfTheCall:
      return AnswerOfTheCall{};
    case support::IntegralAnswer::kMachineBool:
      return MachineBoolAnswer{};
    case support::IntegralAnswer::kMachineInt:
      return MachineIntAnswer{};
  }
  throw InternalError("unknown integral answer");
}

// The same answer with the operation a value of the run.
[[nodiscard]] auto AnswerTypeOf(
    support::IntegralOp op, std::span<const IntegralExtent> integral_operands)
    -> IntegralAnswerType;

// The integral type an application of `op` answers at, given its integral
// operands' extents in order and the integral type it is stated at, if it is
// stated at one: that type, or none where the operation answers a machine
// value. An application stated at a type the operation does not answer at is a
// defect in whatever built it.
[[nodiscard]] auto AnswerShapeOf(
    support::IntegralOp op, std::span<const IntegralExtent> integral_operands,
    std::optional<IntegralShape> stated) -> std::optional<IntegralShape>;

// Refuses a call that hands `op` integral operands of types the operation does
// not take, given those types in order. A call hands as many as the operation
// takes; an operand the operation declares to be of the first operand's type is
// as wide as the first and holds the same states, and has its signedness too
// where the operation reads a number. The front end gives an operator operands
// of its own type (LRM 11.6.1), so a call that does not is a defect in whatever
// built it.
void RequireOperandTypes(
    support::IntegralOp op, std::span<const IntegralShape> integral_operands);

// What an operation answers with where its answer is a machine value.
using MachineAnswer = std::variant<std::monostate, bool, std::int64_t>;

// Applies `op` to the operands `to` holds. An integral answer is written
// through `answer`; a machine one is what the call answers. This is every
// operation's one statement over values of any width, entered with the
// operation known where the caller is compiled, so only that operation's arm
// is compiled in. `to.At(i)` answers the integral value handed as operand `i`,
// and `to.MachineAt<T>(i)` the machine value, text or foreign buffer handed
// there.
template <support::IntegralOp op, typename Operands>
auto ApplyIntegralOperation(const Operands& to, const IntegralView& answer)
    -> MachineAnswer {
  using support::IntegralOp;
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
      return one_bit(detail::ScalarOf(CaseEqual(to.At(0), to.At(1))));
    case IntegralOp::kWildcardEqual:
      return one_bit(WildcardEqual(to.At(0), to.At(1)));
    case IntegralOp::kCasezMatch:
      return one_bit(detail::ScalarOf(CasezMatch(to.At(0), to.At(1))));
    case IntegralOp::kCasexMatch:
      return one_bit(detail::ScalarOf(CasexMatch(to.At(0), to.At(1))));
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
    case IntegralOp::kTakePositions:
      TakePositions(
          answer, to.At(0), to.At(1),
          BitPositions{
              .lsb = static_cast<std::uint64_t>(
                  to.template MachineAt<std::int64_t>(2)),
              .width = static_cast<std::uint64_t>(
                  to.template MachineAt<std::int64_t>(3))});
      return {};
    case IntegralOp::kConvert:
      Convert(answer, to.At(0));
      return {};
    case IntegralOp::kFromInt:
      return number(to.template MachineAt<std::int64_t>(0));
    case IntegralOp::kFromBool:
      return number(to.template MachineAt<bool>(0) ? 1 : 0);
    case IntegralOp::kToInt64:
      return ToInt64(to.At(0));
    case IntegralOp::kToPosition:
      ToPosition(answer, to.At(0));
      return {};
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
      const auto block = to.template MachineAt<std::int64_t>(1);
      if (block <= 0) {
        MisappliedIntegralOperation(
            op,
            "is handed a block size that is not positive, which the front "
            "end has already refused");
      }
      ReverseBlocks(answer, to.At(0), static_cast<std::uint64_t>(block));
      return {};
    }
    case IntegralOp::kFromText: {
      const auto text = to.template MachineAt<std::string_view>(0);
      FromBytes(answer, std::span<const char>{text.data(), text.size()});
      return {};
    }
    case IntegralOp::kReadCanonicalBits:
      ReadCanonicalBitVec(to.template MachineAt<const svBitVecVal*>(0), answer);
      return {};
    case IntegralOp::kReadCanonicalLogic:
      ReadCanonicalLogicVec(
          to.template MachineAt<const svLogicVecVal*>(0), answer);
      return {};
    case IntegralOp::kFromSvLogic:
      FromSvLogic(to.template MachineAt<std::uint8_t>(0), answer);
      return {};
  }
  throw InternalError("unknown integral operation");
}

// The same statement entered with the operation a value of the run, which is
// how the compiler evaluating an operation over constants arrives.
auto ApplyIntegralOperation(
    support::IntegralOp op, std::span<const IntegralOperand> operands,
    const IntegralView& answer) -> MachineAnswer;

}  // namespace lyra::value
