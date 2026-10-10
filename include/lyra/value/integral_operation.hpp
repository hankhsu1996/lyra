#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

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

// An integral value an operation reads the bits of, and one it reads as a
// number.
struct BitsOperand {
  ConstPlanes planes;
  std::uint64_t width = 0;
};
struct NumberOperand {
  ConstPlanes planes;
  std::uint64_t width = 0;
  Signedness signedness = Signedness::kUnsigned;
};

// One operand as an operation is applied to it: an integral value, or the
// machine value, text or foreign buffer the operation is declared to take
// there.
using IntegralOperand = std::variant<
    BitsOperand, NumberOperand, std::int64_t, bool, std::uint8_t,
    std::string_view, const svBitVecVal*, const svLogicVecVal*>;

// An integral answer of the type the call states, and an answer that is a
// machine predicate or number.
struct AnswerOfTheCall {};
struct MachineBoolAnswer {};
struct MachineIntAnswer {};

// The type `op` answers at: the extent the operation fixes, given its integral
// operands' extents in order, or which of the other three it is.
using IntegralAnswerType = std::variant<
    IntegralExtent, AnswerOfTheCall, MachineBoolAnswer, MachineIntAnswer>;

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

// Applies `op` to `operands`. An integral answer is written into `answer`,
// whose planes are of `answer_width` bits; a machine one is what the call
// answers. This is every operation's one statement over planes of any width:
// the compiler evaluating an operation over constants and the library carrying
// one out for a type it was compiled without both arrive here.
auto ApplyIntegralOperation(
    support::IntegralOp op, std::span<const IntegralOperand> operands,
    Planes answer, std::uint64_t answer_width) -> MachineAnswer;

}  // namespace lyra::value
