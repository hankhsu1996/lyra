#include "lyra/value/integral_operation.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

namespace {

using support::IntegralOp;
using support::IntegralOperandKind;
using support::IntegralOperation;
using support::IntegralOperationOf;

// The operands of one application, each read as the operation's arm asks for
// it. An operand of another kind than the arm asks for is a call that was put
// together against another operation's declaration.
class Applied {
 public:
  Applied(IntegralOp op, std::span<const IntegralOperand> operands)
      : op_(op), operands_(operands) {
    if (operands.size() != IntegralOperationOf(op).operands.Kinds().size()) {
      MisappliedIntegralOperation(
          op, "is applied to a number of operands it does not take");
    }
  }

  [[nodiscard]] auto At(std::size_t at) const -> const ConstIntegralView& {
    const auto* value = std::get_if<ConstIntegralView>(&operands_[at]);
    if (value == nullptr) {
      MisappliedIntegralOperation(
          op_, "is handed no integral value where it takes one");
    }
    return *value;
  }

  template <typename T>
  [[nodiscard]] auto MachineAt(std::size_t at) const -> T {
    const T* value = std::get_if<T>(&operands_[at]);
    if (value == nullptr) {
      MisappliedIntegralOperation(
          op_, "is handed an operand of another kind than it takes");
    }
    return *value;
  }

 private:
  IntegralOp op_;
  std::span<const IntegralOperand> operands_;
};

// The statement of the operation at each row, entered by the row's position.
using Application = MachineAnswer (*)(const Applied&, const IntegralView&);

template <std::size_t... row>
constexpr auto ApplicationsAt(std::index_sequence<row...>) {
  return std::array<Application, sizeof...(row)>{&ApplyIntegralOperation<
      support::IntegralOperations()[row].op, Applied>...};
}

constexpr auto kApplications = ApplicationsAt(
    std::make_index_sequence<support::IntegralOperations().size()>());

// The answer rule of the operation at each row, likewise.
using AnswerRule = IntegralAnswerType (*)(std::span<const IntegralExtent>);

template <std::size_t... row>
constexpr auto AnswerRulesAt(std::index_sequence<row...>) {
  return std::array<AnswerRule, sizeof...(row)>{
      &AnswerTypeOf<support::IntegralOperations()[row].op>...};
}

constexpr auto kAnswerRules = AnswerRulesAt(
    std::make_index_sequence<support::IntegralOperations().size()>());

}  // namespace

auto AnswerTypeOf(
    IntegralOp op, std::span<const IntegralExtent> integral_operands)
    -> IntegralAnswerType {
  return kAnswerRules.at(std::to_underlying(op))(integral_operands);
}

void MisappliedIntegralOperation(IntegralOp op, std::string_view what) {
  throw InternalError(
      std::format(
          "the integral operation {} {}", IntegralOperationOf(op).name, what));
}

auto AnswerShapeOf(
    IntegralOp op, std::span<const IntegralExtent> integral_operands,
    std::optional<IntegralShape> stated) -> std::optional<IntegralShape> {
  using Shape = std::optional<IntegralShape>;
  return std::visit(
      Overloaded{
          [&](const IntegralExtent& fixed) -> Shape {
            if (!stated.has_value() || ExtentOf(*stated) != fixed) {
              MisappliedIntegralOperation(
                  op,
                  "is stated at an answer type other than the one the "
                  "operation answers at");
            }
            return stated;
          },
          [&](const AnswerOfTheCall&) -> Shape {
            if (!stated.has_value()) {
              MisappliedIntegralOperation(
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
      MisappliedIntegralOperation(
          op, "is handed fewer integral operands than it takes");
    }
    const IntegralShape& handed = integral_operands[at++];
    switch (kind) {
      case IntegralOperandKind::kSameType: {
        const IntegralShape& first = integral_operands.front();
        const bool read_as_a_number =
            support::IsNumberOperand(operation.operands.Kinds().front());
        if (ExtentOf(handed) != ExtentOf(first) ||
            (read_as_a_number && handed.signedness != first.signedness)) {
          MisappliedIntegralOperation(
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
    MisappliedIntegralOperation(
        op, "is handed more integral operands than it takes");
  }
}

auto ApplyIntegralOperation(
    IntegralOp op, std::span<const IntegralOperand> operands,
    const IntegralView& answer) -> MachineAnswer {
  return kApplications.at(std::to_underlying(op))(
      Applied(op, operands), answer);
}

}  // namespace lyra::value
