#include "lyra/lowering/ast_to_hir/clocking_block.hpp"

#include <cstdint>
#include <optional>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "slang/ast/Expression.h"
#include "slang/ast/SemanticFacts.h"
#include "slang/ast/Symbol.h"
#include "slang/ast/TimingControl.h"
#include "slang/ast/expressions/MiscExpressions.h"
#include "slang/ast/expressions/OperatorExpressions.h"
#include "slang/ast/expressions/SelectExpressions.h"
#include "slang/ast/symbols/MemberSymbols.h"
#include "slang/ast/symbols/VariableSymbols.h"
#include "slang/ast/types/Type.h"

namespace lyra::lowering::ast_to_hir {

namespace {

auto BlockOf(const slang::ast::ClockVarSymbol& clockvar)
    -> const slang::ast::ClockingBlockSymbol& {
  return clockvar.getParentScope()
      ->asSymbol()
      .as<slang::ast::ClockingBlockSymbol>();
}

// The skew `skew` writes, or `none` where it writes nothing.
template <typename SkewKind>
auto SkewKindOf(const slang::ast::ClockingSkew& skew, SkewKind none)
    -> SkewKind {
  if (skew.edge != slang::ast::EdgeKind::None) {
    return EdgeSkewed{.edge = skew.edge, .delay = skew.delay};
  }
  if (skew.delay == nullptr) return none;
  if (skew.delay->kind == slang::ast::TimingControlKind::OneStepDelay) {
    return OneStepSkewed{};
  }
  return DelaySkewed{.delay = skew.delay};
}

// Whether every storage `expr` designates is a net, or every one a variable,
// through the selects and concatenations LRM 14.5 lets a signal be.
enum class Designates : std::uint8_t { kNets, kVariables, kMixed };

auto Join(Designates a, Designates b) -> Designates {
  return a == b ? a : Designates::kMixed;
}

auto DesignatedBy(const slang::ast::Expression& expr) -> Designates {
  if (const auto* named = expr.as_if<slang::ast::ValueExpressionBase>()) {
    return named->symbol.kind == slang::ast::SymbolKind::Net
               ? Designates::kNets
               : Designates::kVariables;
  }
  if (const auto* sel = expr.as_if<slang::ast::ElementSelectExpression>()) {
    return DesignatedBy(sel->value());
  }
  if (const auto* sel = expr.as_if<slang::ast::RangeSelectExpression>()) {
    return DesignatedBy(sel->value());
  }
  if (const auto* concat = expr.as_if<slang::ast::ConcatenationExpression>()) {
    std::optional<Designates> joined;
    for (const slang::ast::Expression* operand : concat->operands()) {
      const Designates one = DesignatedBy(*operand);
      joined = joined.has_value() ? Join(*joined, one) : one;
    }
    return joined.value_or(Designates::kMixed);
  }
  return Designates::kMixed;
}

// Whether `event` is an edge of one signal, which an output skew written as an
// edge is an edge of (LRM 14.3: "When the clocking event specifies a simple
// edge"), rather than a trigger of an event, a clocking block's event, a
// change of any polarity, or a list.
auto IsOneSignalsEdge(const slang::ast::TimingControl& event) -> bool {
  const auto* signal = event.as_if<slang::ast::SignalEventControl>();
  return signal != nullptr && signal->edge != slang::ast::EdgeKind::None &&
         !signal->expr.type->isEvent() &&
         signal->expr.as_if<slang::ast::ArbitrarySymbolExpression>() == nullptr;
}

}  // namespace

auto InputSkewOf(const slang::ast::ClockVarSymbol& clockvar) -> InputSkewKind {
  return SkewKindOf(
      clockvar.inputSkew.hasValue() ? clockvar.inputSkew
                                    : BlockOf(clockvar).getDefaultInputSkew(),
      InputSkewKind{OneStepSkewed{}});
}

auto OutputSkewOf(const slang::ast::ClockVarSymbol& clockvar)
    -> OutputSkewKind {
  return SkewKindOf(
      clockvar.outputSkew.hasValue() ? clockvar.outputSkew
                                     : BlockOf(clockvar).getDefaultOutputSkew(),
      OutputSkewKind{Unskewed{}});
}

auto OutputSignalOf(const slang::ast::ClockVarSymbol& clockvar)
    -> OutputSignal {
  const slang::ast::Expression* signal = clockvar.getInitializer();
  if (signal == nullptr) return UncarriedSignal{};
  switch (DesignatedBy(*signal)) {
    case Designates::kNets:
      return NetSignal{};
    case Designates::kVariables:
      break;
    case Designates::kMixed:
      return UncarriedSignal{};
  }
  // The variable signal carried is one variable this scope declares, named
  // whole.
  const auto* named = signal->as_if<slang::ast::NamedValueExpression>();
  if (named == nullptr) return UncarriedSignal{};
  const auto* variable = named->symbol.as_if<slang::ast::VariableSymbol>();
  if (variable == nullptr ||
      variable->getParentScope() != BlockOf(clockvar).getParentScope()) {
    return UncarriedSignal{};
  }
  return VariableSignal{.variable = variable};
}

auto DrivingVariableNamingOf(const slang::ast::ClockVarSymbol& clockvar)
    -> DrivingVariableNaming {
  return std::visit(
      Overloaded{
          [&](const NetSignal&) {
            return DrivingVariableNaming{
                .name = ClockingDrivingVariableName(clockvar.name),
                .held_by_scope = false};
          },
          [](const VariableSignal& signal) {
            return DrivingVariableNaming{
                .name = ClockingDrivingVariableName(signal.variable->name),
                .held_by_scope = true};
          },
          [](const UncarriedSignal&) -> DrivingVariableNaming {
            throw InternalError(
                "DrivingVariableNamingOf: an output not carried holds no "
                "variable of the block");
          }},
      OutputSignalOf(clockvar));
}

auto InputRefusal(const slang::ast::ClockVarSymbol& clockvar)
    -> std::optional<base::DiagMessage> {
  if (std::holds_alternative<EdgeSkewed>(InputSkewOf(clockvar))) {
    return base::DiagMessage(
        "an input skew written as an edge is not yet supported");
  }
  return std::nullopt;
}

auto OutputRefusal(const slang::ast::ClockVarSymbol& clockvar)
    -> std::optional<base::DiagMessage> {
  const OutputSignal signal = OutputSignalOf(clockvar);
  if (std::holds_alternative<UncarriedSignal>(signal)) {
    return base::DiagMessage(
        "a clocking output whose signal is not nets alone or one variable "
        "of the scope declaring the block is not yet supported");
  }
  if (std::holds_alternative<NetSignal>(signal) &&
      (!clockvar.getType().isIntegral() || !clockvar.getType().isFourState())) {
    return base::DiagMessage(
        "a clocking output driving a net of a type other than a 4-state "
        "packed vector is not yet supported");
  }
  const OutputSkewKind skew = OutputSkewOf(clockvar);
  if (std::holds_alternative<OneStepSkewed>(skew)) {
    return base::DiagMessage(
        "a clocking output skew of '#1step' is not yet supported");
  }
  if (std::holds_alternative<EdgeSkewed>(skew) &&
      !IsOneSignalsEdge(BlockOf(clockvar).getEvent())) {
    return base::DiagMessage(
        "an output skew written as an edge, where the clocking event is not "
        "an edge of one signal, is not yet supported");
  }
  return std::nullopt;
}

auto IsClockingInput(const slang::ast::ClockVarSymbol& clockvar) -> bool {
  return clockvar.direction != slang::ast::ArgumentDirection::Out;
}

auto IsClockingOutput(const slang::ast::ClockVarSymbol& clockvar) -> bool {
  return clockvar.direction != slang::ast::ArgumentDirection::In;
}

}  // namespace lyra::lowering::ast_to_hir
