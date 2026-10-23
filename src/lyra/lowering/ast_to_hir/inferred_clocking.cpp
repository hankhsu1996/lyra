#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"

#include <algorithm>
#include <expected>
#include <iterator>
#include <ranges>
#include <utility>
#include <vector>

#include <slang/analysis/ClockInference.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/parsing/KnownSystemName.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using ClockEntries = std::vector<const slang::ast::SignalEventControl*>;

}  // namespace

auto DefaultClockingEvent(const slang::ast::Scope& scope)
    -> const slang::ast::TimingControl* {
  const slang::ast::Symbol* clocking =
      scope.getCompilation().getDefaultClocking(scope);
  if (clocking == nullptr) {
    return nullptr;
  }
  return &clocking->as<slang::ast::ClockingBlockSymbol>().getEvent();
}

auto ResolvedClockEntries(
    const WalkFrame& frame, const slang::ast::TimingControl& written,
    diag::SourceSpan span) -> diag::Result<ClockEntries> {
  if (written.kind == slang::ast::TimingControlKind::EventList) {
    ClockEntries entries;
    for (const slang::ast::TimingControl* event :
         written.as<slang::ast::EventListControl>().events) {
      auto resolved = ResolvedClockEntries(frame, *event, span);
      if (!resolved) return resolved;
      std::ranges::copy(*resolved, std::back_inserter(entries));
    }
    return entries;
  }
  if (written.kind != slang::ast::TimingControlKind::SignalEvent) {
    throw InternalError(
        "ResolvedClockEntries: a clocking event reached lowering that is "
        "neither an event nor a list of them");
  }
  const auto& event = written.as<slang::ast::SignalEventControl>();
  if (!slang::analysis::ClockInference::isInferredClockCall(event.expr)) {
    return ClockEntries{&event};
  }
  // The call is bound in the declaration whose formal it is the default of,
  // which is how the instance it stands for is found among those enclosing it.
  const slang::ast::Symbol& declaration =
      std::get<slang::ast::CallExpression::SystemCallInfo>(
          event.expr.as<slang::ast::CallExpression>().subroutine)
          .scope->asSymbol();
  const auto innermost_first = frame.expansion_stack | std::views::reverse;
  const auto instance = std::ranges::find(
      innermost_first, &declaration, &ExpansionInstance::declaration);
  if (instance == innermost_first.end()) {
    throw InternalError(
        "ResolvedClockEntries: an '$inferred_clock' stood outside every "
        "instance of the declaration whose formal it defaults");
  }
  if (instance->clock == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTimingControlKind,
        "this '$inferred_clock' names no clocking event, because none is in "
        "force where its sequence or property is instantiated");
  }
  return ResolvedClockEntries(frame, *instance->clock, span);
}

auto IsSameClock(
    const WalkFrame& frame, const slang::ast::TimingControl& first,
    const slang::ast::TimingControl& second, diag::SourceSpan span)
    -> diag::Result<bool> {
  auto first_entries = ResolvedClockEntries(frame, first, span);
  if (!first_entries) return std::unexpected(std::move(first_entries.error()));
  auto second_entries = ResolvedClockEntries(frame, second, span);
  if (!second_entries) {
    return std::unexpected(std::move(second_entries.error()));
  }
  const auto covers = [](const ClockEntries& outer, const ClockEntries& inner) {
    return std::ranges::all_of(inner, [&](const auto* entry) {
      return std::ranges::any_of(outer, [&](const auto* other) {
        return other->isEquivalentTo(*entry);
      });
    });
  };
  return covers(*first_entries, *second_entries) &&
         covers(*second_entries, *first_entries);
}

auto DefaultDisableCondition(const slang::ast::Scope& scope)
    -> const slang::ast::Expression* {
  return scope.getCompilation().getDefaultDisable(scope);
}

auto ResolveWrittenDisable(
    const slang::ast::Scope& assertion_scope,
    const slang::ast::Expression& written) -> const slang::ast::Expression* {
  if (written.kind != slang::ast::ExpressionKind::Call) {
    return &written;
  }
  const auto& call = written.as<slang::ast::CallExpression>();
  if (call.getKnownSystemName() !=
      slang::parsing::KnownSystemName::InferredDisable) {
    return &written;
  }
  return DefaultDisableCondition(assertion_scope);
}

}  // namespace lyra::lowering::ast_to_hir
