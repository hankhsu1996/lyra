#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <iterator>
#include <ranges>
#include <utility>
#include <variant>
#include <vector>

#include <slang/analysis/ClockInference.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/parsing/KnownSystemName.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The scope a name declared beside `declared` stands in: its own, or the one
// holding the clocking block it is declared in, which declares no storage of
// its own a name could stand for.
auto ScopeBeside(const slang::ast::Symbol& declared)
    -> const slang::ast::Scope* {
  const slang::ast::Scope* scope = declared.getParentScope();
  if (scope != nullptr &&
      scope->asSymbol().kind == slang::ast::SymbolKind::ClockingBlock) {
    return scope->asSymbol().getParentScope();
  }
  return scope;
}

}  // namespace

auto ExpansionDeclaringBeside(
    const WalkFrame& frame, const slang::ast::Symbol& declared)
    -> const ExpansionInstance* {
  for (const ExpansionInstance& instance :
       std::views::reverse(frame.expansion_stack)) {
    if (instance.reached_through == nullptr ||
        &declared == instance.declaration ||
        ScopeBeside(declared) != ScopeBeside(*instance.declaration)) {
      continue;
    }
    return &instance;
  }
  return nullptr;
}

auto ClockingEventsOf(const slang::ast::ClockingBlockSymbol& block)
    -> std::vector<const slang::ast::SignalEventControl*> {
  const slang::ast::TimingControl& event = block.getEvent();
  if (event.kind == slang::ast::TimingControlKind::SignalEvent) {
    return {&event.as<slang::ast::SignalEventControl>()};
  }
  if (event.kind != slang::ast::TimingControlKind::EventList) {
    throw InternalError(
        "ClockingEventsOf: a clocking event is an event or a list of them "
        "(LRM 14.3)");
  }
  std::vector<const slang::ast::SignalEventControl*> events;
  for (const slang::ast::TimingControl* entry :
       event.as<slang::ast::EventListControl>().events) {
    if (entry->kind != slang::ast::TimingControlKind::SignalEvent) {
      throw InternalError(
          "ClockingEventsOf: an entry of a clocking event's list is an event "
          "(LRM 9.4.2.1)");
    }
    events.push_back(&entry->as<slang::ast::SignalEventControl>());
  }
  return events;
}

auto DefaultClockingEvent(const slang::ast::Scope& scope)
    -> const slang::ast::TimingControl* {
  const slang::ast::Symbol* clocking =
      scope.getCompilation().getDefaultClocking(scope);
  if (clocking == nullptr) {
    return nullptr;
  }
  return &clocking->as<slang::ast::ClockingBlockSymbol>().getEvent();
}

namespace {

auto WrittenClockEntries(
    const WalkFrame& frame, const slang::ast::TimingControl& written,
    diag::SourceSpan span) -> diag::Result<std::vector<ClockEntry>> {
  if (written.kind == slang::ast::TimingControlKind::EventList) {
    std::vector<ClockEntry> entries;
    for (const slang::ast::TimingControl* event :
         written.as<slang::ast::EventListControl>().events) {
      auto resolved = WrittenClockEntries(frame, *event, span);
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
    return std::vector<ClockEntry>{&event};
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
  if (!instance->clock.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTimingControlKind,
        "this '$inferred_clock' names no clocking event, because none is in "
        "force where its sequence or property is instantiated");
  }
  return ResolvedClockEntries(frame, *instance->clock, span);
}

// Whether two names reach one instance: the same steps to the scope holding
// what each names last.
auto ReachOneInstance(
    const slang::ast::HierarchicalReference& first,
    const slang::ast::HierarchicalReference& second) -> bool {
  if (first.path.size() != second.path.size() || first.path.empty()) {
    return false;
  }
  for (std::size_t at = 0; at + 1 < first.path.size(); ++at) {
    if (first.path[at].symbol != second.path[at].symbol ||
        first.path[at].selector != second.path[at].selector) {
      return false;
    }
  }
  return true;
}

auto IsSameEntry(const ClockEntry& first, const ClockEntry& second) -> bool {
  return std::visit(
      Overloaded{
          [](const slang::ast::SignalEventControl* written,
             const slang::ast::SignalEventControl* other) {
            return written->isEquivalentTo(*other);
          },
          [](const ClockOfAnotherInstance& published,
             const ClockOfAnotherInstance& other) {
            return published.block == other.block &&
                   ReachOneInstance(
                       *published.reached_through, *other.reached_through);
          },
          [](const auto&, const auto&) { return false; }},
      first, second);
}

}  // namespace

auto ResolvedClockEntries(
    const WalkFrame& frame, const ClockingEvent& clock, diag::SourceSpan span)
    -> diag::Result<std::vector<ClockEntry>> {
  return std::visit(
      Overloaded{
          [&](const slang::ast::TimingControl* written)
              -> diag::Result<std::vector<ClockEntry>> {
            return WrittenClockEntries(frame, *written, span);
          },
          [](const ClockOfAnotherInstance& published)
              -> diag::Result<std::vector<ClockEntry>> {
            return std::vector<ClockEntry>{published};
          }},
      clock);
}

auto IsSameClock(
    const WalkFrame& frame, const ClockingEvent& first,
    const ClockingEvent& second, diag::SourceSpan span) -> diag::Result<bool> {
  auto first_entries = ResolvedClockEntries(frame, first, span);
  if (!first_entries) return std::unexpected(std::move(first_entries.error()));
  auto second_entries = ResolvedClockEntries(frame, second, span);
  if (!second_entries) {
    return std::unexpected(std::move(second_entries.error()));
  }
  const auto covers = [](const std::vector<ClockEntry>& outer,
                         const std::vector<ClockEntry>& inner) {
    return std::ranges::all_of(inner, [&](const ClockEntry& entry) {
      return std::ranges::any_of(outer, [&](const ClockEntry& other) {
        return IsSameEntry(other, entry);
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
