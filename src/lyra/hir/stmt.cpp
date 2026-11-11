#include "lyra/hir/stmt.hpp"

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"

namespace lyra::hir {

auto ConcurrentAssertionOf(const StmtData& data)
    -> std::optional<ConcurrentAssertion> {
  return std::visit(
      Overloaded{
          [](const ConcurrentAssertStmt& s)
              -> std::optional<ConcurrentAssertion> {
            return ConcurrentAssertion{s};
          },
          [](const ConcurrentCoverStmt& s)
              -> std::optional<ConcurrentAssertion> {
            return ConcurrentAssertion{s};
          },
          [](const EmptyStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const VarDeclStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ExprStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const BlockStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ForkStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const IfStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const CaseStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const PatternCaseStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const AssertStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const CoverStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ExpectStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ForStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ForeachStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const WhileStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const RepeatStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const DoWhileStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ForeverStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const BreakStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ContinueStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ReturnStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const TimedStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const EventTriggerStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const WaitStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const WaitForkStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const DisableForkStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const DisableStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
          [](const ProceduralContinuousAssignStmt&)
              -> std::optional<ConcurrentAssertion> { return std::nullopt; },
          [](const ProceduralContinuousEndStmt&)
              -> std::optional<ConcurrentAssertion> { return std::nullopt; },
          [](const ConstraintStmt&) -> std::optional<ConcurrentAssertion> {
            return std::nullopt;
          },
      },
      data);
}

}  // namespace lyra::hir
