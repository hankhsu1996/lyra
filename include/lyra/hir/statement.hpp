#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/deferred_assertion.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/rvalue.hpp"

namespace lyra::hir {

enum class StatementKind {
  kBlock,
  kVariableDeclaration,
  kAssignment,
  kExpression,
  kConditional,
  kCase,
  kForLoop,
  kWhileLoop,
  kDoWhileLoop,
  kRepeatLoop,
  kBreak,
  kContinue,
  kTerminate,
  kReturn,
  kDelay,
  kEventWait,
  kNamedEventWait,
  kEventTrigger,
  kImmediateAssertion,
  kDeferredAssertion,
};

// unique/unique0/priority qualifier for if and case statements
// (LRM 12.4.2, 12.5.3)
enum class UniquePriorityCheck : uint8_t {
  kNone,      // No qualifier - standard short-circuit behavior
  kUnique,    // unique: warn on multiple matches OR no match
  kUnique0,   // unique0: warn on multiple matches only (no-match is OK)
  kPriority,  // priority: warn on no match only (multiple matches OK)
};

struct BlockStatementData {
  std::vector<StatementId> statements;

  auto operator==(const BlockStatementData&) const -> bool = default;
};

struct VariableDeclarationStatementData {
  SymbolId symbol;
  std::optional<RValue> initializer;

  auto operator==(const VariableDeclarationStatementData&) const
      -> bool = default;
};

struct AssignmentStatementData {
  ExpressionId target;
  ExpressionId value;

  auto operator==(const AssignmentStatementData&) const -> bool = default;
};

struct ExpressionStatementData {
  ExpressionId expression;

  auto operator==(const ExpressionStatementData&) const -> bool = default;
};

struct ConditionalStatementData {
  ExpressionId condition;
  StatementId then_branch;
  std::optional<StatementId> else_branch;
  UniquePriorityCheck check = UniquePriorityCheck::kNone;

  auto operator==(const ConditionalStatementData&) const -> bool = default;
};

struct CaseItem {
  ExpressionId
      predicate;  // Pre-built match predicate (1-bit, 2-state after clamp)
  std::optional<StatementId> statement;  // nullopt = empty body (e.g., "2: ;")

  auto operator==(const CaseItem&) const -> bool = default;
};

enum class CaseCondition : uint8_t {
  kNormal,  // case: equality comparison
  kCaseZ,   // casez: Z bits from both sides are wildcards
  kCaseX,   // casex: X and Z bits from both sides are wildcards
  kInside,  // case inside: membership test with ranges/wildcards
};

struct CaseStatementData {
  ExpressionId selector;
  std::vector<CaseItem> items;
  std::optional<StatementId> default_statement;
  CaseCondition condition = CaseCondition::kNormal;
  UniquePriorityCheck check = UniquePriorityCheck::kNone;

  auto operator==(const CaseStatementData&) const -> bool = default;
};

struct ForLoopStatementData {
  std::vector<StatementId> var_decls;  // Variable declarations only
  std::vector<ExpressionId>
      init_exprs;  // Expression initializers (assignments)
  std::optional<ExpressionId> condition;
  std::vector<ExpressionId> steps;  // Steps as expressions
  StatementId body;

  auto operator==(const ForLoopStatementData&) const -> bool = default;
};

struct WhileLoopStatementData {
  ExpressionId condition;
  StatementId body;

  auto operator==(const WhileLoopStatementData&) const -> bool = default;
};

struct DoWhileLoopStatementData {
  ExpressionId condition;
  StatementId body;

  auto operator==(const DoWhileLoopStatementData&) const -> bool = default;
};

struct RepeatLoopStatementData {
  ExpressionId count;  // Number of iterations (evaluated once)
  StatementId body;

  auto operator==(const RepeatLoopStatementData&) const -> bool = default;
};

struct BreakStatementData {
  auto operator==(const BreakStatementData&) const -> bool = default;
};

struct ContinueStatementData {
  auto operator==(const ContinueStatementData&) const -> bool = default;
};

enum class TerminationKind : uint8_t {
  kFinish,  // $finish - normal termination
  kStop,    // $stop - pause for debugger
  kExit,    // $exit - normal termination (synonym)
  kFatal,   // $fatal - error termination with message
};

struct TerminateStatementData {
  TerminationKind kind;
  int level;  // 0 = silent, 1 = print time (default), 2 = print time+stats
  std::vector<ExpressionId> message_args;  // For $fatal message (if any)

  auto operator==(const TerminateStatementData&) const -> bool = default;
};

struct ReturnStatementData {
  ExpressionId value;  // kInvalidExpressionId for void/implicit return

  auto operator==(const ReturnStatementData&) const -> bool = default;
};

struct DelayStatementData {
  uint64_t ticks;  // Canonical delay amount in simulation ticks

  auto operator==(const DelayStatementData&) const -> bool = default;
};

// Edge kind for event triggers (LRM 9.4.2)
enum class EventEdgeKind : uint8_t {
  kNone,       // @(signal) - any change
  kPosedge,    // @(posedge signal)
  kNegedge,    // @(negedge signal)
  kBothEdges,  // @(edge signal)
};

struct EventTrigger {
  ExpressionId signal;
  EventEdgeKind edge = EventEdgeKind::kNone;

  auto operator==(const EventTrigger&) const -> bool = default;
};

struct EventWaitStatementData {
  std::vector<EventTrigger> triggers;  // OR semantics

  auto operator==(const EventWaitStatementData&) const -> bool = default;
};

struct NamedEventWaitStatementData {
  ExpressionId event_expr;

  auto operator==(const NamedEventWaitStatementData&) const -> bool = default;
};

struct EventTriggerStatementData {
  ExpressionId target;

  auto operator==(const EventTriggerStatementData&) const -> bool = default;
};

// Immediate assertion kind (LRM 16.3)
enum class ImmediateAssertionKind : uint8_t {
  kAssert,
  kAssume,
  kCover,
};

// Immediate assertion timing (LRM 16.3, 16.4)
// Used during AST-to-HIR lowering to determine which statement kind to
// produce. Not stored on the final HIR nodes: simple assertions become
// ImmediateAssertionStatementData, deferred become
// DeferredAssertionStatementData.
enum class ImmediateAssertionTiming : uint8_t {
  kSimple,
  kObservedDeferred,  // assert #0 / assume #0 / cover #0
  // kFinalDeferred later
};

// Simple immediate assertion statement (LRM 16.3)
// Represents procedural assert/assume/cover with immediate (non-deferred)
// evaluation. Source-shaped: pass/fail actions are child statement nodes.
// For observed deferred assertions (assert #0 / assume #0 / cover #0),
// see DeferredAssertionStatementData.
struct ImmediateAssertionStatementData {
  ImmediateAssertionKind kind = ImmediateAssertionKind::kAssert;
  ExpressionId condition;
  std::optional<StatementId> pass_action;
  std::optional<StatementId> fail_action;

  auto operator==(const ImmediateAssertionStatementData&) const
      -> bool = default;
};

// Observed deferred assertion statement (LRM 16.4)
// Represents assert #0 / assume #0 / cover #0 with deferred evaluation.
// Inline-owns its deferred action semantic content. No side table,
// no site id -- the statement directly owns what happens when the
// deferred action fires, just like a closure owns its captures.
// Source-shaped child statements are kept for source fidelity; the
// deferred actions are the semantic truth.
struct DeferredAssertionStatementData {
  ImmediateAssertionKind kind = ImmediateAssertionKind::kAssert;
  ExpressionId condition;
  // Source-shaped action statements (kept for source fidelity / dump).
  std::optional<StatementId> pass_action;
  std::optional<StatementId> fail_action;
  // Semantic deferred actions (inline-owned).
  bool has_default_fail_report = false;
  std::optional<DeferredAction> deferred_fail_action;
  std::optional<DeferredAction> deferred_pass_action;

  auto operator==(const DeferredAssertionStatementData&) const
      -> bool = default;
};

using StatementData = std::variant<
    BlockStatementData, VariableDeclarationStatementData,
    AssignmentStatementData, ExpressionStatementData, ConditionalStatementData,
    CaseStatementData, ForLoopStatementData, WhileLoopStatementData,
    DoWhileLoopStatementData, RepeatLoopStatementData, BreakStatementData,
    ContinueStatementData, TerminateStatementData, ReturnStatementData,
    DelayStatementData, EventWaitStatementData, NamedEventWaitStatementData,
    EventTriggerStatementData, ImmediateAssertionStatementData,
    DeferredAssertionStatementData>;

struct Statement {
  StatementKind kind;
  SourceSpan span;
  StatementData data;

  auto operator==(const Statement&) const -> bool = default;
};

}  // namespace lyra::hir
