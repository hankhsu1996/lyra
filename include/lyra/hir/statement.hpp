#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

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
  ExpressionId init;  // kInvalidExpressionId if no initializer

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
  std::vector<ExpressionId> expressions;
  StatementId statement;

  auto operator==(const CaseItem&) const -> bool = default;
};

enum class CaseCondition : uint8_t {
  kNormal,  // case: equality comparison
  kCaseZ,   // casez: Z bits from both sides are wildcards
  kCaseX,   // casex: X and Z bits from both sides are wildcards
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

using StatementData = std::variant<
    BlockStatementData, VariableDeclarationStatementData,
    AssignmentStatementData, ExpressionStatementData, ConditionalStatementData,
    CaseStatementData, ForLoopStatementData, WhileLoopStatementData,
    DoWhileLoopStatementData, RepeatLoopStatementData, BreakStatementData,
    ContinueStatementData, TerminateStatementData, ReturnStatementData>;

struct Statement {
  StatementKind kind;
  SourceSpan span;
  StatementData data;

  auto operator==(const Statement&) const -> bool = default;
};

}  // namespace lyra::hir
