#pragma once

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

  auto operator==(const ConditionalStatementData&) const -> bool = default;
};

struct CaseItem {
  std::vector<ExpressionId> expressions;
  StatementId statement;

  auto operator==(const CaseItem&) const -> bool = default;
};

struct CaseStatementData {
  ExpressionId selector;
  std::vector<CaseItem> items;
  std::optional<StatementId> default_statement;

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

using StatementData = std::variant<
    BlockStatementData, VariableDeclarationStatementData,
    AssignmentStatementData, ExpressionStatementData, ConditionalStatementData,
    CaseStatementData, ForLoopStatementData, WhileLoopStatementData,
    DoWhileLoopStatementData, RepeatLoopStatementData>;

struct Statement {
  StatementKind kind;
  SourceSpan span;
  StatementData data;

  auto operator==(const Statement&) const -> bool = default;
};

}  // namespace lyra::hir
