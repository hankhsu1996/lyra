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

using StatementData = std::variant<
    BlockStatementData, VariableDeclarationStatementData,
    AssignmentStatementData, ExpressionStatementData, ConditionalStatementData>;

struct Statement {
  StatementKind kind;
  SourceSpan span;
  StatementData data;

  auto operator==(const Statement&) const -> bool = default;
};

}  // namespace lyra::hir
