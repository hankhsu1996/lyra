#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/operator.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/system_call.hpp"

namespace lyra::hir {

enum class ExpressionKind {
  kConstant,
  kSymbolRef,
  kUnaryOp,
  kBinaryOp,
  kSystemCall,
};

// Re-export from common for backward compatibility
using lyra::common::BinaryOp;
using lyra::common::UnaryOp;

struct ConstantExpressionData {
  ConstId constant;

  auto operator==(const ConstantExpressionData&) const -> bool = default;
};

struct SymbolRefExpressionData {
  SymbolId symbol;

  auto operator==(const SymbolRefExpressionData&) const -> bool = default;
};

struct UnaryExpressionData {
  UnaryOp op;
  ExpressionId operand;

  auto operator==(const UnaryExpressionData&) const -> bool = default;
};

struct BinaryExpressionData {
  BinaryOp op;
  ExpressionId lhs;
  ExpressionId rhs;

  auto operator==(const BinaryExpressionData&) const -> bool = default;
};

using ExpressionData = std::variant<
    ConstantExpressionData, SymbolRefExpressionData, UnaryExpressionData,
    BinaryExpressionData, SystemCallExpressionData>;

struct Expression {
  ExpressionKind kind;
  TypeId type;
  SourceSpan span;
  ExpressionData data;

  auto operator==(const Expression&) const -> bool = default;
};

}  // namespace lyra::hir
