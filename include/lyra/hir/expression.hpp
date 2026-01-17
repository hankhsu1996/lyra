#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/hir/system_call.hpp"

namespace lyra::hir {

enum class ExpressionKind {
  kConstant,
  kNameRef,
  kUnaryOp,
  kBinaryOp,
  kCast,
  kSystemCall,
};

struct ConstantExpressionData {
  ConstId constant;

  auto operator==(const ConstantExpressionData&) const -> bool = default;
};

struct NameRefExpressionData {
  SymbolId symbol;

  auto operator==(const NameRefExpressionData&) const -> bool = default;
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

struct CastExpressionData {
  ExpressionId operand;

  auto operator==(const CastExpressionData&) const -> bool = default;
};

using ExpressionData = std::variant<
    ConstantExpressionData, NameRefExpressionData, UnaryExpressionData,
    BinaryExpressionData, CastExpressionData, SystemCallExpressionData>;

struct Expression {
  ExpressionKind kind;
  TypeId type;
  SourceSpan span;
  ExpressionData data;

  auto operator==(const Expression&) const -> bool = default;
};

}  // namespace lyra::hir
