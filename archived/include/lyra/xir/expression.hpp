#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/operators.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/xir/fwd.hpp"

namespace lyra::xir {

struct ConstInt {
  ConstId value;
};

struct ReadVariable {
  VariableId var;
};

struct UnaryExpr {
  common::UnaryOp op;
  ExprId operand;
};

struct BinaryExpr {
  common::BinaryOp op;
  ExprId lhs;
  ExprId rhs;
};

using ExpressionData =
    std::variant<ConstInt, ReadVariable, UnaryExpr, BinaryExpr>;

struct Expression {
  TypeId type;
  SourceSpan span;
  ExpressionData data;
};

}  // namespace lyra::xir
