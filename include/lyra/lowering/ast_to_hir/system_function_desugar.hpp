#pragma once

#include <optional>
#include <variant>

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/common/math_fn.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

// Category 1: Type conversion -> kCast or kBitCast
enum class ConversionSysFnKind {
  kSigned,
  kUnsigned,
  kItor,
  kRtoi,
  kRealToBits,
  kBitsToReal,
  kShortRealToBits,
  kBitsToShortReal,
};

// Category 2: Operator-lowerable (generic)
struct UnaryOpSysFn {
  hir::UnaryOp op;
};
struct BinaryOpSysFn {
  hir::BinaryOp op;
};

// Category 3: Timescale query -> constant
enum class TimeScaleSysFnKind {
  kTimeunit,
  kTimeprecision,
};

// Category 4: Math functions (IEEE 1800 §20.8)
struct MathSysFn {
  MathFn fn;
};

using PureSysFnClassification = std::variant<
    ConversionSysFnKind, UnaryOpSysFn, BinaryOpSysFn, TimeScaleSysFnKind,
    MathSysFn>;

// Classify a system call as a pure function that should be desugared.
// Returns nullopt if the call is not a pure system function.
auto ClassifyPureSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<PureSysFnClassification>;

// Lower a pure system function to core HIR nodes.
// This function should only be called after ClassifyPureSystemFunction
// returns a valid classification.
auto LowerPureSystemFunction(
    const slang::ast::CallExpression& call,
    const PureSysFnClassification& classification, ExpressionLoweringView view)
    -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
