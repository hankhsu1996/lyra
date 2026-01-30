#pragma once

#include <optional>
#include <variant>

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/common/math_fn.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace lyra::lowering::ast_to_hir {

// "Desugarable" system functions are lowered to ordinary HIR nodes (kCast,
// kUnaryOp, kConstant, etc.) rather than hir::kSystemCall. This is a
// *lowering-form* classification, not a *behavioral* one.
//
// Note: The test taxonomy (tests/sv_features/system_tf/{pure,effect,state}/)
// classifies by runtime semantics (no state dependence vs I/O vs simulation
// state). That's orthogonal: $sformatf is behaviorally "pure" but lowers via
// kSystemCall, while $signed is both behaviorally pure AND desugarable.

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

// Category 4: Math functions (IEEE 1800 20.8)
struct MathSysFn {
  MathFn fn;
};

using DesugarableClassification = std::variant<
    ConversionSysFnKind, UnaryOpSysFn, BinaryOpSysFn, TimeScaleSysFnKind,
    MathSysFn>;

// Classify a system call as desugarable (lowers to ordinary HIR, not
// kSystemCall). Returns nullopt if the call requires kSystemCall lowering.
auto ClassifyDesugarableSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<DesugarableClassification>;

// Lower a desugarable system function to core HIR nodes.
// Precondition: ClassifyDesugarableSystemFunction returned a classification.
auto LowerDesugarableSystemFunction(
    const slang::ast::CallExpression& call,
    const DesugarableClassification& classification,
    ExpressionLoweringView view) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
