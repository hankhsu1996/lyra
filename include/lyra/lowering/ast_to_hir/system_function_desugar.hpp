#pragma once

#include <optional>
#include <variant>

#include <slang/ast/expressions/CallExpression.h>

#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

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

using PureSysFnClassification =
    std::variant<ConversionSysFnKind, UnaryOpSysFn, BinaryOpSysFn>;

// Classify a system call as a pure function that should be desugared.
// Returns nullopt if the call is not a pure system function.
auto ClassifyPureSystemFunction(const slang::ast::CallExpression& call)
    -> std::optional<PureSysFnClassification>;

// Lower a pure system function to core HIR nodes.
// This function should only be called after ClassifyPureSystemFunction
// returns a valid classification.
auto LowerPureSystemFunction(
    const slang::ast::CallExpression& call,
    const PureSysFnClassification& classification, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
