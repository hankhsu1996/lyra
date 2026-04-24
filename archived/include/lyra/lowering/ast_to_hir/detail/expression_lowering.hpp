#pragma once

#include <slang/ast/Expression.h>

#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
struct LoweringFrame;
class SymbolRegistrar;

struct ExpressionLoweringView {
  Context* context;
  SymbolRegistrar* registrar;
  const LoweringFrame* frame;
};

auto LowerExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId;

// Lower expression and coerce to target type.
// Handles unbased-unsized tick literals ('0, '1, 'x, 'z) with fill-to-width.
auto LowerAndCoerce(
    const slang::ast::Expression& expr, TypeId target_type,
    ExpressionLoweringView view) -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
