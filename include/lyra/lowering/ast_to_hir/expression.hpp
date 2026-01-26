#pragma once

#include <slang/ast/Expression.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
struct LoweringFrame;
class SymbolRegistrar;

/// Lower an expression in design-level context (no timescale).
/// Use for port bindings and parameter contexts.
/// Reports a diagnostic for $time/$stime/$realtime (requires scope).
auto LowerDesignLevelExpression(
    const slang::ast::Expression& expr, Context& ctx,
    SymbolRegistrar& registrar) -> hir::ExpressionId;

/// Lower an expression in scoped context (module/process/statement).
/// Use for module bodies, processes, functions, and tasks.
/// $time/$stime/$realtime use the frame for proper scaling.
auto LowerScopedExpression(
    const slang::ast::Expression& expr, Context& ctx,
    SymbolRegistrar& registrar, const LoweringFrame& frame)
    -> hir::ExpressionId;

}  // namespace lyra::lowering::ast_to_hir
