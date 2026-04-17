#pragma once

#include <optional>

#include <slang/ast/Expression.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/fwd.hpp"

namespace slang {
class ConstantValue;
}

namespace slang::ast {
class Scope;
}

namespace lyra::lowering::ast_to_hir {

struct Context;
struct LoweringFrame;
class SymbolRegistrar;

/// Lower a slang ConstantValue to an HIR kConstant expression.
auto LowerConstantValueExpression(
    const slang::ConstantValue& cv, const slang::ast::Type& type,
    SourceSpan span, Context* ctx) -> hir::ExpressionId;

/// Try to evaluate a port-binding expression as a compile-time constant.
/// Uses slang semantic constant evaluation (cached result or active eval).
/// Returns a ConstId (constant arena reference) if the expression is
/// compile-time evaluable, or nullopt if it is a runtime-varying expression.
///
/// This is the single semantic contract for port-binding constant
/// classification. The output is constant identity, not an HIR expression
/// node. All compile-time constant port expressions (literals, folded
/// arithmetic, casts, concat, struct/array aggregate constants, etc.)
/// converge through this path regardless of their HIR representation.
auto TryEvaluatePortBindingConstant(
    const slang::ast::Expression& expr, const slang::ast::Scope& eval_scope,
    SourceSpan span, Context* ctx) -> std::optional<ConstId>;

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

/// Determine if an inside set item should use ==? (wildcard equality).
/// Returns true if the item is a constant with X/Z bits.
auto InsideItemUsesWildcardEq(const slang::ast::Expression& item) -> bool;

}  // namespace lyra::lowering::ast_to_hir
