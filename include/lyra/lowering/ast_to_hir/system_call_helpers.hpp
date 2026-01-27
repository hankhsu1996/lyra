#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"

namespace slang::ast {
class Expression;
}

namespace lyra::lowering::ast_to_hir {

struct LoweringFrame;

// Compute time scaling divisor: 10^(unit_power - global_precision)
// Throws InternalError on invariant violations (negative exponent, overflow).
auto ComputeTimeDivisor(const LoweringFrame& frame) -> uint64_t;

// Emit a raw simulation tick query.
auto EmitRawTicksQuery(SourceSpan span, Context* ctx) -> hir::ExpressionId;

// Create a constant integer expression.
auto MakeConstant(uint64_t value, TypeId type, SourceSpan span, Context* ctx)
    -> hir::ExpressionId;

// Integer floor division scaling + optional type cast.
auto EmitIntegerTimeScaling(
    hir::ExpressionId raw_ticks, uint64_t divisor, TypeId result_type,
    SourceSpan span, Context* ctx) -> hir::ExpressionId;

// Cast to real first, then real division (preserves fractional precision).
auto EmitRealTimeScaling(
    hir::ExpressionId raw_ticks, uint64_t divisor, SourceSpan span,
    Context* ctx) -> hir::ExpressionId;

// Unwrap output argument for system functions that expect lvalues.
// Slang wraps output args as AssignmentExpression; this extracts the LHS.
// Returns: HIR expression for the assignable target.
// Contract: Caller assumes result is a valid lvalue (slang validates this).
auto UnwrapOutputArgument(
    const slang::ast::Expression* arg, ExpressionLoweringView view)
    -> hir::ExpressionId;

// Returns frame if available, or emits error and returns nullptr.
auto RequireModuleFrame(
    std::string_view func_name, SourceSpan span, ExpressionLoweringView view)
    -> const LoweringFrame*;

}  // namespace lyra::lowering::ast_to_hir
