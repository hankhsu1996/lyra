#pragma once

#include <slang/numeric/SVInt.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/source_span.hpp"

namespace slang {
class ConstantValue;
namespace ast {
class Type;
}
}  // namespace slang

namespace lyra::lowering::ast_to_hir {

struct Context;

auto LowerSVIntToIntegralConstant(const slang::SVInt& sv_int)
    -> IntegralConstant;

auto LowerIntegralConstant(
    const slang::SVInt& sv_int, TypeId type, Context* ctx) -> ConstId;

// Create filled constant from tick literal ('0, '1, 'x, 'z).
auto CreateFilledConstant(
    const slang::SVInt& tick_value, TypeId target_type, Context* ctx)
    -> ConstId;

// AST-to-HIR boundary: canonical ConstantValue -> ConstId entry point.
//
// Converts a slang ConstantValue into a Lyra ConstId by interning it
// in the active constant arena. Handles all supported constant types:
// integral, string, real, unpacked struct, unpacked array, packed struct.
// Recursively interns aggregate children.
//
// This is the single entry point for lowering frontend constant values
// into the constant arena. All AST-to-HIR contexts that need a ConstId
// from a frontend constant value must use this function:
//   - expression lowering (LowerConstantValueExpression)
//   - port-binding constant classification (TryEvaluatePortBindingConstant)
//   - any future context that interns frontend constants
//
// Returns invalid ConstId ({}) if the constant form is not supported.
auto LowerConstantValueToConstId(
    const slang::ConstantValue& cv, const slang::ast::Type& type,
    SourceSpan span, Context* ctx) -> ConstId;

}  // namespace lyra::lowering::ast_to_hir
