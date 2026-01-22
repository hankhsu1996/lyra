#pragma once

#include <slang/numeric/SVInt.h>

#include "lyra/common/constant.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;

// Convert slang SVInt to IntegralConstant (pure value, no arena)
auto LowerSVIntToIntegralConstant(const slang::SVInt& sv_int)
    -> IntegralConstant;

// Intern a slang SVInt as a constant with the given type
auto LowerIntegralConstant(
    const slang::SVInt& sv_int, TypeId type, Context* ctx) -> ConstId;

}  // namespace lyra::lowering::ast_to_hir
