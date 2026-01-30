#pragma once

#include <slang/numeric/SVInt.h>

#include "lyra/common/constant.hpp"

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

}  // namespace lyra::lowering::ast_to_hir
