#pragma once

#include <slang/numeric/SVInt.h>

#include "lyra/common/constant.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;

auto LowerIntegralConstant(
    const slang::SVInt& sv_int, TypeId type, Context* ctx) -> ConstId;

}  // namespace lyra::lowering::ast_to_hir
