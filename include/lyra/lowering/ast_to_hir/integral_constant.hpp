#pragma once

#include <slang/numeric/SVInt.h>

#include "lyra/hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

// Convert a slang SVInt into Lyra's canonical IntegralConstant. Performs the
// X/Z plane swap once at this boundary: slang encodes X=(value=0,unknown=1)
// and Z=(value=1,unknown=1); Lyra encodes Z=(value=0,state=1) and
// X=(value=1,state=1).
auto LowerSVIntToIntegralConstant(const slang::SVInt& sv)
    -> hir::IntegralConstant;

}  // namespace lyra::lowering::ast_to_hir
