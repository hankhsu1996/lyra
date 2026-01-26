#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerDisplayEffect(Context& context, const mir::DisplayEffect& display)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
