#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Lower $value$plusargs statement.
// Writes parsed value to output place, stores success (1/0) to dest place.
auto LowerValuePlusargs(Context& context, const mir::ValuePlusargs& vp)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
