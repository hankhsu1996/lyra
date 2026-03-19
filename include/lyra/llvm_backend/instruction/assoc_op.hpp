#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/assoc_op.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
class SlotAccessResolver;

auto LowerAssocOp(Context& ctx, const mir::AssocOp& op) -> Result<void>;

// Resolver-aware overload.
auto LowerAssocOp(
    Context& ctx, SlotAccessResolver& resolver, const mir::AssocOp& op)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
