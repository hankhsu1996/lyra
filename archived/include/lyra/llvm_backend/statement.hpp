#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/execution_mode.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

auto LowerStatement(
    Context& context, const CuFacts& facts, const mir::Statement& statement,
    const ActiveExecutionMode& mode, const BodySiteContext& site_ctx)
    -> Result<void>;

auto LowerStatement(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Statement& statement, const ActiveExecutionMode& mode,
    const BodySiteContext& site_ctx) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
