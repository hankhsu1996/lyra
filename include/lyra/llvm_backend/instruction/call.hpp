#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/execution_mode.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

auto LowerCall(
    Context& context, const CuFacts& facts, const mir::Call& call,
    const ActiveExecutionMode& mode) -> Result<void>;

auto LowerCall(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Call& call, const ActiveExecutionMode& mode) -> Result<void>;

// Lower $value$plusargs via unified Call.
// Exposed for testing and internal use.
auto LowerValuePlusargsCall(
    Context& context, const CuFacts& facts, const mir::Call& call)
    -> Result<void>;

// Lower $fgets via unified Call.
// Exposed for testing and internal use.
auto LowerFgetsCall(
    Context& context, const CuFacts& facts, const mir::Call& call)
    -> Result<void>;

// Lower $fread via unified Call.
// Exposed for testing and internal use.
auto LowerFreadCall(
    Context& context, const CuFacts& facts, const mir::Call& call)
    -> Result<void>;

// Lower $fscanf via unified Call.
// Exposed for testing and internal use.
auto LowerFscanfCall(
    Context& context, const CuFacts& facts, const mir::Call& call)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
