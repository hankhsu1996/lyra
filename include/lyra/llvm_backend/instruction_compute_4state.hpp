#pragma once

#include <cstdint>
#include <vector>

#include "llvm/IR/Value.h"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerCompute4State(
    Context& context, const mir::Compute& compute, uint32_t bit_width)
    -> Result<void>;

auto LowerCaseMatchOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
