#pragma once

#include <vector>

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower string binary comparison (==, !=, <, <=, >, >=).
// Returns i1 zero-extended to result_type.
auto LowerStringBinaryOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* result_type)
    -> Result<llvm::Value*>;

// Evaluate string concatenation and return the new string handle.
// Does NOT store to any place - caller must handle storage.
auto LowerStringConcatValue(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*>;

// Evaluate $sformatf/$sformat/$swrite and return the new string handle.
// Does NOT store to any place - caller must handle storage.
auto LowerSFormatRvalueValue(
    Context& context, const mir::SFormatRvalueInfo& info,
    const std::vector<mir::Operand>& operands) -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
