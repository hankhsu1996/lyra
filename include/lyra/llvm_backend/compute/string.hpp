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

// Lower string concatenation to target place.
// Calls LyraStringConcat runtime function.
auto LowerStringConcat(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target_place)
    -> Result<void>;

// Lower $sformatf/$sformat/$swrite to target place.
// Uses compile-time format parsing via LyraStringFormat* runtime functions.
auto LowerSFormatRvalue(
    Context& context, const mir::SFormatRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target_place)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
