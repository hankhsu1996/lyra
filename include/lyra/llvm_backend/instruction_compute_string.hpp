#pragma once

#include <vector>

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
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
    -> llvm::Value*;

// Lower string concatenation to target place.
// Calls LyraStringConcat runtime function.
void LowerStringConcat(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target_place);

// Lower $sformatf/$sformat/$swrite to target place.
// Uses compile-time format parsing via LyraStringFormat* runtime functions.
void LowerSFormatRvalue(
    Context& context, const mir::SFormatRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target_place);

}  // namespace lyra::lowering::mir_to_llvm
