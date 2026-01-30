#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate rvalue and return computed LLVM value.
// Does NOT store to any place - caller must handle storage.
//
// For packed 4-state types, sets *unknown_out to the unknown plane.
// For 2-state types, sets *unknown_out to nullptr.
// Caller should pass nullptr for unknown_out if not needed.
auto LowerRvalue(
    Context& context, const mir::Rvalue& rvalue, mir::PlaceId target,
    TypeId result_type, llvm::Value** unknown_out) -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
