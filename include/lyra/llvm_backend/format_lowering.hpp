#pragma once

#include <span>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/effect.hpp"

namespace llvm {
class Value;
}

namespace lyra::lowering::mir_to_llvm {

class Context;

// Validate all format ops can be lowered (for early-exit before
// LyraStringFormatStart). Returns error for unsupported types, 4-state values,
// or values wider than 64 bits.
auto ValidateFormatOps(Context& context, std::span<const mir::FormatOp> ops)
    -> Result<void>;

// Lower a single FormatOp to a string format buffer.
// Handles kLiteral, kString, and value formatting.
// Caller must have already called LyraStringFormatStart to create buf.
// Returns error only for unsupported types (rare after ValidateFormatOps).
auto LowerFormatOpToBuffer(
    Context& context, llvm::Value* buf, const mir::FormatOp& op)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
