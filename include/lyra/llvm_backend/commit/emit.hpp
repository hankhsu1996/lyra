#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/ownership.hpp"

namespace llvm {
class Value;
}

namespace lyra::lowering::mir_to_llvm {

class Context;
struct WriteTarget;

// Per-family commit emit helpers. Called by the canonical write dispatcher
// (write_dispatch.cpp) after WriteTarget resolution and source lowering.
// Not for direct use by instruction-level code.

auto CommitStringValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void>;

auto CommitContainerValue(
    Context& ctx, const CuFacts& facts, const WriteTarget& wt,
    llvm::Value* handle, OwnershipPolicy policy, TypeId type_id)
    -> Result<void>;

auto CommitPackedValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* raw, TypeId type_id)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
