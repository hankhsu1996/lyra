#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

// Check if rvalue is a real-typed math operation (unary or binary).
// Does NOT match casts - casts are handled by LowerCastUnified.
auto IsRealMathRvalue(
    const CuFacts& facts, Context& context, const mir::Rvalue& rvalue) -> bool;

// Evaluate a real-typed math rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
// Real types are always 2-state (unknown is nullptr).
auto LowerRealRvalue(
    Context& context, const CuFacts& facts, const mir::Rvalue& rvalue,
    TypeId result_type) -> Result<RvalueValue>;

// Resolver-aware overload.
auto LowerRealRvalue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Rvalue& rvalue, TypeId result_type) -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
