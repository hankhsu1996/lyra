#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct CuFacts;

auto DetermineOwnership(Context& context, const mir::Operand& source)
    -> OwnershipPolicy;

auto AssignPlace(
    Context& context, const CuFacts& facts, const mir::WriteTarget& target,
    const mir::Operand& source) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
