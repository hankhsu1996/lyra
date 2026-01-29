#include "lyra/llvm_backend/instruction.hpp"

#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/assign.hpp"
#include "lyra/llvm_backend/instruction/effect.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerInstruction(Context& context, const mir::Instruction& instruction)
    -> Result<void> {
  // Set origin for error reporting.
  // OriginScope preserves outer origin if instruction.origin is Invalid.
  OriginScope origin_scope(context, instruction.origin);

  // RAII guard for statement-scoped cleanup of owned string temps
  StatementScope scope(context);

  return std::visit(
      common::Overloaded{
          [&context](const mir::Assign& assign) -> Result<void> {
            return LowerAssign(context, assign);
          },
          [&context](const mir::Compute& compute) -> Result<void> {
            return LowerCompute(context, compute);
          },
          [&context](const mir::GuardedAssign& guarded) -> Result<void> {
            return LowerGuardedAssign(context, guarded);
          },
          [&context](const mir::Effect& effect) -> Result<void> {
            return LowerEffectOp(context, effect.op);
          },
          [&context](const mir::NonBlockingAssign& nba) -> Result<void> {
            return LowerNonBlockingAssign(context, nba);
          },
      },
      instruction.data);
}

}  // namespace lyra::lowering::mir_to_llvm
