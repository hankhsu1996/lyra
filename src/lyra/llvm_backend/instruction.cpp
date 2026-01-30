#include "lyra/llvm_backend/instruction.hpp"

#include <variant>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/assign.hpp"
#include "lyra/llvm_backend/instruction/builtin_call.hpp"
#include "lyra/llvm_backend/instruction/call.hpp"
#include "lyra/llvm_backend/instruction/effect.hpp"
#include "lyra/llvm_backend/instruction/value_plusargs.hpp"
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
          [&context](const mir::GuardedStore& guarded) -> Result<void> {
            return LowerGuardedStore(context, guarded);
          },
          [&context](const mir::Effect& effect) -> Result<void> {
            return LowerEffectOp(context, effect.op);
          },
          [&context](const mir::NonBlockingAssign& nba) -> Result<void> {
            return LowerNonBlockingAssign(context, nba);
          },
          [&context](const mir::Call& call) -> Result<void> {
            return LowerCall(context, call);
          },
          [&context](const mir::BuiltinCall& call) -> Result<void> {
            return LowerBuiltinCall(context, call);
          },
          [&context](const mir::ValuePlusargs& vp) -> Result<void> {
            return LowerValuePlusargs(context, vp);
          },
      },
      instruction.data);
}

}  // namespace lyra::lowering::mir_to_llvm
