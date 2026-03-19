#include <expected>
#include <variant>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/context_scope.hpp"
#include "lyra/llvm_backend/instruction/assoc_op.hpp"
#include "lyra/llvm_backend/instruction/builtin_call.hpp"
#include "lyra/llvm_backend/instruction/call.hpp"
#include "lyra/llvm_backend/instruction/deferred_assign.hpp"
#include "lyra/llvm_backend/instruction/effect.hpp"
#include "lyra/llvm_backend/instruction/immediate_assign.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/llvm_backend/statement.hpp"
#include "lyra/mir/assoc_op.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerStatement(Context& context, const mir::Statement& statement)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerStatement(context, canonical, statement);
}

auto LowerStatement(
    Context& context, SlotAccessResolver& resolver,
    const mir::Statement& statement) -> Result<void> {
  OriginScope origin_scope(context, statement.origin);
  StatementScope scope(context);

  return std::visit(
      common::Overloaded{
          [&](const mir::Assign& assign) -> Result<void> {
            return LowerAssign(context, resolver, assign);
          },
          [&](const mir::GuardedAssign& guarded) -> Result<void> {
            return LowerGuardedAssign(context, resolver, guarded);
          },
          [&](const mir::Effect& effect) -> Result<void> {
            return LowerEffectOp(context, resolver, effect.op);
          },
          [&](const mir::DeferredAssign& deferred) -> Result<void> {
            return LowerDeferredAssign(context, resolver, deferred);
          },
          [&](const mir::Call& call) -> Result<void> {
            return LowerCall(context, resolver, call);
          },
          [&](const mir::BuiltinCall& call) -> Result<void> {
            return LowerBuiltinCall(context, resolver, call);
          },
          [&](const mir::DefineTemp& dt) -> Result<void> {
            llvm::Value* val = nullptr;
            auto result = std::visit(
                common::Overloaded{
                    [&](const mir::Operand& op) -> Result<void> {
                      auto op_result = LowerOperandRaw(context, resolver, op);
                      if (!op_result) return std::unexpected(op_result.error());
                      val = *op_result;
                      return {};
                    },
                    [&](const mir::Rvalue& rv) -> Result<void> {
                      auto rv_result =
                          LowerRvalue(context, resolver, rv, dt.type);
                      if (!rv_result) return std::unexpected(rv_result.error());
                      val = rv_result->PackForTemp(context.GetBuilder());
                      return {};
                    },
                },
                dt.rhs);
            if (!result) return std::unexpected(result.error());
            context.BindTemp(dt.temp_id, val, dt.type);
            return {};
          },
          [&](const mir::AssocOp& op) -> Result<void> {
            return LowerAssocOp(context, resolver, op);
          },
      },
      statement.data);
}

}  // namespace lyra::lowering::mir_to_llvm
