#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/builtin_call.hpp"
#include "lyra/llvm_backend/instruction/call.hpp"
#include "lyra/llvm_backend/instruction/deferred_assign.hpp"
#include "lyra/llvm_backend/instruction/effect.hpp"
#include "lyra/llvm_backend/instruction/immediate_assign.hpp"
#include "lyra/llvm_backend/statement.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerStatement(Context& context, const mir::Statement& statement)
    -> Result<void> {
  // Set origin for error reporting.
  // OriginScope preserves outer origin if statement.origin is Invalid.
  OriginScope origin_scope(context, statement.origin);

  // RAII guard for statement-scoped cleanup of owned string temps
  StatementScope scope(context);

  return std::visit(
      common::Overloaded{
          [&context](const mir::Assign& assign) -> Result<void> {
            return LowerAssign(context, assign);
          },
          [&context](const mir::GuardedAssign& guarded) -> Result<void> {
            return LowerGuardedAssign(context, guarded);
          },
          [&context](const mir::Effect& effect) -> Result<void> {
            return LowerEffectOp(context, effect.op);
          },
          [&context](const mir::DeferredAssign& deferred) -> Result<void> {
            return LowerDeferredAssign(context, deferred);
          },
          [&context](const mir::Call& call) -> Result<void> {
            return LowerCall(context, call);
          },
          [&context](const mir::BuiltinCall& call) -> Result<void> {
            return LowerBuiltinCall(context, call);
          },
          [&context](const mir::DefineTemp& dt) -> Result<void> {
            // DefineTemp: evaluate RHS and bind to temp_id (no alloca, no
            // store)
            llvm::Value* val = nullptr;
            auto result = std::visit(
                common::Overloaded{
                    [&](const mir::Operand& op) -> Result<void> {
                      auto op_result = LowerOperandRaw(context, op);
                      if (!op_result) return std::unexpected(op_result.error());
                      val = *op_result;
                      return {};
                    },
                    [&](const mir::Rvalue& rv) -> Result<void> {
                      auto rv_result = LowerRvalue(context, rv, dt.type);
                      if (!rv_result) return std::unexpected(rv_result.error());
                      // Use centralized PackForTemp for consistent 4-state
                      // representation
                      val = rv_result->PackForTemp(context.GetBuilder());
                      return {};
                    },
                },
                dt.rhs);
            if (!result) return std::unexpected(result.error());
            context.BindTemp(dt.temp_id, val, dt.type);
            return {};
          },
      },
      statement.data);
}

}  // namespace lyra::lowering::mir_to_llvm
