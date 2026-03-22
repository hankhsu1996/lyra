#include <expected>
#include <variant>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
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
            TempValue tv;
            auto result = std::visit(
                common::Overloaded{
                    [&](const mir::Operand& op) -> Result<void> {
                      // For temp-to-temp copies, read TempValue directly
                      // to preserve semantic domain without raw round-trip.
                      if (const auto* temp_id =
                              std::get_if<mir::TempId>(&op.payload)) {
                        const auto& src = context.ReadTempValue(temp_id->value);
                        // Guard against illegal retagging.
                        if (!context.IsFourState(dt.type) &&
                            src.domain == ValueDomain::kFourState) {
                          throw common::InternalError(
                              "DefineTemp",
                              std::format(
                                  "temp {}: 2-state destination retagged from "
                                  "4-state source",
                                  dt.temp_id));
                        }
                        // Width compatibility: source payload must match
                        // destination expected width.
                        const auto& types = context.GetTypeArena();
                        const Type& dst_type = types[dt.type];
                        if (IsPacked(dst_type)) {
                          uint32_t dst_width = PackedBitWidth(dst_type, types);
                          uint32_t src_width =
                              src.value->getType()->getIntegerBitWidth();
                          if (src_width != dst_width) {
                            throw common::InternalError(
                                "DefineTemp",
                                std::format(
                                    "temp {}: payload width {} != destination "
                                    "semantic width {}",
                                    dt.temp_id, src_width, dst_width));
                          }
                        }
                        tv = {
                            .declared_type = dt.type,
                            .domain = src.domain,
                            .value = src.value,
                            .unknown = src.unknown};
                        return {};
                      }
                      // Non-temp operands (Place, Constant): use raw shape.
                      auto op_result = LowerOperandRaw(context, resolver, op);
                      if (!op_result) return std::unexpected(op_result.error());
                      llvm::Value* raw = *op_result;
                      if (raw->getType()->isStructTy()) {
                        auto fs = ExtractFourState(context.GetBuilder(), raw);
                        tv = {
                            .declared_type = dt.type,
                            .domain = ValueDomain::kFourState,
                            .value = fs.value,
                            .unknown = fs.unknown};
                      } else {
                        tv = {
                            .declared_type = dt.type,
                            .domain = ValueDomain::kTwoState,
                            .value = raw,
                            .unknown = nullptr};
                      }
                      return {};
                    },
                    [&](const mir::Rvalue& rv) -> Result<void> {
                      auto rv_result =
                          LowerRvalue(context, resolver, rv, dt.type);
                      if (!rv_result) return std::unexpected(rv_result.error());
                      tv = rv_result->ToTempValue(dt.type);
                      return {};
                    },
                },
                dt.rhs);
            if (!result) return std::unexpected(result.error());
            context.BindTempValue(dt.temp_id, tv);
            return {};
          },
          [&](const mir::AssocOp& op) -> Result<void> {
            return LowerAssocOp(context, resolver, op);
          },
      },
      statement.data);
}

}  // namespace lyra::lowering::mir_to_llvm
