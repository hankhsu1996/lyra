#include "lyra/llvm_backend/compute/compute.hpp"

#include <expected>
#include <variant>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/compute/aggregate.hpp"
#include "lyra/llvm_backend/compute/builtin.hpp"
#include "lyra/llvm_backend/compute/cast.hpp"
#include "lyra/llvm_backend/compute/driver.hpp"
#include "lyra/llvm_backend/compute/math.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/real.hpp"
#include "lyra/llvm_backend/compute/string.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/system_tf.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// LowerTestPlusargsRvalue: $test$plusargs (pure, no side effects)
// Returns 1 if a plusarg matching the query prefix exists, 0 otherwise.
auto LowerTestPlusargsRvalue(Context& context, const mir::Rvalue& rvalue)
    -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();

  auto query_or_err = LowerOperand(context, rvalue.operands[0]);
  if (!query_or_err) return std::unexpected(query_or_err.error());

  llvm::Value* result = builder.CreateCall(
      context.GetLyraPlusargsTest(),
      {context.GetEnginePointer(), *query_or_err});

  return RvalueValue::TwoState(result);
}

}  // namespace

auto LowerRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue> {
  const auto& types = context.GetTypeArena();

  return std::visit(
      common::Overloaded{
          [&](const mir::CastRvalueInfo& info) -> Result<RvalueValue> {
            return LowerCastRvalue(context, rvalue, info.target_type);
          },
          [&](const mir::BitCastRvalueInfo& info) -> Result<RvalueValue> {
            return LowerBitCastRvalue(context, rvalue, info.target_type);
          },
          [&](const mir::AggregateRvalueInfo& info) -> Result<RvalueValue> {
            return LowerAggregateRvalue(context, rvalue, result_type, info);
          },
          [&](const mir::SFormatRvalueInfo& info) -> Result<RvalueValue> {
            auto val_or_err =
                LowerSFormatRvalueValue(context, info, rvalue.operands);
            if (!val_or_err) return std::unexpected(val_or_err.error());
            return RvalueValue::TwoState(*val_or_err);
          },
          [&](const mir::BuiltinCallRvalueInfo& info) -> Result<RvalueValue> {
            return LowerBuiltinRvalue(context, rvalue, result_type, info);
          },
          [&](const mir::BinaryRvalueInfo&) -> Result<RvalueValue> {
            if (IsMathRvalue(rvalue)) {
              return LowerMathRvalue(context, rvalue, result_type);
            }
            if (IsRealMathRvalue(context, rvalue)) {
              return LowerRealRvalue(context, rvalue, result_type);
            }
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::UnaryRvalueInfo&) -> Result<RvalueValue> {
            if (IsMathRvalue(rvalue)) {
              return LowerMathRvalue(context, rvalue, result_type);
            }
            if (IsRealMathRvalue(context, rvalue)) {
              return LowerRealRvalue(context, rvalue, result_type);
            }
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<RvalueValue> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              auto val_or_err =
                  LowerStringConcatValue(context, info, rvalue.operands);
              if (!val_or_err) return std::unexpected(val_or_err.error());
              return RvalueValue::TwoState(*val_or_err);
            }
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::ReplicateRvalueInfo& info) -> Result<RvalueValue> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              auto val_or_err =
                  LowerStringReplicateValue(context, info, rvalue.operands);
              if (!val_or_err) return std::unexpected(val_or_err.error());
              return RvalueValue::TwoState(*val_or_err);
            }
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::RuntimeQueryRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::IndexValidityRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::GuardedUseRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(context, rvalue, result_type);
          },
          [&](const mir::MathCallRvalueInfo&) -> Result<RvalueValue> {
            return LowerMathRvalue(context, rvalue, result_type);
          },
          [&](const mir::TestPlusargsRvalueInfo&) -> Result<RvalueValue> {
            return LowerTestPlusargsRvalue(context, rvalue);
          },
          [&](const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue> {
            return LowerSystemTfRvalue(context, rvalue, info);
          },
      },
      rvalue.info);
}

}  // namespace lyra::lowering::mir_to_llvm
