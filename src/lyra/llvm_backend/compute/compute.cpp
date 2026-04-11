#include "lyra/llvm_backend/compute/compute.hpp"

#include <expected>
#include <variant>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/compute/aggregate.hpp"
#include "lyra/llvm_backend/compute/array_query.hpp"
#include "lyra/llvm_backend/compute/builtin.hpp"
#include "lyra/llvm_backend/compute/cast.hpp"
#include "lyra/llvm_backend/compute/driver.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/math.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/real.hpp"
#include "lyra/llvm_backend/compute/string.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/llvm_backend/instruction/system_tf.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// LowerTestPlusargsRvalue: $test$plusargs (pure, no side effects)
// Returns 1 if a plusarg matching the query prefix exists, 0 otherwise.
// Query is stored in info.query as TypedOperand for packed-to-string coercion.
auto LowerTestPlusargsRvalue(
    Context& context, const CuFacts& facts,
    const mir::TestPlusargsRvalueInfo& info) -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();

  llvm::Value* result = nullptr;
  auto status = WithStringHandle(
      context, facts, info.query.operand, info.query.type,
      [&](llvm::Value* query_handle) -> Result<void> {
        result = builder.CreateCall(
            context.GetLyraPlusargsTest(),
            {context.GetEnginePointer(), query_handle});
        return {};
      });

  if (!status) return std::unexpected(status.error());
  return RvalueValue::TwoState(result);
}

// LowerFopenRvalue: $fopen system function
// - MCD mode: $fopen(filename) - opens for writing, returns multi-channel desc
// - FD mode: $fopen(filename, mode) - opens with mode, returns file descriptor
// Uses nested WithStringHandle for automatic handle release.
auto LowerFopenRvalue(
    Context& context, const CuFacts& facts, const mir::FopenRvalueInfo& info)
    -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();

  llvm::Value* result = nullptr;

  // Use nested WithStringHandle for filename (and optionally mode)
  auto status = WithStringHandle(
      context, facts, info.filename.operand, info.filename.type,
      [&](llvm::Value* filename_handle) -> Result<void> {
        if (info.mode) {
          // FD mode: $fopen(filename, mode) - nested WithStringHandle
          return WithStringHandle(
              context, facts, info.mode->operand, info.mode->type,
              [&](llvm::Value* mode_handle) -> Result<void> {
                result = builder.CreateCall(
                    context.GetLyraFopenFd(),
                    {context.GetEnginePointer(), filename_handle, mode_handle},
                    "fopen.fd");
                return {};
              });
        }
        // MCD mode: $fopen(filename)
        result = builder.CreateCall(
            context.GetLyraFopenMcd(),
            {context.GetEnginePointer(), filename_handle}, "fopen.mcd");
        return {};
      });

  if (!status) return std::unexpected(status.error());
  return RvalueValue::TwoState(result);
}

}  // namespace

auto LowerRvalue(
    Context& context, const CuFacts& facts, const mir::Rvalue& rvalue,
    TypeId result_type) -> Result<RvalueValue> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerRvalue(context, facts, canonical, rvalue, result_type);
}

// Intentionally canonical-only rvalue kinds (no module-slot operand reads):
// LowerTestPlusargsRvalue, LowerFopenRvalue, LowerSystemCmdRvalue.
// These interact with the runtime or produce constants and never read
// module-slot places as operands.

auto LowerRvalue(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Rvalue& rvalue, TypeId result_type) -> Result<RvalueValue> {
  const auto& types = *facts.types;

  return std::visit(
      common::Overloaded{
          [&](const mir::CastRvalueInfo& info) -> Result<RvalueValue> {
            return LowerCastRvalue(
                context, facts, resolver, rvalue, info.target_type);
          },
          [&](const mir::BitCastRvalueInfo& info) -> Result<RvalueValue> {
            return LowerBitCastRvalue(
                context, facts, resolver, rvalue, info.target_type);
          },
          [&](const mir::AggregateRvalueInfo& info) -> Result<RvalueValue> {
            return LowerAggregateRvalue(
                context, facts, rvalue, result_type, info);
          },
          [&](const mir::SFormatRvalueInfo& info) -> Result<RvalueValue> {
            auto val_or_err = LowerSFormatRvalueValue(
                context, facts, resolver, info, rvalue.operands);
            if (!val_or_err) return std::unexpected(val_or_err.error());
            return RvalueValue::TwoState(*val_or_err);
          },
          [&](const mir::BuiltinCallRvalueInfo& info) -> Result<RvalueValue> {
            return LowerBuiltinRvalue(
                context, facts, resolver, rvalue, result_type, info);
          },
          [&](const mir::BinaryRvalueInfo&) -> Result<RvalueValue> {
            if (IsMathRvalue(rvalue)) {
              return LowerMathRvalue(
                  context, facts, resolver, rvalue, result_type);
            }
            if (IsRealMathRvalue(facts, context, rvalue)) {
              return LowerRealRvalue(
                  context, facts, resolver, rvalue, result_type);
            }
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::UnaryRvalueInfo&) -> Result<RvalueValue> {
            if (IsMathRvalue(rvalue)) {
              return LowerMathRvalue(
                  context, facts, resolver, rvalue, result_type);
            }
            if (IsRealMathRvalue(facts, context, rvalue)) {
              return LowerRealRvalue(
                  context, facts, resolver, rvalue, result_type);
            }
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<RvalueValue> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              auto val_or_err = LowerStringConcatValue(
                  context, facts, resolver, info, rvalue.operands);
              if (!val_or_err) return std::unexpected(val_or_err.error());
              return RvalueValue::TwoState(*val_or_err);
            }
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::ReplicateRvalueInfo& info) -> Result<RvalueValue> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              auto val_or_err = LowerStringReplicateValue(
                  context, facts, resolver, info, rvalue.operands);
              if (!val_or_err) return std::unexpected(val_or_err.error());
              return RvalueValue::TwoState(*val_or_err);
            }
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::RuntimeQueryRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::IsKnownRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::IndexInRangeRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::GuardedUseRvalueInfo&) -> Result<RvalueValue> {
            return LowerPackedCoreRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::MathCallRvalueInfo&) -> Result<RvalueValue> {
            return LowerMathRvalue(
                context, facts, resolver, rvalue, result_type);
          },
          [&](const mir::TestPlusargsRvalueInfo& info) -> Result<RvalueValue> {
            return LowerTestPlusargsRvalue(context, facts, info);
          },
          [&](const mir::FopenRvalueInfo& info) -> Result<RvalueValue> {
            return LowerFopenRvalue(context, facts, info);
          },
          [&](const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue> {
            return LowerSystemTfRvalue(context, facts, resolver, rvalue, info);
          },
          [&](const mir::ArrayQueryRvalueInfo& info) -> Result<RvalueValue> {
            return LowerArrayQueryRvalue(
                context, facts, resolver, rvalue, info, result_type);
          },
          [&](const mir::SelectRvalueInfo&) -> Result<RvalueValue> {
            auto cond_or_err =
                LowerOperand(context, facts, resolver, rvalue.operands[0]);
            if (!cond_or_err) return std::unexpected(cond_or_err.error());
            auto true_or_err =
                LowerOperandRaw(context, facts, resolver, rvalue.operands[1]);
            if (!true_or_err) return std::unexpected(true_or_err.error());
            auto false_or_err =
                LowerOperandRaw(context, facts, resolver, rvalue.operands[2]);
            if (!false_or_err) return std::unexpected(false_or_err.error());
            auto& builder = context.GetBuilder();
            llvm::Value* cond_i1 = *cond_or_err;
            if (!cond_i1->getType()->isIntegerTy(1)) {
              cond_i1 = builder.CreateICmpNE(
                  cond_i1, llvm::ConstantInt::get(cond_i1->getType(), 0),
                  "select.cond");
            }
            auto* result = builder.CreateSelect(
                cond_i1, *true_or_err, *false_or_err, "select");
            return RvalueValue::TwoState(result);
          },
          [&](const mir::ExternalReadRvalueInfo& info) -> Result<RvalueValue> {
            auto val_or_err = context.LoadExternalRef(info.ref);
            if (!val_or_err) return std::unexpected(val_or_err.error());
            llvm::Value* raw = *val_or_err;
            TypeId ref_type = context.GetExternalRefType(info.ref);
            const Type& type = types[ref_type];
            if (IsPacked(type) && context.IsPackedFourState(type)) {
              auto fs = ExtractFourState(context.GetBuilder(), raw);
              return RvalueValue::FourState(fs.value, fs.unknown);
            }
            return RvalueValue::TwoState(raw);
          },
          [&](const mir::SystemCmdRvalueInfo& info) -> Result<RvalueValue> {
            auto& builder = context.GetBuilder();
            auto* engine = context.GetEnginePointer();
            llvm::Value* result = nullptr;
            if (!info.command) {
              auto* null_ptr = llvm::ConstantPointerNull::get(
                  llvm::PointerType::getUnqual(context.GetLlvmContext()));
              result = builder.CreateCall(
                  context.GetLyraSystemCmd(), {engine, null_ptr});
            } else {
              auto status = WithStringHandle(
                  context, facts, info.command->operand, info.command->type,
                  [&](llvm::Value* handle) -> Result<void> {
                    result = builder.CreateCall(
                        context.GetLyraSystemCmd(), {engine, handle});
                    return {};
                  });
              if (!status) return std::unexpected(status.error());
            }
            return RvalueValue::TwoState(result);
          },
      },
      rvalue.info);
}

}  // namespace lyra::lowering::mir_to_llvm
