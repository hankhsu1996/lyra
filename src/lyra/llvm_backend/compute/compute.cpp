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
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

enum class PlusargsOutputKind { kInt32, kString };

auto GetPlusargsOutputKind(const TypeArena& types, TypeId output_type)
    -> PlusargsOutputKind {
  const Type& type = types[output_type];
  if (type.Kind() == TypeKind::kString) {
    return PlusargsOutputKind::kString;
  }
  return PlusargsOutputKind::kInt32;
}

auto LowerPlusargsCompute(
    Context& context, const mir::Compute& compute,
    const mir::PlusargsRvalueInfo& info) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  // Lower query/format string operand (already LyraStringHandle via
  // LowerOperand)
  auto query_or_err = LowerOperand(context, compute.value.operands[0]);
  if (!query_or_err) return std::unexpected(query_or_err.error());

  auto target_ptr = context.GetPlacePointer(compute.target);
  if (!target_ptr) return std::unexpected(target_ptr.error());

  llvm::Value* result = nullptr;

  if (info.kind == mir::PlusargsKind::kTest) {
    result = builder.CreateCall(
        context.GetLyraPlusargsTest(),
        {context.GetEnginePointer(), *query_or_err});
  } else {
    // kValue: output must be present (MIR invariant)
    if (!info.output) {
      throw common::InternalError(
          "LowerPlusargsCompute", "kValue without output place");
    }

    auto output_ptr = context.GetPlacePointer(*info.output);
    if (!output_ptr) return std::unexpected(output_ptr.error());

    auto kind = GetPlusargsOutputKind(types, info.output_type);
    if (kind == PlusargsOutputKind::kString) {
      result = builder.CreateCall(
          context.GetLyraPlusargsValueString(),
          {context.GetEnginePointer(), *query_or_err, *output_ptr});
    } else {
      result = builder.CreateCall(
          context.GetLyraPlusargsValueInt(),
          {context.GetEnginePointer(), *query_or_err, *output_ptr});
    }
  }

  builder.CreateStore(result, *target_ptr);
  return {};
}

// Handle void user call (side effects only, no result).
auto LowerVoidUserCall(
    Context& context, const mir::Compute& compute,
    const mir::UserCallRvalueInfo& info) -> Result<void> {
  auto& builder = context.GetBuilder();
  llvm::Function* callee = context.GetUserFunction(info.callee);

  std::vector<llvm::Value*> args;
  args.push_back(context.GetDesignPointer());
  args.push_back(context.GetEnginePointer());
  for (const auto& operand : compute.value.operands) {
    auto val_result = LowerOperand(context, operand);
    if (!val_result) return std::unexpected(val_result.error());
    args.push_back(*val_result);
  }
  builder.CreateCall(callee, args);
  return {};
}

// Handle out-param user call (managed return type).
// Caller provides output pointer; callee writes directly to it.
auto LowerOutParamUserCall(
    Context& context, const mir::Compute& compute,
    const mir::UserCallRvalueInfo& info, const mir::Arena& arena)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  llvm::Function* callee = context.GetUserFunction(info.callee);
  const auto& func = arena[info.callee];

  // Get target pointer for out-param output
  auto target_ptr = context.GetPlacePointer(compute.target);
  if (!target_ptr) return std::unexpected(target_ptr.error());

  // CONTRACT: Out-param calling convention for managed returns.
  // - Caller (here): Destroy() any existing value, making out slot
  // uninitialized
  // - Callee: MUST fully initialize the out slot via MoveInit before returning
  // - Valid values include nullptr (represents empty string/container)
  // Do NOT remove this Destroy - callee assumes uninitialized destination.
  Destroy(context, *target_ptr, func.signature.return_type);

  // Build args: [out_ptr, design_ptr, engine_ptr, user_args...]
  std::vector<llvm::Value*> args;
  args.push_back(*target_ptr);  // out-param
  args.push_back(context.GetDesignPointer());
  args.push_back(context.GetEnginePointer());
  for (const auto& operand : compute.value.operands) {
    auto val_result = LowerOperand(context, operand);
    if (!val_result) return std::unexpected(val_result.error());
    args.push_back(*val_result);
  }

  // Void call - callee writes directly to out-param
  builder.CreateCall(callee, args);
  return {};
}

}  // namespace

auto LowerCompute(Context& context, const mir::Compute& compute)
    -> Result<void> {
  const auto& types = context.GetTypeArena();

  return std::visit(
      common::Overloaded{
          [&](const mir::CastRvalueInfo&) -> Result<void> {
            LowerCastUnified(context, compute);
            return {};
          },
          [&](const mir::BitCastRvalueInfo&) -> Result<void> {
            LowerBitCastUnified(context, compute);
            return {};
          },
          [&](const mir::AggregateRvalueInfo& info) -> Result<void> {
            return LowerAggregate(context, compute, info);
          },
          [&](const mir::SFormatRvalueInfo& info) -> Result<void> {
            return LowerSFormatRvalue(
                context, info, compute.value.operands, compute.target);
          },
          [&](const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
            return LowerBuiltin(context, compute, info);
          },
          [&](const mir::BinaryRvalueInfo&) -> Result<void> {
            if (IsMathCompute(context, compute)) {
              return LowerMathCompute(context, compute);
            }
            if (IsRealMathCompute(context, compute)) {
              return LowerRealCompute(context, compute);
            }
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::UnaryRvalueInfo&) -> Result<void> {
            if (IsMathCompute(context, compute)) {
              return LowerMathCompute(context, compute);
            }
            if (IsRealMathCompute(context, compute)) {
              return LowerRealCompute(context, compute);
            }
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<void> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              return LowerStringConcat(
                  context, info, compute.value.operands, compute.target);
            }
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::UserCallRvalueInfo& info) -> Result<void> {
            // Check if this is an out-param call (managed return type)
            if (context.FunctionUsesSret(info.callee)) {
              return LowerOutParamUserCall(
                  context, compute, info, context.GetMirArena());
            }
            llvm::Function* callee = context.GetUserFunction(info.callee);
            if (callee->getReturnType()->isVoidTy()) {
              return LowerVoidUserCall(context, compute, info);
            }
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::RuntimeQueryRvalueInfo&) -> Result<void> {
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::IndexValidityRvalueInfo&) -> Result<void> {
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::GuardedUseRvalueInfo&) -> Result<void> {
            return LowerPackedCoreRvalue(context, compute);
          },
          [&](const mir::MathCallRvalueInfo&) -> Result<void> {
            return LowerMathCompute(context, compute);
          },
          [&](const mir::PlusargsRvalueInfo& info) -> Result<void> {
            return LowerPlusargsCompute(context, compute, info);
          },
          [&](const mir::SystemTfRvalueInfo& info) -> Result<void> {
            return LowerSystemTfRvalue(context, compute, info);
          },
      },
      compute.value.info);
}

}  // namespace lyra::lowering::mir_to_llvm
