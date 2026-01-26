#include "lyra/llvm_backend/instruction_compute.hpp"

#include <expected>
#include <format>
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
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_2state.hpp"
#include "lyra/llvm_backend/instruction_compute_4state.hpp"
#include "lyra/llvm_backend/instruction_compute_aggregate.hpp"
#include "lyra/llvm_backend/instruction_compute_builtin.hpp"
#include "lyra/llvm_backend/instruction_compute_cast.hpp"
#include "lyra/llvm_backend/instruction_compute_math.hpp"
#include "lyra/llvm_backend/instruction_compute_real.hpp"
#include "lyra/llvm_backend/instruction_compute_rvalue.hpp"
#include "lyra/llvm_backend/instruction_compute_string.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Dispatch to 2-state or 4-state based on target type.
auto LowerIntegral(Context& context, const mir::Compute& compute)
    -> Result<void> {
  auto type_info_or_err = ValidateAndGetTypeInfo(context, compute.target);
  if (!type_info_or_err) return std::unexpected(type_info_or_err.error());
  PlaceTypeInfo type_info = *type_info_or_err;
  if (type_info.is_four_state) {
    return LowerCompute4State(context, compute, type_info.bit_width);
  }
  return LowerCompute2State(context, compute, type_info.bit_width);
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
            return LowerIntegral(context, compute);
          },
          [&](const mir::UnaryRvalueInfo&) -> Result<void> {
            if (IsMathCompute(context, compute)) {
              return LowerMathCompute(context, compute);
            }
            if (IsRealMathCompute(context, compute)) {
              return LowerRealCompute(context, compute);
            }
            return LowerIntegral(context, compute);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<void> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              return LowerStringConcat(
                  context, info, compute.value.operands, compute.target);
            }
            return LowerIntegral(context, compute);
          },
          [&](const mir::UserCallRvalueInfo& info) -> Result<void> {
            llvm::Function* callee = context.GetUserFunction(info.callee);
            if (callee->getReturnType()->isVoidTy()) {
              return LowerVoidUserCall(context, compute, info);
            }
            return LowerIntegral(context, compute);
          },
          [&](const mir::RuntimeQueryRvalueInfo&) -> Result<void> {
            return LowerIntegral(context, compute);
          },
          [&](const mir::IndexValidityRvalueInfo&) -> Result<void> {
            return LowerIntegral(context, compute);
          },
          [&](const mir::GuardedUseRvalueInfo&) -> Result<void> {
            return LowerIntegral(context, compute);
          },
          [&](const mir::MathCallRvalueInfo&) -> Result<void> {
            return LowerMathCompute(context, compute);
          },
          [&](const mir::SystemCallRvalueInfo&) -> Result<void> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    "system calls not yet supported in LLVM backend",
                    UnsupportedCategory::kFeature));
          },
          [&](const mir::PlusargsRvalueInfo&) -> Result<void> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    "$plusargs not yet supported in LLVM backend",
                    UnsupportedCategory::kFeature));
          },
          [&](const mir::FopenRvalueInfo&) -> Result<void> {
            auto& builder = context.GetBuilder();
            const auto& rv = compute.value;

            // Validate operand count
            if (rv.operands.size() != 1 && rv.operands.size() != 2) {
              throw common::InternalError(
                  "LowerFopen",
                  std::format(
                      "expected 1 or 2 operands, got {}", rv.operands.size()));
            }

            // Get filename operand (string handle)
            auto filename_or_err = LowerOperand(context, rv.operands[0]);
            if (!filename_or_err)
              return std::unexpected(filename_or_err.error());

            llvm::Value* result = nullptr;
            if (rv.operands.size() == 2) {
              // FD mode: $fopen(filename, mode)
              auto mode_or_err = LowerOperand(context, rv.operands[1]);
              if (!mode_or_err) return std::unexpected(mode_or_err.error());
              result = builder.CreateCall(
                  context.GetLyraFopenFd(),
                  {context.GetEnginePointer(), *filename_or_err, *mode_or_err},
                  "fopen.fd");
            } else {
              // MCD mode: $fopen(filename)
              result = builder.CreateCall(
                  context.GetLyraFopenMcd(),
                  {context.GetEnginePointer(), *filename_or_err}, "fopen.mcd");
            }

            // Store result to target
            auto target_ptr = context.GetPlacePointer(compute.target);
            if (!target_ptr) return std::unexpected(target_ptr.error());
            builder.CreateStore(result, *target_ptr);
            return {};
          },
      },
      compute.value.info);
}

}  // namespace lyra::lowering::mir_to_llvm
