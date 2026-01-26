#include "lyra/llvm_backend/instruction_compute.hpp"

#include <variant>
#include <vector>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
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
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Dispatch to 2-state or 4-state based on target type.
void LowerIntegral(Context& context, const mir::Compute& compute) {
  PlaceTypeInfo type_info = ValidateAndGetTypeInfo(context, compute.target);
  if (type_info.is_four_state) {
    LowerCompute4State(context, compute, type_info.bit_width);
  } else {
    LowerCompute2State(context, compute, type_info.bit_width);
  }
}

// Handle void user call (side effects only, no result).
void LowerVoidUserCall(
    Context& context, const mir::Compute& compute,
    const mir::UserCallRvalueInfo& info) {
  auto& builder = context.GetBuilder();
  llvm::Function* callee = context.GetUserFunction(info.callee);

  std::vector<llvm::Value*> args;
  args.push_back(context.GetDesignPointer());
  args.push_back(context.GetEnginePointer());
  for (const auto& operand : compute.value.operands) {
    args.push_back(LowerOperand(context, operand));
  }
  builder.CreateCall(callee, args);
}

}  // namespace

void LowerCompute(Context& context, const mir::Compute& compute) {
  const auto& types = context.GetTypeArena();

  std::visit(
      common::Overloaded{
          [&](const mir::CastRvalueInfo&) {
            LowerCastUnified(context, compute);
          },
          [&](const mir::BitCastRvalueInfo&) {
            LowerBitCastUnified(context, compute);
          },
          [&](const mir::AggregateRvalueInfo& info) {
            LowerAggregate(context, compute, info);
          },
          [&](const mir::SFormatRvalueInfo& info) {
            LowerSFormatRvalue(
                context, info, compute.value.operands, compute.target);
          },
          [&](const mir::BuiltinCallRvalueInfo& info) {
            LowerBuiltin(context, compute, info);
          },
          [&](const mir::BinaryRvalueInfo&) {
            if (IsMathCompute(context, compute)) {
              LowerMathCompute(context, compute);
            } else if (IsRealMathCompute(context, compute)) {
              LowerRealCompute(context, compute);
            } else {
              LowerIntegral(context, compute);
            }
          },
          [&](const mir::UnaryRvalueInfo&) {
            if (IsMathCompute(context, compute)) {
              LowerMathCompute(context, compute);
            } else if (IsRealMathCompute(context, compute)) {
              LowerRealCompute(context, compute);
            } else {
              LowerIntegral(context, compute);
            }
          },
          [&](const mir::ConcatRvalueInfo& info) {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              LowerStringConcat(
                  context, info, compute.value.operands, compute.target);
            } else {
              LowerIntegral(context, compute);
            }
          },
          [&](const mir::UserCallRvalueInfo& info) {
            llvm::Function* callee = context.GetUserFunction(info.callee);
            if (callee->getReturnType()->isVoidTy()) {
              LowerVoidUserCall(context, compute, info);
            } else {
              LowerIntegral(context, compute);
            }
          },
          [&](const mir::RuntimeQueryRvalueInfo&) {
            LowerIntegral(context, compute);
          },
          [&](const mir::IndexValidityRvalueInfo&) {
            LowerIntegral(context, compute);
          },
          [&](const mir::GuardedUseRvalueInfo&) {
            LowerIntegral(context, compute);
          },
          [&](const mir::MathCallRvalueInfo&) {
            LowerMathCompute(context, compute);
          },
          [&](const mir::SystemCallRvalueInfo&) {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                "system calls not yet supported in LLVM backend");
          },
          [&](const mir::PlusargsRvalueInfo&) {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                "$plusargs not yet supported in LLVM backend");
          },
          [&](const mir::FopenRvalueInfo&) {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                "$fopen not yet supported in LLVM backend");
          },
      },
      compute.value.info);
}

}  // namespace lyra::lowering::mir_to_llvm
