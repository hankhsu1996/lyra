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

auto LowerPlusargsRvalue(
    Context& context, const mir::Rvalue& rvalue, mir::PlaceId target,
    const mir::PlusargsRvalueInfo& info) -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto query_or_err = LowerOperand(context, rvalue.operands[0]);
  if (!query_or_err) return std::unexpected(query_or_err.error());

  llvm::Value* result = nullptr;

  if (info.kind == mir::PlusargsKind::kTest) {
    result = builder.CreateCall(
        context.GetLyraPlusargsTest(),
        {context.GetEnginePointer(), *query_or_err});
  } else {
    // kValue: output must be present (MIR invariant)
    if (!info.output) {
      throw common::InternalError(
          "LowerPlusargsRvalue", "kValue without output place");
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

  return result;
}

}  // namespace

auto LowerRvalue(
    Context& context, const mir::Rvalue& rvalue, mir::PlaceId target,
    TypeId result_type, llvm::Value** unknown_out) -> Result<llvm::Value*> {
  const auto& types = context.GetTypeArena();

  // Initialize unknown_out to nullptr (will be set for 4-state packed)
  if (unknown_out != nullptr) {
    *unknown_out = nullptr;
  }

  // Create a temporary Compute to pass to handlers that still use old API
  mir::Compute compute{.target = target, .value = rvalue};

  return std::visit(
      common::Overloaded{
          [&](const mir::CastRvalueInfo&) -> Result<llvm::Value*> {
            return LowerCastRvalue(context, compute, unknown_out);
          },
          [&](const mir::BitCastRvalueInfo&) -> Result<llvm::Value*> {
            return LowerBitCastRvalue(context, compute, unknown_out);
          },
          [&](const mir::AggregateRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerAggregateRvalue(context, compute, info);
          },
          [&](const mir::SFormatRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerSFormatRvalueValue(context, info, rvalue.operands);
          },
          [&](const mir::BuiltinCallRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerBuiltinRvalue(context, compute, info);
          },
          [&](const mir::BinaryRvalueInfo&) -> Result<llvm::Value*> {
            if (IsMathCompute(context, compute)) {
              return LowerMathRvalue(context, compute, unknown_out);
            }
            if (IsRealMathCompute(context, compute)) {
              return LowerRealRvalue(context, compute);
            }
            return LowerPackedCoreRvalueValue(context, compute, unknown_out);
          },
          [&](const mir::UnaryRvalueInfo&) -> Result<llvm::Value*> {
            if (IsMathCompute(context, compute)) {
              return LowerMathRvalue(context, compute, unknown_out);
            }
            if (IsRealMathCompute(context, compute)) {
              return LowerRealRvalue(context, compute);
            }
            return LowerPackedCoreRvalueValue(context, compute, unknown_out);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<llvm::Value*> {
            if (types[info.result_type].Kind() == TypeKind::kString) {
              return LowerStringConcatValue(context, info, rvalue.operands);
            }
            return LowerPackedCoreRvalueValue(context, compute, unknown_out);
          },
          [&](const mir::RuntimeQueryRvalueInfo&) -> Result<llvm::Value*> {
            return LowerPackedCoreRvalueValue(context, compute, unknown_out);
          },
          [&](const mir::IndexValidityRvalueInfo&) -> Result<llvm::Value*> {
            return LowerPackedCoreRvalueValue(context, compute, unknown_out);
          },
          [&](const mir::GuardedUseRvalueInfo&) -> Result<llvm::Value*> {
            return LowerPackedCoreRvalueValue(context, compute, unknown_out);
          },
          [&](const mir::MathCallRvalueInfo&) -> Result<llvm::Value*> {
            return LowerMathRvalue(context, compute, unknown_out);
          },
          [&](const mir::PlusargsRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerPlusargsRvalue(context, rvalue, target, info);
          },
          [&](const mir::SystemTfRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerSystemTfRvalueValue(context, compute, info);
          },
      },
      rvalue.info);
}

}  // namespace lyra::lowering::mir_to_llvm
