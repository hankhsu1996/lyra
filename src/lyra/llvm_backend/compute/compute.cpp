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
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/llvm_backend/instruction/system_tf.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// LowerTestPlusargsRvalue: $test$plusargs (pure, no side effects)
// Returns 1 if a plusarg matching the query prefix exists, 0 otherwise.
// Query is stored in info.query as TypedOperand for packed-to-string coercion.
auto LowerTestPlusargsRvalue(
    Context& context, const mir::Rvalue& rvalue,
    const mir::TestPlusargsRvalueInfo& info) -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();

  llvm::Value* result = nullptr;
  auto status = WithStringHandle(
      context, info.query.operand, info.query.type,
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
auto LowerFopenRvalue(Context& context, const mir::FopenRvalueInfo& info)
    -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();

  llvm::Value* result = nullptr;

  // Use nested WithStringHandle for filename (and optionally mode)
  auto status = WithStringHandle(
      context, info.filename.operand, info.filename.type,
      [&](llvm::Value* filename_handle) -> Result<void> {
        if (info.mode) {
          // FD mode: $fopen(filename, mode) - nested WithStringHandle
          return WithStringHandle(
              context, info.mode->operand, info.mode->type,
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
          [&](const mir::TestPlusargsRvalueInfo& info) -> Result<RvalueValue> {
            return LowerTestPlusargsRvalue(context, rvalue, info);
          },
          [&](const mir::FopenRvalueInfo& info) -> Result<RvalueValue> {
            return LowerFopenRvalue(context, info);
          },
          [&](const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue> {
            return LowerSystemTfRvalue(context, rvalue, info);
          },
          [&](const mir::ArrayQueryRvalueInfo&) -> Result<RvalueValue> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    "array query functions ($left, $right, etc.) are not "
                    "supported "
                    "in the LLVM backend; use MIR interpreter instead",
                    UnsupportedCategory::kFeature));
          },
      },
      rvalue.info);
}

}  // namespace lyra::lowering::mir_to_llvm
