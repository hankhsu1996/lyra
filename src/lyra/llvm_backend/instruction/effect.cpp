#include "lyra/llvm_backend/instruction/effect.hpp"

#include <cstdint>
#include <expected>
#include <variant>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/display.hpp"
#include "lyra/llvm_backend/instruction/system_tf.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LowerTimeFormatEffect(Context& context, const mir::TimeFormatEffect& tf)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Get engine pointer
  auto* engine_ptr = context.GetEnginePointer();

  // Create global constant for suffix string
  auto* suffix_global =
      builder.CreateGlobalStringPtr(tf.suffix, "timeformat_suffix");

  // Call LyraSetTimeFormat(engine, units, precision, suffix, min_width)
  builder.CreateCall(
      context.GetLyraSetTimeFormat(),
      {engine_ptr,
       llvm::ConstantInt::get(llvm::Type::getInt8Ty(llvm_ctx), tf.units),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), tf.precision),
       suffix_global,
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), tf.min_width)});

  return {};
}

auto LowerStrobeEffect(Context& context, const mir::StrobeEffect& strobe)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Get the thunk function (already declared via DeclareUserFunction)
  llvm::Function* thunk_fn = context.GetUserFunction(strobe.thunk);
  if (thunk_fn == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(), "$strobe thunk function not found",
        UnsupportedCategory::kFeature));
  }

  // Get engine and design state pointers from process state header
  llvm::Value* engine_ptr = context.GetEnginePointer();
  llvm::Value* design_ptr = context.GetDesignPointer();

  // Schedule the thunk for the Postponed region
  // LyraSchedulePostponed(engine, callback, design_state)
  builder.CreateCall(
      context.GetLyraSchedulePostponed(), {engine_ptr, thunk_fn, design_ptr});

  return {};
}

auto LowerMonitorEffect(Context& context, const mir::MonitorEffect& monitor)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Get the setup thunk function (already declared via DeclareUserFunction).
  // The setup thunk handles: initial print, serialization, and registration.
  llvm::Function* setup_fn = context.GetUserFunction(monitor.setup_thunk);
  if (setup_fn == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(), "$monitor setup thunk function not found",
        UnsupportedCategory::kFeature));
  }

  // Get design and engine pointers
  llvm::Value* design_ptr = context.GetDesignPointer();
  llvm::Value* engine_ptr = context.GetEnginePointer();

  // Call setup thunk - it handles initial print + serialization + registration
  // Setup thunk signature: void (DesignState*, Engine*)
  builder.CreateCall(setup_fn, {design_ptr, engine_ptr});

  return {};
}

auto LowerMonitorControlEffect(
    Context& context, const mir::MonitorControlEffect& control)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Get engine pointer
  llvm::Value* engine_ptr = context.GetEnginePointer();

  // Call LyraMonitorSetEnabled(engine, enable)
  auto& llvm_ctx = context.GetLlvmContext();
  builder.CreateCall(
      context.GetLyraMonitorSetEnabled(),
      {engine_ptr,
       llvm::ConstantInt::get(
           llvm::Type::getInt1Ty(llvm_ctx), control.enable ? 1 : 0)});

  return {};
}

auto LowerMemIOEffect(Context& context, const mir::MemIOEffect& mem_io)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  // Get array type info from target_type
  const Type& arr_type = types[mem_io.target_type];
  if (arr_type.Kind() != TypeKind::kUnpackedArray) {
    throw common::InternalError(
        "LowerMemIOEffect", "target must be unpacked array");
  }
  const auto& arr_info = arr_type.AsUnpackedArray();
  TypeId elem_type_id = arr_info.element_type;
  const Type& elem_type = types[elem_type_id];

  // Restriction: packed integrals only
  if (!IsPacked(elem_type)) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$readmem/$writemem: only packed integral elements supported",
        UnsupportedCategory::kType));
  }

  // Determine if 4-state
  bool is_four_state = IsPackedFourState(elem_type, types);

  // Extract type info
  auto element_width = static_cast<int32_t>(PackedBitWidth(elem_type, types));
  auto element_count = static_cast<int32_t>(arr_info.range.Size());
  auto min_addr = static_cast<int64_t>(arr_info.range.Lower());

  // Compute stride_bytes and value_size_bytes from DataLayout (authoritative)
  auto elem_ops_result = context.GetElemOpsForType(elem_type_id);
  if (!elem_ops_result) return std::unexpected(elem_ops_result.error());
  llvm::Type* elem_llvm_type = elem_ops_result->elem_llvm_type;
  int32_t stride_bytes = elem_ops_result->elem_size;

  int32_t value_size_bytes = stride_bytes;
  if (is_four_state) {
    // For 4-state struct {value, x_mask}, value_size is half the stride
    auto* struct_ty = llvm::cast<llvm::StructType>(elem_llvm_type);
    const auto& dl = context.GetModule().getDataLayout();
    value_size_bytes =
        static_cast<int32_t>(dl.getTypeAllocSize(struct_ty->getElementType(0)));
  }

  int32_t element_kind = is_four_state ? 1 : 0;

  // Get target pointer
  auto target_ptr_result = context.GetPlacePointer(mem_io.target);
  if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
  llvm::Value* target_ptr = *target_ptr_result;

  // Lower filename operand (string handle)
  auto filename_result = LowerOperand(context, mem_io.filename);
  if (!filename_result) return std::unexpected(filename_result.error());
  llvm::Value* filename_handle = *filename_result;

  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);

  // Determine step from array direction
  int64_t step = arr_info.range.IsDescending() ? -1 : 1;
  int64_t left = arr_info.range.left;
  int64_t right = arr_info.range.right;

  // Default: left_bound -> right_bound (IEEE LRM semantics)
  llvm::Value* eff_start = llvm::ConstantInt::get(i64_ty, left);
  if (mem_io.start_addr) {
    auto start_result = LowerOperand(context, *mem_io.start_addr);
    if (!start_result) return std::unexpected(start_result.error());
    eff_start = builder.CreateSExtOrTrunc(*start_result, i64_ty, "eff_start");
  }

  llvm::Value* eff_end = llvm::ConstantInt::get(i64_ty, right);
  if (mem_io.end_addr) {
    auto end_result = LowerOperand(context, *mem_io.end_addr);
    if (!end_result) return std::unexpected(end_result.error());
    eff_end = builder.CreateSExtOrTrunc(*end_result, i64_ty, "eff_end");
  }

  // Compute current/final consistent with step direction
  // step > 0: current = min(start,end), final = max(start,end)
  // step < 0: current = max(start,end), final = min(start,end)
  llvm::Value* cmp = builder.CreateICmpSLT(eff_start, eff_end, "start_lt_end");
  llvm::Value* low = builder.CreateSelect(cmp, eff_start, eff_end, "eff_low");
  llvm::Value* high = builder.CreateSelect(cmp, eff_end, eff_start, "eff_high");

  llvm::Value* current_addr = (step > 0) ? low : high;
  llvm::Value* final_addr = (step > 0) ? high : low;

  // Emit runtime call
  std::vector<llvm::Value*> args = {
      filename_handle,
      target_ptr,
      llvm::ConstantInt::get(i32_ty, element_width),
      llvm::ConstantInt::get(i32_ty, stride_bytes),
      llvm::ConstantInt::get(i32_ty, value_size_bytes),
      llvm::ConstantInt::get(i32_ty, element_count),
      llvm::ConstantInt::get(i64_ty, min_addr),
      current_addr,
      final_addr,
      llvm::ConstantInt::get(i64_ty, step),
      llvm::ConstantInt::get(i1_ty, mem_io.is_hex ? 1 : 0),
      llvm::ConstantInt::get(i32_ty, element_kind),
  };

  if (mem_io.is_read) {
    builder.CreateCall(context.GetLyraReadmem(), args);
  } else {
    builder.CreateCall(context.GetLyraWritemem(), args);
  }

  return {};
}

}  // namespace

auto LowerEffectOp(Context& context, const mir::EffectOp& effect_op)
    -> Result<void> {
  return std::visit(
      common::Overloaded{
          [&context](const mir::DisplayEffect& display) -> Result<void> {
            return LowerDisplayEffect(context, display);
          },
          [&context](const mir::SeverityEffect& severity) -> Result<void> {
            return LowerSeverityEffect(context, severity);
          },
          [&context](const mir::MemIOEffect& mem_io) -> Result<void> {
            return LowerMemIOEffect(context, mem_io);
          },
          [&context](const mir::TimeFormatEffect& tf) -> Result<void> {
            return LowerTimeFormatEffect(context, tf);
          },
          [&context](const mir::SystemTfEffect& effect) -> Result<void> {
            return LowerSystemTfEffect(context, effect);
          },
          [&context](const mir::StrobeEffect& strobe) -> Result<void> {
            return LowerStrobeEffect(context, strobe);
          },
          [&context](const mir::MonitorEffect& monitor) -> Result<void> {
            return LowerMonitorEffect(context, monitor);
          },
          [&context](const mir::MonitorControlEffect& control) -> Result<void> {
            return LowerMonitorControlEffect(context, control);
          },
          [&context](const mir::FillPackedEffect& /*fill*/) -> Result<void> {
            // TODO(hankhsu): Implement FillPacked lowering
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    "assignment pattern fill not yet implemented in LLVM "
                    "backend",
                    UnsupportedCategory::kType));
          },
      },
      effect_op);
}

}  // namespace lyra::lowering::mir_to_llvm
