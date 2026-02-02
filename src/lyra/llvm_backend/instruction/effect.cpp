#include "lyra/llvm_backend/instruction/effect.hpp"

#include <cstdint>
#include <expected>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/abi_check.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/llvm_backend/instruction/display.hpp"
#include "lyra/llvm_backend/instruction/system_tf.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"

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
  std::vector<llvm::Value*> setup_args = {design_ptr, engine_ptr};
  VerifyCallAbi(setup_fn, setup_args, "MonitorSetup");
  builder.CreateCall(setup_fn, setup_args);

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

// Thresholds for choosing shift+OR vs runtime helper for element fill.
// Shift+OR generates O(N) LLVM instructions; runtime helper has call overhead.
constexpr uint32_t kShiftOrMaxElements = 32;
constexpr uint32_t kShiftOrMaxTotalBits = 512;

auto UseShiftOr(uint32_t element_count, uint32_t total_bits) -> bool {
  return element_count <= kShiftOrMaxElements &&
         total_bits <= kShiftOrMaxTotalBits;
}

struct FillTargetInfo {
  mir::PlaceId place_id;
  TypeId type_id;
  uint32_t total_bits;
  uint32_t element_bits;
  uint32_t element_count;
  bool is_four_state;
  llvm::Type* storage_type;
};

// Build filled value and commit using CommitPackedValueRaw.
void CommitFilledValue(
    Context& ctx, const FillTargetInfo& t, llvm::Value* filled_val,
    llvm::Value* filled_unk) {
  auto& builder = ctx.GetBuilder();

  llvm::Value* final_value = nullptr;
  if (t.is_four_state) {
    auto* struct_type = llvm::cast<llvm::StructType>(t.storage_type);
    auto* val_type = struct_type->getElementType(0);
    auto* unk_type = struct_type->getElementType(1);

    llvm::Value* store_val =
        builder.CreateZExtOrTrunc(filled_val, val_type, "store.val");
    llvm::Value* store_unk =
        builder.CreateZExtOrTrunc(filled_unk, unk_type, "store.unk");

    final_value = llvm::UndefValue::get(struct_type);
    final_value = builder.CreateInsertValue(final_value, store_val, 0);
    final_value = builder.CreateInsertValue(final_value, store_unk, 1);
  } else {
    // 2-state target: X/Z collapses to 0 (val & ~unk)
    llvm::Value* not_unk = builder.CreateNot(filled_unk, "filled.notunk");
    llvm::Value* known_val =
        builder.CreateAnd(filled_val, not_unk, "filled.known");
    final_value =
        builder.CreateZExtOrTrunc(known_val, t.storage_type, "store.val");
  }

  CommitPackedValueRaw(ctx, t.place_id, final_value);
}

// Bit fill: replicate single bit to all positions using SExt.
void LowerBitFill(Context& ctx, const FillTargetInfo& t, FourStateValue fs) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* target_int_type = llvm::Type::getIntNTy(llvm_ctx, t.total_bits);

  llvm::Value* val_bit = builder.CreateTrunc(fs.value, i1_ty, "fill.val.bit");
  llvm::Value* unk_bit = builder.CreateTrunc(fs.unknown, i1_ty, "fill.unk.bit");
  llvm::Value* filled_val =
      builder.CreateSExt(val_bit, target_int_type, "filled.val");
  llvm::Value* filled_unk =
      builder.CreateSExt(unk_bit, target_int_type, "filled.unk");

  CommitFilledValue(ctx, t, filled_val, filled_unk);
}

// Element fill using shift+OR loop (compile-time unrolled).
void LowerElementFillShiftOr(
    Context& ctx, const FillTargetInfo& t, FourStateValue fs) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  auto* elem_ty = llvm::Type::getIntNTy(llvm_ctx, t.element_bits);
  auto* target_int_type = llvm::Type::getIntNTy(llvm_ctx, t.total_bits);

  llvm::Value* fill_val_elem =
      builder.CreateZExtOrTrunc(fs.value, elem_ty, "fill.val.elem");
  llvm::Value* fill_unk_elem =
      builder.CreateZExtOrTrunc(fs.unknown, elem_ty, "fill.unk.elem");
  llvm::Value* fill_val_ext =
      builder.CreateZExt(fill_val_elem, target_int_type, "fill.val.ext");
  llvm::Value* fill_unk_ext =
      builder.CreateZExt(fill_unk_elem, target_int_type, "fill.unk.ext");

  llvm::Value* filled_val = llvm::ConstantInt::get(target_int_type, 0);
  llvm::Value* filled_unk = llvm::ConstantInt::get(target_int_type, 0);

  for (uint32_t i = 0; i < t.element_count; ++i) {
    uint32_t shift = i * t.element_bits;
    llvm::Value* shift_amt = llvm::ConstantInt::get(target_int_type, shift);
    llvm::Value* shifted_val =
        builder.CreateShl(fill_val_ext, shift_amt, "elem.val");
    llvm::Value* shifted_unk =
        builder.CreateShl(fill_unk_ext, shift_amt, "elem.unk");
    filled_val = builder.CreateOr(filled_val, shifted_val, "filled.val");
    filled_unk = builder.CreateOr(filled_unk, shifted_unk, "filled.unk");
  }

  CommitFilledValue(ctx, t, filled_val, filled_unk);
}

// Element fill using runtime helper (for large arrays).
auto LowerElementFillRuntime(
    Context& ctx, const FillTargetInfo& t, FourStateValue fs) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  // Get addressable storage for target
  auto planes_result = GetPackedPlanesPtr(ctx, t.place_id, t.type_id);
  if (!planes_result) {
    return std::unexpected(planes_result.error());
  }
  const auto& planes = *planes_result;

  // Normalize fill value to element width
  auto* elem_ty = llvm::Type::getIntNTy(llvm_ctx, t.element_bits);
  llvm::Value* fill_val_elem =
      builder.CreateZExtOrTrunc(fs.value, elem_ty, "fill.val.elem");
  llvm::Value* fill_unk_elem =
      builder.CreateZExtOrTrunc(fs.unknown, elem_ty, "fill.unk.elem");

  // For 2-state source going to 2-state target, collapse X/Z to 0
  if (!t.is_four_state) {
    llvm::Value* not_unk = builder.CreateNot(fill_unk_elem, "fill.notunk");
    fill_val_elem = builder.CreateAnd(fill_val_elem, not_unk, "fill.known");
  }

  // Materialize element value in alloca for runtime helper.
  // Runtime expects little-endian word layout (same as RuntimeIntegral planes).
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  llvm::AllocaInst* src_val_alloca = nullptr;
  llvm::AllocaInst* src_unk_alloca = nullptr;

  if (t.element_bits <= 64) {
    // Single word: allocate i64, zero-extend and store
    src_val_alloca = builder.CreateAlloca(i64_ty, nullptr, "src.val");
    llvm::Value* val_word = builder.CreateZExt(fill_val_elem, i64_ty, "val.w0");
    builder.CreateStore(val_word, src_val_alloca);
    if (t.is_four_state) {
      src_unk_alloca = builder.CreateAlloca(i64_ty, nullptr, "src.unk");
      llvm::Value* unk_word =
          builder.CreateZExt(fill_unk_elem, i64_ty, "unk.w0");
      builder.CreateStore(unk_word, src_unk_alloca);
    }
  } else {
    // Multi-word: allocate iN directly and store.
    // Memory layout is little-endian (LSB word first), which matches
    // RuntimeIntegral plane convention. Runtime reads via memcpy.
    src_val_alloca = builder.CreateAlloca(elem_ty, nullptr, "src.val");
    builder.CreateStore(fill_val_elem, src_val_alloca);
    if (t.is_four_state) {
      src_unk_alloca = builder.CreateAlloca(elem_ty, nullptr, "src.unk");
      builder.CreateStore(fill_unk_elem, src_unk_alloca);
    }
  }

  // Call runtime helper
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  llvm::Value* src_unk_arg = src_unk_alloca != nullptr
                                 ? static_cast<llvm::Value*>(src_unk_alloca)
                                 : static_cast<llvm::Value*>(null_ptr);
  builder.CreateCall(
      ctx.GetLyraFillPackedElements(),
      {planes.val_ptr, planes.unk_ptr != nullptr ? planes.unk_ptr : null_ptr,
       llvm::ConstantInt::get(i32_ty, t.total_bits), src_val_alloca,
       src_unk_arg, llvm::ConstantInt::get(i32_ty, t.element_bits),
       llvm::ConstantInt::get(i32_ty, t.element_count)});

  // If design slot, notify after runtime write (use root_ptr, not val_ptr)
  if (planes.signal_id.has_value()) {
    builder.CreateCall(
        ctx.GetLyraNotifySignal(),
        {ctx.GetEnginePointer(), planes.root_ptr,
         llvm::ConstantInt::get(i32_ty, *planes.signal_id)});
  }

  return {};
}

auto LowerFillPackedEffect(Context& context, const mir::FillPackedEffect& fill)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const auto& arena = context.GetMirArena();

  // Validate effect invariants
  if (fill.total_bits != fill.unit_bits * fill.count) {
    throw common::InternalError(
        "LowerFillPackedEffect",
        "invariant violation: total_bits != unit_bits * count");
  }

  // Build target info from effect's semantic payload (destination-driven)
  const mir::Place& place = arena[fill.target];
  TypeId target_type_id = mir::TypeOfPlace(types, place);
  const Type& target_type = types[target_type_id];

  auto storage_type_result = BuildLlvmTypeForTypeId(context, target_type_id);
  if (!storage_type_result) {
    return std::unexpected(storage_type_result.error());
  }

  FillTargetInfo t{
      .place_id = fill.target,
      .type_id = target_type_id,
      .total_bits = fill.total_bits,
      .element_bits = fill.unit_bits,
      .element_count = fill.count,
      .is_four_state = IsPackedFourState(target_type, types),
      .storage_type = *storage_type_result,
  };

  // Lower fill value operand
  auto fill_value_result = LowerOperandRaw(context, fill.fill_value);
  if (!fill_value_result) {
    return std::unexpected(fill_value_result.error());
  }
  auto fs = ExtractFourStateOrZero(builder, *fill_value_result);

  // Select optimization based on unit_bits (destination-driven)
  if (fill.unit_bits == 1) {
    // Bit fill: replicate single bit to all positions using SExt
    LowerBitFill(context, t, fs);
    return {};
  }

  // Element fill: replicate unit_bits-wide value count times
  if (UseShiftOr(t.element_count, t.total_bits)) {
    LowerElementFillShiftOr(context, t, fs);
    return {};
  }
  return LowerElementFillRuntime(context, t, fs);
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

  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);

  // Determine step from array direction
  int64_t step = arr_info.range.IsDescending() ? -1 : 1;
  int64_t left = arr_info.range.left;
  int64_t right = arr_info.range.right;

  // Lower start/end address operands before filename (to avoid early-return
  // issues with string handle release)
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

  // Lower filename and emit runtime call with automatic handle release
  return WithStringHandle(
      context, mem_io.filename.operand, mem_io.filename.type,
      [&](llvm::Value* filename_handle) -> Result<void> {
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
      });
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
          [&context](const mir::FillPackedEffect& fill) -> Result<void> {
            return LowerFillPackedEffect(context, fill);
          },
      },
      effect_op);
}

}  // namespace lyra::lowering::mir_to_llvm
