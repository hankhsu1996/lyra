#include "lyra/llvm_backend/context.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <stdexcept>
#include <utility>
#include <variant>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Get the LLVM type for storage of an integral type
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type* {
  // Round up to the next power-of-2 storage size for efficient access
  if (bit_width <= 8) {
    return llvm::Type::getInt8Ty(ctx);
  }
  if (bit_width <= 16) {
    return llvm::Type::getInt16Ty(ctx);
  }
  if (bit_width <= 32) {
    return llvm::Type::getInt32Ty(ctx);
  }
  if (bit_width <= 64) {
    return llvm::Type::getInt64Ty(ctx);
  }
  // For wider types, use the exact bit width
  return llvm::Type::getIntNTy(ctx, bit_width);
}

// Get the LLVM struct type for a 4-state value: {iN_storage, iN_storage}
auto GetFourStateStructType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType* {
  auto* elem = GetLlvmStorageType(ctx, bit_width);
  return llvm::StructType::get(ctx, {elem, elem});
}

// Convert a TypeId to its LLVM type representation (for GEP type operands)
auto GetLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type* {
  const Type& type = types[type_id];
  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (type.AsIntegral().is_four_state) {
      return GetFourStateStructType(ctx, bit_width);
    }
    return GetLlvmStorageType(ctx, bit_width);
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(ctx);
  }
  if (type.Kind() == TypeKind::kUnpackedArray) {
    const auto& info = type.AsUnpackedArray();
    llvm::Type* elem = GetLlvmTypeForTypeId(ctx, info.element_type, types);
    return llvm::ArrayType::get(elem, info.range.Size());
  }
  if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types);
    if (IsPackedFourState(type, types)) {
      return GetFourStateStructType(ctx, width);
    }
    return GetLlvmStorageType(ctx, width);
  }
  throw common::InternalError(
      "GetLlvmTypeForTypeId",
      std::format("unsupported type kind: {}", static_cast<int>(type.Kind())));
}

}  // namespace

Context::Context(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const Layout& layout, std::unique_ptr<llvm::LLVMContext> llvm_ctx,
    std::unique_ptr<llvm::Module> module)
    : design_(design),
      arena_(arena),
      types_(types),
      layout_(layout),
      llvm_context_(std::move(llvm_ctx)),
      llvm_module_(std::move(module)),
      builder_(*llvm_context_) {
}

auto Context::GetLyraPrintLiteral() -> llvm::Function* {
  if (lyra_print_literal_ == nullptr) {
    // void LyraPrintLiteral(const char* str)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::PointerType::getUnqual(*llvm_context_)}, false);
    lyra_print_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintLiteral",
        llvm_module_.get());
  }
  return lyra_print_literal_;
}

auto Context::GetLyraPrintValue() -> llvm::Function* {
  if (lyra_print_value_ == nullptr) {
    // void LyraPrintValue(int32_t format, const void* data, int32_t width,
    //                     bool is_signed, int32_t output_width,
    //                     int32_t precision, bool zero_pad, bool left_align,
    //                     const void* x_mask, const void* z_mask)
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {i32_ty, ptr_ty, i32_ty, i1_ty, i32_ty, i32_ty, i1_ty, i1_ty, ptr_ty,
         ptr_ty},
        false);
    lyra_print_value_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintValue",
        llvm_module_.get());
  }
  return lyra_print_value_;
}

auto Context::GetLyraPrintEnd() -> llvm::Function* {
  if (lyra_print_end_ == nullptr) {
    // void LyraPrintEnd(int32_t kind)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::Type::getInt32Ty(*llvm_context_)}, false);
    lyra_print_end_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintEnd",
        llvm_module_.get());
  }
  return lyra_print_end_;
}

auto Context::GetLyraRegisterVar() -> llvm::Function* {
  if (lyra_register_var_ == nullptr) {
    // void LyraRegisterVar(const char*, void*, int32_t, int32_t, bool)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, i32_ty, i32_ty, i1_ty}, false);
    lyra_register_var_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRegisterVar",
        llvm_module_.get());
  }
  return lyra_register_var_;
}

auto Context::GetLyraSnapshotVars() -> llvm::Function* {
  if (lyra_snapshot_vars_ == nullptr) {
    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_context_), false);
    lyra_snapshot_vars_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSnapshotVars",
        llvm_module_.get());
  }
  return lyra_snapshot_vars_;
}

auto Context::GetLyraStringFromLiteral() -> llvm::Function* {
  if (lyra_string_from_literal_ == nullptr) {
    // ptr LyraStringFromLiteral(const char* data, int64_t len)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty, i64_ty}, false);
    lyra_string_from_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringFromLiteral",
        llvm_module_.get());
  }
  return lyra_string_from_literal_;
}

auto Context::GetLyraStringCmp() -> llvm::Function* {
  if (lyra_string_cmp_ == nullptr) {
    // int32_t LyraStringCmp(ptr a, ptr b)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(i32_ty, {ptr_ty, ptr_ty}, false);
    lyra_string_cmp_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringCmp",
        llvm_module_.get());
  }
  return lyra_string_cmp_;
}

auto Context::GetLyraStringRetain() -> llvm::Function* {
  if (lyra_string_retain_ == nullptr) {
    // ptr LyraStringRetain(ptr handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(ptr_ty, {ptr_ty}, false);
    lyra_string_retain_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringRetain",
        llvm_module_.get());
  }
  return lyra_string_retain_;
}

auto Context::GetLyraStringRelease() -> llvm::Function* {
  if (lyra_string_release_ == nullptr) {
    // void LyraStringRelease(ptr handle)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_string_release_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStringRelease",
        llvm_module_.get());
  }
  return lyra_string_release_;
}

auto Context::GetLyraRunSimulation() -> llvm::Function* {
  if (lyra_run_simulation_ == nullptr) {
    // void LyraRunSimulation(ptr* processes, ptr* states, uint32_t num)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, i32_ty}, false);
    lyra_run_simulation_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraRunSimulation",
        llvm_module_.get());
  }
  return lyra_run_simulation_;
}

auto Context::GetLyraSuspendDelay() -> llvm::Function* {
  if (lyra_suspend_delay_ == nullptr) {
    // void LyraSuspendDelay(ptr state, i64 ticks, i32 resume_block)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i64_ty = llvm::Type::getInt64Ty(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i64_ty, i32_ty}, false);
    lyra_suspend_delay_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendDelay",
        llvm_module_.get());
  }
  return lyra_suspend_delay_;
}

auto Context::GetLyraSuspendWait() -> llvm::Function* {
  if (lyra_suspend_wait_ == nullptr) {
    // void LyraSuspendWait(ptr state, i32 resume_block, ptr triggers,
    //                      i32 num_triggers)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, i32_ty, ptr_ty, i32_ty},
        false);
    lyra_suspend_wait_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendWait",
        llvm_module_.get());
  }
  return lyra_suspend_wait_;
}

auto Context::GetLyraSuspendRepeat() -> llvm::Function* {
  if (lyra_suspend_repeat_ == nullptr) {
    // void LyraSuspendRepeat(ptr state)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty}, false);
    lyra_suspend_repeat_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraSuspendRepeat",
        llvm_module_.get());
  }
  return lyra_suspend_repeat_;
}

auto Context::GetLyraStorePacked() -> llvm::Function* {
  if (lyra_store_packed_ == nullptr) {
    // void LyraStorePacked(ptr engine, ptr slot, ptr new_value,
    //                      i32 byte_size, i32 signal_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {ptr_ty, ptr_ty, ptr_ty, i32_ty, i32_ty}, false);
    lyra_store_packed_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStorePacked",
        llvm_module_.get());
  }
  return lyra_store_packed_;
}

auto Context::GetLyraStoreString() -> llvm::Function* {
  if (lyra_store_string_ == nullptr) {
    // void LyraStoreString(ptr engine, ptr slot, ptr new_str, i32 signal_id)
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_), {ptr_ty, ptr_ty, ptr_ty, i32_ty},
        false);
    lyra_store_string_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraStoreString",
        llvm_module_.get());
  }
  return lyra_store_string_;
}

auto Context::GetLyraInitRuntime() -> llvm::Function* {
  if (lyra_init_runtime_ == nullptr) {
    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_context_), false);
    lyra_init_runtime_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraInitRuntime",
        llvm_module_.get());
  }
  return lyra_init_runtime_;
}

auto Context::GetLyraReportTime() -> llvm::Function* {
  if (lyra_report_time_ == nullptr) {
    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(*llvm_context_), false);
    lyra_report_time_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraReportTime",
        llvm_module_.get());
  }
  return lyra_report_time_;
}

auto Context::GetHeaderType() const -> llvm::StructType* {
  return layout_.header_type;
}

auto Context::GetDesignStateType() const -> llvm::StructType* {
  return layout_.design.llvm_type;
}

auto Context::GetProcessFrameType() const -> llvm::StructType* {
  return layout_.processes[current_process_index_].frame.llvm_type;
}

auto Context::GetProcessStateType() const -> llvm::StructType* {
  return layout_.processes[current_process_index_].state_type;
}

void Context::BeginFunction(llvm::Function& func) {
  current_function_ = &func;

  // Set up alloca builder at the start of the entry block, before any
  // non-alloca instructions. This ensures all allocas are grouped at the
  // beginning and dominate all uses.
  llvm::BasicBlock& entry = func.getEntryBlock();
  llvm::BasicBlock::iterator insert_point = entry.begin();

  // Skip past any existing allocas to keep them grouped
  while (insert_point != entry.end() &&
         llvm::isa<llvm::AllocaInst>(&*insert_point)) {
    ++insert_point;
  }

  alloca_builder_ = std::make_unique<llvm::IRBuilder<>>(&entry, insert_point);
}

void Context::EndFunction() {
  current_function_ = nullptr;
  alloca_builder_.reset();
}

auto Context::GetOrCreatePlaceStorage(mir::PlaceId place_id)
    -> llvm::AllocaInst* {
  // Check if we already have storage for this place
  auto it = place_storage_.find(place_id);
  if (it != place_storage_.end()) {
    return it->second;
  }

  // Must be inside a function scope
  if (current_function_ == nullptr || alloca_builder_ == nullptr) {
    throw common::InternalError(
        "GetOrCreatePlaceStorage",
        "must call BeginFunction before creating place storage");
  }

  // Get the place from the arena
  const auto& place = arena_[place_id];

  if (!place.projections.empty()) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        current_origin_, "places with projections not yet supported");
  }

  TypeId type_id = place.root.type;
  const Type& type = types_[type_id];

  llvm::Type* llvm_type = nullptr;
  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (type.AsIntegral().is_four_state) {
      llvm_type = GetFourStateStructType(*llvm_context_, bit_width);
    } else {
      llvm_type = GetLlvmStorageType(*llvm_context_, bit_width);
    }
  } else if (type.Kind() == TypeKind::kReal) {
    llvm_type = llvm::Type::getDoubleTy(*llvm_context_);
  } else if (type.Kind() == TypeKind::kString) {
    llvm_type = llvm::PointerType::getUnqual(*llvm_context_);
  } else if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type, types_)) {
      llvm_type = GetFourStateStructType(*llvm_context_, width);
    } else {
      llvm_type = GetLlvmStorageType(*llvm_context_, width);
    }
  } else {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
        current_origin_,
        std::format("type not yet supported: {}", ToString(type)));
  }

  // Re-compute insertion point: after all existing allocas, before any
  // non-alloca This is necessary because the main builder may have inserted
  // non-alloca instructions since we last created an alloca.
  llvm::BasicBlock& entry = current_function_->getEntryBlock();
  llvm::BasicBlock::iterator insert_point = entry.begin();
  while (insert_point != entry.end() &&
         llvm::isa<llvm::AllocaInst>(&*insert_point)) {
    ++insert_point;
  }
  alloca_builder_->SetInsertPoint(&entry, insert_point);

  // Create the alloca using the dedicated alloca builder (entry block)
  auto* alloca = alloca_builder_->CreateAlloca(llvm_type, nullptr, "place");

  // Store in the map
  place_storage_[place_id] = alloca;

  return alloca;
}

auto Context::GetDesignFieldIndex(mir::SlotId slot_id) const -> uint32_t {
  auto it = layout_.design.slot_to_field.find(slot_id);
  if (it == layout_.design.slot_to_field.end()) {
    throw std::runtime_error("design slot not found in layout");
  }
  return it->second;
}

auto Context::GetFrameFieldIndex(mir::PlaceId place_id) const -> uint32_t {
  const auto& frame = layout_.processes[current_process_index_].frame;
  auto it = frame.place_to_field.find(place_id);
  if (it == frame.place_to_field.end()) {
    throw std::runtime_error("frame place not found in layout");
  }
  return it->second;
}

void Context::SetCurrentProcess(size_t process_index) {
  current_process_index_ = process_index;
  state_ptr_ = nullptr;
  design_ptr_ = nullptr;
  frame_ptr_ = nullptr;
  engine_ptr_ = nullptr;
}

auto Context::GetCurrentProcessIndex() const -> size_t {
  return current_process_index_;
}

void Context::SetStatePointer(llvm::Value* state_ptr) {
  state_ptr_ = state_ptr;
}

auto Context::GetStatePointer() -> llvm::Value* {
  return state_ptr_;
}

void Context::SetDesignPointer(llvm::Value* design_ptr) {
  design_ptr_ = design_ptr;
}

auto Context::GetDesignPointer() -> llvm::Value* {
  return design_ptr_;
}

void Context::SetFramePointer(llvm::Value* frame_ptr) {
  frame_ptr_ = frame_ptr;
}

auto Context::GetFramePointer() -> llvm::Value* {
  return frame_ptr_;
}

void Context::SetEnginePointer(llvm::Value* engine_ptr) {
  engine_ptr_ = engine_ptr;
}

auto Context::GetEnginePointer() -> llvm::Value* {
  return engine_ptr_;
}

auto Context::GetPlacePointer(mir::PlaceId place_id) -> llvm::Value* {
  const auto& place = arena_[place_id];

  // Get base pointer from root
  llvm::Value* ptr = nullptr;
  if (place.root.kind == mir::PlaceRoot::Kind::kDesign) {
    if (design_ptr_ == nullptr) {
      throw std::runtime_error("design pointer not set");
    }
    auto slot_id = mir::SlotId{static_cast<uint32_t>(place.root.id)};
    uint32_t field_index = GetDesignFieldIndex(slot_id);
    ptr = builder_.CreateStructGEP(
        GetDesignStateType(), design_ptr_, field_index, "design_slot_ptr");
  } else {
    if (frame_ptr_ == nullptr) {
      throw std::runtime_error("frame pointer not set");
    }
    uint32_t field_index = GetFrameFieldIndex(place_id);
    ptr = builder_.CreateStructGEP(
        GetProcessFrameType(), frame_ptr_, field_index, "frame_slot_ptr");
  }

  // Apply projections (index into arrays, stop at BitRange)
  TypeId current_type = place.root.type;
  for (const auto& proj : place.projections) {
    if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      break;
    }
    const auto* idx = std::get_if<mir::IndexProjection>(&proj.info);
    if (idx == nullptr) {
      throw common::InternalError(
          "GetPlacePointer", "unsupported projection kind in LLVM backend");
    }
    llvm::Type* array_type =
        GetLlvmTypeForTypeId(*llvm_context_, current_type, types_);
    llvm::Value* index = LowerOperand(*this, idx->index);
    ptr = builder_.CreateGEP(
        array_type, ptr, {builder_.getInt32(0), index}, "array_elem_ptr");
    current_type = types_[current_type].AsUnpackedArray().element_type;
  }

  return ptr;
}

auto Context::GetPlaceLlvmType(mir::PlaceId place_id) -> llvm::Type* {
  const auto& place = arena_[place_id];

  TypeId type_id = mir::TypeOfPlace(types_, place);
  const Type& type = types_[type_id];

  if (type.Kind() == TypeKind::kIntegral) {
    uint32_t bit_width = type.AsIntegral().bit_width;
    if (type.AsIntegral().is_four_state) {
      return GetFourStateStructType(*llvm_context_, bit_width);
    }
    return GetLlvmStorageType(*llvm_context_, bit_width);
  }
  if (type.Kind() == TypeKind::kReal) {
    return llvm::Type::getDoubleTy(*llvm_context_);
  }
  if (type.Kind() == TypeKind::kString) {
    return llvm::PointerType::getUnqual(*llvm_context_);
  }
  if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type, types_)) {
      return GetFourStateStructType(*llvm_context_, width);
    }
    return GetLlvmStorageType(*llvm_context_, width);
  }

  throw common::UnsupportedErrorException(
      common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
      current_origin_,
      std::format("type not yet supported: {}", ToString(type)));
}

auto Context::GetPlaceLlvmType4State(uint32_t bit_width) -> llvm::StructType* {
  return GetFourStateStructType(*llvm_context_, bit_width);
}

auto Context::HasBitRangeProjection(mir::PlaceId place_id) const -> bool {
  const auto& place = arena_[place_id];
  return !place.projections.empty() &&
         std::holds_alternative<mir::BitRangeProjection>(
             place.projections.back().info);
}

auto Context::GetBitRangeProjection(mir::PlaceId place_id) const
    -> const mir::BitRangeProjection& {
  const auto& place = arena_[place_id];
  return std::get<mir::BitRangeProjection>(place.projections.back().info);
}

auto Context::GetPlaceBaseType(mir::PlaceId place_id) -> llvm::Type* {
  const auto& place = arena_[place_id];

  // The base type is root.type after applying all non-BitRange projections.
  // BitRange is always the last projection, so just use root.type for the
  // common case (no intermediate projections before the BitRange).
  TypeId base_type_id = place.root.type;
  for (const auto& proj : place.projections) {
    if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
      break;
    }
    const Type& t = types_[base_type_id];
    if (t.Kind() == TypeKind::kUnpackedArray) {
      base_type_id = t.AsUnpackedArray().element_type;
    }
  }

  return GetLlvmTypeForTypeId(*llvm_context_, base_type_id, types_);
}

auto Context::TakeOwnership() -> std::pair<
    std::unique_ptr<llvm::LLVMContext>, std::unique_ptr<llvm::Module>> {
  return {std::move(llvm_context_), std::move(llvm_module_)};
}

void Context::RegisterOwnedTemp(llvm::Value* handle) {
  owned_temps_.push_back(handle);
}

void Context::ClearOwnedTemps() {
  owned_temps_.clear();
}

void Context::ReleaseOwnedTemps() {
  for (llvm::Value* temp : owned_temps_) {
    builder_.CreateCall(GetLyraStringRelease(), {temp});
  }
  owned_temps_.clear();
}

StatementScope::StatementScope(Context& ctx) : ctx_(ctx) {
  ctx_.ClearOwnedTemps();
}

StatementScope::~StatementScope() {
  ctx_.ReleaseOwnedTemps();
}

}  // namespace lyra::lowering::mir_to_llvm
