#include "lyra/llvm_backend/context.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <utility>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

Context::Context(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types,
    const Layout& layout, std::unique_ptr<llvm::LLVMContext> llvm_ctx,
    std::unique_ptr<llvm::Module> module,
    const lowering::DiagnosticContext* diag_ctx)
    : design_(design),
      arena_(arena),
      types_(types),
      layout_(layout),
      llvm_context_(std::move(llvm_ctx)),
      llvm_module_(std::move(module)),
      builder_(*llvm_context_),
      diag_ctx_(diag_ctx) {
}

auto Context::GetElemOpsForType(TypeId elem_type) -> Result<ElemOpsInfo> {
  const Type& type = types_[elem_type];
  auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    // Nested dynamic array or queue: element is a pointer
    auto ptr_size =
        static_cast<int32_t>(llvm_module_->getDataLayout().getPointerSize());
    return ElemOpsInfo{
        .elem_size = ptr_size,
        .elem_llvm_type = ptr_ty,
        .clone_fn = GetLyraDynArrayCloneElem(),
        .destroy_fn = GetLyraDynArrayDestroyElem(),
        .needs_clone = true,
    };
  }

  // POD types: compute size from DataLayout, no clone/destroy needed
  auto llvm_type = BuildLlvmTypeForTypeId(*this, elem_type);
  if (!llvm_type) return std::unexpected(llvm_type.error());
  auto byte_size = static_cast<int32_t>(
      llvm_module_->getDataLayout().getTypeAllocSize(*llvm_type));
  return ElemOpsInfo{
      .elem_size = byte_size,
      .elem_llvm_type = *llvm_type,
      .clone_fn = null_ptr,
      .destroy_fn = null_ptr,
  };
}

auto Context::GetOrCreateUnionStorageInfo(
    TypeId union_type, CachedUnionInfo info) -> CachedUnionInfo {
  auto [it, inserted] = union_storage_cache_.emplace(union_type, info);
  return it->second;
}

auto Context::GetCachedUnionStorageInfo(TypeId union_type) const
    -> const CachedUnionInfo* {
  auto it = union_storage_cache_.find(union_type);
  if (it != union_storage_cache_.end()) {
    return &it->second;
  }
  return nullptr;
}

auto Context::GetOrCreateEnumValuesGlobal(TypeId enum_type)
    -> llvm::GlobalVariable* {
  auto it = enum_values_globals_.find(enum_type);
  if (it != enum_values_globals_.end()) {
    return it->second;
  }

  const Type& type = types_[enum_type];
  const auto& enum_info = type.AsEnum();
  uint32_t bit_width = PackedBitWidth(type, types_);
  uint32_t storage_bits =
      GetLlvmStorageType(*llvm_context_, bit_width)->getIntegerBitWidth();
  auto* elem_type = llvm::Type::getIntNTy(*llvm_context_, storage_bits);

  std::vector<llvm::Constant*> values;
  values.reserve(enum_info.members.size());
  for (const auto& member : enum_info.members) {
    auto build_width = static_cast<uint32_t>(std::max(
        static_cast<size_t>(storage_bits), member.value.value.size() * 64));
    llvm::APInt ap_val(build_width, 0);
    for (size_t w = 0; w < member.value.value.size(); ++w) {
      ap_val.insertBits(llvm::APInt(64, member.value.value[w]), w * 64);
    }
    ap_val = ap_val.trunc(storage_bits);
    values.push_back(llvm::ConstantInt::get(elem_type, ap_val));
  }

  auto* arr_type = llvm::ArrayType::get(elem_type, enum_info.members.size());
  auto* initializer = llvm::ConstantArray::get(arr_type, values);
  auto* global = new llvm::GlobalVariable(
      *llvm_module_, arr_type, true, llvm::GlobalValue::InternalLinkage,
      initializer, "enum.values");

  enum_values_globals_[enum_type] = global;
  return global;
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
  // Clear place storage for next function. With PlaceRootKey keying, different
  // functions' locals with the same ID would otherwise collide.
  place_storage_.clear();
}

auto Context::GetOrCreatePlaceStorage(const mir::PlaceRoot& root)
    -> Result<llvm::AllocaInst*> {
  // Check if we already have storage for this root.
  // Storage is keyed by root identity (kind + id), NOT by PlaceId.
  PlaceRootKey key{.kind = root.kind, .id = root.id};
  auto it = place_storage_.find(key);
  if (it != place_storage_.end()) {
    return it->second;
  }

  // Must be inside a function scope
  if (current_function_ == nullptr || alloca_builder_ == nullptr) {
    throw common::InternalError(
        "GetOrCreatePlaceStorage",
        "must call BeginFunction before creating place storage");
  }

  TypeId type_id = root.type;
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
  } else if (type.Kind() == TypeKind::kShortReal) {
    llvm_type = llvm::Type::getFloatTy(*llvm_context_);
  } else if (
      type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    llvm_type = llvm::PointerType::getUnqual(*llvm_context_);
  } else if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type, types_)) {
      llvm_type = GetFourStateStructType(*llvm_context_, width);
    } else {
      llvm_type = GetLlvmStorageType(*llvm_context_, width);
    }
  } else if (
      type.Kind() == TypeKind::kUnpackedStruct ||
      type.Kind() == TypeKind::kUnpackedArray ||
      type.Kind() == TypeKind::kUnpackedUnion) {
    auto llvm_type_result = BuildLlvmTypeForTypeId(*this, type_id);
    if (!llvm_type_result) return std::unexpected(llvm_type_result.error());
    llvm_type = *llvm_type_result;
  } else {
    return std::unexpected(
        GetDiagnosticContext().MakeUnsupported(
            current_origin_,
            std::format("type not yet supported: {}", ToString(type)),
            UnsupportedCategory::kType));
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
  // NOTE: This is PURE allocation - no initialization.
  // Initialization happens explicitly at function entry via
  // InitializeAllLocals.
  auto* alloca = alloca_builder_->CreateAlloca(llvm_type, nullptr, "place");

  // Store in the map, keyed by root identity
  place_storage_[key] = alloca;

  return alloca;
}

void Context::InitializePlaceStorage(llvm::AllocaInst* alloca, TypeId type_id) {
  // Use unified EmitSVDefaultInit which handles all types including unpacked
  // aggregates with 4-state fields. The builder_ is used for the IR emission.
  // Save current insert point and temporarily switch to alloca_builder_'s
  // insert point (in entry block, after allocas) for initialization.
  auto saved_point = builder_.saveIP();
  builder_.restoreIP(alloca_builder_->saveIP());
  EmitSVDefaultInit(*this, alloca, type_id);
  builder_.restoreIP(saved_point);
}

auto Context::GetDesignFieldIndex(mir::SlotId slot_id) const -> uint32_t {
  auto it = layout_.design.slot_to_field.find(slot_id);
  if (it == layout_.design.slot_to_field.end()) {
    throw common::InternalError(
        "llvm_backend", "design slot not found in layout");
  }
  return it->second;
}

auto Context::GetFrameFieldIndex(mir::PlaceId place_id) const -> uint32_t {
  const auto& place = arena_[place_id];
  PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
  const auto& frame = layout_.processes[current_process_index_].frame;
  auto it = frame.root_to_field.find(key);
  if (it == frame.root_to_field.end()) {
    throw common::InternalError(
        "llvm_backend", "frame place not found in layout");
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

void Context::SetCurrentInstanceId(uint32_t instance_id) {
  current_instance_id_ = instance_id;
}

auto Context::GetCurrentInstanceId() const -> uint32_t {
  return current_instance_id_;
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

void Context::RegisterUserFunction(
    mir::FunctionId func_id, llvm::Function* llvm_func) {
  user_functions_[func_id] = llvm_func;
}

auto Context::GetUserFunction(mir::FunctionId func_id) const
    -> llvm::Function* {
  auto it = user_functions_.find(func_id);
  if (it == user_functions_.end()) {
    throw common::InternalError(
        "GetUserFunction",
        std::format("user function {} not found", func_id.value));
  }
  return it->second;
}

auto Context::HasUserFunction(mir::FunctionId func_id) const -> bool {
  return user_functions_.contains(func_id);
}

void Context::RegisterMonitorLayout(
    mir::FunctionId check_thunk, MonitorLayout layout) {
  monitor_layouts_.emplace(check_thunk, std::move(layout));
}

auto Context::GetMonitorLayout(mir::FunctionId check_thunk) const
    -> const MonitorLayout* {
  auto it = monitor_layouts_.find(check_thunk);
  if (it == monitor_layouts_.end()) {
    return nullptr;
  }
  return &it->second;
}

void Context::RegisterMonitorSetupInfo(
    mir::FunctionId setup_thunk, MonitorSetupInfo info) {
  monitor_setup_infos_.emplace(setup_thunk, std::move(info));
}

auto Context::GetMonitorSetupInfo(mir::FunctionId setup_thunk) const
    -> const MonitorSetupInfo* {
  auto it = monitor_setup_infos_.find(setup_thunk);
  if (it == monitor_setup_infos_.end()) {
    return nullptr;
  }
  return &it->second;
}

auto Context::BuildUserFunctionType(const mir::FunctionSignature& sig)
    -> Result<llvm::FunctionType*> {
  auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);

  // Check if return type requires sret calling convention
  bool uses_sret = RequiresSret(sig.return_type, types_);

  std::vector<llvm::Type*> param_types;

  // For sret: first parameter is the output pointer
  if (uses_sret) {
    param_types.push_back(ptr_ty);  // sret ptr
  }

  // DesignState* and Engine* follow
  param_types.push_back(ptr_ty);  // DesignState*
  param_types.push_back(ptr_ty);  // Engine*

  // Add parameter types from signature
  for (const auto& param : sig.params) {
    const Type& type = types_[param.type];
    llvm::Type* param_ty = nullptr;

    if (type.Kind() == TypeKind::kVoid) {
      throw common::InternalError(
          "BuildUserFunctionType", "void parameter type not allowed");
    }

    if (type.Kind() == TypeKind::kString ||
        type.Kind() == TypeKind::kDynamicArray ||
        type.Kind() == TypeKind::kQueue) {
      param_ty = ptr_ty;
    } else if (type.Kind() == TypeKind::kReal) {
      param_ty = llvm::Type::getDoubleTy(*llvm_context_);
    } else if (type.Kind() == TypeKind::kShortReal) {
      param_ty = llvm::Type::getFloatTy(*llvm_context_);
    } else if (IsPacked(type)) {
      auto width = PackedBitWidth(type, types_);
      if (IsPackedFourState(type, types_)) {
        param_ty = GetFourStateStructType(*llvm_context_, width);
      } else {
        param_ty = GetLlvmStorageType(*llvm_context_, width);
      }
    } else {
      return std::unexpected(
          GetDiagnosticContext().MakeUnsupported(
              current_origin_,
              std::format(
                  "unsupported parameter type: {}", ToString(type.Kind())),
              UnsupportedCategory::kType));
    }

    param_types.push_back(param_ty);
  }

  // Return type: void for sret, otherwise derive from signature
  llvm::Type* llvm_ret_type = nullptr;

  if (uses_sret) {
    // Sret functions return void - the result is written to the sret pointer
    llvm_ret_type = llvm::Type::getVoidTy(*llvm_context_);
  } else {
    const Type& ret_type = types_[sig.return_type];

    if (ret_type.Kind() == TypeKind::kVoid) {
      llvm_ret_type = llvm::Type::getVoidTy(*llvm_context_);
    } else if (ret_type.Kind() == TypeKind::kReal) {
      llvm_ret_type = llvm::Type::getDoubleTy(*llvm_context_);
    } else if (ret_type.Kind() == TypeKind::kShortReal) {
      llvm_ret_type = llvm::Type::getFloatTy(*llvm_context_);
    } else if (IsPacked(ret_type)) {
      auto width = PackedBitWidth(ret_type, types_);
      if (IsPackedFourState(ret_type, types_)) {
        llvm_ret_type = GetFourStateStructType(*llvm_context_, width);
      } else {
        llvm_ret_type = GetLlvmStorageType(*llvm_context_, width);
      }
    } else {
      return std::unexpected(
          GetDiagnosticContext().MakeUnsupported(
              current_origin_,
              std::format(
                  "unsupported return type: {}", ToString(ret_type.Kind())),
              UnsupportedCategory::kType));
    }
  }

  return llvm::FunctionType::get(llvm_ret_type, param_types, false);
}

auto Context::FunctionUsesSret(mir::FunctionId func_id) const -> bool {
  const auto& func = arena_[func_id];
  return RequiresSret(func.signature.return_type, types_);
}

}  // namespace lyra::lowering::mir_to_llvm
