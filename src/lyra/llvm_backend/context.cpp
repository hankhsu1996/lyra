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
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/callable_abi.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/value_repr.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/runtime_instance.hpp"

namespace lyra::lowering::mir_to_llvm {

Context::Context(
    const mir::Arena& arena, const TypeArena& types, const Layout& layout,
    std::unique_ptr<llvm::LLVMContext> llvm_ctx,
    std::unique_ptr<llvm::Module> module,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager, bool force_two_state)
    : arena_(&arena),
      design_arena_(&arena),
      types_(types),
      layout_(layout),
      force_two_state_(force_two_state),
      llvm_context_(std::move(llvm_ctx)),
      llvm_module_(std::move(module)),
      builder_(*llvm_context_),
      diag_ctx_(diag_ctx),
      source_manager_(source_manager) {
}

auto Context::GetElemOpsForType(TypeId elem_type) -> Result<ElemOpsInfo> {
  const Type& type = types_[elem_type];
  auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
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

  if (type.Kind() == TypeKind::kAssociativeArray) {
    auto ptr_size =
        static_cast<int32_t>(llvm_module_->getDataLayout().getPointerSize());
    return ElemOpsInfo{
        .elem_size = ptr_size,
        .elem_llvm_type = ptr_ty,
        .clone_fn = GetLyraAssocCloneElem(),
        .destroy_fn = GetLyraAssocDestroyElem(),
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
      GetBackingLlvmType(*llvm_context_, bit_width)->getIntegerBitWidth();
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

auto Context::GetRuntimeInstanceType() const -> llvm::StructType* {
  return layout_.runtime_instance_type;
}

auto Context::GetRuntimeInstanceStorageType() const -> llvm::StructType* {
  return layout_.runtime_instance_storage_type;
}

auto Context::GetDesignArenaSize() const -> uint64_t {
  return layout_.design.arena_size;
}

auto Context::ResolveDesignSlotIndex(common::SlotId slot_id) const -> uint32_t {
  auto it = layout_.design.slot_to_index.find(slot_id);
  if (it == layout_.design.slot_to_index.end()) {
    throw common::InternalError(
        "ResolveDesignSlotIndex", "design slot not found in layout");
  }
  return it->second;
}

auto Context::GetDesignSlotByteOffset(common::SlotId slot_id) const
    -> uint64_t {
  return layout_.design.GetStorageByteOffset(slot_id);
}

auto Context::GetDesignSlotStorageSpec(common::SlotId slot_id) const
    -> const SlotStorageSpec& {
  return layout_.design.GetStorageSpec(slot_id);
}

auto Context::GetDesignStorageSpecArena() const -> const StorageSpecArena& {
  return layout_.design.storage_spec_arena;
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
  place_alias_.clear();
}

void Context::SetPlaceAlias(const mir::PlaceRoot& root, llvm::Value* ptr) {
  PlaceRootKey key{.kind = root.kind, .id = root.id};
  place_alias_[key] = ptr;
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
    if (IsPackedFourState(type)) {
      llvm_type = GetBackingFourStateType(*llvm_context_, bit_width);
    } else {
      llvm_type = GetBackingLlvmType(*llvm_context_, bit_width);
    }
  } else if (type.Kind() == TypeKind::kReal) {
    llvm_type = llvm::Type::getDoubleTy(*llvm_context_);
  } else if (type.Kind() == TypeKind::kShortReal) {
    llvm_type = llvm::Type::getFloatTy(*llvm_context_);
  } else if (
      type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray ||
      type.Kind() == TypeKind::kChandle) {
    llvm_type = llvm::PointerType::getUnqual(*llvm_context_);
  } else if (IsPacked(type)) {
    auto width = PackedBitWidth(type, types_);
    if (IsPackedFourState(type)) {
      llvm_type = GetBackingFourStateType(*llvm_context_, width);
    } else {
      llvm_type = GetBackingLlvmType(*llvm_context_, width);
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
  // Emit default init at the current builder position (not the alloca builder).
  // EmitSVDefaultInit may create new basic blocks (e.g., for large 4-state
  // unpacked array init loops), which is only valid at the current control
  // flow point, not in the entry block after allocas.
  EmitSVDefaultInit(*this, alloca, type_id);
}

// GetDesignFieldIndex removed -- DesignState is byte arena, not struct.

auto Context::GetFrameFieldIndex(mir::PlaceId place_id) const -> uint32_t {
  const auto& place = LookupPlace(place_id);
  PlaceRootKey key{.kind = place.root.kind, .id = place.root.id};
  const auto& frame = layout_.processes[current_process_index_].frame;
  auto it = frame.root_to_field.find(key);
  if (it == frame.root_to_field.end()) {
    throw common::InternalError(
        "llvm_backend", "frame place not found in layout");
  }
  return it->second;
}

auto Context::HasTemp(int temp_id) const -> bool {
  return temp_entries_.contains(temp_id);
}

auto Context::GetTempType(int temp_id) const -> TypeId {
  return ReadTempValue(temp_id).declared_type;
}

void Context::ClearTemps() {
  temp_entries_.clear();
}

void Context::BindTempValue(int temp_id, const TempValue& tv) {
  // Validate payload convention.
  if (tv.domain == ValueDomain::kTwoState && tv.unknown != nullptr) {
    throw common::InternalError(
        "BindTempValue",
        std::format(
            "temp {} domain is kTwoState but unknown is non-null", temp_id));
  }
  if (tv.domain == ValueDomain::kFourState && tv.unknown == nullptr) {
    throw common::InternalError(
        "BindTempValue",
        std::format(
            "temp {} domain is kFourState but unknown is nullptr", temp_id));
  }

  auto [it, inserted] = temp_entries_.try_emplace(temp_id, tv);
  if (!inserted) {
    throw common::InternalError(
        "BindTempValue", std::format("temp {} already bound", temp_id));
  }
}

auto Context::ReadTempValue(int temp_id) const -> const TempValue& {
  auto it = temp_entries_.find(temp_id);
  if (it == temp_entries_.end()) {
    throw common::InternalError(
        "ReadTempValue", std::format("temp {} not bound", temp_id));
  }
  return it->second;
}

auto Context::TryGetTempConstantInt(int temp_id) const -> llvm::ConstantInt* {
  auto it = temp_entries_.find(temp_id);
  if (it == temp_entries_.end()) {
    return nullptr;
  }
  const auto& tv = it->second;
  // Reject kFourState unconditionally, even if unknown is ConstantInt(0).
  if (tv.domain != ValueDomain::kTwoState) {
    return nullptr;
  }
  return llvm::dyn_cast<llvm::ConstantInt>(tv.value);
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

void Context::SetSlotAddressingMode(SlotAddressingMode mode) {
  slot_addressing_ = mode;
}

auto Context::GetSlotAddressingMode() const -> SlotAddressingMode {
  return slot_addressing_;
}

void Context::SetInstancePointer(llvm::Value* ptr) {
  instance_ptr_ = ptr;
}

auto Context::GetInstancePointer() const -> llvm::Value* {
  return instance_ptr_;
}

void Context::SetThisPointer(llvm::Value* ptr) {
  this_ptr_ = ptr;
}

auto Context::GetThisPointer() const -> llvm::Value* {
  return this_ptr_;
}

void Context::SetDynamicInstanceId(llvm::Value* id) {
  dynamic_instance_id_ = id;
}

auto Context::GetDynamicInstanceId() const -> llvm::Value* {
  return dynamic_instance_id_;
}

void Context::SetExternalRefResolutionEnv(
    std::optional<ExternalRefResolutionEnv> env) {
  ext_ref_env_ = std::move(env);
}

auto Context::LookupPlace(mir::PlaceId place_id) const -> const mir::Place& {
  if (arena_ == nullptr) {
    throw common::InternalError("LookupPlace", "no active MIR arena");
  }
  return (*arena_)[place_id];
}

// V3: Direct external-ref helpers -- consume resolved binding directly
// for operand loads, type queries, signal coordinates, and write-path
// root resolution. All external-ref resolution flows through
// ResolveExternalRefRoot.

auto Context::ResolveExternalRefRoot(mir::ExternalRefId ref_id) const
    -> ResolvedExternalRefRoot {
  if (!ext_ref_env_.has_value() || ext_ref_env_->bindings == nullptr) {
    throw common::InternalError(
        "ResolveExternalRefRoot",
        "env not installed -- kSpecializationLocal scope missing "
        "SetExternalRefResolutionEnv");
  }
  const auto& bindings = *ext_ref_env_->bindings;
  if (ref_id.value >= bindings.size()) {
    throw common::InternalError(
        "ResolveExternalRefRoot",
        std::format(
            "external ref {} out of range (bindings size {})", ref_id.value,
            bindings.size()));
  }
  const auto& binding = bindings[ref_id.value];
  if (ext_ref_env_->construction == nullptr) {
    throw common::InternalError(
        "ResolveExternalRefRoot", "construction not set in env");
  }
  const auto& objects = ext_ref_env_->construction->objects;
  if (binding.target_object.value >= objects.size()) {
    throw common::InternalError(
        "ResolveExternalRefRoot",
        std::format(
            "target_object {} out of range (objects size {})",
            binding.target_object.value, objects.size()));
  }
  const auto& obj = objects[binding.target_object.value];
  if (binding.target_local_slot.value >= obj.slot_count) {
    throw common::InternalError(
        "ResolveExternalRefRoot",
        std::format(
            "target_local_slot {} out of range for object {} "
            "(slot_count {})",
            binding.target_local_slot.value, binding.target_object.value,
            obj.slot_count));
  }
  uint32_t base = obj.design_state_base_slot;
  uint32_t local = binding.target_local_slot.value;
  if (local > UINT32_MAX - base) {
    throw common::InternalError(
        "ResolveExternalRefRoot",
        std::format(
            "design_state_base_slot {} + target_local_slot {} overflows "
            "uint32_t",
            base, local));
  }
  auto global_slot = base + local;
  return ResolvedExternalRefRoot{
      .global_slot = global_slot,
      .type = binding.type,
  };
}

auto Context::GetExternalRefBinding(mir::ExternalRefId ref_id) const
    -> const mir::ResolvedExternalRefBinding& {
  if (!ext_ref_env_.has_value() || ext_ref_env_->bindings == nullptr) {
    throw common::InternalError(
        "GetExternalRefBinding",
        "env not installed -- missing SetExternalRefResolutionEnv");
  }
  const auto& bindings = *ext_ref_env_->bindings;
  if (ref_id.value >= bindings.size()) {
    throw common::InternalError(
        "GetExternalRefBinding",
        std::format(
            "external ref {} out of range (bindings size {})", ref_id.value,
            bindings.size()));
  }
  return bindings[ref_id.value];
}

auto Context::GetConstruction() const -> const mir::ConstructionInput& {
  if (!ext_ref_env_.has_value() || ext_ref_env_->construction == nullptr) {
    throw common::InternalError(
        "GetConstruction", "construction not set in env");
  }
  return *ext_ref_env_->construction;
}

auto Context::NormalizeExternalRefSignalIdentity(
    mir::ExternalRefId ref_id) const -> mir::SignalRef {
  const auto& binding = GetExternalRefBinding(ref_id);
  if (binding.IsPackageOrGlobal()) {
    return mir::SignalRef{
        .scope = mir::SignalRef::Scope::kDesignGlobal,
        .id = binding.GlobalSlotId()};
  }
  const auto& objects = GetConstruction().objects;
  if (binding.target_object.value >= objects.size()) {
    throw common::InternalError(
        "NormalizeExternalRefSignalIdentity",
        std::format(
            "target_object {} out of range (objects size {})",
            binding.target_object.value, objects.size()));
  }
  const auto& obj = objects[binding.target_object.value];
  return mir::SignalRef{
      .scope = mir::SignalRef::Scope::kDesignGlobal,
      .id = obj.design_state_base_slot + binding.target_local_slot.value};
}

auto Context::GetExternalRefType(mir::ExternalRefId ref_id) const -> TypeId {
  return ResolveExternalRefRoot(ref_id).type;
}

auto Context::EmitExternalRefAddress(mir::ExternalRefId ref_id)
    -> llvm::Value* {
  auto root = ResolveExternalRefRoot(ref_id);
  return GetDesignGlobalSlotPointer(root.global_slot);
}

auto Context::LoadExternalRef(mir::ExternalRefId ref_id)
    -> Result<llvm::Value*> {
  auto root = ResolveExternalRefRoot(ref_id);
  auto* ptr = GetDesignGlobalSlotPointer(root.global_slot);
  const Type& type = types_[root.type];

  // Canonical 4-state load for design-global slots (same logic as
  // LoadPlaceValue for kDesignGlobal roots).
  if (auto canonical = TryLoadCanonicalFourStateValue(ptr, type)) {
    return *canonical;
  }

  auto* llvm_type = GetLlvmTypeForTypeId(
      *llvm_context_, root.type, types_, IsForceTwoState());
  return builder_.CreateLoad(llvm_type, ptr, "ext_ref_load");
}

auto Context::EmitExternalRefSignalCoord(mir::ExternalRefId ref_id) const
    -> SignalCoordExpr {
  auto root = ResolveExternalRefRoot(ref_id);
  return SignalCoordExpr::Global(root.global_slot);
}

auto Context::GetWriteDestPointer(const mir::WriteTarget& dest)
    -> Result<llvm::Value*> {
  if (const auto* place = std::get_if<mir::PlaceId>(&dest)) {
    return GetPlacePointer(*place);
  }
  return EmitExternalRefAddress(std::get<mir::ExternalRefId>(dest));
}

void Context::SetSpecSlotInfo(const SpecSlotInfo* info) {
  spec_slot_info_ = info;
}

// Low-level header-field GEP. Returns a pointer to the specified field
// within the process frame header. This is the only place in codegen that
// knows how to navigate from process state to header field address.
auto EmitHeaderFieldGep(
    Context& ctx, llvm::Value* state_arg,
    lyra::runtime::ProcessFrameHeaderField field, llvm::StringRef name)
    -> llvm::Value* {
  auto& builder = ctx.GetBuilder();
  auto* header_ptr = builder.CreateStructGEP(
      ctx.GetProcessStateType(), state_arg, 0, "header_ptr");
  return builder.CreateStructGEP(
      ctx.GetHeaderType(), header_ptr, static_cast<unsigned>(field), name);
}

auto Context::EmitLoadEnginePtr(llvm::Value* state_arg) -> llvm::Value* {
  using F = lyra::runtime::ProcessFrameHeaderField;
  auto* ptr =
      EmitHeaderFieldGep(*this, state_arg, F::kEnginePtr, "engine_ptr_ptr");
  return builder_.CreateLoad(
      llvm::PointerType::getUnqual(*llvm_context_), ptr, "engine_ptr");
}

auto Context::EmitLoadDesignPtr(llvm::Value* state_arg) -> llvm::Value* {
  using F = lyra::runtime::ProcessFrameHeaderField;
  auto* ptr =
      EmitHeaderFieldGep(*this, state_arg, F::kDesignPtr, "design_ptr_ptr");
  return builder_.CreateLoad(
      llvm::PointerType::getUnqual(*llvm_context_), ptr, "design_ptr");
}

auto Context::EmitLoadInstancePtr(llvm::Value* state_arg) -> llvm::Value* {
  using F = lyra::runtime::ProcessFrameHeaderField;
  auto* ptr =
      EmitHeaderFieldGep(*this, state_arg, F::kInstance, "instance_ptr_ptr");
  return builder_.CreateLoad(
      llvm::PointerType::getUnqual(*llvm_context_), ptr, "instance_ptr");
}

auto Context::EmitLoadInstanceInlineBase(llvm::Value* instance_ptr)
    -> llvm::Value* {
  using IF = lyra::runtime::RuntimeInstanceField;
  using SF = lyra::runtime::RuntimeInstanceStorageField;
  auto* storage_ptr = builder_.CreateStructGEP(
      GetRuntimeInstanceType(), instance_ptr,
      static_cast<unsigned>(IF::kStorage), "storage_ptr");
  auto* inline_base_ptr = builder_.CreateStructGEP(
      GetRuntimeInstanceStorageType(), storage_ptr,
      static_cast<unsigned>(SF::kInlineBase), "inline_base_ptr");
  return builder_.CreateLoad(
      llvm::PointerType::getUnqual(*llvm_context_), inline_base_ptr,
      "inline_base");
}

auto Context::EmitLoadInstanceId(llvm::Value* instance_ptr) -> llvm::Value* {
  using IF = lyra::runtime::RuntimeInstanceField;
  auto* id_ptr = builder_.CreateStructGEP(
      GetRuntimeInstanceType(), instance_ptr,
      static_cast<unsigned>(IF::kInstanceId), "instance_id_ptr");
  return builder_.CreateLoad(
      llvm::Type::getInt32Ty(*llvm_context_), id_ptr, "instance_id");
}

void Context::EmitStoreDesignPtr(llvm::Value* state_arg, llvm::Value* value) {
  using F = lyra::runtime::ProcessFrameHeaderField;
  auto* ptr =
      EmitHeaderFieldGep(*this, state_arg, F::kDesignPtr, "design_ptr_ptr");
  builder_.CreateStore(value, ptr);
}

auto Context::EmitOutcomePtr(llvm::Value* state_arg) -> llvm::Value* {
  using F = lyra::runtime::ProcessFrameHeaderField;
  return EmitHeaderFieldGep(*this, state_arg, F::kOutcome, "outcome_ptr");
}

auto Context::EmitLoadDecisionOwnerId(llvm::Value* state_arg) -> llvm::Value* {
  using F = lyra::runtime::ProcessFrameHeaderField;
  // The process frame header field stays process-named (kProcessId); the
  // codegen layer interprets it as the decision owner for process-backed paths.
  auto* ptr =
      EmitHeaderFieldGep(*this, state_arg, F::kProcessId, "decision_owner_ptr");
  return builder_.CreateLoad(
      llvm::Type::getInt32Ty(*llvm_context_), ptr, "decision_owner_id");
}

void Context::EmitProcessStateSetup(llvm::Value* state_arg) {
  SetStatePointer(state_arg);
  SetEnginePointer(EmitLoadEnginePtr(state_arg));
  SetDesignPointer(EmitLoadDesignPtr(state_arg));
  SetCurrentDecisionOwnerId(EmitLoadDecisionOwnerId(state_arg));

  auto* frame_ptr = builder_.CreateStructGEP(
      GetProcessStateType(), state_arg, 1, "frame_ptr");
  SetFramePointer(frame_ptr);
}

void Context::EmitSharedBodyBindingSetup(llvm::Value* state_arg) {
  auto* inst = EmitLoadInstancePtr(state_arg);
  SetInstancePointer(inst);
  SetThisPointer(EmitLoadInstanceInlineBase(inst));
  SetDynamicInstanceId(EmitLoadInstanceId(inst));
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

void Context::SetCurrentDecisionOwnerId(llvm::Value* decision_owner_id) {
  current_decision_owner_id_ = decision_owner_id;
}

auto Context::GetCurrentDecisionOwnerId() -> llvm::Value* {
  return current_decision_owner_id_;
}

void Context::SetDesignStoreMode(DesignStoreMode mode) {
  design_store_mode_ = mode;
}

auto Context::GetDesignStoreMode() const -> DesignStoreMode {
  return design_store_mode_;
}

void Context::SetNotificationPolicy(NotificationPolicy policy) {
  notification_policy_ = policy;
}

auto Context::GetNotificationPolicy() const -> NotificationPolicy {
  return notification_policy_;
}

NotificationPolicyScope::NotificationPolicyScope(
    Context& ctx, NotificationPolicy policy)
    : ctx_(ctx), saved_(ctx.GetNotificationPolicy()) {
  ctx_.SetNotificationPolicy(policy);
}

NotificationPolicyScope::~NotificationPolicyScope() {
  ctx_.SetNotificationPolicy(saved_);
}

auto Context::SaveExecutionContractState() -> ExecutionContractState {
  return {
      .design_store_mode = design_store_mode_,
      .notification_policy = notification_policy_,
      .slot_addressing = slot_addressing_,
      .state_ptr = state_ptr_,
      .design_ptr = design_ptr_,
      .frame_ptr = frame_ptr_,
      .engine_ptr = engine_ptr_,
      .current_decision_owner_id = current_decision_owner_id_,
      .instance_ptr = instance_ptr_,
      .this_ptr = this_ptr_,
      .dynamic_instance_id = dynamic_instance_id_,
  };
}

void Context::RestoreExecutionContractState(
    const ExecutionContractState& state) {
  design_store_mode_ = state.design_store_mode;
  notification_policy_ = state.notification_policy;
  slot_addressing_ = state.slot_addressing;
  state_ptr_ = state.state_ptr;
  design_ptr_ = state.design_ptr;
  frame_ptr_ = state.frame_ptr;
  engine_ptr_ = state.engine_ptr;
  current_decision_owner_id_ = state.current_decision_owner_id;
  instance_ptr_ = state.instance_ptr;
  this_ptr_ = state.this_ptr;
  dynamic_instance_id_ = state.dynamic_instance_id;
}

ExecutionContractScope::ExecutionContractScope(
    Context& ctx, DesignStoreMode mode)
    : ctx_(ctx), saved_(ctx.SaveExecutionContractState()) {
  ctx.SetDesignStoreMode(mode);
  ctx.SetNotificationPolicy(NotificationPolicy::kImmediate);
  ctx.SetSlotAddressingMode(SlotAddressingMode::kDesignGlobal);
  ctx.SetStatePointer(nullptr);
  ctx.SetDesignPointer(nullptr);
  ctx.SetFramePointer(nullptr);
  ctx.SetEnginePointer(nullptr);
  ctx.SetCurrentDecisionOwnerId(nullptr);
  ctx.SetThisPointer(nullptr);
  ctx.SetDynamicInstanceId(nullptr);
  // spec_slot_info is NOT reset: it is session-scoped (set by
  // CompileModuleSpecSession), not per-function.
}

ExecutionContractScope::~ExecutionContractScope() {
  ctx_.RestoreExecutionContractState(saved_);
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

void Context::RegisterDesignFunction(
    SymbolId symbol, mir::FunctionId func_id, llvm::Function* llvm_func) {
  design_functions_.insert_or_assign(
      symbol, DesignFunctionEntry{.func_id = func_id, .llvm_func = llvm_func});
}

auto Context::GetDesignFunction(SymbolId symbol) const
    -> const DesignFunctionEntry& {
  auto it = design_functions_.find(symbol);
  if (it == design_functions_.end()) {
    throw common::InternalError(
        "GetDesignFunction",
        std::format("design function (symbol {}) not found", symbol.value));
  }
  return it->second;
}

void Context::RegisterModuleScopedFunction(
    mir::FunctionId func_id, ModuleFunctionLowering lowering) {
  auto [it, inserted] = module_function_lowering_.emplace(func_id, lowering);
  if (!inserted) {
    throw common::InternalError(
        "RegisterModuleScopedFunction",
        std::format(
            "function {} registered twice (must be registered once per "
            "specialization, not per instance)",
            func_id.value));
  }
}

auto Context::IsModuleScopedFunction(mir::FunctionId func_id) const -> bool {
  return module_function_lowering_.contains(func_id);
}

auto Context::GetModuleFunctionLowering(mir::FunctionId func_id) const
    -> const ModuleFunctionLowering& {
  auto it = module_function_lowering_.find(func_id);
  if (it == module_function_lowering_.end()) {
    throw common::InternalError(
        "GetModuleFunctionLowering",
        std::format(
            "function {} not registered as module-scoped", func_id.value));
  }
  return it->second;
}

void Context::RegisterMonitorLayout(
    mir::FunctionId check_program, MonitorLayout layout) {
  monitor_layouts_.emplace(check_program, std::move(layout));
}

auto Context::GetMonitorLayout(mir::FunctionId check_program) const
    -> const MonitorLayout* {
  auto it = monitor_layouts_.find(check_program);
  if (it == monitor_layouts_.end()) {
    return nullptr;
  }
  return &it->second;
}

void Context::RegisterMonitorSetupInfo(
    mir::FunctionId setup_program, MonitorSetupInfo info) {
  monitor_setup_infos_.emplace(setup_program, std::move(info));
}

auto Context::GetMonitorSetupInfo(mir::FunctionId setup_program) const
    -> const MonitorSetupInfo* {
  auto it = monitor_setup_infos_.find(setup_program);
  if (it == monitor_setup_infos_.end()) {
    return nullptr;
  }
  return &it->second;
}

void Context::RegisterDeferredAssertionSiteInfo(
    mir::DeferredAssertionSiteId site_id,
    const mir::DeferredAssertionSiteInfo* info) {
  if (info == nullptr) {
    throw common::InternalError(
        "RegisterDeferredAssertionSiteInfo",
        std::format("null info for site {}", site_id.Index()));
  }
  auto [it, inserted] =
      deferred_assertion_sites_.emplace(site_id.Index(), info);
  if (!inserted) {
    throw common::InternalError(
        "RegisterDeferredAssertionSiteInfo",
        std::format("duplicate registration for site {}", site_id.Index()));
  }
}

auto Context::GetDeferredAssertionSiteInfo(mir::DeferredAssertionSiteId site_id)
    const -> const mir::DeferredAssertionSiteInfo* {
  auto it = deferred_assertion_sites_.find(site_id.Index());
  if (it == deferred_assertion_sites_.end()) {
    return nullptr;
  }
  return it->second;
}

auto Context::BuildUserFunctionType(
    const mir::FunctionSignature& sig, bool is_module_scoped,
    bool accepts_decision_owner) -> Result<llvm::FunctionType*> {
  auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
  auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);

  // Use MIR return policy (frozen at HIR->MIR lowering)
  bool uses_sret = sig.return_policy == mir::ReturnPolicy::kSretOutParam;

  std::vector<llvm::Type*> param_types;

  // For sret: first parameter is the output pointer
  if (uses_sret) {
    param_types.push_back(ptr_ty);  // sret ptr
  }

  // DesignState* and Engine* follow
  param_types.push_back(ptr_ty);  // DesignState*
  param_types.push_back(ptr_ty);  // Engine*

  // Module-scoped functions receive specialization-local context
  if (is_module_scoped) {
    param_types.push_back(ptr_ty);  // this_ptr (module instance storage)
    param_types.push_back(ptr_ty);  // instance_ptr (RuntimeInstance*)
    param_types.push_back(i32_ty);  // instance_id
  }

  // Decision owner: caller's active decision_owner_id threaded explicitly
  if (accepts_decision_owner) {
    param_types.push_back(i32_ty);  // decision_owner_id
  }

  // Add parameter types from signature.
  // All direct-value types must be representable by the generic callable
  // ABI classifier. Output/inout parameters bypass classification and use
  // pointer-to-destination directly.
  for (const auto& param : sig.params) {
    if (param.kind == mir::PassingKind::kOut ||
        param.kind == mir::PassingKind::kInOut ||
        param.kind == mir::PassingKind::kRef ||
        param.kind == mir::PassingKind::kConstRef) {
      param_types.push_back(ptr_ty);
    } else {
      auto abi_info = ClassifyCallableValueAbi(
          *llvm_context_, param.type, types_, force_two_state_);
      if (!abi_info) {
        return std::unexpected(
            GetDiagnosticContext().MakeUnsupported(
                current_origin_,
                std::format(
                    "aggregate type {} cannot be passed by value",
                    param.type.value),
                UnsupportedCategory::kType));
      }
      param_types.push_back(abi_info->llvm_type);
    }
  }

  // Return type: void for sret, otherwise classified by the generic
  // callable ABI. The classifier is the single source of truth for
  // direct-return LLVM types.
  llvm::Type* llvm_ret_type = nullptr;

  if (uses_sret) {
    llvm_ret_type = llvm::Type::getVoidTy(*llvm_context_);
  } else {
    const Type& ret_type = types_[sig.return_type];

    if (ret_type.Kind() == TypeKind::kVoid) {
      llvm_ret_type = llvm::Type::getVoidTy(*llvm_context_);
    } else {
      auto abi_info = ClassifyCallableValueAbi(
          *llvm_context_, sig.return_type, types_, force_two_state_);
      if (!abi_info) {
        return std::unexpected(
            GetDiagnosticContext().MakeUnsupported(
                current_origin_,
                std::format(
                    "aggregate type {} cannot be returned by value",
                    sig.return_type.value),
                UnsupportedCategory::kType));
      }
      llvm_ret_type = abi_info->llvm_type;
    }
  }

  return llvm::FunctionType::get(llvm_ret_type, param_types, false);
}

auto Context::FunctionUsesSret(mir::FunctionId func_id) const -> bool {
  const auto& func = (*arena_)[func_id];
  return func.signature.return_policy == mir::ReturnPolicy::kSretOutParam;
}

auto Context::RegisterBackEdgeSite(common::OriginId origin) -> uint32_t {
  auto id = static_cast<uint32_t>(back_edge_site_origins_.size());
  back_edge_site_origins_.push_back(origin);
  return id;
}

}  // namespace lyra::lowering::mir_to_llvm
