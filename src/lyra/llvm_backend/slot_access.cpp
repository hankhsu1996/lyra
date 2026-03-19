#include "lyra/llvm_backend/slot_access.hpp"

#include <cstdint>
#include <span>
#include <unordered_set>

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

CanonicalSlotAccess::CanonicalSlotAccess(Context& ctx) : ctx_(ctx) {
}

auto CanonicalSlotAccess::LoadSlotValue(mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  return ctx_.LoadPlaceValue(place_id);
}

auto CanonicalSlotAccess::CommitSlotValue(
    mir::PlaceId target, llvm::Value* value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void> {
  return CommitValue(ctx_, target, value, type_id, policy);
}

auto CanonicalSlotAccess::ManagesPlace(mir::PlaceId /*place_id*/) const
    -> bool {
  return false;
}

void CanonicalSlotAccess::SeedFromCanonical() {
}
void CanonicalSlotAccess::SyncToCanonical() {
}
void CanonicalSlotAccess::SyncAndReloadAll() {
}
void CanonicalSlotAccess::SyncAndReloadSpecific(
    std::span<const mir::SignalRef> /*slots*/) {
}

ActivationLocalSlotAccess::ActivationLocalSlotAccess(
    Context& ctx, std::span<const ManagedSlotStorage> storage)
    : ctx_(ctx) {
  for (const auto& s : storage) {
    managed_[s.slot.id] = s;
  }
}

auto ActivationLocalSlotAccess::FindManagedStorage(mir::PlaceId place_id) const
    -> const ManagedSlotStorage* {
  const auto& arena = ctx_.GetMirArena();
  const auto& place = arena[place_id];
  if (place.root.kind != mir::PlaceRoot::Kind::kModuleSlot) return nullptr;
  if (!place.projections.empty()) return nullptr;
  auto it = managed_.find(static_cast<uint32_t>(place.root.id));
  if (it == managed_.end()) {
    return nullptr;
  }
  return &it->second;
}

auto ActivationLocalSlotAccess::LoadSlotValue(mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  const auto* storage = FindManagedStorage(place_id);
  if (storage == nullptr) {
    return ctx_.LoadPlaceValue(place_id);
  }
  auto& builder = ctx_.GetBuilder();
  return builder.CreateLoad(
      storage->alloca_inst->getAllocatedType(), storage->alloca_inst,
      "actlocal.load");
}

auto ActivationLocalSlotAccess::CommitSlotValue(
    mir::PlaceId target, llvm::Value* value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void> {
  const auto* storage = FindManagedStorage(target);
  if (storage == nullptr) {
    return CommitValue(ctx_, target, value, type_id, policy);
  }
  ctx_.GetBuilder().CreateStore(value, storage->alloca_inst);
  return {};
}

auto ActivationLocalSlotAccess::ManagesPlace(mir::PlaceId place_id) const
    -> bool {
  return FindManagedStorage(place_id) != nullptr;
}

void ActivationLocalSlotAccess::SeedSlot(const ManagedSlotStorage& storage) {
  // Invariant: managed slots are strictly module-local, specialization-
  // addressed. Design-global slots are excluded by eligibility analysis.
  if (storage.slot.scope != mir::SignalRef::Scope::kModuleLocal) {
    throw common::InternalError(
        "SeedSlot", "managed slot must be module-local");
  }
  auto& builder = ctx_.GetBuilder();
  auto* canonical_ptr = ctx_.GetSignalSlotPointer(storage.slot);
  auto* llvm_type = storage.alloca_inst->getAllocatedType();
  auto* val = builder.CreateLoad(llvm_type, canonical_ptr, "actlocal.seed");
  builder.CreateStore(val, storage.alloca_inst);
}

void ActivationLocalSlotAccess::SyncSlot(const ManagedSlotStorage& storage) {
  if (storage.slot.scope != mir::SignalRef::Scope::kModuleLocal) {
    throw common::InternalError(
        "SyncSlot", "managed slot must be module-local");
  }
  auto& builder = ctx_.GetBuilder();
  auto* llvm_type = storage.alloca_inst->getAllocatedType();

  auto* val =
      builder.CreateLoad(llvm_type, storage.alloca_inst, "actlocal.sync");
  auto* canonical_ptr = ctx_.GetSignalSlotPointer(storage.slot);
  auto signal_id = ctx_.EmitSignalId(storage.slot);

  // Delegate to the single activation-local flush helper.
  // This handles store + compare + dirty-mark under the current
  // DesignStoreMode and NotificationPolicy without duplicating
  // commit/notification logic here.
  EmitActivationLocalFlush(ctx_, canonical_ptr, val, signal_id);
}

void ActivationLocalSlotAccess::SeedFromCanonical() {
  for (const auto& [id, storage] : managed_) {
    SeedSlot(storage);
  }
}

void ActivationLocalSlotAccess::SyncToCanonical() {
  // Two-phase sync: store all managed values to canonical first, then
  // emit dirty-marks. This ensures all canonical values are up-to-date
  // before any notification triggers (which may evaluate other slots).
  // Without this ordering, a dirty-mark on slot A can trigger a
  // subscription that reads slot B before B is synced.
  auto& builder = ctx_.GetBuilder();

  // Phase 1: store all managed slot values to canonical memory.
  struct SyncEntry {
    llvm::Value* val;
    llvm::Value* canonical_ptr;
    SignalIdExpr signal_id;
  };
  std::vector<SyncEntry> entries;
  for (const auto& [id, storage] : managed_) {
    if (storage.slot.scope != mir::SignalRef::Scope::kModuleLocal) {
      throw common::InternalError(
          "SyncToCanonical", "managed slot must be module-local");
    }
    auto* llvm_type = storage.alloca_inst->getAllocatedType();
    auto* val =
        builder.CreateLoad(llvm_type, storage.alloca_inst, "actlocal.sync");
    auto* canonical_ptr = ctx_.GetSignalSlotPointer(storage.slot);
    builder.CreateStore(val, canonical_ptr);
    entries.push_back(
        SyncEntry{
            .val = val,
            .canonical_ptr = canonical_ptr,
            .signal_id = ctx_.EmitSignalId(storage.slot),
        });
  }

  // Phase 2: emit dirty-marks (all canonical values already up-to-date).
  if (ctx_.GetDesignStoreMode() == DesignStoreMode::kDirectInit ||
      ctx_.GetNotificationPolicy() == NotificationPolicy::kDeferred) {
    return;
  }
  auto& llvm_ctx = ctx_.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  for (const auto& entry : entries) {
    builder.CreateCall(
        ctx_.GetLyraMarkDirty(),
        {ctx_.GetEnginePointer(), entry.signal_id.Emit(builder),
         llvm::ConstantInt::get(i32_ty, 0), llvm::ConstantInt::get(i32_ty, 0)});
  }
}

void ActivationLocalSlotAccess::SyncAndReloadAll() {
  // Sync scope: all managed slots.
  // Reload scope: all managed slots.
  SyncToCanonical();
  SeedFromCanonical();
}

void ActivationLocalSlotAccess::SyncAndReloadSpecific(
    std::span<const mir::SignalRef> slots) {
  // Sync scope: all managed slots (conservative in v1).
  // Reload scope: only the specified subset.
  SyncToCanonical();
  std::unordered_set<uint32_t> reload_ids;
  for (const auto& slot : slots) {
    reload_ids.insert(slot.id);
  }
  for (const auto& [id, storage] : managed_) {
    if (reload_ids.contains(id)) {
      SeedSlot(storage);
    }
  }
}

namespace {

// Get the LLVM storage type for an activation-local managed slot.
// Asserts v1 invariants: only plain scalar types are eligible for
// activation-local treatment. For these types, the canonical slot
// storage representation is identical to GetLlvmTypeForTypeId:
//   kIntegral -> iN (2-state) or {iN, iN} (4-state)
//   kReal -> double
//   kShortReal -> float
//   kEnum with integral base -> same as kIntegral
auto GetActivationLocalStorageType(
    llvm::LLVMContext& llvm_ctx, TypeId root_type, const TypeArena& types,
    bool force_two_state) -> llvm::Type* {
  const auto& type = types[root_type];
  auto kind = type.Kind();
  if (kind == TypeKind::kEnum) {
    kind = types[type.AsEnum().base_type].Kind();
  }
  if (kind != TypeKind::kIntegral && kind != TypeKind::kReal &&
      kind != TypeKind::kShortReal) {
    throw common::InternalError(
        "GetActivationLocalStorageType",
        "managed slot type must be a v1-eligible scalar (integral, real, "
        "shortreal, or enum with integral base)");
  }
  return GetLlvmTypeForTypeId(llvm_ctx, root_type, types, force_two_state);
}

}  // namespace

auto CreateManagedSlotStorage(const ProcessActivationPlan& plan, Context& ctx)
    -> std::vector<ManagedSlotStorage> {
  auto& llvm_ctx = ctx.GetLlvmContext();
  const auto& types = ctx.GetTypeArena();
  bool force_two_state = ctx.IsForceTwoState();

  std::unordered_set<uint32_t> seen_slot_ids;
  std::vector<ManagedSlotStorage> result;

  auto* func = ctx.GetBuilder().GetInsertBlock()->getParent();
  auto& entry_block = func->getEntryBlock();
  llvm::IRBuilder<> alloca_builder(&entry_block, entry_block.begin());

  for (const auto& segment : plan.segments) {
    for (const auto& managed : segment.managed_slots.slots) {
      if (!seen_slot_ids.insert(managed.slot.id).second) continue;

      auto* alloca_type = GetActivationLocalStorageType(
          llvm_ctx, managed.root_type, types, force_two_state);

      auto* alloca_inst =
          alloca_builder.CreateAlloca(alloca_type, nullptr, "actlocal.shadow");

      result.push_back(
          ManagedSlotStorage{
              .slot = managed.slot,
              .root_type = managed.root_type,
              .alloca_inst = alloca_inst,
          });
    }
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
