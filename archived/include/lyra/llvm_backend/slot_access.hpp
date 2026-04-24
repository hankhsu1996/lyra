#pragma once

#include <cstdint>
#include <span>
#include <unordered_map>
#include <vector>

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/activation_local.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/signal_ref.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// The single explicit boundary where module-slot access mode is decided.
// All module-slot reads and writes in statement/operand lowering go through
// this interface. Non-module-slot accesses (locals, temps) do NOT go through
// this interface -- they use the existing Context paths directly.
//
// Projected reads/writes (bit-range, field, index) are canonical-only in v1
// and do NOT go through this interface.
class SlotAccessResolver {
 public:
  SlotAccessResolver() = default;
  virtual ~SlotAccessResolver() = default;
  SlotAccessResolver(const SlotAccessResolver&) = delete;
  auto operator=(const SlotAccessResolver&) -> SlotAccessResolver& = delete;
  SlotAccessResolver(SlotAccessResolver&&) = default;
  auto operator=(SlotAccessResolver&&) -> SlotAccessResolver& = default;

  // Load a whole-slot module-slot value.
  // Only called for PlaceRoot::kModuleSlot with no projections.
  virtual auto LoadSlotValue(mir::PlaceId place_id) -> Result<llvm::Value*> = 0;

  // Commit a value to a whole-slot module-slot.
  // Only called for PlaceRoot::kModuleSlot with no projections.
  virtual auto CommitSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id,
      OwnershipPolicy policy) -> Result<void> = 0;

  // Commit a non-managed value to a whole-slot module-slot (plain path).
  // No OwnershipPolicy -- lifecycle is not applicable to non-managed types.
  virtual auto CommitPlainSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id)
      -> Result<void> = 0;

  // Query: does this place require commit through this resolver?
  // True for activation-local managed slots (shadow alloca protocol).
  // This is routing, not lifecycle -- non-managed types can be resolver-routed.
  [[nodiscard]] virtual auto RequiresResolverCommit(mir::PlaceId place_id) const
      -> bool = 0;
};

// Segment lifecycle interface for activation-local synchronization.
// Process lowering drives lifecycle actions through this interface.
// Canonical lowering provides a no-op implementation.
class SegmentLifecycle {
 public:
  SegmentLifecycle() = default;
  virtual ~SegmentLifecycle() = default;
  SegmentLifecycle(const SegmentLifecycle&) = delete;
  auto operator=(const SegmentLifecycle&) -> SegmentLifecycle& = delete;
  SegmentLifecycle(SegmentLifecycle&&) = default;
  auto operator=(SegmentLifecycle&&) -> SegmentLifecycle& = default;

  virtual void SeedFromCanonical() = 0;
  virtual void SyncToCanonical() = 0;
  virtual void SyncAndReloadAll() = 0;
  virtual void SyncAndReloadSpecific(std::span<const mir::SignalRef> slots) = 0;
};

class CanonicalSlotAccess final : public SlotAccessResolver,
                                  public SegmentLifecycle {
 public:
  CanonicalSlotAccess(Context& ctx, const CuFacts& facts);

  auto LoadSlotValue(mir::PlaceId place_id) -> Result<llvm::Value*> override;
  auto CommitSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id,
      OwnershipPolicy policy) -> Result<void> override;
  auto CommitPlainSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id)
      -> Result<void> override;
  [[nodiscard]] auto RequiresResolverCommit(mir::PlaceId place_id) const
      -> bool override;

  void SeedFromCanonical() override;
  void SyncToCanonical() override;
  void SyncAndReloadAll() override;
  void SyncAndReloadSpecific(std::span<const mir::SignalRef> slots) override;

 private:
  Context* ctx_;
  const CuFacts* facts_;
};

// Shadow storage for an activation-local managed signal slot.
// Always backed by a persistent process frame field (GEP into ProcessFrameN).
// Suspension-free processes do not use activation-local storage.
struct ManagedSlotStorage {
  mir::SignalRef slot;
  TypeId root_type;
  // GEP into the persistent process frame for this shadow cell.
  llvm::Value* shadow_ptr = nullptr;
  // LLVM type of the shadow cell (matches the frame field type).
  llvm::Type* shadow_type = nullptr;
};

class ActivationLocalSlotAccess final : public SlotAccessResolver,
                                        public SegmentLifecycle {
 public:
  ActivationLocalSlotAccess(
      Context& ctx, const CuFacts& facts,
      std::span<const ManagedSlotStorage> storage);

  auto LoadSlotValue(mir::PlaceId place_id) -> Result<llvm::Value*> override;
  auto CommitSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id,
      OwnershipPolicy policy) -> Result<void> override;
  auto CommitPlainSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id)
      -> Result<void> override;
  [[nodiscard]] auto RequiresResolverCommit(mir::PlaceId place_id) const
      -> bool override;

  void SeedFromCanonical() override;
  void SyncToCanonical() override;
  void SyncAndReloadAll() override;
  void SyncAndReloadSpecific(std::span<const mir::SignalRef> slots) override;

 private:
  auto FindManagedStorage(mir::PlaceId place_id) const
      -> const ManagedSlotStorage*;
  void SeedSlot(const ManagedSlotStorage& storage);
  void SyncSlot(const ManagedSlotStorage& storage);

  Context* ctx_;
  const CuFacts* facts_;
  std::unordered_map<uint32_t, ManagedSlotStorage> managed_;
};

// Create managed slot storage backed by persistent frame fields.
// For processes with suspension only (frame fields survive suspend/resume).
auto CreateManagedSlotStorage(const ProcessActivationPlan& plan, Context& ctx)
    -> std::vector<ManagedSlotStorage>;

// Create managed slot storage backed by stack allocas.
// For suspension-free processes only (single function call, no resume).
auto CreateManagedSlotStorageAsAllocas(
    const ProcessActivationPlan& plan, Context& ctx, const CuFacts& facts)
    -> std::vector<ManagedSlotStorage>;

}  // namespace lyra::lowering::mir_to_llvm
