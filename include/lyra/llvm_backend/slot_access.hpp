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
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/terminator.hpp"

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
  virtual ~SlotAccessResolver() = default;

  // Load a whole-slot module-slot value.
  // Only called for PlaceRoot::kModuleSlot with no projections.
  virtual auto LoadSlotValue(mir::PlaceId place_id) -> Result<llvm::Value*> = 0;

  // Commit a value to a whole-slot module-slot.
  // Only called for PlaceRoot::kModuleSlot with no projections.
  virtual auto CommitSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id,
      OwnershipPolicy policy) -> Result<void> = 0;

  // Query: is this place a managed activation-local slot?
  [[nodiscard]] virtual auto ManagesPlace(mir::PlaceId place_id) const
      -> bool = 0;
};

// Segment lifecycle interface for activation-local synchronization.
// Process lowering drives lifecycle actions through this interface.
// Canonical lowering provides a no-op implementation.
class SegmentLifecycle {
 public:
  virtual ~SegmentLifecycle() = default;

  virtual void SeedFromCanonical() = 0;
  virtual void SyncToCanonical() = 0;
  virtual void SyncAndReloadAll() = 0;
  virtual void SyncAndReloadSpecific(std::span<const mir::SignalRef> slots) = 0;
};

class CanonicalSlotAccess final : public SlotAccessResolver,
                                  public SegmentLifecycle {
 public:
  explicit CanonicalSlotAccess(Context& ctx);

  auto LoadSlotValue(mir::PlaceId place_id) -> Result<llvm::Value*> override;
  auto CommitSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id,
      OwnershipPolicy policy) -> Result<void> override;
  [[nodiscard]] auto ManagesPlace(mir::PlaceId place_id) const -> bool override;

  void SeedFromCanonical() override;
  void SyncToCanonical() override;
  void SyncAndReloadAll() override;
  void SyncAndReloadSpecific(std::span<const mir::SignalRef> slots) override;

 private:
  Context& ctx_;
};

struct ManagedSlotStorage {
  mir::SignalRef slot;
  TypeId root_type;
  llvm::AllocaInst* alloca_inst = nullptr;
};

class ActivationLocalSlotAccess final : public SlotAccessResolver,
                                        public SegmentLifecycle {
 public:
  ActivationLocalSlotAccess(
      Context& ctx, std::span<const ManagedSlotStorage> storage);

  auto LoadSlotValue(mir::PlaceId place_id) -> Result<llvm::Value*> override;
  auto CommitSlotValue(
      mir::PlaceId target, llvm::Value* value, TypeId type_id,
      OwnershipPolicy policy) -> Result<void> override;
  [[nodiscard]] auto ManagesPlace(mir::PlaceId place_id) const -> bool override;

  void SeedFromCanonical() override;
  void SyncToCanonical() override;
  void SyncAndReloadAll() override;
  void SyncAndReloadSpecific(std::span<const mir::SignalRef> slots) override;

 private:
  auto FindManagedStorage(mir::PlaceId place_id) const
      -> const ManagedSlotStorage*;
  void SeedSlot(const ManagedSlotStorage& storage);
  void SyncSlot(const ManagedSlotStorage& storage);

  Context& ctx_;
  std::unordered_map<uint32_t, ManagedSlotStorage> managed_;
};

auto CreateManagedSlotStorage(const ProcessActivationPlan& plan, Context& ctx)
    -> std::vector<ManagedSlotStorage>;

}  // namespace lyra::lowering::mir_to_llvm
