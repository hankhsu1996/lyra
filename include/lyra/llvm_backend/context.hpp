#pragma once

#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "absl/container/flat_hash_map.h"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/slot_id.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/llvm_backend/commit/signal_id_expr.hpp"
#include "lyra/llvm_backend/compute/temp_value.hpp"
#include "lyra/llvm_backend/context_scope.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/external_ref.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra {
class SourceManager;
}

namespace lyra::mir {
struct ScopedSlotRef;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

struct SpecSlotInfo;
class Context;

// Forward declaration for friend access
namespace commit {
class Access;
}  // namespace commit

// Forward declaration for WriteTarget (defined in commit/access.hpp)
struct WriteTarget;
// Forward declaration for ConnectionNotificationMask (codegen_session.hpp)
struct ConnectionNotificationMask;

// How design-slot stores are emitted in the current codegen context.
// Set from the process execution contract at function generation time.
// Queried by the commit layer to select between direct store and
// compare+notify paths.
//
// The three modes correspond to three execution-context categories:
// - init-only: no engine, no subscriptions
// - simulation-only: engine guaranteed non-null, known context
// - cross-context: compiled once, callable from both init and simulation
enum class DesignStoreMode {
  // Init-only contract. Direct store with no compare, no dirty-mark,
  // no engine access. Used for init processes where no runtime engine exists.
  kDirectInit,
  // Simulation-only contract. Compare old/new, store, dirty-mark if changed.
  // Engine guaranteed non-null. No engine-null branch in generated IR.
  // Used for process bodies with a known simulation execution context.
  kNotifySimulation,
  // Cross-context contract. Compare old/new, store, dirty-mark if changed.
  // Engine may be null at runtime (guarded with a branch). Used for
  // user-defined functions/tasks that can be called from both init and
  // simulation contexts.
  kNotifyCrossContext,
};

// When dirty-mark notification fires for design-slot writes.
// Orthogonal to DesignStoreMode (which identifies the execution context).
//
// kImmediate: notification fires inline at each store (default).
// kDeferred: notification is suppressed; the store commits immediately
// to DesignState but the dirty-mark is emitted as a loop-exit edge
// effect. Only legal within qualifying non-yielding regions where
// no scheduler-visible observation can occur before the deferred point.
enum class NotificationPolicy {
  kImmediate,
  kDeferred,
};

// How module-local slots (kModuleSlot) are addressed in the current lowering
// scope. Set explicitly per function scope -- never inferred from nullable
// fields.
//
// kSpecializationLocal: Module slots accessed via this_ptr +
//   rel_byte_offsets. Signal coordination uses typed local/global runtime
//   helpers (engine_ptr, instance_ptr, local_id). Used in shared module
//   behavioral process bodies and module-scoped user functions.
//
// kDesignGlobal: Design-global slots accessed via design_ptr + struct GEP.
//   Signal identity is a constant design-global slot ID. Used in standalone
//   non-module processes (init, connection). Module-local (kModuleSlot)
//   references are invalid in this mode -- they indicate an architecture
//   violation and will throw InternalError.
enum class SlotAddressingMode {
  kDesignGlobal,
  kSpecializationLocal,
};

// Snapshot of per-function execution-contract state on Context.
// Used by ExecutionContractScope for save/restore.
//
// IMPORTANT: This struct must mirror every mutable Context field that
// affects codegen semantics and is set per function/process scope.
// If you add a new mutable field to Context that controls codegen
// behavior (store mode, notification policy, cached pointers, etc.),
// add it here so ExecutionContractScope saves/restores it. Failure to
// do so causes state leakage between process compilations that share
// the same Context.
struct ExecutionContractState {
  DesignStoreMode design_store_mode = DesignStoreMode::kNotifySimulation;
  NotificationPolicy notification_policy = NotificationPolicy::kImmediate;
  SlotAddressingMode slot_addressing = SlotAddressingMode::kDesignGlobal;
  llvm::Value* state_ptr = nullptr;
  llvm::Value* design_ptr = nullptr;
  llvm::Value* frame_ptr = nullptr;
  llvm::Value* engine_ptr = nullptr;
  llvm::Value* current_decision_owner_id = nullptr;
  llvm::Value* instance_ptr = nullptr;
  llvm::Value* this_ptr = nullptr;
  llvm::Value* dynamic_instance_id = nullptr;
};

// RAII guard that sets execution-contract state on Context and restores it
// on scope exit. Covers all per-function execution-contract fields.
// Every executable-body generator (process, shared process, MIR function)
// must enter one of these scopes to set its contract explicitly.
class ExecutionContractScope {
 public:
  ExecutionContractScope(Context& ctx, DesignStoreMode mode);
  ~ExecutionContractScope();
  ExecutionContractScope(const ExecutionContractScope&) = delete;
  auto operator=(const ExecutionContractScope&)
      -> ExecutionContractScope& = delete;
  ExecutionContractScope(ExecutionContractScope&&) = delete;
  auto operator=(ExecutionContractScope&&) -> ExecutionContractScope& = delete;

 private:
  Context& ctx_;
  ExecutionContractState saved_;
};

// Scoped notification policy guard. Sets the policy on construction,
// restores the previous policy on destruction. Used per-block in the
// codegen block loop to ensure early returns cannot leak kDeferred.
class NotificationPolicyScope {
 public:
  NotificationPolicyScope(Context& ctx, NotificationPolicy policy);
  ~NotificationPolicyScope();
  NotificationPolicyScope(const NotificationPolicyScope&) = delete;
  auto operator=(const NotificationPolicyScope&)
      -> NotificationPolicyScope& = delete;
  NotificationPolicyScope(NotificationPolicyScope&&) = delete;
  auto operator=(NotificationPolicyScope&&)
      -> NotificationPolicyScope& = delete;

 private:
  Context& ctx_;
  NotificationPolicy saved_;
};

// Shared context for MIR -> LLVM lowering
class Context {
  friend class commit::Access;
  friend class ExecutionContractScope;

 public:
  Context(
      const mir::Arena& arena, const TypeArena& types, const Layout& layout,
      std::unique_ptr<llvm::LLVMContext> llvm_ctx,
      std::unique_ptr<llvm::Module> module,
      const lowering::DiagnosticContext* diag_ctx,
      const SourceManager* source_manager = nullptr,
      bool force_two_state = false);

  [[nodiscard]] auto GetLlvmContext() -> llvm::LLVMContext& {
    return *llvm_context_;
  }
  [[nodiscard]] auto GetModule() -> llvm::Module& {
    return *llvm_module_;
  }
  [[nodiscard]] auto GetBuilder() -> llvm::IRBuilder<>& {
    return builder_;
  }

  [[nodiscard]] auto GetMirArena() const -> const mir::Arena& {
    return *arena_;
  }

  // Get the design arena for cross-domain resolution.
  // Always valid -- set once at Context construction, never changed.
  [[nodiscard]] auto GetDesignArena() const -> const mir::Arena& {
    return *design_arena_;
  }

  // Canonical Place lookup from the body arena.
  // All place consumers must use this instead of raw arena access.
  [[nodiscard]] auto LookupPlace(mir::PlaceId place_id) const
      -> const mir::Place&;

  // Scoped arena guard: sets the arena on construction, restores on
  // destruction. Only way to switch arenas -- no public setter exposed.
  class ArenaScope {
   public:
    ArenaScope(Context& ctx, const mir::Arena* arena)
        : ctx_(ctx), saved_(ctx.arena_) {
      ctx_.arena_ = arena;
    }
    ~ArenaScope() {
      ctx_.arena_ = saved_;
    }
    ArenaScope(const ArenaScope&) = delete;
    auto operator=(const ArenaScope&) -> ArenaScope& = delete;
    ArenaScope(ArenaScope&&) = delete;
    auto operator=(ArenaScope&&) -> ArenaScope& = delete;

   private:
    Context& ctx_;
    const mir::Arena* saved_;
  };
  // Scoped diagnostic context guard: temporarily replaces diag_ctx_ for one
  // compilation session. Restores previous context on destruction.
  class DiagnosticScope {
   public:
    DiagnosticScope(Context& ctx, const lowering::DiagnosticContext* diag_ctx)
        : ctx_(ctx), saved_(ctx.diag_ctx_) {
      ctx_.diag_ctx_ = diag_ctx;
    }
    ~DiagnosticScope() {
      ctx_.diag_ctx_ = saved_;
    }
    DiagnosticScope(const DiagnosticScope&) = delete;
    auto operator=(const DiagnosticScope&) -> DiagnosticScope& = delete;
    DiagnosticScope(DiagnosticScope&&) = delete;
    auto operator=(DiagnosticScope&&) -> DiagnosticScope& = delete;

   private:
    Context& ctx_;
    const lowering::DiagnosticContext* saved_;
  };

  [[nodiscard]] auto GetTypeArena() const -> const TypeArena& {
    return types_;
  }
  [[nodiscard]] auto GetLayout() const -> const Layout& {
    return layout_;
  }

  [[nodiscard]] auto IsForceTwoState() const -> bool {
    return force_two_state_;
  }
  [[nodiscard]] auto IsFourState(TypeId type_id) const -> bool {
    if (force_two_state_) return false;
    return lyra::IsIntrinsicallyFourState(type_id, types_);
  }
  [[nodiscard]] auto IsPackedFourState(const Type& type) const -> bool {
    if (force_two_state_) return false;
    return lyra::IsIntrinsicallyPackedFourState(type, types_);
  }

  [[nodiscard]] auto GetLyraPrintLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraWarnRateLimited() -> llvm::Function*;
  [[nodiscard]] auto GetLyraEmitReport() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRecordDecisionObservation() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRecordImmediateCoverHit() -> llvm::Function*;
  [[nodiscard]] auto GetLyraEnqueueObservedDeferredAssertion()
      -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintValue() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintEnd() -> llvm::Function*;
  [[nodiscard]] auto GetFormatSpecType() -> llvm::StructType*;
  [[nodiscard]] auto GetLyraRegisterVar() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSnapshotVars() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromCStr() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringCmp() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRetain() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringConcat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromPacked() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringGetView() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringGetCStr() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPackedFromString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunSimulation() -> llvm::Function*;
  [[nodiscard]] auto GetLyraConstructProcessStates() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDestroyProcessStates() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunProcessSync() -> llvm::Function*;
  [[nodiscard]] auto GetLyraIterationLimitPtr() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPlusargsTest() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPlusargsValueInt() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPlusargsValueString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendDelay() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendWait() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendWaitStatic() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendWaitWithLateBound() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendRepeat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAllocTriggers() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFreeTriggers() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendWaitEvent() -> llvm::Function*;
  [[nodiscard]] auto GetLyraTriggerEvent() -> llvm::Function*;
  [[nodiscard]] auto GetLyraResolveSlotPtr() -> llvm::Function*;
  // R5: Resolve RuntimeInstance* from InstanceId at runtime.
  [[nodiscard]] auto GetLyraResolveInstancePtr() -> llvm::Function*;
  // R3 typed coordination helpers (take frame* instead of engine*).
  [[nodiscard]] auto GetLyraMarkDirtyLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraMarkDirtyGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStorePackedLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStorePackedGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreStringLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreStringGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNbaLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNbaGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNbaCanonicalPackedLocal()
      -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNbaCanonicalPackedGlobal()
      -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNbaExtRef() -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNbaCanonicalPackedExtRef()
      -> llvm::Function*;
  [[nodiscard]] auto GetLyraIsTraceObservedLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraIsTraceObservedGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraNotifyContainerMutationLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraNotifyContainerMutationGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraNotifySignalLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraNotifySignalGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraMarkDirtyExtRef() -> llvm::Function*;
  [[nodiscard]] auto GetLyraTerminate() -> llvm::Function*;
  [[nodiscard]] auto GetLyraGetTime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraInitRuntime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraResolveBaseDir() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReportTime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayNew() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayNewCopy() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArraySize() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayElementPtr() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayClone() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayDelete() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreDynArrayLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreDynArrayGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayCloneElem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayDestroyElem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraQueuePushBack() -> llvm::Function*;
  [[nodiscard]] auto GetLyraQueuePushFront() -> llvm::Function*;
  [[nodiscard]] auto GetLyraQueuePopBack() -> llvm::Function*;
  [[nodiscard]] auto GetLyraQueuePopFront() -> llvm::Function*;
  [[nodiscard]] auto GetLyraQueueInsert() -> llvm::Function*;
  [[nodiscard]] auto GetLyraQueueDeleteAt() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFormatStart() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFormatLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFormatValue() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFormatString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFormatFinish() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFormatRuntime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSetTimeFormat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFopenFd() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFopenMcd() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFclose() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFflush() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFWrite() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRegisterStrobe() -> llvm::Function*;
  [[nodiscard]] auto GetLyraMonitorSetEnabled() -> llvm::Function*;
  [[nodiscard]] auto GetLyraMonitorRegister() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReadmemLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReadmemGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReadmemNoNotify() -> llvm::Function*;
  [[nodiscard]] auto GetLyraWritemem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintModulePath() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFillPackedElements() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRandom() -> llvm::Function*;
  [[nodiscard]] auto GetLyraUrandom() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFgetc() -> llvm::Function*;
  [[nodiscard]] auto GetLyraUngetc() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFgets() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFreadLocal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFreadGlobal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFreadNoNotify() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFscanf() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocNew() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocClone() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocGet() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocSet() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocExists() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocDeleteKey() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocDeleteAll() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocSize() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocFirst() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocLast() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocNext() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocPrev() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocSnapshotCreate() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocSnapshotSize() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocSnapshotKeyAt() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocSnapshotRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocCloneElem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAssocDestroyElem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSystemCmd() -> llvm::Function*;
  [[nodiscard]] auto GetLyraGetDpiExportCallContext() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFailMissingDpiExportCallContext()
      -> llvm::Function*;
  [[nodiscard]] auto GetLyraPushCurrentDpiScope() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPopCurrentDpiScope() -> llvm::Function*;
  [[nodiscard]] auto GetLyraResolvePackageExportBinding() -> llvm::Function*;
  [[nodiscard]] auto GetLyraResolveModuleInstanceBinding() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPushDpiExportCallContext() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPopDpiExportCallContext() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReportMissingDecisionOwnerFatal()
      -> llvm::Function*;

  struct ElemOpsInfo {
    int32_t elem_size = 0;
    llvm::Type* elem_llvm_type = nullptr;
    llvm::Constant* clone_fn = nullptr;
    llvm::Constant* destroy_fn = nullptr;
    bool needs_clone = false;
  };
  auto GetElemOpsForType(TypeId elem_type) -> Result<ElemOpsInfo>;

  // Cached union storage info
  struct CachedUnionInfo {
    uint32_t size;
    uint32_t align;
    llvm::Type* storage_type;
  };

  // Get or create a global constant array of enum member values for the given
  // enum type. Returns a [N x iW] global, where N = member count and W = base
  // type bit width. Cached per TypeId.
  auto GetOrCreateEnumValuesGlobal(TypeId enum_type) -> llvm::GlobalVariable*;

  // Get or create cached union storage info
  auto GetOrCreateUnionStorageInfo(TypeId union_type, CachedUnionInfo info)
      -> CachedUnionInfo;
  [[nodiscard]] auto GetCachedUnionStorageInfo(TypeId union_type) const
      -> const CachedUnionInfo*;

  // Type accessors from layout
  [[nodiscard]] auto GetHeaderType() const -> llvm::StructType*;
  [[nodiscard]] auto GetRuntimeInstanceType() const -> llvm::StructType*;
  [[nodiscard]] auto GetRuntimeInstanceStorageType() const -> llvm::StructType*;
  [[nodiscard]] auto GetDesignArenaSize() const -> uint64_t;
  [[nodiscard]] auto GetDesignSlotByteOffset(common::SlotId slot_id) const
      -> uint64_t;
  [[nodiscard]] auto GetDesignSlotStorageSpec(common::SlotId slot_id) const
      -> const SlotStorageSpec&;
  [[nodiscard]] auto GetDesignStorageSpecArena() const
      -> const StorageSpecArena&;
  [[nodiscard]] auto GetProcessFrameType() const -> llvm::StructType*;
  [[nodiscard]] auto GetProcessStateType() const -> llvm::StructType*;

  // Function scope management - must be called when entering/leaving a function
  // BeginFunction sets up the alloca insertion point at the entry block
  void BeginFunction(llvm::Function& func);
  void EndFunction();

  // Place storage management
  // Returns the alloca for a place root, creating it if necessary.
  // Allocas are always inserted in the entry block via alloca_builder_.
  // Key insight: storage is per-root, NOT per-PlaceId. Multiple PlaceIds with
  // the same root (but different projections) share the same storage.
  // NOTE: This is PURE allocation - no initialization. Call
  // InitializePlaceStorage explicitly at function entry for default-value
  // initialization.
  auto GetOrCreatePlaceStorage(const mir::PlaceRoot& root)
      -> Result<llvm::AllocaInst*>;

  // Initialize an allocated place with its default value.
  // Must be called at function entry, not at first-use.
  // Uses EmitSVDefaultInit which handles all types including unpacked
  // aggregates with 4-state fields.
  void InitializePlaceStorage(llvm::AllocaInst* alloca, TypeId type_id);

  // Set a place alias. When aliased, accesses to this place root use the
  // aliased pointer directly instead of allocated storage. Used for inout
  // parameters with managed types where we want reference semantics.
  void SetPlaceAlias(const mir::PlaceRoot& root, llvm::Value* ptr);

  [[nodiscard]] auto GetFrameFieldIndex(mir::PlaceId place_id) const
      -> uint32_t;

  // Per-process setup (set before generating each process function)
  void SetCurrentProcess(size_t process_index);
  [[nodiscard]] auto GetCurrentProcessIndex() const -> size_t;

  // Slot addressing mode -- controls how kModuleSlot roots are lowered.
  // Must be set explicitly per function scope.
  void SetSlotAddressingMode(SlotAddressingMode mode);
  [[nodiscard]] auto GetSlotAddressingMode() const -> SlotAddressingMode;

  // Module behavioral shared-body state.
  // Set by GenerateSharedProcessFunction, cleared on exit.
  void SetInstancePointer(llvm::Value* ptr);
  [[nodiscard]] auto GetInstancePointer() const -> llvm::Value*;
  void SetThisPointer(llvm::Value* ptr);
  [[nodiscard]] auto GetThisPointer() const -> llvm::Value*;
  void SetDynamicInstanceId(llvm::Value* id);
  [[nodiscard]] auto GetDynamicInstanceId() const -> llvm::Value*;
  void SetSpecSlotInfo(const SpecSlotInfo* info);
  [[nodiscard]] auto GetSpecSlotInfo() const -> const SpecSlotInfo* {
    return spec_slot_info_;
  }
  void SetConnectionNotificationMask(const ConnectionNotificationMask* mask) {
    connection_notification_mask_ = mask;
  }
  // External ref resolution environment. Installed as a unit by
  // SpecLocalScope; cleared on scope exit.
  // Contains only specialization-scoped data (recipes for type/access info).
  // Per-instance data is loaded at runtime from instance_ptr->ext_ref_slots.
  struct ExternalRefResolutionEnv {
    const std::vector<mir::ExternalAccessRecipe>* recipes = nullptr;
  };
  void SetExternalRefResolutionEnv(std::optional<ExternalRefResolutionEnv> env);

  // RAII guard for specialization-local scope state.
  // Owns spec_slot_info, connection_notification_mask, and external ref
  // bindings. Clears all three on destruction.
  class SpecLocalScope {
   public:
    SpecLocalScope(
        Context& ctx, const SpecSlotInfo* spec_slot_info,
        const ConnectionNotificationMask* notif_mask,
        std::optional<ExternalRefResolutionEnv> ext_ref_env)
        : ctx_(ctx) {
      ctx_.SetSpecSlotInfo(spec_slot_info);
      ctx_.SetConnectionNotificationMask(notif_mask);
      ctx_.SetExternalRefResolutionEnv(std::move(ext_ref_env));
    }

    ~SpecLocalScope() {
      ctx_.SetExternalRefResolutionEnv(std::nullopt);
      ctx_.SetConnectionNotificationMask(nullptr);
      ctx_.SetSpecSlotInfo(nullptr);
    }

    SpecLocalScope(const SpecLocalScope&) = delete;
    auto operator=(const SpecLocalScope&) -> SpecLocalScope& = delete;
    SpecLocalScope(SpecLocalScope&&) = delete;
    auto operator=(SpecLocalScope&&) -> SpecLocalScope& = delete;

   private:
    Context& ctx_;
  };

  // External-ref helpers. Recipes are specialization-scoped; actual slot
  // resolution uses per-instance data loaded from RuntimeInstance at runtime.

  // Resolved external-ref root: carries the design-global slot as a
  // runtime-loaded LLVM Value and the type as compile-time data.
  struct ResolvedExternalRefRoot {
    llvm::Value* global_slot_value = nullptr;
    TypeId type = {};
  };

  // Resolve external ref to runtime global_slot + type.
  // Emits LLVM IR that loads the design-global slot from the current
  // instance's ext_ref_slots table.
  [[nodiscard]] auto ResolveExternalRefRoot(mir::ExternalRefId ref_id)
      -> ResolvedExternalRefRoot;

  // Normalize an ExternalRefId to topology-resolved signal identity.
  // Returns a compile-time constant SignalRef for trigger/sensitivity
  // metadata. Per-instance external refs require per-instance trigger
  // resolution (not yet implemented); this function uses the global_slot
  // loaded from the current instance's ext_ref_slots table at construction
  // time, which is correct for single-instance bodies and representative-
  // correct for multi-instance bodies in the trigger/sensitivity path.
  //
  // TODO(hankhsu): Per-instance trigger/sensitivity resolution. The signal
  // identity returned here is a compile-time constant derived from the
  // per-instance table built at construction time. For multi-instance
  // bodies, each instance would need its own trigger entries. This is a
  // separate architectural change.
  [[nodiscard]] auto NormalizeExternalRefSignalIdentity(
      mir::ExternalRefId ref_id) const -> mir::SignalRef;

  // Get the type of an external ref from its recipe.
  [[nodiscard]] auto GetExternalRefType(mir::ExternalRefId ref_id) const
      -> TypeId;

  // Emit LLVM IR computing a pointer to the external ref's storage.
  // Loads the global slot from per-instance data, then resolves to pointer.
  [[nodiscard]] auto EmitExternalRefAddress(mir::ExternalRefId ref_id)
      -> llvm::Value*;

  // Load the value at an external ref's storage. Uses canonical 4-state
  // load for design-global slots when applicable.
  [[nodiscard]] auto LoadExternalRef(mir::ExternalRefId ref_id)
      -> Result<llvm::Value*>;

  // Compute the typed signal coordinate for an external ref's storage.
  // Returns a runtime-loaded GlobalRuntime signal coord from the per-instance
  // ext_ref_slots table.
  [[nodiscard]] auto EmitExternalRefSignalCoord(mir::ExternalRefId ref_id)
      -> SignalCoordExpr;

  // Get the target local slot for an external ref from the recipe.
  // Compile-time constant, used for cross-instance local trigger identity.
  [[nodiscard]] auto GetExternalRefTargetLocalSlot(
      mir::ExternalRefId ref_id) const -> uint32_t;

  // Load ext_ref_bindings pointer from RuntimeInstance via instance_ptr_.
  [[nodiscard]] auto EmitLoadExtRefBindingsPtr() -> llvm::Value*;

  // Get the LLVM struct type for ResolvedExtRefBinding: {i32, i32, i32}.
  [[nodiscard]] auto GetExtRefBindingType() -> llvm::StructType*;

  // Resolve a WriteTarget to a storage pointer.
  // PlaceId: delegates to GetPlacePointer.
  // ExternalRefId: delegates to EmitExternalRefAddress.
  [[nodiscard]] auto GetWriteDestPointer(const mir::WriteTarget& dest)
      -> Result<llvm::Value*>;

  [[nodiscard]] auto GetConnectionNotificationMask() const
      -> const ConnectionNotificationMask* {
    return connection_notification_mask_;
  }

  // Resolve the current body's BodyRealizationInfo from Layout.
  // Requires spec_slot_info_ to be set (module-body codegen context).
  // Throws InternalError if spec_slot_info_ is null or body_info is null.
  [[nodiscard]] auto GetCurrentBodyRealizationInfo() const
      -> const Layout::BodyRealizationInfo&;

  // Explicit access APIs for module-local slot pointer formation.
  // Callers must choose the correct API based on the slot's storage shape.

  // Inline value slot: returns pointer to the slot's value bytes.
  // this_ptr + inline_offset. For kInlineValue slots only.
  [[nodiscard]] auto EmitInlineSlotPtr(uint32_t local_slot_id) -> llvm::Value*;

  // Owned handle pointer: returns pointer to the OwnedStorageHandle struct.
  // this_ptr + inline_offset. For kOwnedContainer slots only.
  [[nodiscard]] auto EmitOwnedHandlePtr(uint32_t local_slot_id) -> llvm::Value*;

  // Owned data pointer: loads handle.data from the inline handle.
  // Returns the arena-internal backing-data base pointer.
  [[nodiscard]] auto EmitLoadOwnedDataPtr(llvm::Value* handle_ptr)
      -> llvm::Value*;

  // Transitional dispatch: routes to inline or owned path based on shape.
  // Returns pointer to value bytes (inline) or backing data (owned).
  // Existing callers use this; new code should call explicit APIs above.
  // Will be removed when all callers are migrated.
  [[nodiscard]] auto GetModuleSlotPointer(uint32_t local_slot_id)
      -> llvm::Value*;

  // Centralized LLVM type for OwnedStorageHandle: { ptr, i64 }.
  [[nodiscard]] auto GetOwnedHandleLlvmType() -> llvm::StructType*;

  // Design-global storage: design_ptr + struct GEP via field index.
  [[nodiscard]] auto GetDesignGlobalSlotPointer(uint32_t global_slot_id)
      -> llvm::Value*;

  // Design-global storage with runtime-loaded slot ID.
  // Used by external-ref resolution where the slot varies per instance.
  [[nodiscard]] auto GetDesignGlobalSlotPointer(llvm::Value* global_slot_id)
      -> llvm::Value*;

  // Central dispatch: resolve a design-storage root (kModuleSlot or
  // kDesignGlobal) to its LLVM pointer, using slot_addressing_ to decide the
  // code path. This is the single source of truth for slot root -> pointer.
  [[nodiscard]] auto GetSlotRootPointer(const mir::PlaceRoot& root)
      -> llvm::Value*;

  // Central dispatch for signal-root pointer formation.
  // Converts SignalRef scope to the same addressing-mode dispatch as
  // GetSlotRootPointer. This is the single source of truth for
  // signal root -> pointer (used by trigger/container lowering).
  [[nodiscard]] auto GetSignalSlotPointer(const mir::SignalRef& sig)
      -> llvm::Value*;

  // Emit semantic signal coordinate for a signal ref.
  // kModuleLocal: Local(sig.id) -- body-local slot ordinal.
  // kDesignGlobal: Global(sig.id) -- design-global slot id.
  // kModuleLocal in kDesignGlobal addressing: InternalError.
  [[nodiscard]] auto EmitSignalCoord(const mir::SignalRef& sig)
      -> SignalCoordExpr;

  // Emit runtime signal ID resolved to the storage owner.
  // Use ONLY for dirty-mark / mutation-target formation paths.
  // Do NOT use for subscription/trigger registration (use EmitSignalCoord).
  [[nodiscard]] auto EmitMutationTargetSignalCoord(const mir::SignalRef& sig)
      -> SignalCoordExpr;

  // Resolve the canonical mutation-target SignalRef for a place's root.
  // Returns the pre-emission SignalRef (scope + id) without emitting IR.
  // Returns nullopt if the root has no mutation-target signal identity
  // (e.g., local/temp roots that are not design storage).
  [[nodiscard]] auto ResolveMutationSignalRef(mir::PlaceId place_id) const
      -> std::optional<mir::SignalRef>;

  // Body-owned behavioral dirty-propagation query. Returns true iff
  // body-local behavioral wait triggers reference this slot.
  // For kModuleLocal: reads BodyRealizationInfo.slot_has_behavioral_trigger.
  // For kDesignGlobal: reads layout design_behavioral_trigger bitmap.
  // Does NOT include connection-trigger or runtime notification facts.
  [[nodiscard]] auto RequiresBehavioralDirtyPropagation(
      const mir::SignalRef& sig) const -> bool;

  // Topology-derived connection notification query. Returns true iff
  // any instance of the current body has a connection process that
  // triggers on this slot. Conservative union across all instances.
  // For kModuleLocal: reads ConnectionNotificationMask from context.
  // For kDesignGlobal: reads layout slot_has_connection_trigger bitmap.
  // This is a codegen compilation decision, not a body semantic fact.
  [[nodiscard]] auto RequiresConnectionNotification(
      const mir::SignalRef& sig) const -> bool;

  // Combined static dirty-propagation query. Returns true iff either
  // behavioral propagation or connection notification is required.
  // Convenience wrapper for callers that need one boolean.
  [[nodiscard]] auto RequiresStaticDirtyPropagation(
      const mir::SignalRef& sig) const -> bool;

  // Legacy runtime-interop: resolve a signal reference to a design-global
  // slot index for runtime APIs that still use flat slot identity (trace
  // observation, packed store notifications). For module-local signals,
  // maps through ResolveLegacyRepresentativeDesignSlot. For design-global
  // signals, returns the id directly.
  // Must NOT be used for spec compilation decisions -- only for runtime
  // signal identity at the codegen->runtime boundary.
  [[nodiscard]] auto GetLegacyRuntimeSignalSlot(const mir::SignalRef& sig) const
      -> uint32_t;

  // Emit IR that queries whether the canonical storage-owner slot for
  // the given signal has trace observation at runtime. Returns an i1
  // LLVM value (true if trace-observed, false if safe to suppress).
  // Emits a call to the LyraIsTraceObserved runtime ABI function.
  // Null engine returns false (safe for guarded paths).
  [[nodiscard]] auto EmitIsTraceObserved(const mir::SignalRef& sig)
      -> llvm::Value*;

  // Emit IR trace query by pre-resolved canonical owner slot.
  // Used by packed-store paths where the owner slot was already
  // resolved at policy construction time.
  [[nodiscard]] auto EmitIsTraceObservedOwnerSlot(uint32_t owner_slot)
      -> llvm::Value*;

  // Emit a trace-observation branch: if trace-observed, execute the
  // yes_emitter; otherwise execute the no_emitter. Both paths merge
  // at a shared done block. Builder is left at the done block.
  // yes_name/no_name are used for BasicBlock naming.
  template <typename YesEmitter, typename NoEmitter>
  void EmitTraceBranch(
      const mir::SignalRef& sig, llvm::StringRef yes_name,
      llvm::StringRef no_name, YesEmitter yes_emitter, NoEmitter no_emitter) {
    auto* trace_observed = EmitIsTraceObserved(sig);
    auto& builder = GetBuilder();
    auto& llvm_ctx = GetLlvmContext();
    auto* fn = builder.GetInsertBlock()->getParent();
    auto* yes_bb = llvm::BasicBlock::Create(llvm_ctx, yes_name, fn);
    auto* no_bb = llvm::BasicBlock::Create(llvm_ctx, no_name, fn);
    auto done_name = (yes_name + ".done").str();
    auto* done_bb = llvm::BasicBlock::Create(llvm_ctx, done_name, fn);
    builder.CreateCondBr(trace_observed, yes_bb, no_bb);

    builder.SetInsertPoint(yes_bb);
    yes_emitter();
    if (builder.GetInsertBlock()->getTerminator() == nullptr) {
      builder.CreateBr(done_bb);
    }

    builder.SetInsertPoint(no_bb);
    no_emitter();
    if (builder.GetInsertBlock()->getTerminator() == nullptr) {
      builder.CreateBr(done_bb);
    }

    builder.SetInsertPoint(done_bb);
  }

  // Resolution: MIR storage root -> design-global slot ID.
  // kDesignGlobal roots: identity (root.id is already design-global).
  // kModuleSlot roots: InternalError (module-local roots have no
  //   design-global identity in design-global addressing mode).
  [[nodiscard]] static auto ResolveDesignGlobalSlotId(
      const mir::PlaceRoot& root) -> common::SlotId;
  [[nodiscard]] static auto ResolveDesignGlobalSlotId(const mir::SignalRef& sig)
      -> common::SlotId;
  [[nodiscard]] static auto ResolveDesignGlobalSlotId(
      const mir::ScopedSlotRef& ref) -> common::SlotId;

  // Resolve aliases + get storage pointer for a place root.
  // Returns root slot pointer for kModuleSlot or kDesignGlobal roots.
  // Used by NBA path for notify_base_ptr.
  [[nodiscard]] auto GetStorageRootPointer(mir::PlaceId place_id)
      -> llvm::Value*;

  // Save and restore execution-contract state. Used by ExecutionContractScope.
  [[nodiscard]] auto SaveExecutionContractState() -> ExecutionContractState;
  void RestoreExecutionContractState(const ExecutionContractState& state);

  // Emit GEPs and loads to extract design_ptr, engine_ptr, and frame_ptr from
  // the process state argument, then cache them in the context via the setters
  // below. Must be called once per process function entry block.
  void EmitProcessStateSetup(llvm::Value* state_arg);

  // Load realized instance binding from the frame header for shared bodies.
  // Loads instance pointer, then derives storage base and instance_id
  // from the RuntimeInstance object.
  // Called after EmitProcessStateSetup by shared body generation only.
  void EmitSharedBodyBindingSetup(llvm::Value* state_arg);

  // Canonical typed header-field accessors. All typed process-header field
  // access in llvm backend code must go through these methods. Callers never
  // pass raw field indices or choose LLVM result types for header fields.
  auto EmitLoadEnginePtr(llvm::Value* state_arg) -> llvm::Value*;
  auto EmitLoadDesignPtr(llvm::Value* state_arg) -> llvm::Value*;
  auto EmitLoadDecisionOwnerId(llvm::Value* state_arg) -> llvm::Value*;
  auto EmitLoadInstancePtr(llvm::Value* state_arg) -> llvm::Value*;
  auto EmitLoadInstanceInlineBase(llvm::Value* instance_ptr) -> llvm::Value*;
  auto EmitLoadInstanceId(llvm::Value* instance_ptr) -> llvm::Value*;
  void EmitStoreDesignPtr(llvm::Value* state_arg, llvm::Value* value);
  auto EmitOutcomePtr(llvm::Value* state_arg) -> llvm::Value*;

  // Cached pointers (computed in entry block, reused for all place accesses)
  // state_ptr: the function argument pointing to ProcessStateN
  void SetStatePointer(llvm::Value* state_ptr);
  [[nodiscard]] auto GetStatePointer() -> llvm::Value*;

  // design_ptr: loaded from state->header.design, points to shared DesignState
  void SetDesignPointer(llvm::Value* design_ptr);
  [[nodiscard]] auto GetDesignPointer() -> llvm::Value*;

  // frame_ptr: GEP to state->frame, points to this process's ProcessFrameN
  void SetFramePointer(llvm::Value* frame_ptr);
  [[nodiscard]] auto GetFramePointer() -> llvm::Value*;

  // engine_ptr: loaded from state->header.engine, points to shared Engine
  void SetEnginePointer(llvm::Value* engine_ptr);
  [[nodiscard]] auto GetEnginePointer() -> llvm::Value*;

  // decision_owner_id: loaded from state->header.process_id, i32 identity.
  // The process frame field stays process-named; the codegen layer interprets
  // it as the decision owner for the process-backed owner path.
  void SetCurrentDecisionOwnerId(llvm::Value* decision_owner_id);
  [[nodiscard]] auto GetCurrentDecisionOwnerId() -> llvm::Value*;

  // Design-slot store mode for the current process body.
  // Controls whether stores emit compare+dirty-mark (kNotify) or plain
  // writes (kDirect). Set from the ProcessExecutionKind contract.
  void SetDesignStoreMode(DesignStoreMode mode);
  [[nodiscard]] auto GetDesignStoreMode() const -> DesignStoreMode;

  // Controls when dirty-mark notification fires. Orthogonal to
  // DesignStoreMode. Set per-block during codegen for qualifying
  // non-yielding loops. kDeferred means the store commits immediately
  // but the dirty-mark is emitted at the loop-exit edge.
  void SetNotificationPolicy(NotificationPolicy policy);
  [[nodiscard]] auto GetNotificationPolicy() const -> NotificationPolicy;

  // Get pointer to a place's base storage via GEP into design or frame.
  // Applies all non-BitRange projections (IndexProjection, etc.) and stops
  // at the first BitRangeProjection. For bitrange reads/writes, use
  // ComposeBitRange() to get the composed offset within this base.
  // Note: Automatically resolves aliases for output/inout ports.
  [[nodiscard]] auto GetPlacePointer(mir::PlaceId place_id)
      -> Result<llvm::Value*>;

  // Get the LLVM type for a place's storage
  [[nodiscard]] auto GetPlaceLlvmType(mir::PlaceId place_id)
      -> Result<llvm::Type*>;

  // BitRangeProjection helpers
  [[nodiscard]] auto HasBitRangeProjection(mir::PlaceId place_id) const -> bool;
  [[nodiscard]] auto GetBitRangeProjection(mir::PlaceId place_id) const
      -> const mir::BitRangeProjection&;
  // LLVM type of the base value that GetPlacePointer() points to.
  // Traverses non-BitRange projections only (same boundary as GetPlacePointer).
  [[nodiscard]] auto GetPlaceBaseType(mir::PlaceId place_id)
      -> Result<llvm::Type*>;

  // Load a place's value (for rvalue evaluation in compute/).
  // This is the read-only API for place access - use this instead of
  // GetPlacePointer() in compute/ to enforce the rvalue purity boundary.
  [[nodiscard]] auto LoadPlaceValue(mir::PlaceId place_id)
      -> Result<llvm::Value*>;

  // Load a place's base value before BitRange projection (for bit
  // manipulation). Returns the full base word that contains the bit range.
  [[nodiscard]] auto LoadPlaceBaseValue(mir::PlaceId place_id)
      -> Result<llvm::Value*>;

 private:
  // Shared 4-state canonical load. Returns the loaded value if the type
  // is 4-state packed, nullopt otherwise (caller falls through to typed load).
  auto TryLoadCanonicalFourStateValue(llvm::Value* ptr, const Type& type)
      -> std::optional<llvm::Value*>;

 public:
  struct ComposedBitRange {
    llvm::Value* offset;
    uint32_t width;
  };
  // Compose all chained BitRangeProjections into a single offset+width.
  // Sums all bitrange offsets (emitting LLVM add instructions) and returns
  // the last projection's width. Validates the contiguous-suffix invariant.
  [[nodiscard]] auto ComposeBitRange(mir::PlaceId place_id)
      -> Result<ComposedBitRange>;

  // Get the 4-state struct type for a given semantic bit width
  [[nodiscard]] auto GetPlaceLlvmType4State(uint32_t bit_width)
      -> llvm::StructType*;

  auto TakeOwnership() -> std::pair<
      std::unique_ptr<llvm::LLVMContext>, std::unique_ptr<llvm::Module>>;

  // Origin tracking for error reporting
  void SetCurrentOrigin(common::OriginId origin) {
    current_origin_ = origin;
  }
  [[nodiscard]] auto GetCurrentOrigin() const -> common::OriginId {
    return current_origin_;
  }

  // Access diagnostic context for error reporting.
  [[nodiscard]] auto GetDiagnosticContext() const
      -> const lowering::DiagnosticContext& {
    return *diag_ctx_;
  }

  // Access source manager for origin resolution.
  [[nodiscard]] auto GetSourceManager() const -> const SourceManager* {
    return source_manager_;
  }

  // Register an owned string temp that needs release at end of statement
  void RegisterOwnedTemp(llvm::Value* handle);

  // Clear owned temps (called by StatementScope constructor)
  void ClearOwnedTemps();

  // Release all registered owned temps (called by StatementScope destructor)
  void ReleaseOwnedTemps();

  // Session-scoped declared-function lookup. Installed by
  // DeclaredFunctionScope for the duration of a declare/define pass
  // (Phase 3 or per-body session). Not persistent across sessions.
  [[nodiscard]] auto GetDeclaredFunction(mir::FunctionId func_id) const
      -> llvm::Function*;

  // RAII guard for declared-function lookup scope.
  // The caller owns the map; this scope installs a non-owning pointer.
  class DeclaredFunctionScope {
   public:
    DeclaredFunctionScope(
        Context& ctx,
        const absl::flat_hash_map<mir::FunctionId, llvm::Function*>& funcs);
    ~DeclaredFunctionScope();
    DeclaredFunctionScope(const DeclaredFunctionScope&) = delete;
    auto operator=(const DeclaredFunctionScope&)
        -> DeclaredFunctionScope& = delete;
    DeclaredFunctionScope(DeclaredFunctionScope&&) = delete;
    auto operator=(DeclaredFunctionScope&&) -> DeclaredFunctionScope& = delete;

   private:
    Context& ctx_;
  };

  // Design-global function registry (keyed by SymbolId).
  // Used for DesignFunctionRef resolution in body MIR.
  // Stores the design-arena FunctionId alongside the llvm::Function* so
  // callers can use FunctionId-keyed metadata (sret, module-scoped, etc.).
  struct DesignFunctionEntry {
    mir::FunctionId func_id;
    llvm::Function* llvm_func = nullptr;
  };
  void RegisterDesignFunction(
      SymbolId symbol, mir::FunctionId func_id, llvm::Function* llvm_func);
  [[nodiscard]] auto GetDesignFunction(SymbolId symbol) const
      -> const DesignFunctionEntry&;

  // Deferred assertion site info (borrowed pointer into design sites).
  // Build LLVM function type from MIR function signature.
  // Package-scoped: (DesignState*, Engine*, args...)
  // Module-scoped:  (DesignState*, Engine*, this_ptr*,
  //                  instance_ptr*, instance_id_i32, args...)
  // For managed returns: out_ptr* prepended, return type becomes void.
  [[nodiscard]] auto BuildUserFunctionType(
      const mir::FunctionSignature& sig, bool is_module_scoped,
      bool accepts_decision_owner) -> Result<llvm::FunctionType*>;

  // Check if a function uses out-param calling convention (managed return).
  [[nodiscard]] auto FunctionUsesSret(mir::FunctionId func_id) const -> bool;

  // Iteration limit site tracking for back-edge guard emission.
  // Returns the assigned back-edge site id.
  auto RegisterBackEdgeSite(common::OriginId origin) -> uint32_t;
  [[nodiscard]] auto GetBackEdgeSiteOrigins() const
      -> const std::vector<common::OriginId>& {
    return back_edge_site_origins_;
  }

  // Wait-site ID allocation for persistent wait installation.
  // Returns the next sequential wait-site ID. Process codegen uses this
  // to assign IDs; the entries themselves are returned as process-level
  // output (not stored on Context).
  auto NextWaitSiteId() -> uint32_t {
    return next_wait_site_id_++;
  }

  // SSA temp management (TempValue-based explicit semantic contract).
  // TempValue is defined in compute/temp_value.hpp.

  // Bind an SSA temp value. Must be called exactly once per temp_id.
  void BindTempValue(int temp_id, const TempValue& tv);
  // Read a bound SSA temp value.
  [[nodiscard]] auto ReadTempValue(int temp_id) const -> const TempValue&;
  // Check if a temp_id is bound.
  [[nodiscard]] auto HasTemp(int temp_id) const -> bool;
  // Convenience: returns declared_type from TempValue.
  [[nodiscard]] auto GetTempType(int temp_id) const -> TypeId;
  // Safe constant inspection: returns ConstantInt* only for kTwoState temps
  // with a scalar constant payload. Rejects kFourState unconditionally,
  // even if both planes are constants and the unknown plane is zero.
  [[nodiscard]] auto TryGetTempConstantInt(int temp_id) const
      -> llvm::ConstantInt*;
  // Clear all temp bindings (called at function start).
  void ClearTemps();

 private:
  // Internal helper: resolve SlotId to position index in design layout.
  [[nodiscard]] auto ResolveDesignSlotIndex(common::SlotId slot_id) const
      -> uint32_t;

  // Commit-module-only methods (accessed via friend class commit::Access)
  // Get unified write target (pointer + signal_id) from a place or
  // external ref. All fields derived from the same resolved root.
  [[nodiscard]] auto GetWriteTarget(mir::PlaceId place_id)
      -> Result<WriteTarget>;
  [[nodiscard]] auto GetWriteTarget(mir::ExternalRefId ref_id)
      -> Result<WriteTarget>;

  // Get the mutation-target signal_id for a place's root.
  // Resolves forwarded aliases to the storage owner for dirty-mark identity.
  // Returns nullopt if the root has no notifiable mutation-target signal
  // identity (e.g., local/temp roots that are not design storage).
  [[nodiscard]] auto GetMutationTargetSignalCoord(mir::PlaceId place_id)
      -> std::optional<SignalCoordExpr>;

  // Internal helper: compute pointer from an already-resolved place.
  // The original_place_id is needed for frame field index lookup (for
  // local/temp).
  [[nodiscard]] auto ComputePlacePointer(
      const mir::Place& resolved, mir::PlaceId original_place_id)
      -> Result<llvm::Value*>;

  const mir::Arena* arena_;
  const mir::Arena* design_arena_ = nullptr;
  const TypeArena& types_;
  const Layout& layout_;
  bool force_two_state_ = false;

  std::unique_ptr<llvm::LLVMContext> llvm_context_;
  std::unique_ptr<llvm::Module> llvm_module_;
  llvm::IRBuilder<> builder_;

  // Per-function state for alloca insertion
  llvm::Function* current_function_ = nullptr;
  std::unique_ptr<llvm::IRBuilder<>> alloca_builder_;

  llvm::Function* lyra_print_literal_ = nullptr;
  llvm::Function* lyra_warn_rate_limited_ = nullptr;
  llvm::Function* lyra_emit_report_ = nullptr;
  llvm::Function* lyra_record_decision_observation_ = nullptr;
  llvm::Function* lyra_print_value_ = nullptr;
  llvm::Function* lyra_print_string_ = nullptr;
  llvm::Function* lyra_print_end_ = nullptr;
  llvm::StructType* format_spec_type_ = nullptr;
  llvm::Function* lyra_register_var_ = nullptr;
  llvm::Function* lyra_snapshot_vars_ = nullptr;
  llvm::Function* lyra_string_from_literal_ = nullptr;
  llvm::Function* lyra_string_from_cstr_ = nullptr;
  llvm::Function* lyra_string_cmp_ = nullptr;
  llvm::Function* lyra_string_retain_ = nullptr;
  llvm::Function* lyra_string_release_ = nullptr;
  llvm::Function* lyra_string_concat_ = nullptr;
  llvm::Function* lyra_string_from_packed_ = nullptr;
  llvm::Function* lyra_string_get_view_ = nullptr;
  llvm::Function* lyra_string_get_cstr_ = nullptr;
  llvm::Function* lyra_packed_from_string_ = nullptr;
  llvm::Function* lyra_run_simulation_ = nullptr;
  llvm::Function* lyra_construct_process_states_ = nullptr;
  llvm::Function* lyra_destroy_process_states_ = nullptr;
  llvm::Function* lyra_run_process_sync_ = nullptr;
  llvm::Function* lyra_plusargs_test_ = nullptr;
  llvm::Function* lyra_plusargs_value_int_ = nullptr;
  llvm::Function* lyra_plusargs_value_string_ = nullptr;
  llvm::Function* lyra_suspend_delay_ = nullptr;
  llvm::Function* lyra_suspend_wait_ = nullptr;
  llvm::Function* lyra_suspend_wait_static_ = nullptr;
  llvm::Function* lyra_suspend_wait_with_late_bound_ = nullptr;
  llvm::Function* lyra_suspend_repeat_ = nullptr;
  llvm::Function* lyra_alloc_triggers_ = nullptr;
  llvm::Function* lyra_free_triggers_ = nullptr;
  llvm::Function* lyra_suspend_wait_event_ = nullptr;
  llvm::Function* lyra_trigger_event_ = nullptr;
  llvm::Function* lyra_resolve_slot_ptr_ = nullptr;
  llvm::Function* lyra_resolve_instance_ptr_ = nullptr;
  // R3 typed coordination helpers.
  llvm::StructType* ext_ref_binding_type_ = nullptr;
  llvm::Function* lyra_mark_dirty_local_ = nullptr;
  llvm::Function* lyra_mark_dirty_global_ = nullptr;
  llvm::Function* lyra_mark_dirty_ext_ref_ = nullptr;
  llvm::Function* lyra_store_packed_local_ = nullptr;
  llvm::Function* lyra_store_packed_global_ = nullptr;
  llvm::Function* lyra_store_string_local_ = nullptr;
  llvm::Function* lyra_store_string_global_ = nullptr;
  llvm::Function* lyra_schedule_nba_local_ = nullptr;
  llvm::Function* lyra_schedule_nba_global_ = nullptr;
  llvm::Function* lyra_schedule_nba_canonical_packed_local_ = nullptr;
  llvm::Function* lyra_schedule_nba_canonical_packed_global_ = nullptr;
  llvm::Function* lyra_schedule_nba_ext_ref_ = nullptr;
  llvm::Function* lyra_schedule_nba_canonical_packed_ext_ref_ = nullptr;
  llvm::Function* lyra_is_trace_observed_local_ = nullptr;
  llvm::Function* lyra_is_trace_observed_global_ = nullptr;
  llvm::Function* lyra_notify_container_mutation_local_ = nullptr;
  llvm::Function* lyra_notify_container_mutation_global_ = nullptr;
  llvm::Function* lyra_notify_signal_local_ = nullptr;
  llvm::Function* lyra_notify_signal_global_ = nullptr;
  llvm::Function* lyra_terminate_ = nullptr;
  llvm::Function* lyra_get_time_ = nullptr;
  llvm::Function* lyra_init_runtime_ = nullptr;
  llvm::Function* lyra_resolve_base_dir_ = nullptr;
  llvm::Function* lyra_report_time_ = nullptr;
  llvm::Function* lyra_iteration_limit_ptr_ = nullptr;
  llvm::Function* lyra_dynarray_new_ = nullptr;
  llvm::Function* lyra_dynarray_new_copy_ = nullptr;
  llvm::Function* lyra_dynarray_size_ = nullptr;
  llvm::Function* lyra_dynarray_element_ptr_ = nullptr;
  llvm::Function* lyra_dynarray_clone_ = nullptr;
  llvm::Function* lyra_dynarray_delete_ = nullptr;
  llvm::Function* lyra_dynarray_release_ = nullptr;
  llvm::Function* lyra_store_dynarray_local_ = nullptr;
  llvm::Function* lyra_store_dynarray_global_ = nullptr;
  llvm::Function* lyra_dynarray_clone_elem_ = nullptr;
  llvm::Function* lyra_dynarray_destroy_elem_ = nullptr;
  llvm::Function* lyra_queue_push_back_ = nullptr;
  llvm::Function* lyra_queue_push_front_ = nullptr;
  llvm::Function* lyra_queue_pop_back_ = nullptr;
  llvm::Function* lyra_queue_pop_front_ = nullptr;
  llvm::Function* lyra_queue_insert_ = nullptr;
  llvm::Function* lyra_queue_delete_at_ = nullptr;
  llvm::Function* lyra_string_format_start_ = nullptr;
  llvm::Function* lyra_string_format_literal_ = nullptr;
  llvm::Function* lyra_string_format_value_ = nullptr;
  llvm::Function* lyra_string_format_string_ = nullptr;
  llvm::Function* lyra_string_format_finish_ = nullptr;
  llvm::Function* lyra_string_format_runtime_ = nullptr;
  llvm::Function* lyra_set_timeformat_ = nullptr;
  llvm::Function* lyra_fopen_fd_ = nullptr;
  llvm::Function* lyra_fopen_mcd_ = nullptr;
  llvm::Function* lyra_fclose_ = nullptr;
  llvm::Function* lyra_fflush_ = nullptr;
  llvm::Function* lyra_fwrite_ = nullptr;
  llvm::Function* lyra_register_strobe_ = nullptr;
  llvm::Function* lyra_monitor_set_enabled_ = nullptr;
  llvm::Function* lyra_monitor_register_ = nullptr;
  llvm::Function* lyra_readmem_local_ = nullptr;
  llvm::Function* lyra_readmem_global_ = nullptr;
  llvm::Function* lyra_readmem_no_notify_ = nullptr;
  llvm::Function* lyra_writemem_ = nullptr;
  llvm::Function* lyra_print_module_path_ = nullptr;
  llvm::Function* lyra_fill_packed_elements_ = nullptr;
  llvm::Function* lyra_random_ = nullptr;
  llvm::Function* lyra_urandom_ = nullptr;
  llvm::Function* lyra_fgetc_ = nullptr;
  llvm::Function* lyra_ungetc_ = nullptr;
  llvm::Function* lyra_fgets_ = nullptr;
  llvm::Function* lyra_fread_local_ = nullptr;
  llvm::Function* lyra_fread_global_ = nullptr;
  llvm::Function* lyra_fread_no_notify_ = nullptr;
  llvm::Function* lyra_fscanf_ = nullptr;
  llvm::Function* lyra_assoc_new_ = nullptr;
  llvm::Function* lyra_assoc_release_ = nullptr;
  llvm::Function* lyra_assoc_clone_ = nullptr;
  llvm::Function* lyra_assoc_get_ = nullptr;
  llvm::Function* lyra_assoc_set_ = nullptr;
  llvm::Function* lyra_assoc_exists_ = nullptr;
  llvm::Function* lyra_assoc_delete_key_ = nullptr;
  llvm::Function* lyra_assoc_delete_all_ = nullptr;
  llvm::Function* lyra_assoc_size_ = nullptr;
  llvm::Function* lyra_assoc_first_ = nullptr;
  llvm::Function* lyra_assoc_last_ = nullptr;
  llvm::Function* lyra_assoc_next_ = nullptr;
  llvm::Function* lyra_assoc_prev_ = nullptr;
  llvm::Function* lyra_assoc_snapshot_create_ = nullptr;
  llvm::Function* lyra_assoc_snapshot_size_ = nullptr;
  llvm::Function* lyra_assoc_snapshot_key_at_ = nullptr;
  llvm::Function* lyra_assoc_snapshot_release_ = nullptr;
  llvm::Function* lyra_assoc_clone_elem_ = nullptr;
  llvm::Function* lyra_assoc_destroy_elem_ = nullptr;
  llvm::Function* lyra_system_cmd_ = nullptr;
  llvm::Function* lyra_get_dpi_export_call_context_ = nullptr;
  llvm::Function* lyra_fail_missing_dpi_export_call_context_ = nullptr;
  llvm::Function* lyra_push_current_dpi_scope_ = nullptr;
  llvm::Function* lyra_pop_current_dpi_scope_ = nullptr;
  llvm::Function* lyra_resolve_package_export_binding_ = nullptr;
  llvm::Function* lyra_resolve_module_instance_binding_ = nullptr;
  llvm::Function* lyra_push_dpi_export_call_context_ = nullptr;
  llvm::Function* lyra_pop_dpi_export_call_context_ = nullptr;
  llvm::Function* lyra_report_missing_decision_owner_fatal_ = nullptr;

  // Maps PlaceRootKey to its LLVM alloca storage.
  // Storage is per-root, NOT per-PlaceId. Multiple PlaceIds with the same root
  // (but different projections) share the same storage.
  std::unordered_map<PlaceRootKey, llvm::AllocaInst*, PlaceRootKeyHash>
      place_storage_;

  // Maps PlaceRootKey to an aliased pointer (for inout params with managed
  // types). Checked before place_storage_. When set, accesses to this place
  // root use the aliased pointer directly instead of allocated storage.
  std::unordered_map<PlaceRootKey, llvm::Value*, PlaceRootKeyHash> place_alias_;

  // Cached enum member values globals (per enum TypeId)
  absl::flat_hash_map<TypeId, llvm::GlobalVariable*> enum_values_globals_;

  // Cached union storage info (per union TypeId)
  absl::flat_hash_map<TypeId, CachedUnionInfo> union_storage_cache_;

  // Current process index (set before generating each process)
  size_t current_process_index_ = 0;

  // Cached pointers for current process function
  llvm::Value* state_ptr_ = nullptr;
  llvm::Value* design_ptr_ = nullptr;
  llvm::Value* frame_ptr_ = nullptr;
  llvm::Value* engine_ptr_ = nullptr;
  llvm::Value* current_decision_owner_id_ = nullptr;
  DesignStoreMode design_store_mode_ = DesignStoreMode::kNotifySimulation;
  NotificationPolicy notification_policy_ = NotificationPolicy::kImmediate;

  // Per-function shared-body state. Saved/restored by ExecutionContractScope.
  SlotAddressingMode slot_addressing_ = SlotAddressingMode::kDesignGlobal;
  llvm::Value* instance_ptr_ = nullptr;
  llvm::Value* this_ptr_ = nullptr;
  llvm::Value* dynamic_instance_id_ = nullptr;
  const SpecSlotInfo* spec_slot_info_ = nullptr;
  const ConnectionNotificationMask* connection_notification_mask_ = nullptr;
  // External ref resolution state. Env carries bindings + construction
  // for ResolveExternalRefRoot.
  std::optional<ExternalRefResolutionEnv> ext_ref_env_;

  // Current origin for error reporting
  common::OriginId current_origin_ = common::OriginId::Invalid();

  // Diagnostic context for error reporting (resolves OriginId -> SourceSpan)
  const lowering::DiagnosticContext* diag_ctx_ = nullptr;

  // Source manager for resolving SourceSpan -> file/line/col
  const SourceManager* source_manager_ = nullptr;

  // Owned string temps that need release at end of current statement
  std::vector<llvm::Value*> owned_temps_;

  // Session-scoped declared-function lookup (non-owning pointer).
  // Installed by DeclaredFunctionScope; null between sessions.
  const absl::flat_hash_map<mir::FunctionId, llvm::Function*>*
      declared_functions_ = nullptr;

  // Design-global function registry: SymbolId -> llvm::Function*
  // For DesignFunctionRef resolution in body MIR.
  struct SymbolIdHash {
    auto operator()(SymbolId id) const noexcept -> size_t {
      return std::hash<uint32_t>{}(id.value);
    }
  };
  absl::flat_hash_map<SymbolId, DesignFunctionEntry, SymbolIdHash>
      design_functions_;

  // SSA temp bindings: temp_id -> TempValue (explicit semantic contract).
  // Temps defined by block params (split PHI nodes) or statements.
  absl::flat_hash_map<int, TempValue> temp_entries_;

  // Iteration limit site origins accumulated during process codegen.
  // Index = back-edge site id, value = origin of the back-edge terminator.
  std::vector<common::OriginId> back_edge_site_origins_;

  // Wait-site ID counter. Incremented by NextWaitSiteId().
  uint32_t next_wait_site_id_ = 0;

  // Lazy-initialized runtime function for cover hit recording.
  llvm::Function* lyra_record_immediate_cover_hit_ = nullptr;
  // Lazy-initialized runtime function for deferred assertion enqueue.
  llvm::Function* lyra_enqueue_observed_deferred_assertion_ = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
