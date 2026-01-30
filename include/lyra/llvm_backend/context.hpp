#pragma once

#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context_scope.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Forward declaration for friend access
namespace commit {
class Access;
}  // namespace commit

// Forward declaration for WriteTarget (defined in commit/access.hpp)
struct WriteTarget;

// Shared context for MIR → LLVM lowering
class Context {
  friend class commit::Access;

 public:
  Context(
      const mir::Design& design, const mir::Arena& arena,
      const TypeArena& types, const Layout& layout,
      std::unique_ptr<llvm::LLVMContext> llvm_ctx,
      std::unique_ptr<llvm::Module> module,
      const lowering::DiagnosticContext* diag_ctx);

  [[nodiscard]] auto GetLlvmContext() -> llvm::LLVMContext& {
    return *llvm_context_;
  }
  [[nodiscard]] auto GetModule() -> llvm::Module& {
    return *llvm_module_;
  }
  [[nodiscard]] auto GetBuilder() -> llvm::IRBuilder<>& {
    return builder_;
  }

  [[nodiscard]] auto GetMirDesign() const -> const mir::Design& {
    return design_;
  }
  [[nodiscard]] auto GetMirArena() const -> const mir::Arena& {
    return arena_;
  }
  [[nodiscard]] auto GetTypeArena() const -> const TypeArena& {
    return types_;
  }
  [[nodiscard]] auto GetLayout() const -> const Layout& {
    return layout_;
  }

  [[nodiscard]] auto GetLyraPrintLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintValue() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintEnd() -> llvm::Function*;
  [[nodiscard]] auto GetFormatSpecType() -> llvm::StructType*;
  [[nodiscard]] auto GetLyraRegisterVar() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSnapshotVars() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringCmp() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRetain() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringConcat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunSimulation() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunProcessSync() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPlusargsTest() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPlusargsValueInt() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPlusargsValueString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendDelay() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendWait() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendRepeat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraAllocTriggers() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFreeTriggers() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStorePacked() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNba() -> llvm::Function*;
  [[nodiscard]] auto GetLyraTerminate() -> llvm::Function*;
  [[nodiscard]] auto GetLyraGetTime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraInitRuntime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReportTime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayNew() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayNewCopy() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArraySize() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayElementPtr() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayClone() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayDelete() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDynArrayRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreDynArray() -> llvm::Function*;
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
  [[nodiscard]] auto GetLyraSchedulePostponed() -> llvm::Function*;
  [[nodiscard]] auto GetLyraMonitorSetEnabled() -> llvm::Function*;
  [[nodiscard]] auto GetLyraMonitorRegister() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReadmem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraWritemem() -> llvm::Function*;
  [[nodiscard]] auto GetLyraNotifySignal() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintModulePath() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFillPackedElements() -> llvm::Function*;

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
  [[nodiscard]] auto GetDesignStateType() const -> llvm::StructType*;
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

  // FieldIndex accessors (encapsulate map lookups)
  [[nodiscard]] auto GetDesignFieldIndex(mir::SlotId slot_id) const -> uint32_t;
  [[nodiscard]] auto GetFrameFieldIndex(mir::PlaceId place_id) const
      -> uint32_t;

  // Per-process setup (set before generating each process function)
  void SetCurrentProcess(size_t process_index);
  [[nodiscard]] auto GetCurrentProcessIndex() const -> size_t;

  // Per-process instance_id for %m support
  void SetCurrentInstanceId(uint32_t instance_id);
  [[nodiscard]] auto GetCurrentInstanceId() const -> uint32_t;

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

  // Resolve alias chains for a place. If the place's root is an aliased design
  // slot, returns the resolved place (with target root + composed projections).
  // Handles chained aliases with cycle detection.
  [[nodiscard]] auto ResolveAliases(mir::PlaceId place_id) -> mir::Place;

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

  // Register an owned string temp that needs release at end of statement
  void RegisterOwnedTemp(llvm::Value* handle);

  // Clear owned temps (called by StatementScope constructor)
  void ClearOwnedTemps();

  // Release all registered owned temps (called by StatementScope destructor)
  void ReleaseOwnedTemps();

  // User function registry for function calls
  void RegisterUserFunction(mir::FunctionId func_id, llvm::Function* llvm_func);
  [[nodiscard]] auto GetUserFunction(mir::FunctionId func_id) const
      -> llvm::Function*;
  [[nodiscard]] auto HasUserFunction(mir::FunctionId func_id) const -> bool;

  // Monitor layout: snapshot encoding info (codegen artifact, not in MIR).
  // Keyed by check_thunk FunctionId. Contains only layout info (offsets,
  // sizes). format_ops come from the check thunk's own DisplayEffect (correct
  // MIR context).
  struct MonitorLayout {
    std::vector<uint32_t> offsets;     // Per-operand offset into buffer
    std::vector<uint32_t> byte_sizes;  // Per-operand byte size
    uint32_t total_size = 0;           // Total buffer size
  };
  void RegisterMonitorLayout(mir::FunctionId check_thunk, MonitorLayout layout);
  [[nodiscard]] auto GetMonitorLayout(mir::FunctionId check_thunk) const
      -> const MonitorLayout*;

  // Monitor setup thunk marker (codegen artifact, not stored in MIR).
  // When present, LLVM lowering appends serialization + registration.
  // Layout is looked up via check_thunk (single source of truth).
  struct MonitorSetupInfo {
    mir::FunctionId check_thunk;  // For registration and layout lookup
  };
  void RegisterMonitorSetupInfo(
      mir::FunctionId setup_thunk, MonitorSetupInfo info);
  [[nodiscard]] auto GetMonitorSetupInfo(mir::FunctionId setup_thunk) const
      -> const MonitorSetupInfo*;

  // Build LLVM function type from MIR function signature.
  // All user functions receive (DesignState*, Engine*, args...).
  // For managed returns: (out_ptr*, DesignState*, Engine*, args...) -> void
  // Note: We use out-param convention but NOT LLVM's sret attribute (which is
  // for aggregates, not pointer handles).
  [[nodiscard]] auto BuildUserFunctionType(const mir::FunctionSignature& sig)
      -> Result<llvm::FunctionType*>;

  // Check if a function uses out-param calling convention (managed return).
  [[nodiscard]] auto FunctionUsesSret(mir::FunctionId func_id) const -> bool;

 private:
  // Commit-module-only methods (accessed via friend class commit::Access)
  // Get unified write target (pointer + signal_id) from a place.
  // All fields derived from the same alias-resolved place, ensuring
  // consistency.
  [[nodiscard]] auto GetWriteTarget(mir::PlaceId place_id)
      -> Result<WriteTarget>;

  // Get the canonical root signal_id for notification.
  // Resolves aliases, then returns the resolved root's slot ID.
  // Returns nullopt if the resolved root is not a design slot.
  [[nodiscard]] auto GetCanonicalRootSignalId(mir::PlaceId place_id)
      -> std::optional<uint32_t>;

  // Internal helper: compute pointer from an already-resolved place.
  // The original_place_id is needed for frame field index lookup (for
  // local/temp).
  [[nodiscard]] auto ComputePlacePointer(
      const mir::Place& resolved, mir::PlaceId original_place_id)
      -> Result<llvm::Value*>;

  const mir::Design& design_;
  const mir::Arena& arena_;
  const TypeArena& types_;
  const Layout& layout_;

  std::unique_ptr<llvm::LLVMContext> llvm_context_;
  std::unique_ptr<llvm::Module> llvm_module_;
  llvm::IRBuilder<> builder_;

  // Per-function state for alloca insertion
  llvm::Function* current_function_ = nullptr;
  std::unique_ptr<llvm::IRBuilder<>> alloca_builder_;

  llvm::Function* lyra_print_literal_ = nullptr;
  llvm::Function* lyra_print_value_ = nullptr;
  llvm::Function* lyra_print_string_ = nullptr;
  llvm::Function* lyra_print_end_ = nullptr;
  llvm::StructType* format_spec_type_ = nullptr;
  llvm::Function* lyra_register_var_ = nullptr;
  llvm::Function* lyra_snapshot_vars_ = nullptr;
  llvm::Function* lyra_string_from_literal_ = nullptr;
  llvm::Function* lyra_string_cmp_ = nullptr;
  llvm::Function* lyra_string_retain_ = nullptr;
  llvm::Function* lyra_string_release_ = nullptr;
  llvm::Function* lyra_string_concat_ = nullptr;
  llvm::Function* lyra_run_simulation_ = nullptr;
  llvm::Function* lyra_run_process_sync_ = nullptr;
  llvm::Function* lyra_plusargs_test_ = nullptr;
  llvm::Function* lyra_plusargs_value_int_ = nullptr;
  llvm::Function* lyra_plusargs_value_string_ = nullptr;
  llvm::Function* lyra_suspend_delay_ = nullptr;
  llvm::Function* lyra_suspend_wait_ = nullptr;
  llvm::Function* lyra_suspend_repeat_ = nullptr;
  llvm::Function* lyra_alloc_triggers_ = nullptr;
  llvm::Function* lyra_free_triggers_ = nullptr;
  llvm::Function* lyra_store_packed_ = nullptr;
  llvm::Function* lyra_store_string_ = nullptr;
  llvm::Function* lyra_schedule_nba_ = nullptr;
  llvm::Function* lyra_terminate_ = nullptr;
  llvm::Function* lyra_get_time_ = nullptr;
  llvm::Function* lyra_init_runtime_ = nullptr;
  llvm::Function* lyra_report_time_ = nullptr;
  llvm::Function* lyra_dynarray_new_ = nullptr;
  llvm::Function* lyra_dynarray_new_copy_ = nullptr;
  llvm::Function* lyra_dynarray_size_ = nullptr;
  llvm::Function* lyra_dynarray_element_ptr_ = nullptr;
  llvm::Function* lyra_dynarray_clone_ = nullptr;
  llvm::Function* lyra_dynarray_delete_ = nullptr;
  llvm::Function* lyra_dynarray_release_ = nullptr;
  llvm::Function* lyra_store_dynarray_ = nullptr;
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
  llvm::Function* lyra_schedule_postponed_ = nullptr;
  llvm::Function* lyra_monitor_set_enabled_ = nullptr;
  llvm::Function* lyra_monitor_register_ = nullptr;
  llvm::Function* lyra_readmem_ = nullptr;
  llvm::Function* lyra_writemem_ = nullptr;
  llvm::Function* lyra_notify_signal_ = nullptr;
  llvm::Function* lyra_print_module_path_ = nullptr;
  llvm::Function* lyra_fill_packed_elements_ = nullptr;

  // Maps PlaceRootKey to its LLVM alloca storage.
  // Storage is per-root, NOT per-PlaceId. Multiple PlaceIds with the same root
  // (but different projections) share the same storage.
  std::unordered_map<PlaceRootKey, llvm::AllocaInst*, PlaceRootKeyHash>
      place_storage_;

  // Cached enum member values globals (per enum TypeId)
  absl::flat_hash_map<TypeId, llvm::GlobalVariable*> enum_values_globals_;

  // Cached union storage info (per union TypeId)
  absl::flat_hash_map<TypeId, CachedUnionInfo> union_storage_cache_;

  // Current process index (set before generating each process)
  size_t current_process_index_ = 0;

  // Current instance_id for %m support (set before generating each process)
  uint32_t current_instance_id_ = UINT32_MAX;

  // Cached pointers for current process function
  llvm::Value* state_ptr_ = nullptr;
  llvm::Value* design_ptr_ = nullptr;
  llvm::Value* frame_ptr_ = nullptr;
  llvm::Value* engine_ptr_ = nullptr;

  // Current origin for error reporting
  common::OriginId current_origin_ = common::OriginId::Invalid();

  // Diagnostic context for error reporting (resolves OriginId → SourceSpan)
  const lowering::DiagnosticContext* diag_ctx_ = nullptr;

  // Owned string temps that need release at end of current statement
  std::vector<llvm::Value*> owned_temps_;

  // User function registry: FunctionId -> llvm::Function*
  absl::flat_hash_map<mir::FunctionId, llvm::Function*> user_functions_;

  // Monitor layouts (codegen artifact, not MIR semantics).
  // Keyed by check_thunk FunctionId - single source of truth for encoding.
  absl::flat_hash_map<mir::FunctionId, MonitorLayout> monitor_layouts_;

  // Monitor setup thunk markers (codegen artifact, not MIR semantics).
  // Just stores check_thunk reference; layout is looked up from
  // monitor_layouts_.
  absl::flat_hash_map<mir::FunctionId, MonitorSetupInfo> monitor_setup_infos_;
};

}  // namespace lyra::lowering::mir_to_llvm
