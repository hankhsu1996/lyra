#pragma once

#include <memory>
#include <vector>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// RAII guard for statement-scoped cleanup of owned string temps.
// Destructor emits LyraStringRelease calls for all registered temps.
class StatementScope {
 public:
  explicit StatementScope(Context& ctx);
  ~StatementScope();

  StatementScope(const StatementScope&) = delete;
  auto operator=(const StatementScope&) -> StatementScope& = delete;
  StatementScope(StatementScope&&) = delete;
  auto operator=(StatementScope&&) -> StatementScope& = delete;

 private:
  Context& ctx_;
};

// Shared context for MIR → LLVM lowering
class Context {
 public:
  Context(
      const mir::Design& design, const mir::Arena& arena,
      const TypeArena& types, const Layout& layout,
      std::unique_ptr<llvm::LLVMContext> llvm_ctx,
      std::unique_ptr<llvm::Module> module);

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
  [[nodiscard]] auto GetLyraRegisterVar() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSnapshotVars() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringCmp() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRetain() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringConcat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunSimulation() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendDelay() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendWait() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSuspendRepeat() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStorePacked() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStoreString() -> llvm::Function*;
  [[nodiscard]] auto GetLyraScheduleNba() -> llvm::Function*;
  [[nodiscard]] auto GetLyraFinishSimulation() -> llvm::Function*;
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

  struct ElemOpsInfo {
    int32_t elem_size = 0;
    llvm::Type* elem_llvm_type = nullptr;
    llvm::Constant* clone_fn = nullptr;
    llvm::Constant* destroy_fn = nullptr;
    bool needs_clone = false;
  };
  auto GetElemOpsForType(TypeId elem_type) -> ElemOpsInfo;

  // Get or create a global constant array of enum member values for the given
  // enum type. Returns a [N x iW] global, where N = member count and W = base
  // type bit width. Cached per TypeId.
  auto GetOrCreateEnumValuesGlobal(TypeId enum_type) -> llvm::GlobalVariable*;

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
  // Returns the alloca for a place, creating it if necessary
  // Allocas are always inserted in the entry block via alloca_builder_
  auto GetOrCreatePlaceStorage(mir::PlaceId place_id) -> llvm::AllocaInst*;

  // FieldIndex accessors (encapsulate map lookups)
  [[nodiscard]] auto GetDesignFieldIndex(mir::SlotId slot_id) const -> uint32_t;
  [[nodiscard]] auto GetFrameFieldIndex(mir::PlaceId place_id) const
      -> uint32_t;

  // Per-process setup (set before generating each process function)
  void SetCurrentProcess(size_t process_index);
  [[nodiscard]] auto GetCurrentProcessIndex() const -> size_t;

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

  // Get pointer to a place's base storage via GEP into design or frame.
  // Applies all non-BitRange projections (IndexProjection, etc.) and stops
  // at the first BitRangeProjection. For bitrange reads/writes, use
  // ComposeBitRange() to get the composed offset within this base.
  [[nodiscard]] auto GetPlacePointer(mir::PlaceId place_id) -> llvm::Value*;

  // Get the LLVM type for a place's storage
  [[nodiscard]] auto GetPlaceLlvmType(mir::PlaceId place_id) -> llvm::Type*;

  // BitRangeProjection helpers
  [[nodiscard]] auto HasBitRangeProjection(mir::PlaceId place_id) const -> bool;
  [[nodiscard]] auto GetBitRangeProjection(mir::PlaceId place_id) const
      -> const mir::BitRangeProjection&;
  // LLVM type of the base value that GetPlacePointer() points to.
  // Traverses non-BitRange projections only (same boundary as GetPlacePointer).
  [[nodiscard]] auto GetPlaceBaseType(mir::PlaceId place_id) -> llvm::Type*;

  struct ComposedBitRange {
    llvm::Value* offset;
    uint32_t width;
  };
  // Compose all chained BitRangeProjections into a single offset+width.
  // Sums all bitrange offsets (emitting LLVM add instructions) and returns
  // the last projection's width. Validates the contiguous-suffix invariant.
  [[nodiscard]] auto ComposeBitRange(mir::PlaceId place_id) -> ComposedBitRange;

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

  // Register an owned string temp that needs release at end of statement
  void RegisterOwnedTemp(llvm::Value* handle);

  // Clear owned temps (called by StatementScope constructor)
  void ClearOwnedTemps();

  // Release all registered owned temps (called by StatementScope destructor)
  void ReleaseOwnedTemps();

 private:
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
  llvm::Function* lyra_register_var_ = nullptr;
  llvm::Function* lyra_snapshot_vars_ = nullptr;
  llvm::Function* lyra_string_from_literal_ = nullptr;
  llvm::Function* lyra_string_cmp_ = nullptr;
  llvm::Function* lyra_string_retain_ = nullptr;
  llvm::Function* lyra_string_release_ = nullptr;
  llvm::Function* lyra_string_concat_ = nullptr;
  llvm::Function* lyra_run_simulation_ = nullptr;
  llvm::Function* lyra_suspend_delay_ = nullptr;
  llvm::Function* lyra_suspend_wait_ = nullptr;
  llvm::Function* lyra_suspend_repeat_ = nullptr;
  llvm::Function* lyra_store_packed_ = nullptr;
  llvm::Function* lyra_store_string_ = nullptr;
  llvm::Function* lyra_schedule_nba_ = nullptr;
  llvm::Function* lyra_finish_simulation_ = nullptr;
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

  // Maps PlaceId to its LLVM alloca storage
  absl::flat_hash_map<mir::PlaceId, llvm::AllocaInst*> place_storage_;

  // Cached enum member values globals (per enum TypeId)
  absl::flat_hash_map<TypeId, llvm::GlobalVariable*> enum_values_globals_;

  // Current process index (set before generating each process)
  size_t current_process_index_ = 0;

  // Cached pointers for current process function
  llvm::Value* state_ptr_ = nullptr;
  llvm::Value* design_ptr_ = nullptr;
  llvm::Value* frame_ptr_ = nullptr;
  llvm::Value* engine_ptr_ = nullptr;

  // Current origin for error reporting
  common::OriginId current_origin_ = common::OriginId::Invalid();

  // Owned string temps that need release at end of current statement
  std::vector<llvm::Value*> owned_temps_;
};

}  // namespace lyra::lowering::mir_to_llvm
