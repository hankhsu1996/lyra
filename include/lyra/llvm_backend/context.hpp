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
  [[nodiscard]] auto GetLyraPrintEnd() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRegisterVar() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSnapshotVars() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringCmp() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRetain() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRelease() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunSimulation() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRunSimulationMulti() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDesignStoreAndNotify() -> llvm::Function*;
  [[nodiscard]] auto GetLyraDesignStoreStringAndNotify() -> llvm::Function*;
  [[nodiscard]] auto GetLyraInitRuntime() -> llvm::Function*;
  [[nodiscard]] auto GetLyraReportTime() -> llvm::Function*;

  // Type accessors from layout
  [[nodiscard]] auto GetSuspendRecordType() const -> llvm::StructType*;
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

  // Get pointer to suspend record via GEP: state->header.suspend
  [[nodiscard]] auto GetSuspendRecordPointer() -> llvm::Value*;

  // Get pointer to a place's storage via GEP into design or frame.
  // For places with BitRangeProjection, returns pointer to the base
  // (pre-shift).
  [[nodiscard]] auto GetPlacePointer(mir::PlaceId place_id) -> llvm::Value*;

  // Get the LLVM type for a place's storage
  [[nodiscard]] auto GetPlaceLlvmType(mir::PlaceId place_id) -> llvm::Type*;

  // BitRangeProjection helpers
  [[nodiscard]] auto HasBitRangeProjection(mir::PlaceId place_id) const -> bool;
  [[nodiscard]] auto GetBitRangeProjection(mir::PlaceId place_id) const
      -> const mir::BitRangeProjection&;
  [[nodiscard]] auto GetPlaceBaseType(mir::PlaceId place_id) -> llvm::Type*;

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
  llvm::Function* lyra_print_end_ = nullptr;
  llvm::Function* lyra_register_var_ = nullptr;
  llvm::Function* lyra_snapshot_vars_ = nullptr;
  llvm::Function* lyra_string_from_literal_ = nullptr;
  llvm::Function* lyra_string_cmp_ = nullptr;
  llvm::Function* lyra_string_retain_ = nullptr;
  llvm::Function* lyra_string_release_ = nullptr;
  llvm::Function* lyra_run_simulation_ = nullptr;
  llvm::Function* lyra_run_simulation_multi_ = nullptr;
  llvm::Function* lyra_design_store_and_notify_ = nullptr;
  llvm::Function* lyra_design_store_string_and_notify_ = nullptr;
  llvm::Function* lyra_init_runtime_ = nullptr;
  llvm::Function* lyra_report_time_ = nullptr;

  // Maps PlaceId to its LLVM alloca storage
  absl::flat_hash_map<mir::PlaceId, llvm::AllocaInst*> place_storage_;

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
