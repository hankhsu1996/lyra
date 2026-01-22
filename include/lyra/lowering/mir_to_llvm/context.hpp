#pragma once

#include <memory>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
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
      const TypeArena& types);

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

  [[nodiscard]] auto GetLyraPrintLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintValue() -> llvm::Function*;
  [[nodiscard]] auto GetLyraPrintEnd() -> llvm::Function*;
  [[nodiscard]] auto GetLyraRegisterVar() -> llvm::Function*;
  [[nodiscard]] auto GetLyraSnapshotVars() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringFromLiteral() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringCmp() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRetain() -> llvm::Function*;
  [[nodiscard]] auto GetLyraStringRelease() -> llvm::Function*;

  // Place storage management
  // Returns the alloca for a place, creating it if necessary
  auto GetOrCreatePlaceStorage(mir::PlaceId place_id) -> llvm::AllocaInst*;

  // Get existing storage for a place (returns nullptr if not found)
  [[nodiscard]] auto GetPlaceStorage(mir::PlaceId place_id) const
      -> llvm::AllocaInst*;

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

  std::unique_ptr<llvm::LLVMContext> llvm_context_;
  std::unique_ptr<llvm::Module> llvm_module_;
  llvm::IRBuilder<> builder_;

  llvm::Function* lyra_print_literal_ = nullptr;
  llvm::Function* lyra_print_value_ = nullptr;
  llvm::Function* lyra_print_end_ = nullptr;
  llvm::Function* lyra_register_var_ = nullptr;
  llvm::Function* lyra_snapshot_vars_ = nullptr;
  llvm::Function* lyra_string_from_literal_ = nullptr;
  llvm::Function* lyra_string_cmp_ = nullptr;
  llvm::Function* lyra_string_retain_ = nullptr;
  llvm::Function* lyra_string_release_ = nullptr;

  // Maps PlaceId to its LLVM alloca storage
  absl::flat_hash_map<mir::PlaceId, llvm::AllocaInst*> place_storage_;

  // Current origin for error reporting
  common::OriginId current_origin_ = common::OriginId::Invalid();

  // Owned string temps that need release at end of current statement
  std::vector<llvm::Value*> owned_temps_;
};

}  // namespace lyra::lowering::mir_to_llvm
