#pragma once

#include <cstdint>
#include <optional>

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/runtime/signal_coord.hpp"

namespace lyra::lowering::mir_to_llvm {

// Semantic signal coordinate for codegen emission.
//
// Modes:
//   kLocal: body-local slot ordinal. Same-instance signal.
//   kLocalCrossInstance: body-local slot ordinal in a DIFFERENT instance.
//     Carries both the target InstanceId (for metadata) and a runtime
//     instance pointer (for address materialization).
//   kGlobal: design-global slot id. Package/global state.
//   kExtRef: external-ref write. Dirty resolves through per-instance tables.
//   kBoundChild: cross-instance child write via pre-bound frame context.
//     No InstanceId. Instance pointer + signal id loaded from frame at runtime.
class SignalCoordExpr {
 public:
  enum class Kind : uint8_t {
    kLocal,
    kLocalCrossInstance,
    kGlobal,
    // External-ref write: dirty notification resolves through per-instance
    // ext-ref target tables at runtime. value_ carries the ext-ref index.
    kExtRef,
    // Narrow migration-only representation:
    // valid only for mutation-target plumbing of synthesized
    // expression-connection writes. Must not be produced by parsing,
    // lowering of ordinary expressions, or trigger analysis.
    kBoundChild,
  };

  static auto Local(uint32_t id) -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kLocal;
    e.value_ = id;
    return e;
  }

  // Cross-instance local signal. Carries both stable identity
  // (InstanceId for metadata) and runtime pointer (for address
  // materialization).
  static auto LocalWithInstance(
      uint32_t id, runtime::InstanceId instance_id, llvm::Value* instance_ptr)
      -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kLocalCrossInstance;
    e.value_ = id;
    e.instance_id_override_ = instance_id;
    e.instance_override_ = instance_ptr;
    return e;
  }

  static auto Global(uint32_t id) -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kGlobal;
    e.value_ = id;
    return e;
  }

  // External-ref write/NBA notification. Dirty mark resolves through
  // per-instance ext-ref target tables to the target instance's local signal.
  // value_ is the ext-ref recipe index.
  static auto ExtRef(uint32_t ref_index) -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kExtRef;
    e.value_ = ref_index;
    return e;
  }

  // Bound child write: cross-instance child destination with pre-resolved
  // instance pointer and runtime-loaded local signal id. No InstanceId.
  // Signal id and instance pointer are both loaded from the process frame
  // payload at runtime.
  static auto BoundChild(
      llvm::Value* local_signal_id, llvm::Value* instance_ptr)
      -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kBoundChild;
    e.instance_override_ = instance_ptr;
    e.runtime_value_ = local_signal_id;
    return e;
  }

  [[nodiscard]] auto GetKind() const -> Kind {
    return kind_;
  }
  [[nodiscard]] auto IsLocal() const -> bool {
    return kind_ == Kind::kLocal || kind_ == Kind::kLocalCrossInstance ||
           kind_ == Kind::kBoundChild;
  }
  [[nodiscard]] auto IsGlobal() const -> bool {
    return kind_ == Kind::kGlobal;
  }
  [[nodiscard]] auto IsExtRef() const -> bool {
    return kind_ == Kind::kExtRef;
  }

  [[nodiscard]] auto Value() const -> uint32_t {
    return value_;
  }

  // Returns the target instance pointer for local signals.
  // Cross-instance: returns the resolved target instance pointer.
  // Same-instance: returns default_ptr (caller's context instance).
  // Global: returns default_ptr.
  [[nodiscard]] auto GetInstancePointer(llvm::Value* default_ptr) const
      -> llvm::Value* {
    return instance_override_ != nullptr ? instance_override_ : default_ptr;
  }

  // True if this local signal targets a different instance than the
  // current process's own instance.
  [[nodiscard]] auto HasInstanceOverride() const -> bool {
    return kind_ == Kind::kLocalCrossInstance || kind_ == Kind::kBoundChild;
  }

  // Returns the target InstanceId for cross-instance local signals.
  // Used by trigger/dependency metadata emission (not address materialization).
  [[nodiscard]] auto GetInstanceIdOverride() const
      -> std::optional<runtime::InstanceId> {
    if (kind_ == Kind::kLocalCrossInstance) {
      return instance_id_override_;
    }
    return std::nullopt;
  }

  // Emit the semantic id value as an LLVM i32 value.
  // For compile-time-constant signals, returns ConstantInt.
  // For runtime-loaded signals (GlobalRuntime), returns the pre-loaded value.
  [[nodiscard]] auto Emit(llvm::IRBuilder<>& builder) const -> llvm::Value* {
    if (runtime_value_ != nullptr) return runtime_value_;
    auto& ctx = builder.getContext();
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), value_);
  }

 private:
  SignalCoordExpr() = default;

  Kind kind_ = Kind::kGlobal;
  uint32_t value_ = 0;
  llvm::Value* instance_override_ = nullptr;
  runtime::InstanceId instance_id_override_ = runtime::InstanceId{0};
  // Non-null for GlobalRuntime: the signal ID is a runtime-loaded LLVM
  // value, not a compile-time constant. Emit() returns this directly.
  llvm::Value* runtime_value_ = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
