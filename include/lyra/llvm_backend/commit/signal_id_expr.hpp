#pragma once

#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace lyra::lowering::mir_to_llvm {

// Semantic signal coordinate for codegen emission.
//
// Carries the domain (kLocal for instance-owned, kGlobal for package/global)
// and a constant integer id within that domain. This is a pure semantic
// carrier -- it does not know how to lower to engine-internal coordinates.
//
// kLocal: body-local slot ordinal. Identity within a module body.
// kGlobal: design-global slot id. Identity for package/global state.
class SignalCoordExpr {
 public:
  enum class Kind : uint8_t {
    kLocal,
    kGlobal,
  };

  static auto Local(uint32_t id) -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kLocal;
    e.value_ = id;
    return e;
  }

  // R5: Local signal with explicit target instance pointer override.
  // Used for cross-instance writes (e.g., connection processes writing
  // to child instance signals). When set, callers must use this pointer
  // instead of context.GetInstancePointer().
  static auto LocalWithInstance(uint32_t id, llvm::Value* instance_ptr)
      -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kLocal;
    e.value_ = id;
    e.instance_override_ = instance_ptr;
    return e;
  }

  static auto Global(uint32_t id) -> SignalCoordExpr {
    SignalCoordExpr e;
    e.kind_ = Kind::kGlobal;
    e.value_ = id;
    return e;
  }

  [[nodiscard]] auto GetKind() const -> Kind {
    return kind_;
  }
  [[nodiscard]] auto IsLocal() const -> bool {
    return kind_ == Kind::kLocal;
  }
  [[nodiscard]] auto IsGlobal() const -> bool {
    return kind_ == Kind::kGlobal;
  }

  [[nodiscard]] auto Value() const -> uint32_t {
    return value_;
  }

  // Returns the target instance pointer for local signals.
  // If an instance override is set (cross-instance write), returns that.
  // Otherwise returns the default context instance pointer.
  [[nodiscard]] auto GetInstancePointer(llvm::Value* default_ptr) const
      -> llvm::Value* {
    return instance_override_ != nullptr ? instance_override_ : default_ptr;
  }

  // True if this local signal targets a different instance than the
  // current process's own instance.
  [[nodiscard]] auto HasInstanceOverride() const -> bool {
    return instance_override_ != nullptr;
  }

  // Emit the semantic id value as an LLVM i32 constant.
  // This is the raw local or global id, NOT a dense coordination coordinate.
  [[nodiscard]] auto Emit(llvm::IRBuilder<>& builder) const -> llvm::Value* {
    auto& ctx = builder.getContext();
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), value_);
  }

 private:
  SignalCoordExpr() = default;

  Kind kind_ = Kind::kGlobal;
  uint32_t value_ = 0;
  llvm::Value* instance_override_ = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
