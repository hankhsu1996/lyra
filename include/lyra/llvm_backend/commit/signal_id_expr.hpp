#pragma once

#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

// Represents a signal ID that may be a compile-time constant or a dynamic
// value (for shared processes where the signal ID depends on instance offset).
class SignalIdExpr {
 public:
  enum class Kind { kConst, kDynamic };

  static auto Const(uint32_t v) -> SignalIdExpr {
    SignalIdExpr e;
    e.kind_ = Kind::kConst;
    e.const_value_ = v;
    return e;
  }

  static auto Dynamic(llvm::Value* v) -> SignalIdExpr {
    if (v == nullptr) {
      throw common::InternalError(
          "SignalIdExpr::Dynamic", "dynamic_value must not be null");
    }
    if (!v->getType()->isIntegerTy(32)) {
      throw common::InternalError(
          "SignalIdExpr::Dynamic", "dynamic_value must be i32");
    }
    SignalIdExpr e;
    e.kind_ = Kind::kDynamic;
    e.dynamic_value_ = v;
    return e;
  }

  [[nodiscard]] auto GetKind() const -> Kind {
    return kind_;
  }
  [[nodiscard]] auto IsConst() const -> bool {
    return kind_ == Kind::kConst;
  }
  [[nodiscard]] auto IsDynamic() const -> bool {
    return kind_ == Kind::kDynamic;
  }

  [[nodiscard]] auto ConstValue() const -> uint32_t {
    if (kind_ != Kind::kConst) {
      throw common::InternalError(
          "SignalIdExpr::ConstValue",
          "cannot read const_value from a dynamic SignalIdExpr");
    }
    return const_value_;
  }

  [[nodiscard]] auto Emit(llvm::IRBuilder<>& builder) const -> llvm::Value* {
    if (kind_ == Kind::kConst) {
      auto& ctx = builder.getContext();
      return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), const_value_);
    }
    if (!dynamic_value_->getType()->isIntegerTy(32)) {
      throw common::InternalError(
          "SignalIdExpr::Emit", "dynamic_value must be i32 at point of use");
    }
    return dynamic_value_;
  }

 private:
  SignalIdExpr() = default;

  Kind kind_ = Kind::kConst;
  uint32_t const_value_ = 0;
  llvm::Value* dynamic_value_ = nullptr;
};

}  // namespace lyra::lowering::mir_to_llvm
