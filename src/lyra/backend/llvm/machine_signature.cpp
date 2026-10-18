#include "lyra/backend/llvm/machine_signature.hpp"

#include <optional>

#include <llvm/IR/Attributes.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include "lyra/base/internal_error.hpp"

namespace lyra::backend::llvm_backend {

auto ToLlvmType(llvm::LLVMContext& context, support::MachineKind kind)
    -> llvm::Type* {
  using support::MachineKind;
  switch (kind) {
    case MachineKind::kVoid:
      return llvm::Type::getVoidTy(context);
    case MachineKind::kAddress:
      return llvm::PointerType::getUnqual(context);
    case MachineKind::kTruth:
      return llvm::Type::getInt1Ty(context);
    case MachineKind::kInt8:
      return llvm::Type::getInt8Ty(context);
    case MachineKind::kInt32:
      return llvm::Type::getInt32Ty(context);
    case MachineKind::kInt64:
      return llvm::Type::getInt64Ty(context);
    case MachineKind::kFloat:
      return llvm::Type::getFloatTy(context);
    case MachineKind::kDouble:
      return llvm::Type::getDoubleTy(context);
    case MachineKind::kAggregate:
      throw InternalError(
          "llvm codegen: a structure crosses as its own fields, which its "
          "machine kind does not state -- please report this as a bug");
  }
  throw InternalError("llvm codegen: unknown machine kind");
}

auto MachineKindOf(llvm::Type* type) -> std::optional<support::MachineKind> {
  using support::MachineKind;
  if (type->isVoidTy()) {
    return MachineKind::kVoid;
  }
  if (type->isPointerTy()) {
    return MachineKind::kAddress;
  }
  if (type->isIntegerTy(1)) {
    return MachineKind::kTruth;
  }
  if (type->isIntegerTy(8)) {
    return MachineKind::kInt8;
  }
  if (type->isIntegerTy(32)) {
    return MachineKind::kInt32;
  }
  if (type->isIntegerTy(64)) {
    return MachineKind::kInt64;
  }
  if (type->isFloatTy()) {
    return MachineKind::kFloat;
  }
  if (type->isDoubleTy()) {
    return MachineKind::kDouble;
  }
  if (type->isStructTy()) {
    return MachineKind::kAggregate;
  }
  return std::nullopt;
}

void MarkBoolsAsCpp(llvm::Function* function) {
  for (unsigned i = 0; i < function->arg_size(); ++i) {
    if (function->getArg(i)->getType()->isIntegerTy(1)) {
      function->addParamAttr(i, llvm::Attribute::ZExt);
    }
  }
  if (function->getReturnType()->isIntegerTy(1)) {
    function->addRetAttr(llvm::Attribute::ZExt);
  }
}

}  // namespace lyra::backend::llvm_backend
