#include "lyra/backend/llvm/codegen_types.hpp"

#include <variant>

#include <llvm/IR/LLVMContext.h>

#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenTypes::CodeGenTypes(
    llvm::LLVMContext& ctx, const lir::CompilationUnit& unit)
    : ctx_(&ctx),
      unit_(&unit),
      ptr_ty_(llvm::PointerType::getUnqual(ctx)),
      span_ty_(
          llvm::StructType::get(ctx, {ptr_ty_, llvm::Type::getInt64Ty(ctx)})) {
}

auto CodeGenTypes::Void() const -> llvm::Type* {
  return llvm::Type::getVoidTy(*ctx_);
}

auto CodeGenTypes::Map(lir::TypeId id) -> llvm::Type* {
  if (auto it = cache_.find(id); it != cache_.end()) {
    return it->second;
  }
  llvm::Type* mapped = std::visit(
      Overloaded{
          [&](const lir::VoidType&) -> llvm::Type* { return Void(); },
          [&](const lir::MachineIntType& m) -> llvm::Type* {
            return llvm::IntegerType::get(*ctx_, m.bit_width);
          },
          [&](const lir::MachineFloatType& m) -> llvm::Type* {
            return m.bit_width == 32 ? llvm::Type::getFloatTy(*ctx_)
                                     : llvm::Type::getDoubleTy(*ctx_);
          },
          [&](const auto&) -> llvm::Type* { return ptr_ty_; }},
      unit_->types.Get(id).data);
  cache_.emplace(id, mapped);
  return mapped;
}

}  // namespace lyra::backend::llvm_backend
