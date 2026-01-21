#include "lyra/lowering/mir_to_llvm/context.hpp"

namespace lyra::lowering::mir_to_llvm {

Context::Context(
    const mir::Design& design, const mir::Arena& arena, const TypeArena& types)
    : design_(design),
      arena_(arena),
      types_(types),
      llvm_context_(std::make_unique<llvm::LLVMContext>()),
      llvm_module_(
          std::make_unique<llvm::Module>("lyra_module", *llvm_context_)),
      builder_(*llvm_context_) {
}

auto Context::GetLyraPrintLiteral() -> llvm::Function* {
  if (lyra_print_literal_ == nullptr) {
    // void LyraPrintLiteral(const char* str)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::PointerType::getUnqual(*llvm_context_)}, false);
    lyra_print_literal_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintLiteral",
        llvm_module_.get());
  }
  return lyra_print_literal_;
}

auto Context::GetLyraPrintValue() -> llvm::Function* {
  if (lyra_print_value_ == nullptr) {
    // void LyraPrintValue(int32_t format, const void* data, int32_t width,
    //                     bool is_signed, const void* x_mask, const void*
    //                     z_mask)
    auto* i32_ty = llvm::Type::getInt32Ty(*llvm_context_);
    auto* ptr_ty = llvm::PointerType::getUnqual(*llvm_context_);
    auto* i1_ty = llvm::Type::getInt1Ty(*llvm_context_);
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {i32_ty, ptr_ty, i32_ty, i1_ty, ptr_ty, ptr_ty}, false);
    lyra_print_value_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintValue",
        llvm_module_.get());
  }
  return lyra_print_value_;
}

auto Context::GetLyraPrintEnd() -> llvm::Function* {
  if (lyra_print_end_ == nullptr) {
    // void LyraPrintEnd(int32_t kind)
    auto* fn_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(*llvm_context_),
        {llvm::Type::getInt32Ty(*llvm_context_)}, false);
    lyra_print_end_ = llvm::Function::Create(
        fn_type, llvm::Function::ExternalLinkage, "LyraPrintEnd",
        llvm_module_.get());
  }
  return lyra_print_end_;
}

auto Context::TakeOwnership() -> std::pair<
    std::unique_ptr<llvm::LLVMContext>, std::unique_ptr<llvm::Module>> {
  return {std::move(llvm_context_), std::move(llvm_module_)};
}

}  // namespace lyra::lowering::mir_to_llvm
