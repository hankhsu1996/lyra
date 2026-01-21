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

auto Context::GetPrintfFunction() -> llvm::Function* {
  if (printf_function_ == nullptr) {
    // int printf(const char* format, ...)
    auto* printf_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*llvm_context_),
        {llvm::PointerType::getUnqual(*llvm_context_)}, true);
    printf_function_ = llvm::Function::Create(
        printf_type, llvm::Function::ExternalLinkage, "printf",
        llvm_module_.get());
  }
  return printf_function_;
}

auto Context::TakeOwnership() -> std::pair<
    std::unique_ptr<llvm::LLVMContext>, std::unique_ptr<llvm::Module>> {
  return {std::move(llvm_context_), std::move(llvm_module_)};
}

}  // namespace lyra::lowering::mir_to_llvm
