#include <memory>
#include <string>
#include <utility>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/emit.hpp"

namespace lyra::backend::llvm_backend {

EmittedModule::EmittedModule(
    std::unique_ptr<llvm::LLVMContext> context,
    std::unique_ptr<llvm::Module> module)
    : context_(std::move(context)), module_(std::move(module)) {
}

EmittedModule::EmittedModule(EmittedModule&&) noexcept = default;
auto EmittedModule::operator=(EmittedModule&&) noexcept
    -> EmittedModule& = default;
EmittedModule::~EmittedModule() = default;

auto EmittedModule::Print() const -> std::string {
  std::string out;
  llvm::raw_string_ostream os(out);
  module_->print(os, nullptr);
  return os.str();
}

auto EmitModule(const lir::CompilationUnit& unit) -> EmittedModule {
  return CodeGenModule(unit).Run();
}

}  // namespace lyra::backend::llvm_backend
