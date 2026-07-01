#pragma once

#include <memory>
#include <string>

namespace llvm {
class LLVMContext;
class Module;
}  // namespace llvm

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// The backend's product: a self-contained LLVM module and the context it lives
// in, kept together because the module references the context. Textual IR is a
// projection (`Print`) for inspection, not the product; the module itself is
// what downstream executes.
class EmittedModule {
 public:
  EmittedModule(
      std::unique_ptr<llvm::LLVMContext> context,
      std::unique_ptr<llvm::Module> module);
  EmittedModule(EmittedModule&&) noexcept;
  auto operator=(EmittedModule&&) noexcept -> EmittedModule&;
  EmittedModule(const EmittedModule&) = delete;
  auto operator=(const EmittedModule&) -> EmittedModule& = delete;
  ~EmittedModule();

  [[nodiscard]] auto Print() const -> std::string;

 private:
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
};

// Lowers one self-contained LIR compilation unit to an LLVM module. Every value
// type is an opaque runtime handle reached through the runtime facade; a
// callable whose result is a coroutine is a process step body, and a coroutine
// value is a `{step_fn, env}` closure.
auto EmitModule(const lir::CompilationUnit& unit) -> EmittedModule;

}  // namespace lyra::backend::llvm_backend
