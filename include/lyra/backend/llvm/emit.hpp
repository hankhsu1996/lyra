#pragma once

#include <memory>
#include <string>

#include "lyra/diag/diagnostic.hpp"

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

  // The context and module together, ownership transferred out. A consumer that
  // hands the module to an execution engine takes both, since the module
  // references the context and the two must share a lifetime.
  struct Owned {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::Module> module;
  };

  [[nodiscard]] auto Print() const -> std::string;
  [[nodiscard]] auto Release() && -> Owned;

 private:
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
};

// Lowers one self-contained LIR compilation unit to an LLVM module. Every value
// type is an opaque runtime handle reached through the runtime facade; a
// callable whose result is a coroutine is a process step body, and a coroutine
// value is an opaque handle the runtime builds from a step entry and its
// environment. A construct this backend has no entry for is refused, naming
// what was met.
auto EmitModule(const lir::CompilationUnit& unit)
    -> diag::Result<EmittedModule>;

}  // namespace lyra::backend::llvm_backend
