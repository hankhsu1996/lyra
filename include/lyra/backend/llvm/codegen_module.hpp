#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/runtime_abi.hpp"
#include "lyra/lir/class_id.hpp"

namespace llvm {
class Function;
}  // namespace llvm

namespace lyra::lir {
struct Class;
struct CompilationUnit;
struct Function;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Module-level code generation: owns the context and module, declares every
// callable's signature, drives per-function body generation, and yields the
// verified module. The narrow accessors hand the per-function generation the
// shared services it needs without exposing the whole module emitter.
class CodeGenModule {
 public:
  explicit CodeGenModule(const lir::CompilationUnit& unit);

  auto Run() -> EmittedModule;

  auto Context() -> llvm::LLVMContext& {
    return *context_;
  }
  auto Module() -> llvm::Module& {
    return *module_;
  }
  auto Types() -> CodeGenTypes& {
    return types_;
  }
  auto Runtime() -> RuntimeAbi& {
    return runtime_abi_;
  }
  auto Unit() const -> const lir::CompilationUnit& {
    return *unit_;
  }

  // The LLVM function a `MethodRef` names. Method identity is structural -- the
  // class and the method's index -- never a reconstructed symbol name.
  auto MethodFunction(lir::ClassId class_id, std::uint32_t index)
      -> llvm::Function*;

 private:
  void DeclareCallable(
      const lir::Class& cls, lir::ClassId class_id, const lir::Function& fn,
      std::optional<std::uint32_t> method_index);

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  CodeGenTypes types_;
  RuntimeAbi runtime_abi_;
  std::vector<std::pair<const lir::Function*, llvm::Function*>> callables_;
  std::map<std::pair<std::uint32_t, std::uint32_t>, llvm::Function*> methods_;
};

}  // namespace lyra::backend::llvm_backend
