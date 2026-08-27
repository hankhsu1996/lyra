#pragma once

#include <memory>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/runtime_abi.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class Constant;
class Function;
}  // namespace llvm

namespace lyra::lir {
struct CompilationUnit;
struct Function;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Module-level code generation: owns the context and module, declares every
// callable's signature, drives per-function body generation, and yields the
// verified module. The narrow accessors hand the per-function generation the
// shared internals it needs without exposing the whole module emitter.
class CodeGenModule {
 public:
  explicit CodeGenModule(const lir::CompilationUnit& unit);

  auto Run() -> diag::Result<EmittedModule>;

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

  // The LLVM function a unit function was emitted as, reached by the identity a
  // call or a code reference carries. Identity is the function's own, never a
  // reconstructed symbol name.
  auto UnitFunction(lir::FunctionId function) -> llvm::Function*;

  // The definition-reference projection of an object type: the address of that
  // class's runtime definition, as an external symbol the host resolves. A
  // construct that builds an instance of the class passes this opaque reference
  // to the runtime; the generated code never inspects it. A class of this unit
  // and a class another unit publishes are named the same way, since the record
  // the host builds has one shape either way.
  auto ScopeDefinitionRef(lir::TypeId object_type) -> llvm::Constant*;

 private:
  auto DeclareCallable(const lir::Function& fn) -> llvm::Function*;

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  CodeGenTypes types_;
  RuntimeAbi runtime_abi_;
  std::vector<llvm::Function*> functions_;
};

}  // namespace lyra::backend::llvm_backend
