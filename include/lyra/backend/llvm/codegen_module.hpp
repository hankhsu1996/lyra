#pragma once

#include <memory>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class Constant;
class Function;
class GlobalVariable;
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
  auto Unit() const -> const lir::CompilationUnit& {
    return *unit_;
  }

  // The LLVM function a unit function was emitted as, reached by the identity a
  // call or a code reference carries. Identity is the function's own, never a
  // reconstructed symbol name.
  auto UnitFunction(lir::FunctionId function) -> llvm::Function*;

  // The definition-reference projection of a type whose values the runtime
  // builds -- a scope class, or a closure: the address of that declaration's
  // runtime definition, as an external symbol the host resolves. A construct
  // passes this opaque reference to the runtime; the generated code never
  // inspects it. A declaration of this unit and one another unit publishes are
  // named the same way, since the record the host builds has one shape either
  // way.
  auto DefinitionRef(lir::TypeId type) -> llvm::Constant*;

  // The module-level home of one type's descriptor. The description is settled
  // by the type, so the run builds it once and every later use loads what the
  // first left here. It starts null, which is the one state a built descriptor
  // is never in: the runtime hands back the address of storage it owns.
  auto PackedTypeCell(lir::TypeId integral) -> llvm::GlobalVariable*;

 private:
  auto DeclareCallable(const lir::Function& fn) -> llvm::Function*;
  auto DeclareDescriptorCell() -> llvm::GlobalVariable*;

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  CodeGenTypes types_;
  base::Translation<lir::FunctionId, llvm::Function*> functions_;
  base::Translation<lir::TypeId, llvm::GlobalVariable*> packed_type_cells_;
};

}  // namespace lyra::backend::llvm_backend
