#pragma once

#include <memory>
#include <unordered_set>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class Constant;
class Function;
class GlobalVariable;
}  // namespace llvm

namespace lyra::lir {
struct CompilationUnit;
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

  // Whether this function is how a scope of the design hierarchy is built. Such
  // a function is reached through its class's definition rather than by name --
  // which is all one unit holds of another unit's scope -- so it is emitted
  // with the one prototype every class's construction shares, and reads back
  // the values its own class is parameterized by from the span that prototype
  // ends in.
  [[nodiscard]] auto IsScopeConstruction(lir::FunctionId function) const
      -> bool;

  // The definition-reference projection of a type whose values the runtime
  // builds -- a scope class, or a closure: the address of that declaration's
  // runtime definition, as an external symbol the host resolves. A construct
  // passes this opaque reference to the runtime; the generated code never
  // inspects it. A declaration of this unit and one another unit publishes are
  // named the same way, since the record the host builds has one shape either
  // way.
  auto DefinitionRef(lir::TypeId type) -> diag::Result<llvm::Constant*>;

  // The same for one body's own variables: the address of the description the
  // host builds from what that body states, as an external symbol. A body that
  // states no variables never asks for one.
  auto VariableSchemaRef(const lir::Function& fn) -> llvm::Constant*;

  // The module-level home of one type's descriptor. The description is settled
  // by the type, so the run builds it once and every later use loads what the
  // first left here. It starts null, which is the one state a built descriptor
  // is never in: the runtime hands back the address of storage it owns.
  auto TypeDescriptorCell(lir::TypeDescriptorId descriptor)
      -> llvm::GlobalVariable*;

  // The module-level home of one constant. Same shape and same reason as the
  // cell above: the value is settled before the run, so the run builds it once
  // and every later use loads what the first left here.
  auto IntegralConstantCell(lir::IntegralConstantId constant)
      -> llvm::GlobalVariable*;

 private:
  auto DeclareCallable(lir::FunctionId id) -> llvm::Function*;
  auto DeclareTypeDescriptorCell(lir::TypeDescriptorId descriptor)
      -> llvm::GlobalVariable*;
  auto DeclareIntegralConstantCell(lir::IntegralConstantId constant)
      -> llvm::GlobalVariable*;

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  CodeGenTypes types_;
  base::Translation<lir::FunctionId, llvm::Function*> functions_;
  // Which of the unit's functions a class names as its construction, read the
  // other way round from how the unit states it: a class names the function
  // that builds a value of it, and what asks here is a function being emitted.
  std::unordered_set<lir::FunctionId> scope_constructions_;
  base::Translation<lir::TypeDescriptorId, llvm::GlobalVariable*>
      type_descriptor_cells_;
  base::Translation<lir::IntegralConstantId, llvm::GlobalVariable*>
      integral_constant_cells_;
};

}  // namespace lyra::backend::llvm_backend
