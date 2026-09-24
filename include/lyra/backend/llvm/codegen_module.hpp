#pragma once

#include <array>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/backend/llvm/codegen_types.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/struct_id.hpp"
#include "lyra/lir/type_descriptor_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace llvm {
class Constant;
class Function;
class GlobalVariable;
class IRBuilderBase;
}  // namespace llvm

namespace lyra::lir {
struct CompilationUnit;
struct Member;
struct StaticStorage;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// Module-level code generation: owns the context and module, declares every
// callable's signature, drives per-function body generation, and yields the
// verified module. The narrow accessors hand the per-function generation the
// shared internals it needs without exposing the whole module emitter.
class CodeGenModule {
 public:
  CodeGenModule(const lir::CompilationUnit& unit, TimeResolution time);

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

  // The cell holding the runtime definition of a type whose values the runtime
  // builds -- a scope class, a struct, or a closure. The unit that declares one
  // fills its cell where it states everything else it declares; a reference
  // loads it and forwards the address without inspecting it, and a declaration
  // of this unit and one of another are named the same way.
  auto DefinitionRef(lir::TypeId type) -> diag::Result<llvm::GlobalVariable*>;

  // The module-level home of one body's variable description. The unit states
  // what its bodies need where it states everything else it declares, so the
  // description is in place before any body can be entered and every entry
  // loads the address left here. A body that states no variables has no cell.
  auto VariableSchemaCell(lir::FunctionId fn) -> llvm::GlobalVariable*;

  // The module-level home of one thing a unit declares, under the symbol the
  // program knows it by. The unit that declares it is the one that fills the
  // cell, and every other unit reaches the same cell by the same symbol, so
  // what a reference loads is what the declaring unit built whichever unit the
  // reference is in.
  auto DeclaredCell(const std::string& symbol) -> llvm::GlobalVariable*;

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
  auto DeclareVariableSchemaCell(lir::FunctionId id) -> llvm::GlobalVariable*;
  // Emits the body that states this unit's declarations, and asks the target to
  // run it before the program starts. Composing the program is what settles
  // when that is -- a linker collects it, a session runs it on the way up --
  // and the unit states the same thing either way.
  auto EmitUnitDeclaration() -> diag::Result<void>;
  // States what one body's variables need, as the bytes the runtime realizes
  // storage from. Refuses a variable whose type has no storage on this backend,
  // which is where every other unrealizable form is refused too.
  auto StateVariableSchema(llvm::IRBuilderBase& builder, lir::FunctionId id)
      -> diag::Result<void>;
  // States what one cell this unit shares needs, and fills the cell every
  // reference to it loads. Refuses a cell whose type has no storage on this
  // backend, beside where a variable of the same type is refused.
  auto StateSharedStorage(
      llvm::IRBuilderBase& builder, const lir::StaticStorage& storage)
      -> diag::Result<void>;
  // States one closure this unit declares: what its captures need, and the body
  // a call runs in the protocol that body answers to.
  auto StateClosure(llvm::IRBuilderBase& builder, lir::ClosureId id)
      -> diag::Result<void>;
  // States one class this unit declares: what it adds to its lineage, and --
  // for a class whose values stand in the design hierarchy -- the program the
  // runtime drives an instance through and the names it answers from.
  auto StateClass(llvm::IRBuilderBase& builder, lir::ClassId id)
      -> diag::Result<void>;
  // States one compiler-generated record this unit declares, which adds storage
  // for its fields and answers no name.
  auto StateStruct(llvm::IRBuilderBase& builder, lir::StructId id)
      -> diag::Result<void>;
  // One call into the runtime from a declaration body, whose result is
  // discarded where the entry answers with one.
  auto StateCall(
      llvm::IRBuilderBase& builder, RuntimeOp op,
      std::span<llvm::Value* const> args, llvm::Type* result) -> llvm::Value*;
  // A name a declaration answers to, as the bytes and their length -- which is
  // how every name crosses, since the runtime holds what it was handed rather
  // than a copy. One spelling is emitted once however many declarations answer
  // to it.
  auto StatedName(std::string_view name) -> std::array<llvm::Value*, 2>;
  // The bytes describing what one set of members needs, as a constant of this
  // module. Refuses a member whose type has no storage on this backend, naming
  // it as `what`.
  auto DescribedStorage(
      std::span<const lir::Member> members, MemberSlotRole role,
      std::string_view what) -> diag::Result<llvm::Constant*>;
  auto DeclareTypeDescriptorCell(lir::TypeDescriptorId descriptor)
      -> llvm::GlobalVariable*;
  auto DeclareIntegralConstantCell(lir::IntegralConstantId constant)
      -> llvm::GlobalVariable*;

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  const lir::CompilationUnit* unit_;
  TimeResolution time_;
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
  // Null for a body that states no variables, which is the one shape that never
  // asks for its description.
  base::Translation<lir::FunctionId, llvm::GlobalVariable*>
      variable_schema_cells_;
  std::unordered_map<std::string, llvm::Constant*> stated_names_;
  std::uint32_t described_storage_count_ = 0;
};

}  // namespace lyra::backend::llvm_backend
