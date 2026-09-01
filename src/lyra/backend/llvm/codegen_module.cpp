#include "lyra/backend/llvm/codegen_module.hpp"

#include <format>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("lyra", *context_)),
      unit_(&unit),
      types_(*context_, unit) {
}

auto CodeGenModule::Run() -> diag::Result<EmittedModule> {
  // Every function is declared before any body is generated, because a body may
  // call one whose own body is generated later, including itself.
  functions_.reserve(unit_->functions.size());
  for (const lir::Function& fn : unit_->functions) {
    functions_.push_back(DeclareCallable(fn));
  }
  packed_type_cells_.reserve(unit_->types.size());
  for (const lir::TypeId id : unit_->types.Ids()) {
    packed_type_cells_.push_back(
        unit_->packed_type_initializers.Get(id).has_value()
            ? DeclareDescriptorCell()
            : nullptr);
  }
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    auto generated =
        CodeGenFunction(*this, unit_->functions.Get(id), functions_[id.value])
            .Run();
    if (!generated) {
      return std::unexpected(std::move(generated.error()));
    }
  }

  std::string error;
  llvm::raw_string_ostream os(error);
  if (llvm::verifyModule(*module_, &os)) {
    throw InternalError(
        std::format("llvm codegen: produced an invalid module: {}", os.str()));
  }
  return EmittedModule{std::move(context_), std::move(module_)};
}

auto CodeGenModule::DeclareCallable(const lir::Function& fn)
    -> llvm::Function* {
  std::vector<llvm::Type*> params;
  params.reserve(fn.params.size());
  for (const lir::ValueId param : fn.params) {
    params.push_back(types_.Map(fn.values.Get(param).type));
  }
  auto* fn_ty =
      llvm::FunctionType::get(types_.Map(fn.result_type), params, false);
  return llvm::Function::Create(
      fn_ty, llvm::Function::ExternalLinkage, fn.name, module_.get());
}

auto CodeGenModule::UnitFunction(lir::FunctionId function) -> llvm::Function* {
  return functions_.at(function.value);
}

auto CodeGenModule::DefinitionRef(lir::TypeId type) -> llvm::Constant* {
  // A declaration this unit compiles already carries the symbol it was emitted
  // under; a class another unit publishes is composed from the unit and class a
  // signature named, the same way that unit composed it. Both resolve to a
  // record the host built, so the reference is the same kind of symbol either
  // way.
  const std::string name = std::visit(
      Overloaded{
          [&](const lir::ObjectType& o) -> std::string {
            return unit_->classes.Get(o.class_id).name;
          },
          [&](const lir::ExternalUnitObjectType& e) -> std::string {
            const lir::ExternalUnitObject& object =
                unit_->external_unit_objects.Get(e.object);
            return std::format("{}.{}", object.unit_name, object.class_name);
          },
          [&](const lir::ClosureType& c) -> std::string {
            return unit_->closures.Get(c.closure_id).name;
          },
          [&](const auto&) -> std::string {
            throw InternalError(
                "llvm codegen: a definition reference requires a declaration "
                "the runtime builds values of");
          }},
      unit_->types.Get(type).data);
  // The definition is opaque to generated code, which only forwards its
  // address; an i8 placeholder gives the external symbol a type without
  // encoding the runtime struct's layout.
  return module_->getOrInsertGlobal(
      DefinitionSymbolName(name), llvm::Type::getInt8Ty(*context_));
}

auto CodeGenModule::PackedTypeCell(lir::TypeId integral)
    -> llvm::GlobalVariable* {
  return packed_type_cells_.at(integral.value);
}

// Constructing a global appends it to the module, which owns it from then on;
// what the list keeps is the module's cells, not a second owner of them.
auto CodeGenModule::DeclareDescriptorCell() -> llvm::GlobalVariable* {
  auto* ptr_ty = types_.Ptr();
  return new llvm::GlobalVariable(
      *module_, ptr_ty, false, llvm::GlobalValue::PrivateLinkage,
      llvm::ConstantPointerNull::get(ptr_ty));
}

}  // namespace lyra::backend::llvm_backend
