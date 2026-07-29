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
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("lyra", *context_)),
      unit_(&unit),
      types_(*context_, unit),
      runtime_abi_(*module_, *context_, types_) {
}

auto CodeGenModule::Run() -> EmittedModule {
  // Every function is declared before any body is generated, because a body may
  // call one whose own body is generated later, including itself.
  functions_.reserve(unit_->functions.size());
  for (const lir::Function& fn : unit_->functions) {
    functions_.push_back(DeclareCallable(fn));
  }
  for (std::uint32_t i = 0; i < unit_->functions.size(); ++i) {
    CodeGenFunction(
        *this, unit_->functions.Get(lir::FunctionId{i}), functions_[i])
        .Run();
  }

  std::string error;
  llvm::raw_string_ostream os(error);
  if (llvm::verifyModule(*module_, &os)) {
    throw InternalError(
        std::format("llvm codegen: produced an invalid module: {}", os.str()));
  }
  return {std::move(context_), std::move(module_)};
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

auto CodeGenModule::UnitDefinitionRef(lir::TypeId object_type)
    -> llvm::Constant* {
  const auto* external = std::get_if<lir::ExternalUnitObjectType>(
      &unit_->types.Get(object_type).data);
  if (external == nullptr) {
    throw InternalError(
        "llvm codegen: a unit definition reference requires an external-unit "
        "type");
  }
  // The definition is opaque to generated code, which only forwards its
  // address; an i8 placeholder gives the external symbol a type without
  // encoding the runtime struct's layout.
  return module_->getOrInsertGlobal(
      UnitDefinitionSymbolName(external->unit_name),
      llvm::Type::getInt8Ty(*context_));
}

}  // namespace lyra::backend::llvm_backend
