#include "lyra/backend/llvm/codegen_module.hpp"

#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/symbol_name.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("lyra", *context_)),
      unit_(&unit),
      types_(*context_, unit),
      functions_(unit.functions.size()) {
  for (const lir::ClassId id : unit.classes.Ids()) {
    const lir::Class& cls = unit.classes.Get(id);
    // A class the runtime drives is the one a construction reaches holding a
    // definition and nothing else, so its construction is what answers to the
    // shared prototype. A class standing in the tree that supplies no way to
    // run is what a unit promised of its object: nothing constructs one through
    // a definition, and what extends it enters it by name with its own
    // arguments already in hand.
    if (cls.tree_program.has_value()) {
      scope_constructions_.insert(cls.constructor);
    }
  }
}

auto CodeGenModule::IsScopeConstruction(lir::FunctionId function) const
    -> bool {
  return scope_constructions_.contains(function);
}

auto CodeGenModule::Run() -> diag::Result<EmittedModule> {
  // Every function is declared before any body is generated, because a body may
  // call one whose own body is generated later, including itself.
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    functions_.Append(DeclareCallable(id));
  }
  type_descriptor_cells_ =
      base::Translation<lir::TypeDescriptorId, llvm::GlobalVariable*>(
          unit_->type_descriptor_initializers.size());
  for (const lir::TypeDescriptorId id :
       unit_->type_descriptor_initializers.Ids()) {
    type_descriptor_cells_.Append(DeclareTypeDescriptorCell(id));
  }
  integral_constant_cells_ =
      base::Translation<lir::IntegralConstantId, llvm::GlobalVariable*>(
          unit_->integral_constant_initializers.size());
  for (const lir::IntegralConstantId id :
       unit_->integral_constant_initializers.Ids()) {
    integral_constant_cells_.Append(DeclareIntegralConstantCell(id));
  }
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    auto generated = CodeGenFunction(*this, id).Run();
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

namespace {

// What the linker is told to do where a second artifact defines the same name.
// A definition several of them write is one the session keeps one of, which is
// what every target spells as a weak definition; one this artifact owns is
// written once and a repeat of it is a program that does not link.
auto LinkageOf(lir::Definition definition) -> llvm::GlobalValue::LinkageTypes {
  switch (definition) {
    case lir::Definition::kOwned:
      return llvm::Function::ExternalLinkage;
    case lir::Definition::kShared:
      return llvm::Function::WeakODRLinkage;
  }
  throw InternalError("llvm codegen: unknown definition kind");
}

}  // namespace

auto CodeGenModule::DeclareCallable(lir::FunctionId id) -> llvm::Function* {
  const lir::Function& fn = unit_->functions.Get(id);
  std::vector<llvm::Type*> params;
  params.reserve(fn.params.size());
  for (const lir::ValueId param : fn.params) {
    params.push_back(types_.Map(fn.values.Get(param).type));
  }
  // A scope's construction answers to the one prototype every class's shares,
  // because what reaches it holds the class's definition and not its name: the
  // parameters the construction has in common with every other, and then the
  // values this class alone is parameterized by, collected into one span.
  if (IsScopeConstruction(id)) {
    params.resize(kScopeConstructSharedParams);
    params.push_back(types_.Span());
  }
  auto* fn_ty =
      llvm::FunctionType::get(types_.Map(fn.result_type), params, false);
  return llvm::Function::Create(
      fn_ty, LinkageOf(fn.definition), fn.name, module_.get());
}

auto CodeGenModule::UnitFunction(lir::FunctionId function) -> llvm::Function* {
  return functions_.Get(function);
}

auto CodeGenModule::DefinitionRef(lir::TypeId type)
    -> diag::Result<llvm::Constant*> {
  const std::optional<std::string> symbol = lir::DefinitionSymbol(*unit_, type);
  if (!symbol.has_value()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedExpressionForm,
        std::format(
            "llvm codegen: a value of type {} has no definition the runtime "
            "builds values of",
            unit_->types.Get(type).KindName()));
  }
  // The definition is opaque to generated code, which only forwards its
  // address; an i8 placeholder gives the external symbol a type without
  // encoding the runtime struct's layout.
  return module_->getOrInsertGlobal(*symbol, llvm::Type::getInt8Ty(*context_));
}

auto CodeGenModule::VariableSchemaRef(const lir::Function& fn)
    -> llvm::Constant* {
  // The description is opaque to generated code, which only forwards its
  // address; an i8 placeholder gives the external symbol a type without
  // encoding the runtime struct's layout.
  return module_->getOrInsertGlobal(
      lir::VariableSchemaSymbol(fn.name), llvm::Type::getInt8Ty(*context_));
}

auto CodeGenModule::TypeDescriptorCell(lir::TypeDescriptorId descriptor)
    -> llvm::GlobalVariable* {
  return type_descriptor_cells_.Get(descriptor);
}

auto CodeGenModule::IntegralConstantCell(lir::IntegralConstantId constant)
    -> llvm::GlobalVariable* {
  return integral_constant_cells_.Get(constant);
}

// The module owns its globals, so what the list keeps is the module's cells
// rather than a second owner of them. The label reaches no linker, so a type's
// own identity is enough to tell one cell from another.
auto CodeGenModule::DeclareTypeDescriptorCell(lir::TypeDescriptorId descriptor)
    -> llvm::GlobalVariable* {
  llvm::PointerType* ptr_ty = types_.Ptr();
  auto* cell = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("type_descriptor_{}", descriptor.value), ptr_ty));
  cell->setLinkage(llvm::GlobalValue::PrivateLinkage);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  return cell;
}

auto CodeGenModule::DeclareIntegralConstantCell(
    lir::IntegralConstantId constant) -> llvm::GlobalVariable* {
  llvm::PointerType* ptr_ty = types_.Ptr();
  auto* cell = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("constant_{}", constant.value), ptr_ty));
  cell->setLinkage(llvm::GlobalValue::PrivateLinkage);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  return cell;
}

}  // namespace lyra::backend::llvm_backend
