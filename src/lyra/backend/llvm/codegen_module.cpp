#include "lyra/backend/llvm/codegen_module.hpp"

#include <format>
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
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::backend::llvm_backend {

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("lyra", *context_)),
      unit_(&unit),
      types_(*context_, unit),
      functions_(unit.functions.size()) {
}

auto CodeGenModule::Run() -> diag::Result<EmittedModule> {
  // Every function is declared before any body is generated, because a body may
  // call one whose own body is generated later, including itself.
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    functions_.Append(DeclareCallable(id));
  }
  packed_type_cells_ = base::Translation<lir::TypeId, llvm::GlobalVariable*>(
      unit_->types.size());
  for (const lir::TypeId id : unit_->types.Ids()) {
    packed_type_cells_.Append(
        unit_->packed_type_initializers.Get(id).has_value()
            ? DeclarePackedTypeCell(id)
            : nullptr);
  }
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    auto generated =
        CodeGenFunction(*this, unit_->functions.Get(id), functions_.Get(id))
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

auto CodeGenModule::DeclareCallable(lir::FunctionId id) -> llvm::Function* {
  const lir::Function& fn = unit_->functions.Get(id);
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
  return functions_.Get(function);
}

auto CodeGenModule::DefinitionRef(lir::TypeId type)
    -> diag::Result<llvm::Constant*> {
  // A declaration this unit compiles already carries the symbol it was emitted
  // under; a class another unit publishes is composed from the unit and class a
  // signature named, the same way that unit composed it. Both resolve to a
  // record the host built, so the reference is the same kind of symbol either
  // way.
  const lir::Type& described = unit_->types.Get(type);
  auto name = described.Visit(
      Overloaded{
          [&](const lir::ObjectType& o) -> diag::Result<std::string> {
            return unit_->classes.Get(o.class_id).name;
          },
          [&](const lir::ExternalUnitObjectType& e)
              -> diag::Result<std::string> {
            const lir::ExternalUnitObject& object =
                unit_->external_unit_objects.Get(e.object);
            return std::format("{}.{}", object.unit_name, object.class_name);
          },
          [&](const lir::ClosureType& c) -> diag::Result<std::string> {
            return unit_->closures.Get(c.closure_id).name;
          },
          [&](const auto&) -> diag::Result<std::string> {
            return diag::Fail(
                diag::DiagCode::kUnsupportedExpressionForm,
                std::format(
                    "llvm codegen: a value of type {} has no definition the "
                    "runtime builds values of",
                    described.KindName()));
          }});
  if (!name) return std::unexpected(std::move(name.error()));
  // The definition is opaque to generated code, which only forwards its
  // address; an i8 placeholder gives the external symbol a type without
  // encoding the runtime struct's layout.
  return module_->getOrInsertGlobal(
      DefinitionSymbolName(*name), llvm::Type::getInt8Ty(*context_));
}

auto CodeGenModule::PackedTypeCell(lir::TypeId integral)
    -> llvm::GlobalVariable* {
  return packed_type_cells_.Get(integral);
}

// The module owns its globals, so what the list keeps is the module's cells
// rather than a second owner of them. The label reaches no linker, so a type's
// own identity is enough to tell one cell from another.
auto CodeGenModule::DeclarePackedTypeCell(lir::TypeId integral)
    -> llvm::GlobalVariable* {
  llvm::PointerType* ptr_ty = types_.Ptr();
  auto* cell = llvm::cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      std::format("packed_type_{}", integral.value), ptr_ty));
  cell->setLinkage(llvm::GlobalValue::PrivateLinkage);
  cell->setInitializer(llvm::ConstantPointerNull::get(ptr_ty));
  return cell;
}

}  // namespace lyra::backend::llvm_backend
