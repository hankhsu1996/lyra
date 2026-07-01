#include "lyra/backend/llvm/codegen_module.hpp"

#include <format>
#include <string>
#include <utility>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/backend/llvm/codegen_function.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// The LLVM symbol a callable is emitted under: its class and its name. The
// symbol is for readability and linkage; method identity is resolved
// structurally, never by reconstructing this name.
auto SymbolName(const lir::Class& cls, const lir::Function& fn) -> std::string {
  return std::format("{}.{}", cls.name, fn.name);
}

}  // namespace

CodeGenModule::CodeGenModule(const lir::CompilationUnit& unit)
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>("lyra", *context_)),
      unit_(&unit),
      types_(*context_, unit),
      runtime_abi_(*module_, *context_, types_) {
}

auto CodeGenModule::Run() -> EmittedModule {
  for (std::uint32_t i = 0; i < unit_->classes.size(); ++i) {
    const lir::ClassId class_id{i};
    const lir::Class& cls = unit_->classes.Get(class_id);
    DeclareCallable(cls, class_id, cls.constructor, std::nullopt);
    for (std::uint32_t m = 0; m < cls.methods.size(); ++m) {
      DeclareCallable(cls, class_id, cls.methods[m], m);
    }
  }

  for (const auto& [lir_fn, llvm_fn] : callables_) {
    CodeGenFunction(*this, *lir_fn, llvm_fn).Run();
  }

  std::string error;
  llvm::raw_string_ostream os(error);
  if (llvm::verifyModule(*module_, &os)) {
    throw InternalError(
        std::format("llvm codegen: produced an invalid module: {}", os.str()));
  }
  return {std::move(context_), std::move(module_)};
}

void CodeGenModule::DeclareCallable(
    const lir::Class& cls, lir::ClassId class_id, const lir::Function& fn,
    std::optional<std::uint32_t> method_index) {
  std::vector<llvm::Type*> params;
  params.reserve(fn.params.size());
  for (const lir::ValueId param : fn.params) {
    params.push_back(types_.Map(fn.values.Get(param).type));
  }
  auto* fn_ty =
      llvm::FunctionType::get(types_.Map(fn.result_type), params, false);
  auto* value = llvm::Function::Create(
      fn_ty, llvm::Function::ExternalLinkage, SymbolName(cls, fn),
      module_.get());
  callables_.emplace_back(&fn, value);
  if (method_index.has_value()) {
    methods_.emplace(std::pair{class_id.value, *method_index}, value);
  }
}

auto CodeGenModule::MethodFunction(lir::ClassId class_id, std::uint32_t index)
    -> llvm::Function* {
  return methods_.at(std::pair{class_id.value, index});
}

}  // namespace lyra::backend::llvm_backend
