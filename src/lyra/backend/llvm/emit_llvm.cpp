#include <cstdint>
#include <memory>
#include <string>
#include <utility>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/backend/llvm/codegen_module.hpp"
#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/symbol_name.hpp"

namespace lyra::backend::llvm_backend {

EmittedModule::EmittedModule(
    std::unique_ptr<llvm::LLVMContext> context,
    std::unique_ptr<llvm::Module> module)
    : context_(std::move(context)), module_(std::move(module)) {
}

EmittedModule::EmittedModule(EmittedModule&&) noexcept = default;
auto EmittedModule::operator=(EmittedModule&&) noexcept
    -> EmittedModule& = default;
EmittedModule::~EmittedModule() = default;

auto EmittedModule::Print() const -> std::string {
  std::string out;
  llvm::raw_string_ostream os(out);
  module_->print(os, nullptr);
  return os.str();
}

auto EmittedModule::Module() const -> const llvm::Module& {
  return *module_;
}

auto EmittedModule::Release() && -> Owned {
  return Owned{.context = std::move(context_), .module = std::move(module_)};
}

auto EmitModule(const lir::CompilationUnit& unit, TimeResolution time)
    -> diag::Result<EmittedModule> {
  return CodeGenModule(unit, time).Run();
}

auto EmitProgramEntry(const lir::CompilationUnit& design_root)
    -> EmittedModule {
  if (!design_root.root.has_value()) {
    throw InternalError("llvm codegen: the design root roots no object tree");
  }
  auto context = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>(kProgramEntrySymbol, *context);
  llvm::PointerType* const ptr_ty = llvm::PointerType::getUnqual(*context);
  llvm::IntegerType* const int_ty = llvm::Type::getInt32Ty(*context);

  // The root's definition is read from the cell every reference to its class
  // loads, which the root's own unit fills where it states its declarations.
  const lir::ClassId root = *design_root.root;
  llvm::Constant* const root_cell = module->getOrInsertGlobal(
      lir::ClassDefinitionSymbol(
          design_root.name,
          lir::SymbolPartOf(design_root.classes.Get(root).name, root.value)),
      ptr_ty);
  llvm::Constant* const label_bytes =
      llvm::ConstantDataArray::getString(*context, design_root.name, false);
  auto* const label = llvm::cast<llvm::GlobalVariable>(
      module->getOrInsertGlobal("root_label", label_bytes->getType()));
  label->setLinkage(llvm::GlobalValue::PrivateLinkage);
  label->setConstant(true);
  label->setInitializer(label_bytes);

  auto* const entry = llvm::Function::Create(
      llvm::FunctionType::get(int_ty, {int_ty, ptr_ty}, false),
      llvm::Function::ExternalLinkage, kProgramEntrySymbol, module.get());
  llvm::IRBuilder<> builder(llvm::BasicBlock::Create(*context, "", entry));
  const llvm::FunctionCallee run = module->getOrInsertFunction(
      RuntimeSymbol(RuntimeOp::kRunProgram),
      llvm::FunctionType::get(
          int_ty, {int_ty, ptr_ty, ptr_ty, ptr_ty, int_ty}, false));
  builder.CreateRet(builder.CreateCall(
      run,
      {entry->getArg(0), entry->getArg(1), root_cell, label,
       builder.getInt32(static_cast<std::uint32_t>(design_root.name.size()))}));
  return EmittedModule{std::move(context), std::move(module)};
}

}  // namespace lyra::backend::llvm_backend
