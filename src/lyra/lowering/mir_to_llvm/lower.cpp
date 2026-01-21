#include "lyra/lowering/mir_to_llvm/lower.hpp"

#include "llvm/Support/raw_ostream.h"
#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/lowering/mir_to_llvm/process.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerMirToLlvm(const LoweringInput& input) -> LoweringResult {
  Context context(*input.design, *input.mir_arena, *input.type_arena);

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  // Create main function: int main()
  auto* main_type =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(llvm_ctx), false);
  auto* main_func = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, "main", &module);

  auto* entry = llvm::BasicBlock::Create(llvm_ctx, "entry", main_func);
  builder.SetInsertPoint(entry);

  // Lower all initial processes
  for (const auto& element : input.design->elements) {
    if (!std::holds_alternative<mir::Module>(element)) {
      continue;
    }
    const auto& mir_module = std::get<mir::Module>(element);
    for (mir::ProcessId proc_id : mir_module.processes) {
      const auto& process = (*input.mir_arena)[proc_id];
      if (process.kind == mir::ProcessKind::kOnce) {
        LowerProcess(context, process);
      }
    }
  }

  // Return 0
  builder.CreateRet(llvm::ConstantInt::get(llvm_ctx, llvm::APInt(32, 0)));

  auto [ctx, mod] = context.TakeOwnership();
  return LoweringResult{
      .context = std::move(ctx),
      .module = std::move(mod),
  };
}

auto DumpLlvmIr(const LoweringResult& result) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  result.module->print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
