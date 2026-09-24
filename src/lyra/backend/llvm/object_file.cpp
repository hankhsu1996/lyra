#include "lyra/backend/llvm/object_file.hpp"

#include <filesystem>
#include <format>
#include <memory>
#include <string>
#include <system_error>
#include <utility>

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Transforms/Coroutines/CoroCleanup.h>
#include <llvm/Transforms/Coroutines/CoroEarly.h>
#include <llvm/Transforms/Coroutines/CoroSplit.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

void LowerCoroutines(llvm::Module& module) {
  llvm::PassBuilder builder;
  llvm::LoopAnalysisManager loops;
  llvm::FunctionAnalysisManager functions;
  llvm::CGSCCAnalysisManager call_graph;
  llvm::ModuleAnalysisManager modules;
  builder.registerModuleAnalyses(modules);
  builder.registerCGSCCAnalyses(call_graph);
  builder.registerFunctionAnalyses(functions);
  builder.registerLoopAnalyses(loops);
  builder.crossRegisterProxies(loops, functions, call_graph, modules);

  llvm::ModulePassManager passes;
  passes.addPass(llvm::CoroEarlyPass());
  llvm::CGSCCPassManager split;
  split.addPass(llvm::CoroSplitPass());
  passes.addPass(
      llvm::createModuleToPostOrderCGSCCPassAdaptor(std::move(split)));
  passes.addPass(llvm::CoroCleanupPass());
  passes.run(module, modules);
}

auto WriteObjectFile(EmittedModule module, const std::filesystem::path& path)
    -> diag::Result<void> {
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  const std::string triple = llvm::sys::getProcessTriple();
  std::string lookup_error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(triple, lookup_error);
  if (target == nullptr) {
    throw InternalError(
        std::format(
            "llvm codegen: this compiler carries no target for the machine it "
            "runs on ({}): {}",
            triple, lookup_error));
  }
  // No processor is named, so the code uses what every machine of this kind
  // has. The code is position-independent, because the executable the host's
  // linker makes by default is.
  const std::unique_ptr<llvm::TargetMachine> machine(
      target->createTargetMachine(
          triple, "", "", llvm::TargetOptions{}, llvm::Reloc::PIC_));

  EmittedModule::Owned owned = std::move(module).Release();
  owned.module->setTargetTriple(triple);
  owned.module->setDataLayout(machine->createDataLayout());
  LowerCoroutines(*owned.module);

  std::error_code opened;
  llvm::raw_fd_ostream out(path.string(), opened, llvm::sys::fs::OF_None);
  if (opened) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, std::format(
                                          "failed to open '{}' for writing: {}",
                                          path.string(), opened.message()));
  }
  llvm::legacy::PassManager passes;
  if (machine->addPassesToEmitFile(
          passes, out, nullptr, llvm::CodeGenFileType::CGFT_ObjectFile)) {
    throw InternalError(
        std::format(
            "llvm codegen: the target for {} cannot write an object file",
            triple));
  }
  passes.run(*owned.module);
  out.close();
  if (out.has_error()) {
    // A stream still holding an error when it is destroyed ends the process,
    // so the error is taken off it once it has been read.
    const std::error_code failed = out.error();
    out.clear_error();
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        std::format(
            "failed to write '{}': {}", path.string(), failed.message()));
  }
  return {};
}

}  // namespace lyra::backend::llvm_backend
