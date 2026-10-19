#include "lyra/backend/llvm/object_file.hpp"

#include <filesystem>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <system_error>
#include <utility>

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/IPO/ThinLTOBitcodeWriter.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/backend/llvm/target_machine.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// The analyses a pipeline runs with, registered with one another as every
// pipeline the pass builder makes expects.
struct Analyses {
  explicit Analyses(llvm::PassBuilder& builder) {
    builder.registerModuleAnalyses(modules);
    builder.registerCGSCCAnalyses(call_graph);
    builder.registerFunctionAnalyses(functions);
    builder.registerLoopAnalyses(loops);
    builder.crossRegisterProxies(loops, functions, call_graph, modules);
  }

  llvm::LoopAnalysisManager loops;
  llvm::FunctionAnalysisManager functions;
  llvm::CGSCCAnalysisManager call_graph;
  llvm::ModuleAnalysisManager modules;
};

// A target machine for `kTargetCpu`. A target machine holds state its code
// generation writes, so each compile makes its own.
auto CreateTargetMachine(
    const CompileTarget& target, llvm::CodeGenOptLevel level)
    -> std::unique_ptr<llvm::TargetMachine> {
  return std::unique_ptr<llvm::TargetMachine>(
      target.target->createTargetMachine(
          target.triple, kTargetCpu, "", llvm::TargetOptions{}, kRelocation,
          std::nullopt, level));
}

// Binds a module to the machine it is compiled for. Every function it defines
// states the processor, as clang states it on every function it emits, because
// that is what decides whether a call to a function compiled elsewhere may be
// inlined.
void BindToMachine(llvm::Module& module, const llvm::TargetMachine& machine) {
  module.setTargetTriple(machine.getTargetTriple());
  module.setDataLayout(machine.createDataLayout());
  for (llvm::Function& function : module) {
    if (function.isDeclaration()) {
      continue;
    }
    function.addFnAttr("target-cpu", machine.getTargetCPU());
    function.addFnAttr("tune-cpu", kTuneCpu);
  }
}

// What clang does at `-O<n>`: the per-module default pipeline at that level,
// then machine code at the level clang pairs with it.
void WriteObject(
    llvm::Module& module, llvm::OptimizationLevel level,
    llvm::raw_pwrite_stream& out) {
  const std::unique_ptr<llvm::TargetMachine> machine =
      CreateTargetMachine(ProcessTarget(), CodeGenLevelOf(level));
  BindToMachine(module, *machine);
  llvm::PassBuilder builder(machine.get());
  Analyses analyses(builder);
  builder.buildPerModuleDefaultPipeline(level).run(module, analyses.modules);
  llvm::legacy::PassManager passes;
  if (machine->addPassesToEmitFile(
          passes, out, nullptr, llvm::CodeGenFileType::ObjectFile)) {
    throw InternalError(
        std::format(
            "llvm codegen: the target for {} cannot write an object file",
            machine->getTargetTriple().str()));
  }
  passes.run(module);
}

// What clang does at `-O<n> -flto=thin`: the ThinLTO pre-link pipeline at that
// level, then the module written as bitcode with its summary.
void WriteThinLtoBitcode(
    llvm::Module& module, llvm::OptimizationLevel level,
    llvm::raw_ostream& out) {
  const std::unique_ptr<llvm::TargetMachine> machine =
      CreateTargetMachine(ProcessTarget(), CodeGenLevelOf(level));
  BindToMachine(module, *machine);
  llvm::PassBuilder builder(machine.get());
  Analyses analyses(builder);
  llvm::ModulePassManager passes =
      builder.buildThinLTOPreLinkDefaultPipeline(level);
  passes.addPass(llvm::ThinLTOBitcodeWriterPass(out, nullptr));
  passes.run(module, analyses.modules);
}

}  // namespace

auto CompileModule(
    EmittedModule module, const std::filesystem::path& output,
    llvm::OptimizationLevel level, ModuleOutput form) -> diag::Result<void> {
  EmittedModule::Owned owned = std::move(module).Release();
  const std::string path = output.string();
  std::error_code opened;
  llvm::raw_fd_ostream out(path, opened, llvm::sys::fs::OF_None);
  if (opened) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot open '{}' for writing ({})", path,
        opened.message());
  }
  switch (form) {
    case ModuleOutput::kObject:
      WriteObject(*owned.module, level, out);
      break;
    case ModuleOutput::kThinLtoBitcode:
      WriteThinLtoBitcode(*owned.module, level, out);
      break;
  }
  out.close();
  if (out.has_error()) {
    // A stream still holding an error when it is destroyed ends the process,
    // so the error is taken off it once it has been read.
    const std::error_code failed = out.error();
    out.clear_error();
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot write '{}' ({})", path,
        failed.message());
  }
  return {};
}

}  // namespace lyra::backend::llvm_backend
