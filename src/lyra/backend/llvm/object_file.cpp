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
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include "lyra/backend/llvm/emit.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::backend::llvm_backend {

namespace {

// The kind of machine every object is compiled for, and the target that
// compiles for it.
struct NativeTarget {
  std::string triple;
  const llvm::Target* target;
};

// Registered and looked up once for the whole process, the first time any
// module is compiled; a function-local static is initialized exactly once
// however many compiles reach it at the same moment.
auto ThisMachine() -> const NativeTarget& {
  static const NativeTarget host = [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    std::string triple = llvm::sys::getProcessTriple();
    std::string lookup_error;
    const llvm::Target* target =
        llvm::TargetRegistry::lookupTarget(triple, lookup_error);
    if (target == nullptr) {
      throw InternalError(
          std::format(
              "llvm codegen: this compiler carries no target for the machine "
              "it runs on ({}): {}",
              triple, lookup_error));
    }
    return NativeTarget{.triple = std::move(triple), .target = target};
  }();
  return host;
}

// A target machine holds state its code generation writes, so each compile
// makes its own. No processor is named, so the code uses what every machine of
// this kind has. The code is position-independent, because the executable the
// host's linker makes by default is. How hard code generation works follows
// the pipeline's level, as a C++ compiler's `-O` sets both.
auto MachineFor(const NativeTarget& host, const llvm::OptimizationLevel& level)
    -> std::unique_ptr<llvm::TargetMachine> {
  const llvm::CodeGenOpt::Level codegen = level == llvm::OptimizationLevel::O0
                                              ? llvm::CodeGenOpt::None
                                              : llvm::CodeGenOpt::Default;
  return std::unique_ptr<llvm::TargetMachine>(host.target->createTargetMachine(
      host.triple, "", "", llvm::TargetOptions{}, llvm::Reloc::PIC_,
      std::nullopt, codegen));
}

void RunPipeline(
    llvm::Module& module, llvm::TargetMachine& machine,
    const llvm::OptimizationLevel& level) {
  llvm::PassBuilder builder(&machine);
  llvm::LoopAnalysisManager loops;
  llvm::FunctionAnalysisManager functions;
  llvm::CGSCCAnalysisManager call_graph;
  llvm::ModuleAnalysisManager modules;
  builder.registerModuleAnalyses(modules);
  builder.registerCGSCCAnalyses(call_graph);
  builder.registerFunctionAnalyses(functions);
  builder.registerLoopAnalyses(loops);
  builder.crossRegisterProxies(loops, functions, call_graph, modules);
  builder.buildPerModuleDefaultPipeline(level).run(module, modules);
}

}  // namespace

auto WriteObjectFile(
    EmittedModule module, const std::filesystem::path& object,
    const llvm::OptimizationLevel& level) -> diag::Result<void> {
  const NativeTarget& host = ThisMachine();
  const std::unique_ptr<llvm::TargetMachine> machine = MachineFor(host, level);
  EmittedModule::Owned owned = std::move(module).Release();
  owned.module->setTargetTriple(host.triple);
  owned.module->setDataLayout(machine->createDataLayout());
  RunPipeline(*owned.module, *machine, level);

  const std::string path = object.string();
  std::error_code opened;
  llvm::raw_fd_ostream out(path, opened, llvm::sys::fs::OF_None);
  if (opened) {
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        std::format(
            "failed to open '{}' for writing: {}", path, opened.message()));
  }
  llvm::legacy::PassManager passes;
  if (machine->addPassesToEmitFile(
          passes, out, nullptr, llvm::CodeGenFileType::CGFT_ObjectFile)) {
    throw InternalError(
        std::format(
            "llvm codegen: the target for {} cannot write an object file",
            host.triple));
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
        std::format("failed to write '{}': {}", path, failed.message()));
  }
  return {};
}

}  // namespace lyra::backend::llvm_backend
