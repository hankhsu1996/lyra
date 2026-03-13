#include "lyra/llvm_backend/emit.hpp"

#include <format>
#include <memory>
#include <mutex>
#include <string>

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
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void InitializeLlvmTargets() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

auto ToCodeGenOpt(OptLevel level) -> llvm::CodeGenOpt::Level {
  switch (level) {
    case OptLevel::kO0:
      return llvm::CodeGenOpt::None;
    case OptLevel::kO1:
      return llvm::CodeGenOpt::Less;
    case OptLevel::kO2:
      return llvm::CodeGenOpt::Default;
    case OptLevel::kO3:
      return llvm::CodeGenOpt::Aggressive;
  }
  throw common::InternalError("ToCodeGenOpt", "unknown OptLevel");
}

}  // namespace

auto CreateHostTargetMachine(OptLevel opt_level)
    -> std::unique_ptr<llvm::TargetMachine> {
  InitializeLlvmTargets();

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName().str();

  llvm::StringMap<bool> feature_map;
  llvm::sys::getHostCPUFeatures(feature_map);
  llvm::SubtargetFeatures subtarget_features;
  for (const auto& kv : feature_map) {
    if (kv.getValue()) {
      subtarget_features.AddFeature(kv.getKey().str());
    }
  }
  std::string features = subtarget_features.getString();

  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(triple, error);
  if (target == nullptr) {
    throw common::InternalError(
        "CreateHostTargetMachine",
        std::format("failed to lookup target for '{}': {}", triple, error));
  }

  std::unique_ptr<llvm::TargetMachine> tm(target->createTargetMachine(
      triple, cpu, features, llvm::TargetOptions(),
      llvm::Reloc::PIC_,  // Required for shared library linking
      std::nullopt, ToCodeGenOpt(opt_level)));
  if (tm == nullptr) {
    throw common::InternalError(
        "CreateHostTargetMachine",
        std::format("failed to create TargetMachine for '{}'", triple));
  }

  return tm;
}

void OptimizeModule(
    llvm::Module& module, llvm::TargetMachine& target_machine,
    OptLevel opt_level) {
  if (opt_level == OptLevel::kO0) return;

  llvm::PassBuilder pb(&target_machine);

  llvm::LoopAnalysisManager lam;
  llvm::FunctionAnalysisManager fam;
  llvm::CGSCCAnalysisManager cgam;
  llvm::ModuleAnalysisManager mam;

  pb.registerModuleAnalyses(mam);
  pb.registerCGSCCAnalyses(cgam);
  pb.registerFunctionAnalyses(fam);
  pb.registerLoopAnalyses(lam);
  pb.crossRegisterProxies(lam, fam, cgam, mam);

  auto level = llvm::OptimizationLevel::O2;
  switch (opt_level) {
    case OptLevel::kO0:
      return;
    case OptLevel::kO1:
      level = llvm::OptimizationLevel::O1;
      break;
    case OptLevel::kO2:
      level = llvm::OptimizationLevel::O2;
      break;
    case OptLevel::kO3:
      level = llvm::OptimizationLevel::O3;
      break;
  }

  auto mpm = pb.buildPerModuleDefaultPipeline(level);
  mpm.run(module, mam);
}

auto EmitObjectFile(
    llvm::Module& module, llvm::TargetMachine& target_machine,
    const std::filesystem::path& output_path)
    -> std::expected<void, std::string> {
  std::error_code ec;
  llvm::raw_fd_ostream dest(output_path.string(), ec, llvm::sys::fs::OF_None);
  if (ec) {
    return std::unexpected(
        std::format(
            "cannot open '{}': {}", output_path.string(), ec.message()));
  }

  llvm::legacy::PassManager pass;
  if (target_machine.addPassesToEmitFile(
          pass, dest, nullptr, llvm::CGFT_ObjectFile)) {
    return std::unexpected("target does not support object file emission");
  }

  pass.run(module);
  dest.flush();
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
