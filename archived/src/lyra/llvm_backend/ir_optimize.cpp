#include "lyra/llvm_backend/ir_optimize.hpp"

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/opt_level.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto ToLlvmOptimizationLevel(OptLevel level) -> llvm::OptimizationLevel {
  switch (level) {
    case OptLevel::kO1:
      return llvm::OptimizationLevel::O1;
    case OptLevel::kO2:
      return llvm::OptimizationLevel::O2;
    case OptLevel::kO3:
      return llvm::OptimizationLevel::O3;
    case OptLevel::kO0:
      break;
  }
  throw common::InternalError(
      "ToLlvmOptimizationLevel", "called with O0 (no pipeline)");
}

}  // namespace

void OptimizeModule(llvm::Module& module, OptLevel level) {
  // Pre-optimization verification: catches lowering bugs.
  if (llvm::verifyModule(module, &llvm::errs())) {
    throw common::InternalError(
        "OptimizeModule", "LLVM IR verification failed before optimization");
  }

  if (level == OptLevel::kO0) return;

  // Standard LLVM new PassManager setup.
  llvm::PassBuilder pb;
  llvm::LoopAnalysisManager lam;
  llvm::FunctionAnalysisManager fam;
  llvm::CGSCCAnalysisManager cgam;
  llvm::ModuleAnalysisManager mam;

  pb.registerModuleAnalyses(mam);
  pb.registerCGSCCAnalyses(cgam);
  pb.registerFunctionAnalyses(fam);
  pb.registerLoopAnalyses(lam);
  pb.crossRegisterProxies(lam, fam, cgam, mam);

  // Use LLVM's standard default pipeline for the requested level.
  // The optimization recipe is owned by LLVM, not by Lyra.
  llvm::OptimizationLevel llvm_level = ToLlvmOptimizationLevel(level);
  llvm::ModulePassManager mpm = pb.buildPerModuleDefaultPipeline(llvm_level);
  mpm.run(module, mam);

  // Post-optimization verification: catches pipeline integration issues.
  if (llvm::verifyModule(module, &llvm::errs())) {
    throw common::InternalError(
        "OptimizeModule", "LLVM IR verification failed after optimization");
  }
}

}  // namespace lyra::lowering::mir_to_llvm
