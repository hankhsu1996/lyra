#include "lyra/llvm_backend/execution.hpp"

#include <iostream>

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"

namespace lyra::lowering::mir_to_llvm {

auto ExecuteJit(LoweringResult& result) -> int {
  // Initialize LLVM targets for JIT
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();

  // Verify module
  std::string error;
  llvm::raw_string_ostream error_stream(error);
  if (llvm::verifyModule(*result.module, &error_stream)) {
    std::cerr << "LLVM module verification failed:\n" << error << "\n";
    return 1;
  }

  // Create execution engine
  std::string engine_error;
  std::unique_ptr<llvm::ExecutionEngine> engine(
      llvm::EngineBuilder(std::move(result.module))
          .setErrorStr(&engine_error)
          .setEngineKind(llvm::EngineKind::JIT)
          .create());

  if (!engine) {
    std::cerr << "Failed to create execution engine: " << engine_error << "\n";
    return 1;
  }

  // Find and run main
  auto* main_func = engine->FindFunctionNamed("main");
  if (main_func == nullptr) {
    std::cerr << "Could not find main function\n";
    return 1;
  }

  // Run the function
  auto result_val = engine->runFunction(main_func, {});
  return static_cast<int>(result_val.IntVal.getZExtValue());
}

}  // namespace lyra::lowering::mir_to_llvm
