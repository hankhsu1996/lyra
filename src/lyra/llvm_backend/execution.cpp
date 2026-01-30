#include "lyra/llvm_backend/execution.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <mutex>
#include <optional>
#include <string>
#include <utility>

#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>

namespace lyra::lowering::mir_to_llvm {

namespace {

// Initialize LLVM native targets (thread-safe, once per process).
void InitializeLlvm() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

// Execute with given symbol resolution strategy.
// If runtime_path is provided, load symbols from that library.
// Otherwise, resolve symbols from the host process.
auto ExecuteWithOrcJitImpl(
    LoweringResult& result,
    const std::optional<std::filesystem::path>& runtime_path)
    -> std::expected<int, std::string> {
  InitializeLlvm();

  // Create LLJIT instance
  auto jit = llvm::orc::LLJITBuilder().create();
  if (!jit) {
    return std::unexpected(
        std::format(
            "failed to create JIT: {}", llvm::toString(jit.takeError())));
  }

  // Add symbol generator for runtime functions
  auto& dylib = (*jit)->getMainJITDylib();
  if (runtime_path) {
    // Load runtime library from shared object file
    auto gen = llvm::orc::DynamicLibrarySearchGenerator::Load(
        runtime_path->string().c_str(),
        (*jit)->getDataLayout().getGlobalPrefix());
    if (!gen) {
      return std::unexpected(
          std::format(
              "failed to load runtime '{}': {}", runtime_path->string(),
              llvm::toString(gen.takeError())));
    }
    dylib.addGenerator(std::move(*gen));
  } else {
    // Resolve symbols from the host process (for tests where runtime is
    // statically linked)
    auto gen = llvm::orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        (*jit)->getDataLayout().getGlobalPrefix());
    if (!gen) {
      return std::unexpected(
          std::format(
              "failed to get host process symbols: {}",
              llvm::toString(gen.takeError())));
    }
    dylib.addGenerator(std::move(*gen));
  }

  // Set module data layout and triple to match JIT target
  result.module->setDataLayout((*jit)->getDataLayout());
  result.module->setTargetTriple((*jit)->getTargetTriple().str());

  // Create thread-safe context and module (takes ownership)
  llvm::orc::ThreadSafeContext tsc(std::move(result.context));
  auto tsm = llvm::orc::ThreadSafeModule(std::move(result.module), tsc);

  // Add module to JIT
  if (auto err = (*jit)->addIRModule(std::move(tsm))) {
    return std::unexpected(
        std::format(
            "failed to add module: {}", llvm::toString(std::move(err))));
  }

  // Lookup and call main (Lyra-internal entry point, not user-accessible)
  auto main_sym = (*jit)->lookup("main");
  if (!main_sym) {
    return std::unexpected(
        std::format(
            "symbol 'main' not found: {}",
            llvm::toString(main_sym.takeError())));
  }

  auto* main_fn = main_sym->toPtr<int()>();
  return main_fn();
}

}  // namespace

auto ExecuteWithOrcJit(
    LoweringResult& result, const std::filesystem::path& runtime_path)
    -> std::expected<int, std::string> {
  return ExecuteWithOrcJitImpl(result, runtime_path);
}

auto ExecuteWithOrcJitInProcess(LoweringResult& result)
    -> std::expected<int, std::string> {
  return ExecuteWithOrcJitImpl(result, std::nullopt);
}

}  // namespace lyra::lowering::mir_to_llvm
