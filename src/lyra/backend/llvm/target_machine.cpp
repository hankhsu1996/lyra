#include "lyra/backend/llvm/target_machine.hpp"

#include <format>
#include <optional>
#include <string>
#include <utility>

#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>

#include "lyra/base/internal_error.hpp"

namespace lyra::backend::llvm_backend {

auto ProcessTarget() -> const CompileTarget& {
  // A function-local static is initialized exactly once however many compiles
  // reach it at the same moment.
  static const CompileTarget host = [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    // Reading the symbol table of bitcode that holds inline assembly parses it.
    llvm::InitializeNativeTargetAsmParser();
    llvm::Triple triple(llvm::sys::getProcessTriple());
    std::string lookup_error;
    const llvm::Target* target =
        llvm::TargetRegistry::lookupTarget(triple, lookup_error);
    if (target == nullptr) {
      throw InternalError(
          std::format(
              "llvm codegen: this compiler carries no target for the machine "
              "it runs on ({}): {}",
              triple.str(), lookup_error));
    }
    return CompileTarget{.triple = std::move(triple), .target = target};
  }();
  return host;
}

auto CodeGenLevelOf(llvm::OptimizationLevel level) -> llvm::CodeGenOptLevel {
  const int number = static_cast<int>(level);
  const std::optional<llvm::CodeGenOptLevel> paired =
      llvm::CodeGenOpt::getLevel(number);
  if (!paired) {
    throw InternalError(
        std::format(
            "llvm codegen: no code generation level pairs with -O{}", number));
  }
  return *paired;
}

}  // namespace lyra::backend::llvm_backend
