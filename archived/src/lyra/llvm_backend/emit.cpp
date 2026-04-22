#include "lyra/llvm_backend/emit.hpp"

#include <format>
#include <string>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>

namespace lyra::lowering::mir_to_llvm {

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
