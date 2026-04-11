#pragma once

#include <expected>
#include <filesystem>
#include <string>

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

namespace lyra::lowering::mir_to_llvm {

// Emit an LLVM module as a native object file.
// The module must have DataLayout/TargetTriple already set (by LowerMirToLlvm).
auto EmitObjectFile(
    llvm::Module& module, llvm::TargetMachine& target_machine,
    const std::filesystem::path& output_path)
    -> std::expected<void, std::string>;

}  // namespace lyra::lowering::mir_to_llvm
