#pragma once

#include <expected>
#include <filesystem>
#include <memory>
#include <string>

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

#include "lyra/common/opt_level.hpp"

namespace lyra::lowering::mir_to_llvm {

// Create a TargetMachine configured for the host CPU.
// Initializes LLVM native targets on first call (thread-safe).
auto CreateHostTargetMachine(OptLevel opt_level = OptLevel::kO2)
    -> std::unique_ptr<llvm::TargetMachine>;

// Emit an LLVM module as a native object file.
// The module must have DataLayout/TargetTriple already set (by LowerMirToLlvm).
auto EmitObjectFile(
    llvm::Module& module, llvm::TargetMachine& target_machine,
    const std::filesystem::path& output_path)
    -> std::expected<void, std::string>;

}  // namespace lyra::lowering::mir_to_llvm
