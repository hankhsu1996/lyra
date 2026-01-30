#pragma once

#include <expected>
#include <filesystem>
#include <string>

#include "lyra/llvm_backend/lower.hpp"

namespace lyra::lowering::mir_to_llvm {

// Execute LLVM module using in-process ORC JIT.
// Takes ownership of result.context and result.module.
// runtime_path: absolute path to liblyra_runtime.so
// Returns exit code on success, error message on failure.
auto ExecuteWithOrcJit(
    LoweringResult& result, const std::filesystem::path& runtime_path)
    -> std::expected<int, std::string>;

// Execute LLVM module using in-process ORC JIT with host process symbols.
// Runtime symbols (Lyra*) are resolved from the host process rather than
// loading an external shared library. This is for test use where the test
// binary is linked against the runtime library directly.
// Takes ownership of result.context and result.module.
// Returns exit code on success, error message on failure.
auto ExecuteWithOrcJitInProcess(LoweringResult& result)
    -> std::expected<int, std::string>;

}  // namespace lyra::lowering::mir_to_llvm
