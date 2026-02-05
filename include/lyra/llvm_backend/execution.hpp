#pragma once

#include <expected>
#include <filesystem>
#include <memory>
#include <string>

#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/lower.hpp"

namespace lyra::lowering::mir_to_llvm {

// Holds compiled JIT state. Must stay alive during simulation
// (LLJIT owns the compiled code memory).
class JitSession {
 public:
  JitSession();
  ~JitSession();
  JitSession(JitSession&&) noexcept;
  JitSession& operator=(JitSession&&) noexcept;

  // Run the compiled simulation. Returns exit code.
  auto Run() -> int;

 private:
  friend auto CompileJit(
      LoweringResult&, const std::filesystem::path&, OptLevel)
      -> std::expected<JitSession, std::string>;
  friend auto CompileJitInProcess(LoweringResult&, OptLevel)
      -> std::expected<JitSession, std::string>;
  struct Impl;
  std::unique_ptr<Impl> impl_;
};

// Compile LLVM module to native code using ORC JIT.
// Takes ownership of result.context and result.module.
// runtime_path: absolute path to liblyra_runtime.so
// Returns JitSession on success (call Run() to execute).
auto CompileJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    OptLevel opt_level = OptLevel::kO2)
    -> std::expected<JitSession, std::string>;

// Compile LLVM module using in-process ORC JIT with host process symbols.
// Runtime symbols (Lyra*) are resolved from the host process rather than
// loading an external shared library. This is for test use where the test
// binary is linked against the runtime library directly.
// Takes ownership of result.context and result.module.
auto CompileJitInProcess(
    LoweringResult& result, OptLevel opt_level = OptLevel::kO2)
    -> std::expected<JitSession, std::string>;

// One-shot: compile + run. Convenience wrapper for callers that don't need
// separate phases (e.g., test framework).
auto ExecuteWithOrcJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    OptLevel opt_level = OptLevel::kO2) -> std::expected<int, std::string>;

auto ExecuteWithOrcJitInProcess(
    LoweringResult& result, OptLevel opt_level = OptLevel::kO2)
    -> std::expected<int, std::string>;

}  // namespace lyra::lowering::mir_to_llvm
