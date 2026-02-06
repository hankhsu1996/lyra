#pragma once

#include <expected>
#include <filesystem>
#include <memory>
#include <string>

#include "lyra/common/opt_level.hpp"
#include "lyra/llvm_backend/lower.hpp"

namespace lyra::lowering::mir_to_llvm {

// Sub-phase timings from JIT compilation (seconds).
struct JitCompileTimings {
  double create_jit = 0.0;
  double load_runtime = 0.0;
  double add_ir = 0.0;
  double lookup_main = 0.0;
  double codegen = 0.0;          // Object emission (inside lookup_main)
  double linking = 0.0;          // JIT linking/relocation (inside lookup_main)
  double link_graph = 0.0;       // Object parsing to LinkGraph
  double link_alloc = 0.0;       // Prune + memory allocation
  double link_fixup = 0.0;       // Resolve + copy + fixup
  double link_finalize = 0.0;    // Memory finalization
  bool complete = false;         // True only if all phases succeeded
  bool has_link_detail = false;  // True when JITLink sub-phase data available
};

// ORC JIT statistics for diagnosing link-time costs.
struct JitOrcStats {
  std::string linker_backend;      // "JITLink" or "RTDyld"
  int object_count = 0;            // Number of object buffers emitted
  int64_t total_object_bytes = 0;  // Total bytes of emitted objects
  int relocation_count = 0;        // Edges where isRelocation()
  int symbol_count = 0;            // Defined + external symbols
  int section_count = 0;           // LinkGraph sections
  int block_count = 0;             // LinkGraph blocks
};

// Holds compiled JIT state. Must stay alive during simulation
// (LLJIT owns the compiled code memory).
class JitSession {
 public:
  JitSession();
  ~JitSession();
  JitSession(const JitSession&) = delete;
  auto operator=(const JitSession&) -> JitSession& = delete;
  JitSession(JitSession&&) noexcept;
  auto operator=(JitSession&&) noexcept -> JitSession&;

  // Run the compiled simulation. Returns exit code.
  auto Run() -> int;

  // Sub-phase timings from JIT compilation.
  [[nodiscard]] auto Timings() const -> const JitCompileTimings&;

  // ORC JIT statistics (linker backend, object count/size).
  [[nodiscard]] auto OrcStats() const -> const JitOrcStats&;

 private:
  friend auto CompileJit(
      LoweringResult&, const std::filesystem::path&, OptLevel, bool)
      -> std::expected<JitSession, std::string>;
  friend auto CompileJitInProcess(LoweringResult&, OptLevel, bool)
      -> std::expected<JitSession, std::string>;
  struct Impl;
  std::unique_ptr<Impl> impl_;
};

// Compile LLVM module to native code using ORC JIT.
// Takes ownership of result.context and result.module.
// runtime_path: absolute path to liblyra_runtime.so
// emit_progress: if true, print JITLink progress to stderr every 5s
// Returns JitSession on success (call Run() to execute).
auto CompileJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    OptLevel opt_level = OptLevel::kO2, bool emit_progress = false)
    -> std::expected<JitSession, std::string>;

// Compile LLVM module using in-process ORC JIT with host process symbols.
// Runtime symbols (Lyra*) are resolved from the host process rather than
// loading an external shared library. This is for test use where the test
// binary is linked against the runtime library directly.
// Takes ownership of result.context and result.module.
auto CompileJitInProcess(
    LoweringResult& result, OptLevel opt_level = OptLevel::kO2,
    bool emit_progress = false) -> std::expected<JitSession, std::string>;

// One-shot: compile + run. Convenience wrapper for callers that don't need
// separate phases (e.g., test framework).
auto ExecuteWithOrcJit(
    LoweringResult& result, const std::filesystem::path& runtime_path,
    OptLevel opt_level = OptLevel::kO2) -> std::expected<int, std::string>;

auto ExecuteWithOrcJitInProcess(
    LoweringResult& result, OptLevel opt_level = OptLevel::kO2)
    -> std::expected<int, std::string>;

}  // namespace lyra::lowering::mir_to_llvm
