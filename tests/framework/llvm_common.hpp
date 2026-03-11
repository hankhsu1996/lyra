#pragma once

#include <expected>
#include <filesystem>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Find a runtime library by name in runfiles or sibling path.
auto FindRuntimeLibrary(std::string_view lib_name)
    -> std::optional<std::filesystem::path>;

// Result of spawning a subprocess with captured stdout/stderr.
struct SubprocessResult {
  int exit_code = -1;
  std::string stdout_text;
  std::string stderr_text;
};

// Environment variable override for subprocess execution.
using EnvOverrides = std::vector<std::pair<std::string, std::string>>;

// Spawn a subprocess, capturing stdout and stderr concurrently.
// Optional env_overrides set/replace environment variables for the child.
auto RunSubprocess(
    const std::filesystem::path& exe, std::span<const std::string> args,
    const EnvOverrides& env_overrides = {}) -> SubprocessResult;

// Link a test executable against the shared runtime library.
// Test-only path: uses dynamic linking for fast iteration (no rpath, no
// bundling). Returns {exe_path, runtime_dir} on success. The caller must
// set LD_LIBRARY_PATH=runtime_dir when executing the binary.
struct TestLinkResult {
  std::filesystem::path exe_path;
  std::filesystem::path runtime_dir;  // directory containing the .so
};

auto LinkTestExecutable(
    const std::filesystem::path& object_path,
    const std::filesystem::path& output_dir, const std::string& name)
    -> std::expected<TestLinkResult, std::string>;

// Result of LLVM IR preparation (owns all lowering artifacts).
struct LlvmPreparationResult {
  // Lowering artifacts (must be kept alive for llvm_result to be valid)
  lowering::ast_to_hir::LoweringResult hir_result;
  lowering::hir_to_mir::LoweringResult mir_result;
  std::unique_ptr<lowering::OriginMapLookup> origin_lookup;
  std::unique_ptr<lowering::DiagnosticContext> diag_ctx;

  // The LLVM module (references type_arena from hir_result)
  lowering::mir_to_llvm::LoweringResult llvm_result;

  // Compiler observability output (e.g., specialization map dump).
  // Separate from simulation output; routed to TestResult::compiler_output.
  std::string compiler_output;

  // Frontend phase timings (seconds).
  double parse_seconds = 0.0;
  double hir_lower_seconds = 0.0;
  double mir_lower_seconds = 0.0;
  double llvm_lower_seconds = 0.0;
};

// Prepare LLVM IR from test case.
// Handles AST -> HIR -> MIR -> LLVM lowering with test hooks for variable
// inspection. work_directory is used for file I/O tests.
auto PrepareLlvmModule(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state = false)
    -> std::expected<LlvmPreparationResult, std::string>;

}  // namespace lyra::test
