#pragma once

#include <expected>
#include <filesystem>
#include <memory>
#include <optional>
#include <span>
#include <string>

#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Find liblyra_runtime.so in runfiles or sibling path.
auto FindRuntimeLibrary() -> std::optional<std::filesystem::path>;

// Result of spawning a subprocess with captured stdout/stderr.
struct SubprocessResult {
  int exit_code = -1;
  std::string stdout_text;
  std::string stderr_text;
};

// Spawn a subprocess, capturing stdout and stderr concurrently.
auto RunSubprocess(
    const std::filesystem::path& exe, std::span<const std::string> args)
    -> SubprocessResult;

// Result of LLVM IR preparation (owns all lowering artifacts).
struct LlvmPreparationResult {
  // Lowering artifacts (must be kept alive for llvm_result to be valid)
  lowering::ast_to_hir::LoweringResult hir_result;
  lowering::hir_to_mir::LoweringResult mir_result;
  std::unique_ptr<lowering::OriginMapLookup> origin_lookup;
  std::unique_ptr<lowering::DiagnosticContext> diag_ctx;

  // The LLVM module (references type_arena from hir_result)
  lowering::mir_to_llvm::LoweringResult llvm_result;
};

// Prepare LLVM IR from test case.
// Handles AST -> HIR -> MIR -> LLVM lowering with test hooks for variable
// inspection. work_directory is used for file I/O tests.
auto PrepareLlvmModule(
    const TestCase& test_case, const std::filesystem::path& work_directory,
    bool force_two_state = false, bool share_procs = false)
    -> std::expected<LlvmPreparationResult, std::string>;

}  // namespace lyra::test
