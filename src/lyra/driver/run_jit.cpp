#include "run_jit.hpp"

#include <format>
#include <string>
#include <utility>
#include <vector>

#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "pipeline.hpp"
#include "print.hpp"
#include "runtime_path.hpp"
#include "verbose_logger.hpp"

namespace lyra::driver {

auto RunJit(const CompilationInput& input) -> int {
  VerboseLogger vlog(input.verbose);

  auto result = CompileToMir(input, vlog);
  if (!result) {
    result.error().Print();
    return 1;
  }
  auto compilation = std::move(*result);

  // Create diagnostic context for LLVM backend error reporting
  lowering::OriginMapLookup origin_lookup(
      &compilation.mir.origin_map, compilation.hir.hir_arena.get());
  lowering::DiagnosticContext diag_ctx(origin_lookup);

  lowering::mir_to_llvm::LoweringInput llvm_input{
      .design = &compilation.mir.design,
      .mir_arena = compilation.mir.mir_arena.get(),
      .type_arena = compilation.hir.type_arena.get(),
      .diag_ctx = &diag_ctx,
      .fs_base_dir = input.fs_base_dir.string(),
      .plusargs = input.plusargs,
  };

  std::expected<lowering::mir_to_llvm::LoweringResult, Diagnostic> llvm_result;
  {
    PhaseTimer timer(vlog, "lower_llvm");
    llvm_result = lowering::mir_to_llvm::LowerMirToLlvm(llvm_input);
  }
  if (!llvm_result) {
    PrintDiagnostic(llvm_result.error(), *compilation.hir.source_manager);
    return 1;
  }

  if (input.stats_top_n >= 0) {
    PrintLlvmStats(*llvm_result->module, input.stats_top_n);
  }

  std::vector<std::string> tried_paths;
  auto runtime_path = FindRuntimeLibrary(tried_paths);
  if (runtime_path.empty()) {
    std::string msg = "runtime library not found\n       tried:";
    for (const auto& path : tried_paths) {
      msg += std::format("\n         - {}", path);
    }
    msg += "\n       hint: set LYRA_RUNTIME_PATH environment variable";
    PrintError(msg);
    return 1;
  }

  std::expected<int, std::string> exec_result;
  {
    PhaseTimer timer(vlog, "jit", true);
    exec_result = lowering::mir_to_llvm::ExecuteWithOrcJit(
        *llvm_result, runtime_path, input.opt_level);
  }
  if (!exec_result) {
    PrintError(std::format("JIT execution failed: {}", exec_result.error()));
    return 1;
  }

  return *exec_result;
}

}  // namespace lyra::driver
