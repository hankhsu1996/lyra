#include "run_jit.hpp"

#include <cstdio>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "pipeline.hpp"
#include "print.hpp"
#include "runtime_path.hpp"
#include "verbose_logger.hpp"

namespace lyra::driver {

namespace {

void PrintMirStats(
    const lowering::hir_to_mir::LoweringStats& stats, FILE* sink = stderr) {
  fmt::print(
      sink,
      "[lyra][stats][mir] place_temps={} value_temps={} "
      "materialize_to_place={} mir_stmts={}\n",
      stats.place_temps, stats.value_temps, stats.materialize_to_place,
      stats.mir_stmts);
  std::fflush(sink);
}

}  // namespace

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
      .enable_trace = input.enable_trace,
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

  // Collect LLVM stats BEFORE JIT (module is consumed during compilation)
  bool emit_stats = input.stats_top_n >= 0;
  LlvmStats llvm_stats;
  if (emit_stats) {
    llvm_stats = CollectLlvmStats(*llvm_result->module);
  }

  // Phase 1: JIT compilation
  std::expected<lowering::mir_to_llvm::JitSession, std::string> session;
  {
    PhaseTimer timer(vlog, "jit_compile");
    session = lowering::mir_to_llvm::CompileJit(
        *llvm_result, runtime_path, input.opt_level);
  }
  if (!session) {
    PrintError(std::format("JIT compilation failed: {}", session.error()));
    return 1;
  }

  // Print ALL compilation stats BEFORE simulation (survives timeout)
  if (emit_stats) {
    PrintMirStats(compilation.mir.stats);
    vlog.PrintPhaseSummary();
    PrintLlvmStats(llvm_stats, input.stats_top_n);
  }

  // Phase 2: simulation
  {
    PhaseTimer timer(vlog, "sim", true);
    return session->Run();
  }
}

}  // namespace lyra::driver
