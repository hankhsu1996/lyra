#include "run_jit.hpp"

#include <cstdio>
#include <format>
#include <string>
#include <unistd.h>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TimeProfiler.h>

#include "frontend.hpp"
#include "llvm_stats.hpp"
#include "lyra/llvm_backend/execution.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "pipeline.hpp"
#include "print.hpp"
#include "process_stats.hpp"
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

// RAII guard for LLVM time-trace profiling.
class TimeTraceGuard {
 public:
  explicit TimeTraceGuard(bool enabled) : enabled_(enabled) {
    if (!enabled_) return;
    llvm::timeTraceProfilerInitialize(500, "lyra");
    filename_ =
        std::format("lyra-jit-{}.time-trace.json", static_cast<int>(getpid()));
  }

  ~TimeTraceGuard() {
    if (!enabled_) return;
    if (auto err = llvm::timeTraceProfilerWrite(filename_, "lyra")) {
      fmt::print(
          stderr, "[lyra] warning: failed to write time-trace: {}\n",
          llvm::toString(std::move(err)));
    } else {
      fmt::print(stderr, "[lyra] time-trace written to {}\n", filename_);
    }
    std::fflush(stderr);
    llvm::timeTraceProfilerCleanup();
  }

  TimeTraceGuard(const TimeTraceGuard&) = delete;
  auto operator=(const TimeTraceGuard&) -> TimeTraceGuard& = delete;
  TimeTraceGuard(TimeTraceGuard&&) = delete;
  auto operator=(TimeTraceGuard&&) -> TimeTraceGuard& = delete;

 private:
  bool enabled_;
  std::string filename_;
};

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

  // Print pre-JIT stats now so they survive if JIT times out.
  if (emit_stats) {
    PrintMirStats(compilation.mir.stats);
    vlog.PrintPhaseSummary();
    PrintLlvmStats(llvm_stats, input.stats_top_n);
    PrintProcessStats(
        compilation.mir.design, *compilation.mir.mir_arena,
        compilation.mir.origin_map, *compilation.hir.hir_arena,
        *compilation.hir.source_manager, llvm_stats);
  }

  // Phase 1: JIT compilation
  TimeTraceGuard time_trace_guard(input.time_trace);
  std::expected<lowering::mir_to_llvm::JitSession, std::string> session;
  {
    PhaseTimer timer(vlog, "jit_compile");
    session = lowering::mir_to_llvm::CompileJit(
        *llvm_result, runtime_path, input.opt_level, emit_stats);
  }
  if (!session) {
    PrintError(std::format("JIT compilation failed: {}", session.error()));
    return 1;
  }

  // Print JIT/ORC stats after compilation.
  if (emit_stats) {
    const auto& jt = session->Timings();
    if (jt.complete) {
      fmt::print(
          stderr,
          "[lyra][stats][jit] create_jit={:.3f}s load_runtime={:.3f}s "
          "add_ir={:.3f}s lookup_main={:.3f}s "
          "codegen={:.3f}s linking={:.3f}s",
          jt.create_jit, jt.load_runtime, jt.add_ir, jt.lookup_main, jt.codegen,
          jt.linking);
      if (jt.has_link_detail) {
        fmt::print(
            stderr,
            " link_graph={:.3f}s link_alloc={:.3f}s"
            " link_fixup={:.3f}s link_finalize={:.3f}s",
            jt.link_graph, jt.link_alloc, jt.link_fixup, jt.link_finalize);
        if (jt.finalize_perm > 0.0 || jt.finalize_overhead > 0.0) {
          fmt::print(
              stderr, " finalize_perm={:.3f}s finalize_overhead={:.3f}s",
              jt.finalize_perm, jt.finalize_overhead);
        }
      }
      fmt::print(stderr, "\n");
      std::fflush(stderr);
    }
    const auto& orc = session->OrcStats();
    fmt::print(
        stderr, "[lyra][stats][orc] linker={} objects={} obj_bytes={}",
        orc.linker_backend, orc.object_count, orc.total_object_bytes);
    if (orc.relocation_count > 0) {
      fmt::print(
          stderr, " relocs={} syms={} sections={} blocks={}",
          orc.relocation_count, orc.symbol_count, orc.section_count,
          orc.block_count);
    }
    if (orc.finalize_segments > 0) {
      fmt::print(
          stderr, " finalize_segments={} finalize_bytes={}",
          orc.finalize_segments, orc.finalize_bytes);
    }
    fmt::print(stderr, "\n");
    std::fflush(stderr);
  }

  // Phase 2: simulation
  {
    PhaseTimer timer(vlog, "sim", true);
    return session->Run();
  }
}

}  // namespace lyra::driver
