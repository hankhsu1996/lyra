#include "run_mir.hpp"

#include <cstdio>
#include <iostream>

#include <fmt/core.h>

#include "frontend.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "pipeline.hpp"
#include "print.hpp"
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

auto RunMir(const CompilationInput& input) -> int {
  VerboseLogger vlog(input.verbose);

  auto compilation = CompileToMir(input, vlog);
  if (!compilation) {
    compilation.error().Print();
    return 1;
  }

  // Print compile-pipeline stats before simulation (survives timeout)
  if (input.stats_top_n >= 0) {
    PrintMirStats(compilation->mir.stats);
    vlog.PrintPhaseSummary();
  }

  mir::interp::SimulationResult result;
  {
    PhaseTimer timer(vlog, "run", true);
    result = mir::interp::RunSimulation(
        compilation->mir.design, *compilation->mir.mir_arena,
        *compilation->hir.type_arena, &std::cout, input.plusargs,
        input.fs_base_dir, input.enable_system, nullptr, input.enable_trace);
  }

  if (!result.error_message.empty()) {
    PrintError(result.error_message);
  }

  return result.exit_code;
}

}  // namespace lyra::driver
