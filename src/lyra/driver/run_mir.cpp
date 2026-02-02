#include "run_mir.hpp"

#include <iostream>

#include "frontend.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "pipeline.hpp"
#include "print.hpp"
#include "verbose_logger.hpp"

namespace lyra::driver {

auto RunMir(const CompilationInput& input) -> int {
  VerboseLogger vlog(input.verbose);

  auto compilation = CompileToMir(input, vlog);
  if (!compilation) {
    compilation.error().Print();
    return 1;
  }

  mir::interp::SimulationResult result;
  {
    PhaseTimer timer(vlog, "run");
    result = mir::interp::RunSimulation(
        compilation->mir.design, *compilation->mir.mir_arena,
        *compilation->hir.type_arena, &std::cout, input.plusargs,
        input.fs_base_dir);
  }

  if (!result.error_message.empty()) {
    PrintError(result.error_message);
  }

  return result.exit_code;
}

}  // namespace lyra::driver
