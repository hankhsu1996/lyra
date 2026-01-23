#include "run_mir.hpp"

#include <iostream>

#include "lyra/mir/interp/interpreter.hpp"
#include "pipeline.hpp"
#include "print.hpp"

namespace lyra::driver {

auto RunMir(const std::vector<std::string>& files) -> int {
  auto compilation = CompileToMir(files);
  if (!compilation) {
    compilation.error().Print();
    return 1;
  }

  auto result = mir::interp::RunSimulation(
      compilation->mir.design, *compilation->mir.mir_arena,
      *compilation->hir.type_arena, &std::cout);

  if (!result.error_message.empty()) {
    PrintError(result.error_message);
  }

  return result.exit_code;
}

}  // namespace lyra::driver
