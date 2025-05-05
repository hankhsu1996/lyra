#include "driver/driver.hpp"

#include <iostream>
#include <memory>

#include <slang/ast/Compilation.h>

#include "core/execution_context.hpp"
#include "core/simulation_result.hpp"
#include "frontend/slang_frontend.hpp"
#include "interpreter/lir_simulation_scheduler.hpp"
#include "lowering/ast_to_mir/ast_to_mir.hpp"
#include "lowering/mir_to_lir/mir_to_lir.hpp"

namespace lyra::driver {

auto Driver::RunFromSource(
    const std::string& code, const DriverOptions& options) -> SimulationResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  return RunWithCompilation(std::move(compilation), options);
}

auto Driver::RunFromFiles(
    const std::vector<std::string>& paths, const DriverOptions& options)
    -> SimulationResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromFiles(paths);
  return RunWithCompilation(std::move(compilation), options);
}

auto Driver::RunWithCompilation(
    std::unique_ptr<slang::ast::Compilation> compilation,
    const DriverOptions& options) -> SimulationResult {
  const auto& root = compilation->getRoot();

  auto mir = lowering::AstToMir(root);
  auto lir = lowering::MirToLir(*mir);

  if (options.dump_lir) {
    std::cout << "[ Dumped LIR ]\n"
              << lir->ToString(common::FormatMode::kContextual) << std::endl;
  }

  auto context = std::make_unique<ExecutionContext>();

  interpreter::LIRSimulationScheduler scheduler(*lir, *context);
  auto final_time = scheduler.Run();

  return SimulationResult{
      .context = std::move(context), .final_time = final_time};
}

}  // namespace lyra::driver
