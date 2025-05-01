#include "simulation/simulator.hpp"

#include <iostream>
#include <memory>

#include <slang/ast/Compilation.h>

#include "core/execution_context.hpp"
#include "core/simulation_preparation.hpp"
#include "core/simulation_result.hpp"
#include "core/simulation_scheduler.hpp"
#include "frontend/slang_frontend.hpp"
#include "lowering/ast_to_mir/ast_to_mir.hpp"
#include "lowering/mir_to_lir/mir_to_lir.hpp"

namespace lyra {

auto Simulator::RunFromSource(
    const std::string& code, const SimulationOptions& options)
    -> SimulationResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  return RunWithCompilation(std::move(compilation), options);
}

auto Simulator::RunFromFiles(
    const std::vector<std::string>& paths, const SimulationOptions& options)
    -> SimulationResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromFiles(paths);
  return RunWithCompilation(std::move(compilation), options);
}

auto Simulator::RunWithCompilation(
    std::unique_ptr<slang::ast::Compilation> compilation,
    const SimulationOptions& options) -> SimulationResult {
  const auto& root = compilation->getRoot();

  auto mir = lowering::AstToMir(root);
  auto lir = lowering::MirToLir(*mir);

  if (options.dump_lir) {
    std::cout << "[ Dumped LIR ]\n"
              << lir->ToString(common::FormatMode::kContextual) << std::endl;
  }

  auto context = std::make_unique<ExecutionContext>();
  SimulationPreparation::InitializeSignals(*lir, *context);
  auto triggers = SimulationPreparation::BuildVariableTriggerMap(*lir);

  SimulationScheduler scheduler(*lir, *context, std::move(triggers));
  auto final_time = scheduler.Run();

  return SimulationResult{
      .context = std::move(context), .final_time = final_time};
}

}  // namespace lyra
