#include <iostream>
#include <memory>
#include <string>

#include <mir/module.hpp>
#include <slang/ast/Compilation.h>
#include <spdlog/spdlog.h>

#include "core/execution_context.hpp"
#include "core/simulation_scheduler.hpp"
#include "frontend/slang_frontend.hpp"
#include "lowering/ast_to_mir/ast_to_mir.hpp"
#include "lowering/mir_to_lir/mir_to_lir.hpp"

auto main() -> int {
  spdlog::set_level(spdlog::level::debug);
  spdlog::flush_on(spdlog::level::debug);

  std::cout << "===== Lyra Simulator Prototype =====\n";

  const std::string test_file_path = "src/main/test.sv";
  std::cout << "Parsing file: " << test_file_path << "\n";

  lyra::frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromFiles({test_file_path});
  const auto& root = compilation->getRoot();

  // Convert AST to MIR
  auto mir = lyra::lowering::AstToMir(root);

  // Convert MIR to LIR
  auto lir = lyra::lowering::MirToLir(*mir);

  // Dump LIR
  std::cout << "\n--- LIR Dump ---\n";
  std::cout << *lir << "\n";

  // Run interpreter
  std::cout << "\n--- Simulation Result ---\n";

  // Prepare Simulation
  lyra::ExecutionContext execution_context;
  lyra::SimulationPreparation::InitializeSignals(*lir, execution_context);
  lyra::VariableTriggerMap variable_triggers =
      lyra::SimulationPreparation::BuildVariableTriggerMap(*lir);

  // Create and Run SimulationScheduler
  lyra::SimulationScheduler scheduler(
      *lir, execution_context, std::move(variable_triggers));
  scheduler.Run();

  // Output final signal values
  for (const auto& signal : lir->signals) {
    const auto& val = execution_context.signal_table.Read(signal);
    std::cout << signal << " = " << val.AsInt() << "\n";
  }

  return 0;
}
