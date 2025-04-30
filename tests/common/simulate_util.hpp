#pragma once

#include <memory>
#include <string>

#include <core/execution_context.hpp>
#include <core/simulation_preparation.hpp>
#include <core/simulation_scheduler.hpp>
#include <frontend/slang_frontend.hpp>
#include <lowering/ast_to_mir/ast_to_mir.hpp>
#include <lowering/mir_to_lir/mir_to_lir.hpp>
#include <mir/module.hpp>
#include <slang/ast/Compilation.h>

namespace lyra::test {

// Simulate a given SV string and return the final ExecutionContext
inline auto Simulate(const std::string& code)
    -> std::shared_ptr<ExecutionContext> {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  const auto& root = compilation->getRoot();
  auto mir = lowering::AstToMir(root);
  auto lir = lowering::MirToLir(*mir);

  auto execution_context = std::make_shared<ExecutionContext>();
  SimulationPreparation::InitializeSignals(*lir, *execution_context);
  auto triggers = SimulationPreparation::BuildVariableTriggerMap(*lir);
  SimulationScheduler scheduler(*lir, *execution_context, std::move(triggers));
  scheduler.Run();
  return execution_context;
}

}  // namespace lyra::test
