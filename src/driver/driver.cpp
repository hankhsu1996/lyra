#include "driver/driver.hpp"

#include <iostream>
#include <memory>

#include <slang/ast/Compilation.h>

#include "driver/driver_result.hpp"
#include "frontend/slang_frontend.hpp"
#include "interpreter/simulation_runner.hpp"
#include "lowering/ast_to_mir/ast_to_mir.hpp"
#include "lowering/mir_to_lir/mir_to_lir.hpp"
#include "runtime/execution_context.hpp"

namespace lyra::driver {

auto Driver::RunFromSource(
    const std::string& code, const DriverOptions& options) -> DriverResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  return RunWithCompilation(std::move(compilation), options);
}

auto Driver::RunFromFiles(
    const std::vector<std::string>& paths, const DriverOptions& options)
    -> DriverResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromFiles(paths);
  return RunWithCompilation(std::move(compilation), options);
}

auto Driver::RunWithCompilation(
    std::unique_ptr<slang::ast::Compilation> compilation,
    const DriverOptions& options) -> DriverResult {
  const auto& root = compilation->getRoot();

  auto mir = lowering::AstToMir(root);
  auto lir = lowering::MirToLir(*mir);

  if (options.dump_lir) {
    std::cout << "[ Dumped LIR ]\n"
              << lir->ToString(common::FormatMode::kContextual) << std::endl;
  }

  auto context = std::make_unique<ExecutionContext>();

  interpreter::SimulationRunner runner(*lir, *context);
  auto final_time = runner.Run();

  return DriverResult{.context = std::move(context), .final_time = final_time};
}

}  // namespace lyra::driver
