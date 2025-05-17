#include "lyra/driver/driver.hpp"

#include <iostream>
#include <memory>

#include <slang/ast/Compilation.h>

#include "lyra/driver/driver_result.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/simulation_runner.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"

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

  auto context = std::make_unique<interpreter::SimulationContext>();

  interpreter::SimulationRunner runner(*lir, *context);
  runner.Run();

  return DriverResult{
      .compilation = std::move(compilation),
      .context = std::move(context),
      .lir_context = lir->context};
}

}  // namespace lyra::driver
