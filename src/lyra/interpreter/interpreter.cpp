#include "lyra/interpreter/interpreter.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/common/indent.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/interpreter_options.hpp"
#include "lyra/interpreter/interpreter_result.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/simulation_runner.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"

namespace lyra::interpreter {

using lyra::lowering::ast_to_mir::AstToMir;
using lyra::lowering::mir_to_lir::MirToLir;

auto Interpreter::RunFromSource(
    const std::string& code, const InterpreterOptions& options)
    -> InterpreterResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  return RunWithCompilation(std::move(compilation), options);
}

auto Interpreter::RunFromFiles(
    const std::vector<std::string>& paths, const InterpreterOptions& options)
    -> InterpreterResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromFiles(paths);
  return RunWithCompilation(std::move(compilation), options);
}

auto Interpreter::RunWithCompilation(
    std::unique_ptr<slang::ast::Compilation> compilation,
    const InterpreterOptions& options) -> InterpreterResult {
  const auto& root = compilation->getRoot();

  auto mir = AstToMir(root);
  auto lir = MirToLir(*mir);

  if (options.dump_lir) {
    std::cout << "[ Dumped LIR ]\n"
              << lir->ToString(common::FormatMode::kContextual) << std::endl;
  }

  auto context = std::make_unique<SimulationContext>();

  SimulationRunner runner(*lir, *context);
  runner.Run();

  return InterpreterResult{
      .compilation = std::move(compilation),
      .context = std::move(context),
      .lir_context = lir->context};
}

}  // namespace lyra::interpreter
