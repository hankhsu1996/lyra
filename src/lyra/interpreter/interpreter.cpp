#include "lyra/interpreter/interpreter.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/common/diagnostic.hpp"
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
    const std::string& code, const std::string& top,
    const InterpreterOptions& options) -> InterpreterResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  return RunWithCompilation(std::move(compilation), top, options);
}

auto Interpreter::RunFromFiles(
    const std::vector<std::string>& paths, const std::string& top,
    const InterpreterOptions& options) -> InterpreterResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromFiles(paths);
  return RunWithCompilation(std::move(compilation), top, options);
}

auto Interpreter::RunWithCompilation(
    std::unique_ptr<slang::ast::Compilation> compilation,
    const std::string& top, const InterpreterOptions& options)
    -> InterpreterResult {
  const auto& root = compilation->getRoot();

  // Get modules from AST. If top is specified, returns hierarchy in order.
  // If top is empty, returns all modules (for backwards compatibility).
  auto modules = AstToMir(root, top);

  // Lower all modules to LIR
  std::vector<std::unique_ptr<lir::Module>> lir_modules;
  lir_modules.reserve(modules.size());
  for (const auto& mir : modules) {
    lir_modules.push_back(MirToLir(*mir));
  }

  if (options.dump_lir) {
    std::cout << "[ Dumped LIR - " << lir_modules.size() << " modules ]\n";
    for (const auto& lir : lir_modules) {
      std::cout << lir->ToString(common::FormatMode::kContextual) << "\n"
                << std::endl;
    }
  }

  auto context = std::make_unique<SimulationContext>();
  // Use multi-module constructor for hierarchical support
  SimulationRunner runner(lir_modules, *context);
  runner.Run();

  return InterpreterResult{
      .compilation = std::move(compilation),
      .context = std::move(context),
      .lir_context = lir_modules.back()->context,  // Last is top module
      .top_instance = runner.GetTopInstance()};
}

}  // namespace lyra::interpreter
