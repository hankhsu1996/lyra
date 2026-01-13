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
#include "lyra/lowering/mir_to_lir/link.hpp"
#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"
#include "lyra/lowering/mir_to_lir/module.hpp"

namespace lyra::interpreter {

using lyra::lowering::ast_to_mir::AstToMir;
using lyra::lowering::mir_to_lir::MirToLir;

auto Interpreter::RunFromSource(
    const std::string& code, const std::string& top,
    const InterpreterOptions& options) -> InterpreterResult {
  frontend::SlangFrontend slang_frontend;
  auto compilation = slang_frontend.LoadFromString(code);
  auto source_manager = slang_frontend.GetSourceManagerPtr();
  return RunWithCompilation(
      std::move(compilation), std::move(source_manager), top, options);
}

auto Interpreter::RunFromFiles(
    const std::vector<std::string>& paths, const std::string& top,
    const InterpreterOptions& options) -> InterpreterResult {
  frontend::SlangFrontend slang_frontend;
  frontend::FrontendOptions frontend_options;
  frontend_options.include_dirs = options.include_dirs;
  frontend_options.defines = options.defines;
  auto compilation = slang_frontend.LoadFromFiles(paths, frontend_options);
  auto source_manager = slang_frontend.GetSourceManagerPtr();
  return RunWithCompilation(
      std::move(compilation), std::move(source_manager), top, options);
}

auto Interpreter::RunWithCompilation(
    std::unique_ptr<slang::ast::Compilation> compilation,
    std::shared_ptr<slang::SourceManager> source_manager,
    const std::string& top, const InterpreterOptions& options)
    -> InterpreterResult {
  // Get modules from AST. If top is specified, returns hierarchy in order.
  // If top is empty, returns all modules (for backwards compatibility).
  auto lowering_result = AstToMir(*compilation, top);

  // Lower all modules to LIR
  std::vector<std::unique_ptr<lir::Module>> lir_modules;
  lir_modules.reserve(lowering_result.modules.size());
  for (const auto& mir : lowering_result.modules) {
    lir_modules.push_back(MirToLir(*mir));
  }

  // Lower packages to LIR (init process + functions)
  auto pkg_result =
      lowering::mir_to_lir::LowerPackages(lowering_result.packages);

  // Link phase: resolve references to pointers.
  // Must be called after all lowering completes and before simulation starts.
  lowering::mir_to_lir::LinkFunctionCalls(lir_modules, pkg_result.functions);
  lowering::mir_to_lir::LinkSubmodules(lir_modules);

  if (options.dump_lir) {
    std::cout << "[ Dumped LIR - " << lir_modules.size() << " modules ]\n";
    for (const auto& lir : lir_modules) {
      std::cout << lir->ToString(common::FormatMode::kContextual) << "\n"
                << std::endl;
    }
  }

  auto context = std::make_unique<SimulationContext>();
  // Initialize plusargs from options
  context->plusargs = sdk::PlusargsTable(options.plusargs);
  // Use multi-module constructor for hierarchical support
  SimulationRunner runner(
      lir_modules, lowering_result.packages, pkg_result.init_process,
      pkg_result.context, std::move(pkg_result.functions), *context);
  runner.Run();

  return InterpreterResult{
      .compilation = std::move(compilation),
      .context = std::move(context),
      .lir_context = lir_modules.back()->context,  // Last is top module
      .top_instance = runner.GetTopInstance(),
      .source_manager = std::move(source_manager)};
}

}  // namespace lyra::interpreter
