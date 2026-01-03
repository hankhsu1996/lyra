#include <argparse/argparse.hpp>
#include <iostream>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/compiler/codegen.hpp"
#include "lyra/compiler/compiler.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/interpreter.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

namespace {

auto RunCommand(const std::vector<std::string>& files, bool use_interpreter)
    -> int {
  if (files.empty()) {
    std::cerr << "lyra run: no input files\n";
    return 1;
  }

  try {
    if (use_interpreter) {
      lyra::interpreter::Interpreter::RunFromFiles(files);
      return 0;
    }

    // Codegen backend - run simulation
    auto result = lyra::compiler::Compiler::RunFromFiles(files, {});
    if (!result.Success()) {
      std::cerr << "lyra run: " << result.ErrorMessage() << "\n";
      return 1;
    }
    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra run: " << e.what() << "\n";
    return 1;
  }
}

auto EmitCommand(const std::vector<std::string>& files) -> int {
  if (files.empty()) {
    std::cerr << "lyra emit: no input files\n";
    return 1;
  }

  try {
    lyra::frontend::SlangFrontend frontend;
    auto compilation = frontend.LoadFromFiles(files);
    if (!compilation) {
      std::cerr << "lyra emit: failed to parse\n";
      return 1;
    }

    const auto& root = compilation->getRoot();
    auto mir = lyra::lowering::ast_to_mir::AstToMir(root);
    if (!mir) {
      std::cerr << "lyra emit: failed to lower to MIR\n";
      return 1;
    }

    lyra::compiler::Codegen codegen;
    std::cout << codegen.Generate(*mir);
    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra emit: " << e.what() << "\n";
    return 1;
  }
}

auto CheckCommand(const std::vector<std::string>& files) -> int {
  if (files.empty()) {
    std::cerr << "lyra check: no input files\n";
    return 1;
  }

  try {
    lyra::frontend::SlangFrontend frontend;
    auto compilation = frontend.LoadFromFiles(files);
    if (!compilation) {
      std::cerr << "lyra check: failed to parse\n";
      return 1;
    }
    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra check: " << e.what() << "\n";
    return 1;
  }
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("SystemVerilog simulator");

  // Subcommand: run
  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Simulate design");
  run_cmd.add_argument("files").remaining().help("SystemVerilog source files");
  run_cmd.add_argument("--interpret", "-i")
      .flag()
      .help("Use interpreter instead of codegen");

  // Subcommand: emit
  argparse::ArgumentParser emit_cmd("emit");
  emit_cmd.add_description("Emit C++ code to stdout");
  emit_cmd.add_argument("files").remaining().help("SystemVerilog source files");

  // Subcommand: check
  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Parse and validate only");
  check_cmd.add_argument("files").remaining().help(
      "SystemVerilog source files");

  program.add_subparser(run_cmd);
  program.add_subparser(emit_cmd);
  program.add_subparser(check_cmd);

  try {
    program.parse_args(argc, argv);
  } catch (const std::exception& err) {
    std::cerr << err.what() << "\n";
    std::cerr << program;
    return 1;
  }

  if (program.is_subcommand_used("run")) {
    auto files = run_cmd.get<std::vector<std::string>>("files");
    bool use_interpreter = run_cmd.get<bool>("--interpret");
    return RunCommand(files, use_interpreter);
  }

  if (program.is_subcommand_used("emit")) {
    auto files = emit_cmd.get<std::vector<std::string>>("files");
    return EmitCommand(files);
  }

  if (program.is_subcommand_used("check")) {
    auto files = check_cmd.get<std::vector<std::string>>("files");
    return CheckCommand(files);
  }

  // No subcommand provided
  std::cout << program;
  return 0;
}
