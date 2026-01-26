#include <argparse/argparse.hpp>
#include <exception>
#include <filesystem>
#include <format>
#include <iostream>
#include <span>
#include <string>

#include "commands.hpp"
#include "input.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "print.hpp"

auto main(int argc, char* argv[]) -> int {
  auto args = lyra::driver::PreprocessArgs(
      std::span<char*>(argv, static_cast<size_t>(argc)));

  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("A modern SystemVerilog simulation toolchain");
  program.add_argument("-C").help("Run as if started in <dir>").metavar("dir");

  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Run simulation");
  run_cmd.add_argument("--backend")
      .default_value(std::string("llvm"))
      .help("Execution backend: llvm (default) or mir (development)");
  lyra::driver::AddCompilationFlags(run_cmd);
  run_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Check source files for errors");
  lyra::driver::AddCompilationFlags(check_cmd);
  check_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: hir, mir, or llvm");
  lyra::driver::AddCompilationFlags(dump_cmd);
  dump_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  argparse::ArgumentParser init_cmd("init");
  init_cmd.add_description("Create a new Lyra project");
  init_cmd.add_argument("name").nargs(0, 1).help("Project name");
  init_cmd.add_argument("--force", "-f")
      .default_value(false)
      .implicit_value(true)
      .help("Overwrite existing lyra.toml");

  program.add_subparser(run_cmd);
  program.add_subparser(check_cmd);
  program.add_subparser(dump_cmd);
  program.add_subparser(init_cmd);

  try {
    program.parse_args(args);
  } catch (const std::exception& err) {
    lyra::driver::PrintError(err.what());
    std::cerr << program;
    return 1;
  }

  // Handle -C before dispatching subcommands
  if (auto dir = program.present("-C")) {
    std::error_code ec;
    std::filesystem::current_path(*dir, ec);
    if (ec) {
      lyra::driver::PrintError(
          std::format("cannot change to '{}': {}", *dir, ec.message()));
      return 1;
    }
  }

  // Safety net: catch any unexpected exceptions from subcommand execution.
  // - InternalError: compiler bug, should not happen in normal operation
  // - Other exceptions: truly unexpected failures from third-party/stdlib code
  try {
    if (program.is_subcommand_used("run")) {
      return lyra::driver::RunCommand(run_cmd);
    }
    if (program.is_subcommand_used("check")) {
      return lyra::driver::CheckCommand(check_cmd);
    }
    if (program.is_subcommand_used("dump")) {
      return lyra::driver::DumpCommand(dump_cmd);
    }
    if (program.is_subcommand_used("init")) {
      return lyra::driver::InitCommand(init_cmd);
    }
  } catch (const lyra::common::InternalError& e) {
    std::cerr << "lyra: internal compiler error: " << e.what() << "\n";
    std::cerr << "This is a bug. Please report it at "
                 "https://github.com/lyra-lang/lyra/issues\n";
    return 1;
  } catch (const std::exception& e) {
    lyra::driver::PrintDiagnostic(
        lyra::Diagnostic::HostError(
            std::format("unexpected error: {}", e.what())));
    return 1;
  } catch (...) {
    lyra::driver::PrintDiagnostic(
        lyra::Diagnostic::HostError("unexpected unknown error"));
    return 1;
  }

  std::cout << program;
  return 0;
}
