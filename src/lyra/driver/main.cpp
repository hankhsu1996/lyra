#include <argparse/argparse.hpp>
#include <exception>
#include <optional>
#include <string>
#include <vector>

#include "config.hpp"
#include "dump.hpp"
#include "print.hpp"
#include "run_llvm.hpp"
#include "run_mir.hpp"

namespace {

auto RunCommand(const argparse::ArgumentParser& cmd) -> int {
  auto backend = cmd.get<std::string>("--backend");

  // Validate backend
  if (backend != "mir" && backend != "llvm") {
    lyra::driver::PrintError(
        "unknown backend '" + backend + "', use 'mir' or 'llvm'");
    return 1;
  }

  // Get CLI files (if provided)
  std::vector<std::string> cli_files;
  if (auto files = cmd.present<std::vector<std::string>>("files")) {
    cli_files = *files;
  }

  std::vector<std::string> files;

  // Try to load lyra.toml
  auto config_path = lyra::driver::FindConfig();
  if (config_path) {
    try {
      auto config = lyra::driver::LoadConfig(*config_path);
      files = config.files;
      files.insert(files.end(), cli_files.begin(), cli_files.end());
    } catch (const std::exception& e) {
      lyra::driver::PrintError(e.what());
      return 1;
    }
  } else if (!cli_files.empty()) {
    files = cli_files;
  } else {
    lyra::driver::PrintError("no lyra.toml found and no files specified");
    return 1;
  }

  if (backend == "llvm") {
    return lyra::driver::RunLlvm(files);
  }
  return lyra::driver::RunMir(files);
}

auto DumpCommand(const argparse::ArgumentParser& cmd) -> int {
  std::string format = cmd.get<std::string>("format");
  std::string file = cmd.get<std::string>("file");

  if (format == "hir") {
    return lyra::driver::DumpHir(file);
  }
  if (format == "mir") {
    return lyra::driver::DumpMir(file);
  }
  if (format == "llvm") {
    return lyra::driver::DumpLlvm(file);
  }

  lyra::driver::PrintError(
      "unknown format '" + format + "', use 'hir', 'mir', or 'llvm'");
  return 1;
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("SystemVerilog compiler and simulator");

  // Subcommand: run
  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Run simulation");
  run_cmd.add_argument("--backend")
      .default_value(std::string("llvm"))
      .help("Execution backend: llvm (default) or mir (development)");
  run_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  // Subcommand: dump
  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: hir, mir, or llvm");
  dump_cmd.add_argument("file").help("SystemVerilog source file");

  program.add_subparser(run_cmd);
  program.add_subparser(dump_cmd);

  try {
    program.parse_args(argc, argv);
  } catch (const std::exception& err) {
    lyra::driver::PrintError(err.what());
    std::cerr << program;
    return 1;
  }

  if (program.is_subcommand_used("run")) {
    return RunCommand(run_cmd);
  }

  if (program.is_subcommand_used("dump")) {
    return DumpCommand(dump_cmd);
  }

  // No subcommand provided
  std::cout << program;
  return 0;
}
