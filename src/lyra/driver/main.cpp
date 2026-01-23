#include <argparse/argparse.hpp>
#include <exception>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "config.hpp"
#include "dump.hpp"
#include "frontend.hpp"
#include "print.hpp"
#include "run_llvm.hpp"
#include "run_mir.hpp"

namespace {

// Split attached flag forms: -DFOO → -D FOO, -Ipath → -I path, -Wfoo → -W foo
auto PreprocessArgs(std::span<char*> argv) -> std::vector<std::string> {
  std::vector<std::string> result;
  for (char* raw_arg : argv) {
    std::string_view arg = raw_arg;
    if (arg.size() > 2 &&
        (arg.starts_with("-D") || arg.starts_with("-I") ||
         arg.starts_with("-W")) &&
        arg[1] != '-') {
      result.emplace_back(arg.substr(0, 2));
      result.emplace_back(arg.substr(2));
    } else {
      result.emplace_back(arg);
    }
  }
  return result;
}

void AddCompilationFlags(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--top").help("Top module name");
  cmd.add_argument("-I").append().help("Include search path (repeatable)");
  cmd.add_argument("-D").append().help("Preprocessor define (repeatable)");
  cmd.add_argument("-W").append().help("Warning flag (repeatable)");
}

auto BuildInput(
    const argparse::ArgumentParser& cmd,
    const std::optional<lyra::driver::ProjectConfig>& config)
    -> std::optional<lyra::driver::CompilationInput> {
  lyra::driver::CompilationInput input;

  // Files: CLI replaces config entirely
  if (auto files = cmd.present<std::vector<std::string>>("files")) {
    input.files = *files;
  } else if (config) {
    input.files = config->files;
  }

  if (input.files.empty()) {
    lyra::driver::PrintError("no input files");
    return std::nullopt;
  }

  // Top: CLI overrides config (scalar)
  if (auto top = cmd.present<std::string>("--top")) {
    input.top = *top;
  } else if (config) {
    input.top = config->top;
  }

  // List options: config + CLI merged
  if (config) {
    input.incdir = config->incdir;
    input.defines = config->defines;
  }
  if (auto vals = cmd.present<std::vector<std::string>>("-I")) {
    input.incdir.insert(input.incdir.end(), vals->begin(), vals->end());
  }
  if (auto vals = cmd.present<std::vector<std::string>>("-D")) {
    input.defines.insert(input.defines.end(), vals->begin(), vals->end());
  }
  if (auto vals = cmd.present<std::vector<std::string>>("-W")) {
    input.warnings = *vals;
  }

  return input;
}

auto LoadOptionalConfig() -> std::optional<lyra::driver::ProjectConfig> {
  auto config_path = lyra::driver::FindConfig();
  if (!config_path) {
    return std::nullopt;
  }
  return lyra::driver::LoadConfig(*config_path);
}

auto RunCommand(const argparse::ArgumentParser& cmd) -> int {
  auto backend = cmd.get<std::string>("--backend");

  if (backend != "mir" && backend != "llvm") {
    lyra::driver::PrintError(
        "unknown backend '" + backend + "', use 'mir' or 'llvm'");
    return 1;
  }

  std::optional<lyra::driver::ProjectConfig> config;
  try {
    config = LoadOptionalConfig();
  } catch (const std::exception& e) {
    lyra::driver::PrintError(e.what());
    return 1;
  }

  auto input = BuildInput(cmd, config);
  if (!input) {
    return 1;
  }

  if (backend == "llvm") {
    return lyra::driver::RunLlvm(*input);
  }
  return lyra::driver::RunMir(*input);
}

auto DumpCommand(const argparse::ArgumentParser& cmd) -> int {
  auto format = cmd.get<std::string>("format");

  if (format != "hir" && format != "mir" && format != "llvm") {
    lyra::driver::PrintError(
        "unknown format '" + format + "', use 'hir', 'mir', or 'llvm'");
    return 1;
  }

  std::optional<lyra::driver::ProjectConfig> config;
  try {
    config = LoadOptionalConfig();
  } catch (const std::exception& e) {
    lyra::driver::PrintError(e.what());
    return 1;
  }

  auto input = BuildInput(cmd, config);
  if (!input) {
    return 1;
  }

  if (format == "hir") {
    return lyra::driver::DumpHir(*input);
  }
  if (format == "mir") {
    return lyra::driver::DumpMir(*input);
  }
  return lyra::driver::DumpLlvm(*input);
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  auto args = PreprocessArgs(std::span<char*>(argv, static_cast<size_t>(argc)));

  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("SystemVerilog compiler and simulator");

  // Subcommand: run
  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Run simulation");
  run_cmd.add_argument("--backend")
      .default_value(std::string("llvm"))
      .help("Execution backend: llvm (default) or mir (development)");
  AddCompilationFlags(run_cmd);
  run_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  // Subcommand: dump
  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: hir, mir, or llvm");
  AddCompilationFlags(dump_cmd);
  dump_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  program.add_subparser(run_cmd);
  program.add_subparser(dump_cmd);

  try {
    program.parse_args(args);
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
