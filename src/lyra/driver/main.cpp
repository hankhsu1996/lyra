#include <argparse/argparse.hpp>
#include <cstddef>
#include <cstring>
#include <exception>
#include <filesystem>
#include <format>
#include <iostream>
#include <span>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include "check.hpp"
#include "commands.hpp"
#include "dump.hpp"
#include "input.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "print.hpp"
#include "run_llvm.hpp"
#include "run_mir.hpp"

namespace {

// Find the first "--" separator in argv.
// Returns index of "--" or argv.size() if not found.
// argv[0..result) = main args, argv[result+1..end) = plusargs
auto FindPlusargsSeparator(std::span<char*> argv) -> size_t {
  for (size_t i = 1; i < argv.size(); ++i) {
    if (std::strcmp(argv[i], "--") == 0) {
      return i;
    }
  }
  return argv.size();
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  // Split on first "--": everything before goes to argparse, after is plusargs
  std::span<char*> all_args(argv, static_cast<size_t>(argc));
  size_t sep_idx = FindPlusargsSeparator(all_args);

  // Main args: all_args[0..sep_idx)
  auto args = lyra::driver::PreprocessArgs(all_args.subspan(0, sep_idx));

  // Plusargs: all_args[sep_idx+1..end) - collect verbatim
  std::vector<std::string> plusargs;
  for (size_t i = sep_idx + 1; i < all_args.size(); ++i) {
    plusargs.emplace_back(all_args[i]);
  }

  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("A modern SystemVerilog simulation toolchain");
  program.add_argument("-C").help("Run as if started in <dir>").metavar("dir");
  program.add_argument("--no-project")
      .default_value(false)
      .implicit_value(true)
      .help("Run without a project (ad-hoc mode, CWD-relative paths)");

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

  // Capture --no-project flag (global, affects compilation commands)
  bool no_project = program.get<bool>("--no-project");

  // Helper: prepare input for compilation commands
  auto prepare = [&](const argparse::ArgumentParser& cmd) {
    auto input = lyra::driver::PrepareInput(cmd, no_project);
    if (!input) {
      lyra::driver::PrintDiagnostic(input.error());
    }
    return input;
  };

  // Safety net: catch any unexpected exceptions from subcommand execution.
  // - InternalError: compiler bug, should not happen in normal operation
  // - Other exceptions: truly unexpected failures from third-party/stdlib code
  try {
    if (program.is_subcommand_used("run")) {
      auto backend =
          lyra::driver::ParseBackend(run_cmd.get<std::string>("--backend"));
      if (!backend) {
        lyra::driver::PrintDiagnostic(backend.error());
        return 1;
      }
      auto input = prepare(run_cmd);
      if (!input) return 1;

      input->plusargs = std::move(plusargs);

      return *backend == lyra::driver::Backend::kLlvm
                 ? lyra::driver::RunLlvm(*input)
                 : lyra::driver::RunMir(*input);
    }

    if (program.is_subcommand_used("check")) {
      auto input = prepare(check_cmd);
      if (!input) return 1;
      return lyra::driver::Check(*input);
    }

    if (program.is_subcommand_used("dump")) {
      auto format =
          lyra::driver::ParseDumpFormat(dump_cmd.get<std::string>("format"));
      if (!format) {
        lyra::driver::PrintDiagnostic(format.error());
        return 1;
      }
      auto input = prepare(dump_cmd);
      if (!input) return 1;

      switch (*format) {
        case lyra::driver::DumpFormat::kHir:
          return lyra::driver::DumpHir(*input);
        case lyra::driver::DumpFormat::kMir:
          return lyra::driver::DumpMir(*input);
        case lyra::driver::DumpFormat::kLlvm:
          return lyra::driver::DumpLlvm(*input);
      }
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
