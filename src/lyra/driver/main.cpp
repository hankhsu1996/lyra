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

#include <fmt/core.h>

#include "check.hpp"
#include "commands.hpp"
#include "compilation_output.hpp"
#include "compile.hpp"
#include "dump.hpp"
#include "input.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "run_aot.hpp"
#include "run_jit.hpp"
#include "run_lli.hpp"

namespace {

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
  std::span<char*> all_args(argv, static_cast<size_t>(argc));
  size_t sep_idx = FindPlusargsSeparator(all_args);

  auto args = lyra::driver::PreprocessArgs(all_args.subspan(0, sep_idx));

  std::vector<std::string> plusargs;
  for (size_t i = sep_idx + 1; i < all_args.size(); ++i) {
    plusargs.emplace_back(all_args[i]);
  }

  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("A modern SystemVerilog simulation toolchain");
  program.add_argument("-C").help("Run as if started in <dir>").metavar("dir");

  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Run simulation");
  run_cmd.add_argument("--backend")
      .default_value(std::string("aot"))
      .help("Execution backend: aot (default), jit, or lli");
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

  argparse::ArgumentParser compile_cmd("compile");
  compile_cmd.add_description("Compile to a native executable");
  compile_cmd.add_argument("-o", "--output")
      .default_value(std::string("out"))
      .help("Output directory (default: out/)");
  compile_cmd.add_argument("--name").help(
      "Executable name (default: top module name)");
  lyra::driver::AddCompilationFlags(compile_cmd);
  compile_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  argparse::ArgumentParser init_cmd("init");
  init_cmd.add_description("Create a new Lyra project");
  init_cmd.add_argument("name").nargs(0, 1).help("Project name");
  init_cmd.add_argument("--force", "-f")
      .default_value(false)
      .implicit_value(true)
      .help("Overwrite existing lyra.toml");

  program.add_subparser(run_cmd);
  program.add_subparser(compile_cmd);
  program.add_subparser(check_cmd);
  program.add_subparser(dump_cmd);
  program.add_subparser(init_cmd);

  try {
    program.parse_args(args);
  } catch (const std::exception& err) {
    lyra::driver::CompilationOutput output(lyra::driver::DriverOutputOptions{});
    output.PrintError(err.what());
    output.Flush();
    // Argparse library requires direct stream write for help text.
    std::cerr << program;
    return 1;
  }

  // Pre-command output boundary for early validation errors.
  lyra::driver::CompilationOutput output(lyra::driver::DriverOutputOptions{});

  if (auto dir = program.present("-C")) {
    std::error_code ec;
    std::filesystem::current_path(*dir, ec);
    if (ec) {
      output.PrintError(
          std::format("cannot change to '{}': {}", *dir, ec.message()));
      output.Flush();
      return 1;
    }
  }

  auto prepare = [&](const argparse::ArgumentParser& cmd) {
    bool no_project = cmd.get<bool>("--no-project");
    auto input = lyra::driver::PrepareInput(cmd, no_project);
    if (!input) {
      output.PrintDiagnostic(input.error());
      output.Flush();
    }
    return input;
  };

  try {
    if (program.is_subcommand_used("run")) {
      auto backend =
          lyra::driver::ParseBackend(run_cmd.get<std::string>("--backend"));
      if (!backend) {
        output.PrintDiagnostic(backend.error());
        output.Flush();
        return 1;
      }
      auto input = prepare(run_cmd);
      if (!input) return 1;

      input->plusargs = std::move(plusargs);

      switch (*backend) {
        case lyra::driver::Backend::kAot:
          return lyra::driver::RunAot(*input);
        case lyra::driver::Backend::kJit:
          return lyra::driver::RunJit(*input);
        case lyra::driver::Backend::kLli:
          return lyra::driver::RunLli(*input);
      }
    }

    if (program.is_subcommand_used("compile")) {
      auto input = prepare(compile_cmd);
      if (!input) return 1;

      std::string exe_name;
      if (auto name = compile_cmd.present<std::string>("--name")) {
        exe_name = *name;
      } else {
        exe_name = input->top.empty() ? "simulation" : input->top;
      }

      lyra::driver::CompileOptions compile_options{
          .output_dir =
              std::filesystem::path(compile_cmd.get<std::string>("-o")),
          .name = exe_name,
      };
      auto compile_result = lyra::driver::Compile(*input, compile_options);
      if (!compile_result) return compile_result.error();
      fmt::print(stderr, "compiled: {}\n", compile_result->string());
      return 0;
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
        output.PrintDiagnostic(format.error());
        output.Flush();
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
    output.PrintError(e.what());
    output.Flush();
    return 1;
  } catch (const std::exception& e) {
    output.PrintDiagnostic(
        lyra::Diagnostic::HostError(
            std::format("unexpected error: {}", e.what())));
    output.Flush();
    return 1;
  } catch (...) {
    output.PrintDiagnostic(
        lyra::Diagnostic::HostError("unexpected unknown error"));
    output.Flush();
    return 1;
  }

  std::cout << program;
  return 0;
}
