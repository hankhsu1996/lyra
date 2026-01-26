#include "commands.hpp"

#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>

#include "argparse/argparse.hpp"
#include "check.hpp"
#include "dump.hpp"
#include "input.hpp"
#include "print.hpp"
#include "run_llvm.hpp"
#include "run_mir.hpp"

namespace lyra::driver {
namespace {

namespace fs = std::filesystem;

}  // namespace

auto RunCommand(const argparse::ArgumentParser& cmd) -> int {
  auto backend = cmd.get<std::string>("--backend");

  if (backend != "mir" && backend != "llvm") {
    PrintError("unknown backend '" + backend + "', use 'mir' or 'llvm'");
    return 1;
  }

  auto config_result = LoadOptionalConfig();
  if (!config_result) {
    PrintDiagnostic(config_result.error());
    return 1;
  }

  auto input = BuildInput(cmd, *config_result);
  if (!input) {
    PrintDiagnostic(input.error());
    return 1;
  }

  if (backend == "llvm") {
    return RunLlvm(*input);
  }
  return RunMir(*input);
}

auto DumpCommand(const argparse::ArgumentParser& cmd) -> int {
  auto format = cmd.get<std::string>("format");

  if (format != "hir" && format != "mir" && format != "llvm") {
    PrintError("unknown format '" + format + "', use 'hir', 'mir', or 'llvm'");
    return 1;
  }

  auto config_result = LoadOptionalConfig();
  if (!config_result) {
    PrintDiagnostic(config_result.error());
    return 1;
  }

  auto input = BuildInput(cmd, *config_result);
  if (!input) {
    PrintDiagnostic(input.error());
    return 1;
  }

  if (format == "hir") {
    return DumpHir(*input);
  }
  if (format == "mir") {
    return DumpMir(*input);
  }
  return DumpLlvm(*input);
}

auto CheckCommand(const argparse::ArgumentParser& cmd) -> int {
  auto config_result = LoadOptionalConfig();
  if (!config_result) {
    PrintDiagnostic(config_result.error());
    return 1;
  }

  auto input = BuildInput(cmd, *config_result);
  if (!input) {
    PrintDiagnostic(input.error());
    return 1;
  }

  return Check(*input);
}

auto InitCommand(const argparse::ArgumentParser& cmd) -> int {
  std::optional<std::string> name;
  if (auto n = cmd.present<std::string>("name")) {
    name = *n;
  }
  bool force = cmd.get<bool>("--force");

  fs::path project_dir;
  std::string project_name;
  bool create_directory = false;

  if (name) {
    project_dir = fs::path(*name);
    if (project_dir.is_relative()) {
      project_dir = fs::current_path() / project_dir;
    }
    project_name = project_dir.filename().string();
    create_directory = true;

    if (fs::exists(project_dir)) {
      PrintError(
          std::format("directory '{}' already exists", project_dir.string()));
      return 1;
    }
  } else {
    project_dir = fs::current_path();
    project_name = project_dir.filename().string();

    if (fs::exists(project_dir / "lyra.toml") && !force) {
      PrintError("lyra.toml already exists (use --force to overwrite)");
      return 1;
    }
  }

  if (create_directory) {
    fs::create_directories(project_dir);

    std::ofstream sv_file(project_dir / (project_name + ".sv"));
    sv_file << std::format(
        "module {};\n"
        "  initial begin\n"
        "    $display(\"Hello from {}!\");\n"
        "  end\n"
        "endmodule\n",
        project_name, project_name);
  }

  std::ofstream toml_file(project_dir / "lyra.toml");
  toml_file << std::format(
      "[package]\n"
      "name = \"{}\"\n"
      "top = \"{}\"\n"
      "\n"
      "[sources]\n"
      "files = [\"{}.sv\"]\n",
      project_name, project_name, project_name);

  std::cout << std::format("Created project '{}'\n", project_name);
  return 0;
}

}  // namespace lyra::driver
