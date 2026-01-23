#include <argparse/argparse.hpp>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "check.hpp"
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

namespace fs = std::filesystem;

struct CommandFileResult {
  std::vector<std::string> files;
  std::vector<std::string> incdirs;
  std::vector<std::string> defines;
};

// Parse a command file supporting:
// - Comments (# or //)
// - +incdir+<path> for include directories
// - +define+<macro>[=<value>] for defines
// - File paths (everything else)
auto ParseCommandFile(const fs::path& path, bool relative_to_file)
    -> CommandFileResult {
  CommandFileResult result;
  std::ifstream in(path);
  if (!in) {
    throw std::runtime_error(
        std::format("cannot open command file '{}'", path.string()));
  }

  fs::path base =
      relative_to_file ? fs::absolute(path).parent_path() : fs::current_path();

  std::string line;
  while (std::getline(in, line)) {
    auto start = line.find_first_not_of(" \t");
    if (start == std::string::npos) {
      continue;
    }
    auto end = line.find_last_not_of(" \t\r\n");
    line = line.substr(start, end - start + 1);

    if (line.empty() || line[0] == '#') {
      continue;
    }
    if (line.size() >= 2 && line[0] == '/' && line[1] == '/') {
      continue;
    }

    if (line.starts_with("+incdir+")) {
      fs::path inc_path(line.substr(8));
      if (inc_path.is_relative()) {
        inc_path = base / inc_path;
      }
      result.incdirs.push_back(fs::absolute(inc_path).string());
      continue;
    }

    if (line.starts_with("+define+")) {
      result.defines.push_back(line.substr(8));
      continue;
    }

    fs::path file_path(line);
    if (file_path.is_relative()) {
      file_path = base / file_path;
    }
    result.files.push_back(fs::absolute(file_path).string());
  }
  return result;
}

void AddCompilationFlags(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--top").help("Top module name");
  cmd.add_argument("-I", "--include-directory")
      .append()
      .help("Include search path (repeatable)");
  cmd.add_argument("-D", "--define-macro")
      .append()
      .help("Preprocessor define (repeatable)");
  cmd.add_argument("-W").append().help("Warning flag (repeatable)");
  cmd.add_argument("-f").append().help("Command file (paths relative to CWD)");
  cmd.add_argument("-F").append().help(
      "Command file (paths relative to file itself)");
}

auto BuildInput(
    const argparse::ArgumentParser& cmd,
    const std::optional<lyra::driver::ProjectConfig>& config)
    -> std::optional<lyra::driver::CompilationInput> {
  lyra::driver::CompilationInput input;

  // Expand command files first
  std::vector<std::string> cmdfile_files;
  std::vector<std::string> cmdfile_incdirs;
  std::vector<std::string> cmdfile_defines;

  try {
    if (auto vals = cmd.present<std::vector<std::string>>("-f")) {
      for (const auto& f : *vals) {
        auto expanded = ParseCommandFile(f, false);
        cmdfile_files.insert(
            cmdfile_files.end(), expanded.files.begin(), expanded.files.end());
        cmdfile_incdirs.insert(
            cmdfile_incdirs.end(), expanded.incdirs.begin(),
            expanded.incdirs.end());
        cmdfile_defines.insert(
            cmdfile_defines.end(), expanded.defines.begin(),
            expanded.defines.end());
      }
    }
    if (auto vals = cmd.present<std::vector<std::string>>("-F")) {
      for (const auto& f : *vals) {
        auto expanded = ParseCommandFile(f, true);
        cmdfile_files.insert(
            cmdfile_files.end(), expanded.files.begin(), expanded.files.end());
        cmdfile_incdirs.insert(
            cmdfile_incdirs.end(), expanded.incdirs.begin(),
            expanded.incdirs.end());
        cmdfile_defines.insert(
            cmdfile_defines.end(), expanded.defines.begin(),
            expanded.defines.end());
      }
    }
  } catch (const std::exception& e) {
    lyra::driver::PrintError(e.what());
    return std::nullopt;
  }

  // Files: CLI replaces config entirely; command files are additive to CLI
  if (auto files = cmd.present<std::vector<std::string>>("files")) {
    input.files = *files;
  } else if (!cmdfile_files.empty()) {
    input.files = {};
  } else if (config) {
    input.files = config->files;
  }
  input.files.insert(
      input.files.end(), cmdfile_files.begin(), cmdfile_files.end());

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

  // List options: config + command files + CLI merged
  if (config) {
    input.incdir = config->incdir;
    input.defines = config->defines;
    input.warnings = config->warnings;
  }
  input.incdir.insert(
      input.incdir.end(), cmdfile_incdirs.begin(), cmdfile_incdirs.end());
  input.defines.insert(
      input.defines.end(), cmdfile_defines.begin(), cmdfile_defines.end());
  if (auto vals = cmd.present<std::vector<std::string>>("-I")) {
    input.incdir.insert(input.incdir.end(), vals->begin(), vals->end());
  }
  if (auto vals = cmd.present<std::vector<std::string>>("-D")) {
    input.defines.insert(input.defines.end(), vals->begin(), vals->end());
  }
  if (auto vals = cmd.present<std::vector<std::string>>("-W")) {
    input.warnings.insert(input.warnings.end(), vals->begin(), vals->end());
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
      lyra::driver::PrintError(
          std::format("directory '{}' already exists", project_dir.string()));
      return 1;
    }
  } else {
    project_dir = fs::current_path();
    project_name = project_dir.filename().string();

    if (fs::exists(project_dir / "lyra.toml") && !force) {
      lyra::driver::PrintError(
          "lyra.toml already exists (use --force to overwrite)");
      return 1;
    }
  }

  try {
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
  } catch (const std::exception& e) {
    lyra::driver::PrintError(e.what());
    return 1;
  }
}

auto CheckCommand(const argparse::ArgumentParser& cmd) -> int {
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

  return lyra::driver::Check(*input);
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  auto args = PreprocessArgs(std::span<char*>(argv, static_cast<size_t>(argc)));

  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("SystemVerilog compiler and simulator");
  program.add_argument("-C").help("Run as if started in <dir>").metavar("dir");

  // Subcommand: run
  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Run simulation");
  run_cmd.add_argument("--backend")
      .default_value(std::string("llvm"))
      .help("Execution backend: llvm (default) or mir (development)");
  AddCompilationFlags(run_cmd);
  run_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  // Subcommand: check
  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Check source files for errors");
  AddCompilationFlags(check_cmd);
  check_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  // Subcommand: dump
  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: hir, mir, or llvm");
  AddCompilationFlags(dump_cmd);
  dump_cmd.add_argument("files").remaining().help(
      "Source files (uses lyra.toml if not specified)");

  // Subcommand: init
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
    fs::current_path(*dir, ec);
    if (ec) {
      lyra::driver::PrintError(
          std::format("cannot change to '{}': {}", *dir, ec.message()));
      return 1;
    }
  }

  if (program.is_subcommand_used("run")) {
    return RunCommand(run_cmd);
  }

  if (program.is_subcommand_used("check")) {
    return CheckCommand(check_cmd);
  }

  if (program.is_subcommand_used("dump")) {
    return DumpCommand(dump_cmd);
  }

  if (program.is_subcommand_used("init")) {
    return InitCommand(init_cmd);
  }

  // No subcommand provided
  std::cout << program;
  return 0;
}
