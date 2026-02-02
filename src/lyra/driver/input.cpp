#include "input.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "argparse/argparse.hpp"
#include "config.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

auto ParseBackend(const std::string& s) -> lyra::Result<Backend> {
  if (s == "jit") return Backend::kJit;
  if (s == "lli") return Backend::kLli;
  if (s == "mir") return Backend::kMir;
  return std::unexpected(
      Diagnostic::HostError(
          "unknown backend '" + s + "', use 'jit', 'lli', or 'mir'"));
}

auto ParseDumpFormat(const std::string& s) -> lyra::Result<DumpFormat> {
  if (s == "hir") return DumpFormat::kHir;
  if (s == "mir") return DumpFormat::kMir;
  if (s == "llvm") return DumpFormat::kLlvm;
  return std::unexpected(
      Diagnostic::HostError(
          "unknown format '" + s + "', use 'hir', 'mir', or 'llvm'"));
}

namespace {

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
    -> lyra::Result<CommandFileResult> {
  CommandFileResult result;
  std::ifstream in(path);
  if (!in) {
    return std::unexpected(
        Diagnostic::HostError(
            std::format("cannot open command file '{}'", path.string())));
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

}  // namespace

auto PreprocessArgs(std::span<char*> argv) -> std::vector<std::string> {
  std::vector<std::string> result;
  for (char* raw_arg : argv) {
    std::string_view arg = raw_arg;
    if (arg.size() > 2 &&
        (arg.starts_with("-D") || arg.starts_with("-I") ||
         arg.starts_with("-W") || arg.starts_with("-O") ||
         arg.starts_with("-G")) &&
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
  cmd.add_argument("--no-project")
      .default_value(false)
      .implicit_value(true)
      .help("Run without a project (ad-hoc mode)");
  cmd.add_argument("--top").help("Top module name");
  cmd.add_argument("-I", "--include-directory")
      .append()
      .help("Include search path (repeatable)");
  cmd.add_argument("-D", "--define-macro")
      .append()
      .help("Preprocessor define (repeatable)");
  cmd.add_argument("-W").append().help("Warning flag (repeatable)");
  cmd.add_argument("-O")
      .default_value(std::string("2"))
      .help("Optimization level (0, 1, 2, 3)");
  cmd.add_argument("-f").append().help("Command file (paths relative to CWD)");
  cmd.add_argument("-F").append().help(
      "Command file (paths relative to file itself)");
  cmd.add_argument("-G")
      .append()
      .metavar("NAME=VALUE")
      .help("Override top-level module parameter (repeatable)");
  cmd.add_argument("--pedantic")
      .implicit_value(true)
      .help(
          "Enable strict LRM compliance (disallow implicit enum conversions)");
}

auto BuildInput(
    const argparse::ArgumentParser& cmd,
    const std::optional<ProjectConfig>& config)
    -> lyra::Result<CompilationInput> {
  CompilationInput input;

  // Expand command files first
  std::vector<std::string> cmdfile_files;
  std::vector<std::string> cmdfile_incdirs;
  std::vector<std::string> cmdfile_defines;

  if (auto vals = cmd.present<std::vector<std::string>>("-f")) {
    for (const auto& f : *vals) {
      auto expanded = ParseCommandFile(f, false);
      if (!expanded) return std::unexpected(expanded.error());
      cmdfile_files.insert(
          cmdfile_files.end(), expanded->files.begin(), expanded->files.end());
      cmdfile_incdirs.insert(
          cmdfile_incdirs.end(), expanded->incdirs.begin(),
          expanded->incdirs.end());
      cmdfile_defines.insert(
          cmdfile_defines.end(), expanded->defines.begin(),
          expanded->defines.end());
    }
  }
  if (auto vals = cmd.present<std::vector<std::string>>("-F")) {
    for (const auto& f : *vals) {
      auto expanded = ParseCommandFile(f, true);
      if (!expanded) return std::unexpected(expanded.error());
      cmdfile_files.insert(
          cmdfile_files.end(), expanded->files.begin(), expanded->files.end());
      cmdfile_incdirs.insert(
          cmdfile_incdirs.end(), expanded->incdirs.begin(),
          expanded->incdirs.end());
      cmdfile_defines.insert(
          cmdfile_defines.end(), expanded->defines.begin(),
          expanded->defines.end());
    }
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
    return std::unexpected(Diagnostic::HostError("no input files"));
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
  if (auto vals = cmd.present<std::vector<std::string>>("-G")) {
    input.param_overrides = *vals;
  }

  // Parse optimization level
  auto opt_str = cmd.get<std::string>("-O");
  if (opt_str == "0") {
    input.opt_level = OptLevel::kO0;
  } else if (opt_str == "1") {
    input.opt_level = OptLevel::kO1;
  } else if (opt_str == "2") {
    input.opt_level = OptLevel::kO2;
  } else if (opt_str == "3") {
    input.opt_level = OptLevel::kO3;
  } else {
    return std::unexpected(
        Diagnostic::HostError(
            "invalid optimization level: " + opt_str + ", use 0-3"));
  }

  // Pedantic: CLI presence overrides config, else fall back to config
  if (auto p = cmd.present<bool>("--pedantic")) {
    input.pedantic = *p;
  } else if (config) {
    input.pedantic = config->pedantic;
  }

  return input;
}

auto LoadProjectConfig() -> lyra::Result<ProjectConfig> {
  auto config_path = FindConfig();
  if (!config_path) {
    return std::unexpected(
        Diagnostic::HostError(
            "no lyra.toml found\n"
            "       hint: run from within a project, use -C to specify project "
            "dir,\n"
            "             or use --no-project for ad-hoc mode"));
  }
  return LoadConfig(*config_path);
}

auto PrepareInput(const argparse::ArgumentParser& cmd, bool no_project)
    -> lyra::Result<CompilationInput> {
  // Capture effective CWD (after -C chdir in main.cpp, before config search)
  auto effective_cwd = fs::absolute(fs::current_path()).lexically_normal();

  std::optional<ProjectConfig> config;
  fs::path fs_base_dir;

  if (no_project) {
    // Ad-hoc mode: use effective CWD as base dir, no config required
    fs_base_dir = effective_cwd;
  } else {
    // Project mode: require lyra.toml
    auto config_result = LoadProjectConfig();
    if (!config_result) {
      return std::unexpected(config_result.error());
    }
    config = std::move(*config_result);
    fs_base_dir = fs::absolute(config->root_dir).lexically_normal();
  }

  auto input = BuildInput(cmd, config);
  if (!input) {
    return std::unexpected(input.error());
  }
  input->fs_base_dir = fs_base_dir;

  return input;
}

}  // namespace lyra::driver
