#include "input.hpp"

#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "argparse/argparse.hpp"
#include "config.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {
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

  return input;
}

auto LoadOptionalConfig() -> lyra::Result<std::optional<ProjectConfig>> {
  auto config_path = FindConfig();
  if (!config_path) {
    return std::nullopt;
  }
  auto config = LoadConfig(*config_path);
  if (!config) return std::unexpected(config.error());
  return *config;
}

}  // namespace lyra::driver
