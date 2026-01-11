#include <argparse/argparse.hpp>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "embedded_sdk.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/indent.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/config/project_config.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/interpreter.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"
#include "lyra/toolchain/toolchain.hpp"

namespace fs = std::filesystem;

namespace {

// Write string content to file
void WriteFile(const fs::path& path, const std::string& content) {
  std::ofstream out(path);
  if (!out) {
    throw std::runtime_error(
        std::format(
            "cannot write '{}': {}", path.string(), std::strerror(errno)));
  }
  out << content;
}

// CLI options for project commands
struct CliOptions {
  std::vector<std::string> files;
  std::vector<std::string> incdir;
  std::vector<std::string> cmdfiles_cwd;   // -f: paths relative to CWD
  std::vector<std::string> cmdfiles_file;  // -F: paths relative to file
  std::optional<std::string> top;
};

// Resolved configuration after merging CLI and lyra.toml
struct ResolvedConfig {
  std::string name;
  std::string top;
  std::vector<std::string> files;
  std::vector<std::string> incdir;
  std::string out_dir = "out";
  fs::path root_dir;
};

// Parse a command file (one file per line, # and // comments)
// relative_to_file: if true, resolve relative paths from file's directory (-F)
//                   if false, resolve relative paths from CWD (-f)
auto ParseCommandFile(const fs::path& path, bool relative_to_file)
    -> std::vector<std::string> {
  std::vector<std::string> files;
  std::ifstream in(path);
  if (!in) {
    throw std::runtime_error(
        std::format(
            "cannot open command file '{}': {}", path.string(),
            std::strerror(errno)));
  }

  // Base directory for relative path resolution
  fs::path base;
  if (relative_to_file) {
    base = path.parent_path();
    if (base.empty()) {
      base = fs::current_path();
    }
  } else {
    base = fs::current_path();
  }

  std::string line;
  while (std::getline(in, line)) {
    auto start = line.find_first_not_of(" \t");
    if (start == std::string::npos) {
      continue;
    }
    auto end = line.find_last_not_of(" \t\r\n");
    line = line.substr(start, end - start + 1);

    // Skip comments (# or //)
    if (line.empty() || line[0] == '#') {
      continue;
    }
    if (line.size() >= 2 && line[0] == '/' && line[1] == '/') {
      continue;
    }

    fs::path p(line);
    if (p.is_relative()) {
      p = base / p;
    }
    files.push_back(fs::absolute(p).string());
  }
  return files;
}

// Resolve configuration from CLI options and optional lyra.toml
auto ResolveConfig(const CliOptions& cli)
    -> std::expected<ResolvedConfig, std::string> {
  ResolvedConfig resolved;

  // Try loading lyra.toml
  auto config_path = lyra::config::FindConfig();
  std::optional<lyra::config::ProjectConfig> toml;

  if (config_path) {
    try {
      toml = lyra::config::LoadConfig(*config_path);
      resolved.name = toml->name;
      resolved.out_dir = toml->out_dir;
      resolved.root_dir = toml->root_dir;
    } catch (const lyra::DiagnosticException& e) {
      return std::unexpected(e.GetDiagnostic().message);
    }
  } else {
    resolved.name = "unnamed";
    resolved.root_dir = fs::current_path();
  }

  // Expand command files
  std::vector<std::string> cli_files = cli.files;

  // -f: paths relative to CWD
  for (const auto& f : cli.cmdfiles_cwd) {
    try {
      auto expanded = ParseCommandFile(f, false);
      cli_files.insert(cli_files.end(), expanded.begin(), expanded.end());
    } catch (const std::exception& e) {
      return std::unexpected(e.what());
    }
  }

  // -F: paths relative to command file
  for (const auto& f : cli.cmdfiles_file) {
    try {
      auto expanded = ParseCommandFile(f, true);
      cli_files.insert(cli_files.end(), expanded.begin(), expanded.end());
    } catch (const std::exception& e) {
      return std::unexpected(e.what());
    }
  }

  // Resolve CLI files relative to current directory
  for (auto& file : cli_files) {
    fs::path p(file);
    if (p.is_relative()) {
      file = fs::absolute(p).string();
    }
  }

  // Files: CLI replaces TOML
  if (!cli_files.empty()) {
    resolved.files = cli_files;
  } else if (toml) {
    resolved.files = toml->files;
  }

  if (resolved.files.empty()) {
    return std::unexpected("no source files specified");
  }

  // Incdir: CLI merges with TOML
  if (toml) {
    resolved.incdir = toml->incdir;
  }
  for (const auto& inc : cli.incdir) {
    fs::path p(inc);
    if (p.is_relative()) {
      p = fs::absolute(p);
    }
    resolved.incdir.push_back(p.string());
  }

  // Top: CLI overrides TOML
  if (cli.top) {
    resolved.top = *cli.top;
  } else if (toml) {
    resolved.top = toml->top;
  }

  if (resolved.top.empty()) {
    return std::unexpected("--top is required when not using lyra.toml");
  }

  return resolved;
}

// Extract CLI options from a parsed argument parser
auto ExtractCliOptions(const argparse::ArgumentParser& cmd) -> CliOptions {
  CliOptions opts;
  if (auto v = cmd.present<std::string>("--top")) {
    opts.top = *v;
  }
  if (auto v = cmd.present<std::vector<std::string>>("-I")) {
    opts.incdir = *v;
  }
  if (auto v = cmd.present<std::vector<std::string>>("-f")) {
    opts.cmdfiles_cwd = *v;
  }
  if (auto v = cmd.present<std::vector<std::string>>("-F")) {
    opts.cmdfiles_file = *v;
  }
  if (auto v = cmd.present<std::vector<std::string>>("files")) {
    opts.files = *v;
  }
  return opts;
}

// Add common CLI options to a subparser
void AddCliOptions(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--top")
      .help("Top module name (overrides lyra.toml)")
      .metavar("<module>");

  cmd.add_argument("-I", "--include-directory")
      .help("Add include search path (merges with lyra.toml)")
      .metavar("<dir>")
      .append();

  cmd.add_argument("-f")
      .help("Command file (paths relative to current directory)")
      .metavar("<file>")
      .append();

  cmd.add_argument("-F")
      .help("Command file (paths relative to file itself)")
      .metavar("<file>")
      .append();

  cmd.add_argument("files")
      .help("Source files (replaces lyra.toml files)")
      .remaining()
      .nargs(argparse::nargs_pattern::any);
}

// Write embedded SDK headers to output directory
void WriteSdkHeaders(const fs::path& output_dir) {
  for (const auto& [relpath, content] : lyra::embedded::kEmbeddedFiles) {
    fs::path dest = output_dir / "include" / relpath;
    fs::create_directories(dest.parent_path());
    WriteFile(dest, content);
  }
}

// Generate CMakeLists.txt
auto GenerateCMakeLists(const std::string& module_name) -> std::string {
  return std::format(
      R"(cmake_minimum_required(VERSION 3.21)
project({} CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Use ccache if available
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  set(CMAKE_CXX_COMPILER_LAUNCHER ${{CCACHE_PROGRAM}})
endif()

add_executable(sim main.cpp)
target_include_directories(sim PRIVATE include)
)",
      module_name);
}

// Generate CMakePresets.json (uses clang by default)
auto GenerateCMakePresets() -> std::string {
  return R"({
  "version": 6,
  "configurePresets": [
    {
      "name": "default",
      "binaryDir": "${sourceDir}/build",
      "cacheVariables": {
        "CMAKE_CXX_COMPILER": "clang++"
      }
    }
  ],
  "buildPresets": [
    {
      "name": "default",
      "configurePreset": "default"
    }
  ]
}
)";
}

// Generate compile_commands.json for clangd
auto GenerateCompileCommands(const fs::path& output_dir) -> std::string {
  auto abs_path = fs::absolute(output_dir).string();
  return std::format(
      R"([
  {{
    "directory": "{}",
    "arguments": ["clang++", "-std=c++23", "-Iinclude", "-c", "main.cpp", "-o", "main.o"],
    "file": "main.cpp"
  }}
]
)",
      abs_path);
}

// Generate .clang-format for generated project
auto GenerateClangFormat() -> std::string {
  return R"(Language: Cpp
BasedOnStyle: Google
IncludeBlocks: Regroup
IncludeCategories:
  - Regex: "^<lyra/.*>"
    Priority: 2
  - Regex: "^<.*>"
    Priority: 1
AlignAfterOpenBracket: AlwaysBreak
AllowShortFunctionsOnASingleLine: false
)";
}

// Generate .clang-tidy for generated project
// Enables useful checks but disables rules that don't apply to generated code
auto GenerateClangTidy() -> std::string {
  return R"(Checks: >
  -*,
  modernize-*,
  readability-*,
  cppcoreguidelines-*,
  clang-analyzer-*,
  -readability-identifier-naming,
  -readability-identifier-length,
  -readability-magic-numbers,
  -readability-convert-member-functions-to-static,
  -readability-static-accessed-through-instance,
  -readability-redundant-member-init,
  -readability-function-cognitive-complexity,
  -cppcoreguidelines-avoid-magic-numbers,
  -cppcoreguidelines-avoid-capturing-lambda-coroutines,
  -misc-include-cleaner,
)";
}

// Generate .gitignore
auto GenerateGitignore() -> std::string {
  return "build/\n";
}

// Check toolchain before build operations
// Returns false if toolchain check fails and errors were printed
auto EnsureToolchain() -> bool {
  auto status = lyra::toolchain::CheckToolchain();
  if (!status.ok) {
    for (const auto& error : status.errors) {
      lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, error));
    }
    return false;
  }
  return true;
}

// Clean up stale CMake cache if the project was moved or rebuilt in a
// different location. CMakeCache.txt stores absolute paths, so we need to
// detect and remove it when the paths no longer match.
void CleanStaleCMakeCache(const fs::path& out_path) {
  auto cache_file = out_path / "build" / "CMakeCache.txt";
  if (!fs::exists(cache_file)) {
    return;
  }

  std::ifstream file(cache_file);
  std::string line;
  std::string cached_dir;

  while (std::getline(file, line)) {
    constexpr std::string_view kPrefix = "CMAKE_CACHEFILE_DIR:INTERNAL=";
    if (line.starts_with(kPrefix)) {
      cached_dir = line.substr(kPrefix.size());
      break;
    }
  }

  auto expected_dir = (out_path / "build").string();
  if (!cached_dir.empty() && cached_dir != expected_dir) {
    fs::remove_all(out_path / "build");
  }
}

// Generate main.cpp
auto GenerateMain(
    const std::string& module_name, const std::string& header_file,
    int8_t global_precision_power) -> std::string {
  std::string precision_init;
  if (global_precision_power !=
      lyra::common::TimeScale::kDefaultPrecisionPower) {
    precision_init = std::format(
        "    // Initialize global precision for %t formatting\n"
        "    lyra::sdk::global_precision_power = {};\n",
        static_cast<int>(global_precision_power));
  }
  return std::format(
      "#include \"design/{}\"\n\n"
      "auto main() -> int {{\n"
      "{}"
      "    {} dut;\n"
      "    return dut.Run().exit_code;\n"
      "}}\n",
      header_file, precision_init, module_name);
}

// Forward declaration
auto EmitCommandInternal(
    const std::vector<std::string>& source_files, const std::string& out_dir,
    const std::vector<std::string>& inc_dirs, const std::string& top) -> int;

auto RunCommand(const CliOptions& cli, bool use_interpreter) -> int {
  auto config_result = ResolveConfig(cli);
  if (!config_result) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, config_result.error()));
    return 1;
  }
  const auto& config = *config_result;

  try {
    if (use_interpreter) {
      // TODO(hankhsu): Add include directory support to interpreter
      auto result = lyra::interpreter::Interpreter::RunFromFiles(
          config.files, config.top);
      std::cout << result.CapturedOutput();
      // Return non-zero exit code if simulation was stopped via $stop
      return result.Stopped() ? 1 : 0;
    }

    // Check toolchain before build (skip for interpreter mode)
    if (!EnsureToolchain()) {
      return 1;
    }

    // emit + build + run
    auto out_path = config.root_dir / config.out_dir;
    int result = EmitCommandInternal(
        config.files, out_path.string(), config.incdir, config.top);
    if (result != 0) {
      return result;
    }

    CleanStaleCMakeCache(out_path);

    // Build step (in out directory)
    std::string build_cmd = std::format(
        "cd {} && cmake --preset default > /dev/null && "
        "cmake --build build > /dev/null",
        out_path.string());
    int build_result = std::system(build_cmd.c_str());
    if (build_result != 0) {
      return build_result;
    }

    // Run step (from project directory - keeps CWD so $readmemh paths resolve
    // relative to the project root, not the out/ directory)
    auto binary_path = out_path / "build" / "sim";
    return std::system(binary_path.string().c_str());
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

auto EmitCommandInternal(
    const std::vector<std::string>& source_files, const std::string& out_dir,
    const std::vector<std::string>& inc_dirs, const std::string& top) -> int {
  lyra::frontend::SlangFrontend frontend;
  try {
    lyra::frontend::FrontendOptions options;
    options.include_dirs = inc_dirs;
    auto compilation = frontend.LoadFromFiles(source_files, options);
    if (!compilation) {
      // Diagnostic already printed by frontend
      return 1;
    }

    auto lowering_result =
        lyra::lowering::ast_to_mir::AstToMir(*compilation, top);

    // Create output directory structure
    fs::path out_path(out_dir);
    fs::path design_dir = out_path / "include" / "design";
    fs::create_directories(design_dir);

    // Copy SDK headers
    WriteSdkHeaders(out_path);

    lyra::compiler::Codegen codegen;

    // Generate packages header if there are packages
    bool has_packages = !lowering_result.packages.empty();
    if (has_packages) {
      std::string packages_code =
          codegen.GeneratePackages(lowering_result.packages);
      WriteFile(design_dir / "packages.hpp", packages_code);
    }

    // Generate header file for each module
    // Track which module names have been written (for template specializations)
    std::unordered_set<std::string> written_modules;
    for (const auto& mir : lowering_result.modules) {
      fs::path module_path = design_dir / (mir->name + ".hpp");
      bool is_first_in_file = !written_modules.contains(mir->name);
      written_modules.insert(mir->name);

      bool emit_file_header = is_first_in_file;
      bool emit_primary_template = is_first_in_file;
      std::string header_code = codegen.GenerateModuleHeader(
          *mir, has_packages, emit_file_header, emit_primary_template);

      // Use append mode for subsequent specializations of the same module
      auto mode = is_first_in_file ? std::ios::out : std::ios::app;
      std::ofstream out(module_path, mode);
      out << header_code;
    }

    // Use top module (last in list) for main.cpp
    const auto& top_module = *lowering_result.modules.back();
    std::string header_file = top_module.name + ".hpp";

    // Generate main.cpp with global precision for %t formatting
    WriteFile(
        out_path / "main.cpp",
        GenerateMain(
            top_module.name, header_file, codegen.GetGlobalPrecisionPower()));

    // Generate build configuration files
    WriteFile(out_path / "CMakeLists.txt", GenerateCMakeLists(top_module.name));
    WriteFile(out_path / "CMakePresets.json", GenerateCMakePresets());
    WriteFile(
        out_path / "compile_commands.json", GenerateCompileCommands(out_path));
    WriteFile(out_path / ".clang-format", GenerateClangFormat());
    WriteFile(out_path / ".clang-tidy", GenerateClangTidy());
    WriteFile(out_path / ".gitignore", GenerateGitignore());

    return 0;
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic(), frontend.GetSourceManager());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

auto EmitCommand(const CliOptions& cli) -> int {
  auto config_result = ResolveConfig(cli);
  if (!config_result) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, config_result.error()));
    return 1;
  }
  const auto& config = *config_result;

  try {
    auto out_path = config.root_dir / config.out_dir;
    return EmitCommandInternal(
        config.files, out_path.string(), config.incdir, config.top);
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

auto CheckCommand(const CliOptions& cli) -> int {
  auto config_result = ResolveConfig(cli);
  if (!config_result) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, config_result.error()));
    return 1;
  }
  const auto& config = *config_result;

  lyra::frontend::SlangFrontend frontend;
  try {
    lyra::frontend::FrontendOptions options;
    options.include_dirs = config.incdir;
    auto compilation = frontend.LoadFromFiles(config.files, options);
    if (!compilation) {
      // Diagnostic already printed by frontend
      return 1;
    }
    return 0;
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic(), frontend.GetSourceManager());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

enum class DumpFormat { kCpp, kMir, kLir };

auto DumpCommand(const std::vector<std::string>& files, DumpFormat format)
    -> int {
  if (files.empty()) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, "no input files"));
    return 1;
  }

  lyra::frontend::SlangFrontend frontend;
  try {
    auto compilation = frontend.LoadFromFiles(files);
    if (!compilation) {
      // Diagnostic already printed by frontend
      return 1;
    }

    // Empty top = get all modules for dump
    auto lowering_result =
        lyra::lowering::ast_to_mir::AstToMir(*compilation, "");

    // Dump packages first (for MIR format)
    if (format == DumpFormat::kMir) {
      for (const auto& pkg : lowering_result.packages) {
        std::cout << "package " << pkg->name << "\n";
        for (const auto& type_decl : pkg->types) {
          std::cout << "  typedef " << type_decl.name << " = "
                    << type_decl.type.ToString() << "\n";
        }
        for (const auto& var : pkg->variables) {
          std::cout << "  var " << var.variable.symbol->name << ": "
                    << var.variable.type.ToString();
          if (var.initializer) {
            std::cout << " = " << var.initializer->ToString();
          }
          std::cout << "\n";
        }
        std::cout << "endpackage\n\n";
      }
    }

    for (const auto& mir : lowering_result.modules) {
      switch (format) {
        case DumpFormat::kCpp: {
          lyra::compiler::Codegen codegen;
          std::cout << codegen.Generate(*mir);
          break;
        }
        case DumpFormat::kMir: {
          std::cout << mir->ToString();
          break;
        }
        case DumpFormat::kLir: {
          auto lir = lyra::lowering::mir_to_lir::MirToLir(*mir);
          std::cout << lir->ToString(lyra::common::FormatMode::kContextual);
          break;
        }
      }
    }

    return 0;
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic(), frontend.GetSourceManager());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

auto BuildCommand(const CliOptions& cli) -> int {
  auto config_result = ResolveConfig(cli);
  if (!config_result) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, config_result.error()));
    return 1;
  }
  const auto& config = *config_result;

  // Check toolchain before build
  if (!EnsureToolchain()) {
    return 1;
  }

  try {
    auto out_path = config.root_dir / config.out_dir;

    // emit
    int result = EmitCommandInternal(
        config.files, out_path.string(), config.incdir, config.top);
    if (result != 0) {
      return result;
    }

    CleanStaleCMakeCache(out_path);

    // cmake build (suppress stdout, keep stderr for errors)
    std::string cmd = std::format(
        "cd {} && cmake --preset default > /dev/null && "
        "cmake --build build > /dev/null",
        out_path.string());
    return std::system(cmd.c_str());
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

auto InitCommand(std::optional<std::string> name, bool force) -> int {
  fs::path project_dir;
  std::string project_name;
  bool create_directory = false;

  if (name.has_value()) {
    // Create subdirectory (existing behavior)
    project_dir = fs::path(*name);
    if (project_dir.is_relative()) {
      project_dir = fs::current_path() / project_dir;
    }
    project_name = project_dir.filename().string();
    create_directory = true;

    if (fs::exists(project_dir)) {
      lyra::PrintDiagnostic(
          lyra::Diagnostic::Error(
              {}, std::format(
                      "directory '{}' already exists", project_dir.string())));
      return 1;
    }
  } else {
    // Initialize in current directory
    project_dir = fs::current_path();
    project_name = project_dir.filename().string();

    // Check for existing lyra.toml
    if (fs::exists(project_dir / "lyra.toml") && !force) {
      lyra::PrintDiagnostic(
          lyra::Diagnostic::Error(
              {}, "lyra.toml already exists (use --force to overwrite)"));
      return 1;
    }
  }

  try {
    if (create_directory) {
      fs::create_directories(project_dir);

      // Create starter SV file only for new projects
      std::string sv = std::format(
          R"(module {};
  initial begin
    $display("Hello from {}!");
  end
endmodule
)",
          project_name, project_name);
      WriteFile(project_dir / (project_name + ".sv"), sv);
    }

    // Create lyra.toml
    std::string config = std::format(
        R"([package]
name = "{}"
top = "{}"

[sources]
files = ["{}.sv"]
)",
        project_name, project_name, project_name);
    WriteFile(project_dir / "lyra.toml", config);

    if (create_directory) {
      std::cout << "Created project '" << project_name << "'\n";
    } else {
      std::cout << "Initialized project '" << project_name << "'\n";
    }
    return 0;
  } catch (const lyra::DiagnosticException& e) {
    lyra::PrintDiagnostic(e.GetDiagnostic());
    return 1;
  } catch (const std::exception& e) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, e.what()));
    return 1;
  }
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("SystemVerilog simulator");

  // Global option: change to directory before running command (like git -C)
  program.add_argument("-C")
      .help("Run as if lyra was started in <dir>")
      .metavar("<dir>");

  // Subcommand: run
  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Simulate design");
  run_cmd.add_argument("--interpret", "-i")
      .flag()
      .help("Use interpreter instead of codegen");
  AddCliOptions(run_cmd);

  // Subcommand: emit
  argparse::ArgumentParser emit_cmd("emit");
  emit_cmd.add_description("Generate C++ code");
  AddCliOptions(emit_cmd);

  // Subcommand: check
  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Parse and validate");
  AddCliOptions(check_cmd);

  // Subcommand: dump
  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: cpp, mir, or lir");
  dump_cmd.add_argument("files").remaining().help("SystemVerilog source files");

  // Subcommand: build
  argparse::ArgumentParser build_cmd("build");
  build_cmd.add_description("Generate and build C++ project");
  AddCliOptions(build_cmd);

  // Subcommand: init
  argparse::ArgumentParser init_cmd("init");
  init_cmd.add_description("Create a new Lyra project");
  init_cmd.add_argument("name")
      .help("Project name (default: current directory)")
      .nargs(argparse::nargs_pattern::optional);
  init_cmd.add_argument("-f", "--force")
      .help("Overwrite existing lyra.toml")
      .flag();

  program.add_subparser(run_cmd);
  program.add_subparser(emit_cmd);
  program.add_subparser(check_cmd);
  program.add_subparser(dump_cmd);
  program.add_subparser(build_cmd);
  program.add_subparser(init_cmd);

  try {
    program.parse_args(argc, argv);
  } catch (const std::exception& err) {
    lyra::PrintDiagnostic(lyra::Diagnostic::Error({}, err.what()));
    std::cerr << program;
    return 1;
  }

  // Handle -C option: change directory before running command
  if (auto dir = program.present("-C")) {
    std::error_code ec;
    fs::current_path(*dir, ec);
    if (ec) {
      lyra::PrintDiagnostic(
          lyra::Diagnostic::Error(
              {},
              std::format("cannot change to '{}': {}", *dir, ec.message())));
      return 1;
    }
  }

  if (program.is_subcommand_used("run")) {
    auto cli = ExtractCliOptions(run_cmd);
    bool use_interpreter = run_cmd.get<bool>("--interpret");
    return RunCommand(cli, use_interpreter);
  }

  if (program.is_subcommand_used("emit")) {
    auto cli = ExtractCliOptions(emit_cmd);
    return EmitCommand(cli);
  }

  if (program.is_subcommand_used("check")) {
    auto cli = ExtractCliOptions(check_cmd);
    return CheckCommand(cli);
  }

  if (program.is_subcommand_used("dump")) {
    auto format_str = dump_cmd.get<std::string>("format");

    // Check if files were provided (remaining() arguments need present() check)
    std::vector<std::string> files;
    if (auto opt = dump_cmd.present<std::vector<std::string>>("files")) {
      files = std::move(*opt);
    }

    DumpFormat format{};
    if (format_str == "cpp") {
      format = DumpFormat::kCpp;
    } else if (format_str == "mir") {
      format = DumpFormat::kMir;
    } else if (format_str == "lir") {
      format = DumpFormat::kLir;
    } else {
      lyra::PrintDiagnostic(
          lyra::Diagnostic::Error(
              {},
              std::format(
                  "unknown format '{}' (use cpp, mir, or lir)", format_str)));
      return 1;
    }
    return DumpCommand(files, format);
  }

  if (program.is_subcommand_used("build")) {
    auto cli = ExtractCliOptions(build_cmd);
    return BuildCommand(cli);
  }

  if (program.is_subcommand_used("init")) {
    auto name = init_cmd.present<std::string>("name");
    bool force = init_cmd.get<bool>("--force");
    return InitCommand(name, force);
  }

  // No subcommand provided
  std::cout << program;
  return 0;
}
