#include <argparse/argparse.hpp>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "embedded_sdk.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/indent.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/compiler/codegen.hpp"
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
    throw std::runtime_error("Failed to create file: " + path.string());
  }
  out << content;
}

// Write embedded SDK headers to output directory
void WriteSdkHeaders(const fs::path& output_dir) {
  fs::path sdk_dest = output_dir / "include" / "lyra" / "sdk";
  fs::create_directories(sdk_dest);

  for (const auto& [filename, content] : lyra::embedded::kSdkFiles) {
    WriteFile(sdk_dest / filename, content);
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

auto RunCommand(bool use_interpreter) -> int {
  auto config_path = lyra::config::FindConfig();
  if (!config_path) {
    lyra::PrintDiagnostic(
        lyra::Diagnostic::Error({}, "no lyra.toml found in current directory"));
    return 1;
  }

  try {
    auto config = lyra::config::LoadConfig(*config_path);

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

    std::string cmd = std::format(
        "cd {} && cmake --preset default > /dev/null && "
        "cmake --build build > /dev/null && ./build/sim",
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

    const auto& root = compilation->getRoot();
    auto modules = lyra::lowering::ast_to_mir::AstToMir(root, top);

    // Create output directory structure
    fs::path out_path(out_dir);
    fs::path design_dir = out_path / "include" / "design";
    fs::create_directories(design_dir);

    // Copy SDK headers
    WriteSdkHeaders(out_path);

    // Generate header file for each module
    lyra::compiler::Codegen codegen;
    for (const auto& mir : modules) {
      std::string code = codegen.Generate(*mir);
      std::string header_file = mir->name + ".hpp";
      fs::path module_path = design_dir / header_file;

      // Wrap code with pragma once
      std::string header_code = "#pragma once\n\n" + code;
      WriteFile(module_path, header_code);
    }

    // Use top module (last in list) for main.cpp
    const auto& top_module = *modules.back();
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

auto EmitCommand() -> int {
  auto config_path = lyra::config::FindConfig();
  if (!config_path) {
    lyra::PrintDiagnostic(
        lyra::Diagnostic::Error({}, "no lyra.toml found in current directory"));
    return 1;
  }

  try {
    auto config = lyra::config::LoadConfig(*config_path);
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

auto CheckCommand() -> int {
  auto config_path = lyra::config::FindConfig();
  if (!config_path) {
    lyra::PrintDiagnostic(
        lyra::Diagnostic::Error({}, "no lyra.toml found in current directory"));
    return 1;
  }

  lyra::frontend::SlangFrontend frontend;
  try {
    auto config = lyra::config::LoadConfig(*config_path);

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

    const auto& root = compilation->getRoot();
    // Empty top = get all modules for dump
    auto modules = lyra::lowering::ast_to_mir::AstToMir(root, "");

    for (const auto& mir : modules) {
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

auto BuildCommand() -> int {
  auto config_path = lyra::config::FindConfig();
  if (!config_path) {
    lyra::PrintDiagnostic(
        lyra::Diagnostic::Error({}, "no lyra.toml found in current directory"));
    return 1;
  }

  // Check toolchain before build
  if (!EnsureToolchain()) {
    return 1;
  }

  try {
    auto config = lyra::config::LoadConfig(*config_path);
    auto out_path = config.root_dir / config.out_dir;

    // emit
    int result = EmitCommandInternal(
        config.files, out_path.string(), config.incdir, config.top);
    if (result != 0) {
      return result;
    }

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
  run_cmd.add_description("Simulate design (requires lyra.toml)");
  run_cmd.add_argument("--interpret", "-i")
      .flag()
      .help("Use interpreter instead of codegen");

  // Subcommand: emit
  argparse::ArgumentParser emit_cmd("emit");
  emit_cmd.add_description("Generate C++ code (requires lyra.toml)");

  // Subcommand: check
  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Parse and validate (requires lyra.toml)");

  // Subcommand: dump
  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: cpp, mir, or lir");
  dump_cmd.add_argument("files").remaining().help("SystemVerilog source files");

  // Subcommand: build
  argparse::ArgumentParser build_cmd("build");
  build_cmd.add_description(
      "Generate and build C++ project (requires lyra.toml)");

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
    bool use_interpreter = run_cmd.get<bool>("--interpret");
    return RunCommand(use_interpreter);
  }

  if (program.is_subcommand_used("emit")) {
    return EmitCommand();
  }

  if (program.is_subcommand_used("check")) {
    return CheckCommand();
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
    return BuildCommand();
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
