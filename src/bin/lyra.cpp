#include <argparse/argparse.hpp>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/compiler/codegen.hpp"
#include "lyra/compiler/compiler.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/interpreter.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

namespace fs = std::filesystem;

namespace {

// SDK header files to copy
const std::vector<std::string> kSdkFiles = {
    "sdk.hpp",   "module.hpp",     "scheduler.hpp", "task.hpp",
    "delay.hpp", "wait_event.hpp", "integer.hpp"};

// Convert CamelCase to snake_case (handles acronyms like CPU -> cpu)
auto ToSnakeCase(const std::string& name) -> std::string {
  std::string result;
  for (size_t i = 0; i < name.size(); ++i) {
    char c = name[i];
    if (std::isupper(c) != 0) {
      // Insert underscore only if previous char is lowercase
      if (i > 0 && std::islower(name[i - 1]) != 0) {
        result += '_';
      }
      result += static_cast<char>(std::tolower(c));
    } else {
      result += c;
    }
  }
  return result;
}

// Find SDK directory relative to binary or workspace
auto FindSdkDirectory() -> std::optional<fs::path> {
  // Try relative to current working directory (workspace root)
  fs::path workspace_sdk = "include/lyra/sdk";
  if (fs::exists(workspace_sdk)) {
    return workspace_sdk;
  }
  return std::nullopt;
}

// Write string content to file
void WriteFile(const fs::path& path, const std::string& content) {
  std::ofstream out(path);
  if (!out) {
    throw std::runtime_error("Failed to create file: " + path.string());
  }
  out << content;
}

// Copy SDK headers to output directory
void CopySdkHeaders(const fs::path& output_dir) {
  auto sdk_src = FindSdkDirectory();
  if (!sdk_src) {
    throw std::runtime_error(
        "Cannot find SDK headers. Run from workspace root.");
  }

  fs::path sdk_dest = output_dir / "include" / "lyra" / "sdk";
  fs::create_directories(sdk_dest);

  for (const auto& file : kSdkFiles) {
    fs::copy_file(
        *sdk_src / file, sdk_dest / file, fs::copy_options::overwrite_existing);
  }
}

// Generate CMakeLists.txt
auto GenerateCMakeLists(const std::string& module_name) -> std::string {
  return std::format(
      R"(cmake_minimum_required(VERSION 3.21)
project({} CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

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

// Generate main.cpp
auto GenerateMain(
    const std::string& module_name, const std::string& header_file)
    -> std::string {
  return "#include \"design/" + header_file +
         "\"\n\n"
         "int main() {\n"
         "    " +
         module_name +
         " dut;\n"
         "    dut.RunInitials();\n"
         "    return 0;\n"
         "}\n";
}

auto RunCommand(const std::vector<std::string>& files, bool use_interpreter)
    -> int {
  if (files.empty()) {
    std::cerr << "lyra run: no input files\n";
    return 1;
  }

  try {
    if (use_interpreter) {
      lyra::interpreter::Interpreter::RunFromFiles(files);
      return 0;
    }

    // Codegen backend - run simulation
    auto result = lyra::compiler::Compiler::RunFromFiles(files, {});
    if (!result.Success()) {
      std::cerr << "lyra run: " << result.ErrorMessage() << "\n";
      return 1;
    }
    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra run: " << e.what() << "\n";
    return 1;
  }
}

auto EmitCommand(
    const std::vector<std::string>& files, const std::string& output_dir)
    -> int {
  if (files.empty()) {
    std::cerr << "lyra emit: no input files\n";
    return 1;
  }

  try {
    lyra::frontend::SlangFrontend frontend;
    auto compilation = frontend.LoadFromFiles(files);
    if (!compilation) {
      std::cerr << "lyra emit: failed to parse\n";
      return 1;
    }

    const auto& root = compilation->getRoot();
    auto mir = lyra::lowering::ast_to_mir::AstToMir(root);
    if (!mir) {
      std::cerr << "lyra emit: failed to lower to MIR\n";
      return 1;
    }

    lyra::compiler::Codegen codegen;
    std::string code = codegen.Generate(*mir);

    // If no output directory, print to stdout
    if (output_dir.empty()) {
      std::cout << code;
      return 0;
    }

    // Create output directory structure
    fs::path out_path(output_dir);
    fs::path design_dir = out_path / "include" / "design";
    fs::create_directories(design_dir);

    // Copy SDK headers
    CopySdkHeaders(out_path);

    // Write generated module code
    std::string module_name = mir->name;
    std::string header_file = ToSnakeCase(module_name) + ".hpp";
    fs::path module_path = design_dir / header_file;

    // Wrap code with pragma once and proper includes
    std::string header_code = "#pragma once\n\n" + code;
    WriteFile(module_path, header_code);

    // Generate main.cpp
    WriteFile(out_path / "main.cpp", GenerateMain(module_name, header_file));

    // Generate CMakeLists.txt and CMakePresets.json
    WriteFile(out_path / "CMakeLists.txt", GenerateCMakeLists(module_name));
    WriteFile(out_path / "CMakePresets.json", GenerateCMakePresets());

    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra emit: " << e.what() << "\n";
    return 1;
  }
}

auto CheckCommand(const std::vector<std::string>& files) -> int {
  if (files.empty()) {
    std::cerr << "lyra check: no input files\n";
    return 1;
  }

  try {
    lyra::frontend::SlangFrontend frontend;
    auto compilation = frontend.LoadFromFiles(files);
    if (!compilation) {
      std::cerr << "lyra check: failed to parse\n";
      return 1;
    }
    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra check: " << e.what() << "\n";
    return 1;
  }
}

}  // namespace

auto main(int argc, char* argv[]) -> int {
  argparse::ArgumentParser program("lyra", "0.1.0");
  program.add_description("SystemVerilog simulator");

  // Subcommand: run
  argparse::ArgumentParser run_cmd("run");
  run_cmd.add_description("Simulate design");
  run_cmd.add_argument("files").remaining().help("SystemVerilog source files");
  run_cmd.add_argument("--interpret", "-i")
      .flag()
      .help("Use interpreter instead of codegen");

  // Subcommand: emit
  argparse::ArgumentParser emit_cmd("emit");
  emit_cmd.add_description("Emit generated C++ code");
  emit_cmd.add_argument("-o", "--output")
      .default_value(std::string{})
      .help("Output directory (if omitted, prints to stdout)");
  emit_cmd.add_argument("files").remaining().help("SystemVerilog source files");

  // Subcommand: check
  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Parse and validate only");
  check_cmd.add_argument("files").remaining().help(
      "SystemVerilog source files");

  program.add_subparser(run_cmd);
  program.add_subparser(emit_cmd);
  program.add_subparser(check_cmd);

  try {
    program.parse_args(argc, argv);
  } catch (const std::exception& err) {
    std::cerr << err.what() << "\n";
    std::cerr << program;
    return 1;
  }

  if (program.is_subcommand_used("run")) {
    auto files = run_cmd.get<std::vector<std::string>>("files");
    bool use_interpreter = run_cmd.get<bool>("--interpret");
    return RunCommand(files, use_interpreter);
  }

  if (program.is_subcommand_used("emit")) {
    auto files = emit_cmd.get<std::vector<std::string>>("files");
    auto output_dir = emit_cmd.get<std::string>("--output");
    return EmitCommand(files, output_dir);
  }

  if (program.is_subcommand_used("check")) {
    auto files = check_cmd.get<std::vector<std::string>>("files");
    return CheckCommand(files);
  }

  // No subcommand provided
  std::cout << program;
  return 0;
}
