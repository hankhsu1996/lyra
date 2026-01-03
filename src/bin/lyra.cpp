#include <argparse/argparse.hpp>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <slang/ast/Compilation.h>

#include "embedded_sdk.hpp"
#include "lyra/compiler/codegen.hpp"
#include "lyra/compiler/compiler.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/interpreter.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "lyra/lowering/mir_to_lir/mir_to_lir.hpp"

namespace fs = std::filesystem;

namespace {

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

// Generate main.cpp
auto GenerateMain(
    const std::string& module_name, const std::string& header_file)
    -> std::string {
  return "#include \"design/" + header_file +
         "\"\n\n"
         "auto main() -> int {\n"
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

    // Create output directory structure
    fs::path out_path(output_dir);
    fs::path design_dir = out_path / "include" / "design";
    fs::create_directories(design_dir);

    // Copy SDK headers
    WriteSdkHeaders(out_path);

    // Write generated module code
    std::string module_name = mir->name;
    std::string header_file = ToSnakeCase(module_name) + ".hpp";
    fs::path module_path = design_dir / header_file;

    // Wrap code with pragma once and proper includes
    std::string header_code = "#pragma once\n\n" + code;
    WriteFile(module_path, header_code);

    // Generate main.cpp
    WriteFile(out_path / "main.cpp", GenerateMain(module_name, header_file));

    // Generate build configuration files
    WriteFile(out_path / "CMakeLists.txt", GenerateCMakeLists(module_name));
    WriteFile(out_path / "CMakePresets.json", GenerateCMakePresets());
    WriteFile(
        out_path / "compile_commands.json", GenerateCompileCommands(out_path));
    WriteFile(out_path / ".clang-tidy", GenerateClangTidy());

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

enum class DumpFormat { kCpp, kMir, kLir };

auto DumpCommand(const std::vector<std::string>& files, DumpFormat format)
    -> int {
  if (files.empty()) {
    std::cerr << "lyra dump: no input files\n";
    return 1;
  }

  try {
    lyra::frontend::SlangFrontend frontend;
    auto compilation = frontend.LoadFromFiles(files);
    if (!compilation) {
      std::cerr << "lyra dump: failed to parse\n";
      return 1;
    }

    const auto& root = compilation->getRoot();
    auto mir = lyra::lowering::ast_to_mir::AstToMir(root);
    if (!mir) {
      std::cerr << "lyra dump: failed to lower to MIR\n";
      return 1;
    }

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

    return 0;
  } catch (const std::exception& e) {
    std::cerr << "lyra dump: " << e.what() << "\n";
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
  emit_cmd.add_argument("--out-dir")
      .default_value(std::string{"out"})
      .help("Output directory");
  emit_cmd.add_argument("files").remaining().help("SystemVerilog source files");

  // Subcommand: check
  argparse::ArgumentParser check_cmd("check");
  check_cmd.add_description("Parse and validate only");
  check_cmd.add_argument("files").remaining().help(
      "SystemVerilog source files");

  // Subcommand: dump
  argparse::ArgumentParser dump_cmd("dump");
  dump_cmd.add_description("Dump internal representations (for debugging)");
  dump_cmd.add_argument("format").help("Output format: cpp, mir, or lir");
  dump_cmd.add_argument("files").remaining().help("SystemVerilog source files");

  program.add_subparser(run_cmd);
  program.add_subparser(emit_cmd);
  program.add_subparser(check_cmd);
  program.add_subparser(dump_cmd);

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
    auto output_dir = emit_cmd.get<std::string>("--out-dir");
    return EmitCommand(files, output_dir);
  }

  if (program.is_subcommand_used("check")) {
    auto files = check_cmd.get<std::vector<std::string>>("files");
    return CheckCommand(files);
  }

  if (program.is_subcommand_used("dump")) {
    auto format_str = dump_cmd.get<std::string>("format");
    auto files = dump_cmd.get<std::vector<std::string>>("files");

    DumpFormat format{};
    if (format_str == "cpp") {
      format = DumpFormat::kCpp;
    } else if (format_str == "mir") {
      format = DumpFormat::kMir;
    } else if (format_str == "lir") {
      format = DumpFormat::kLir;
    } else {
      std::cerr << "lyra dump: unknown format '" << format_str
                << "' (use cpp, mir, or lir)\n";
      return 1;
    }
    return DumpCommand(files, format);
  }

  // No subcommand provided
  std::cout << program;
  return 0;
}
