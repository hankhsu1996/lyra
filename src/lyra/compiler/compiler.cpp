#include "lyra/compiler/compiler.hpp"

#include <array>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <sys/wait.h>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/common/timescale.hpp"
#include "lyra/compiler/codegen.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::compiler {

namespace {

auto GetSdkIncludePath() -> std::filesystem::path {
  // In Bazel tests, TEST_SRCDIR points to the runfiles directory
  // The SDK headers are at $TEST_SRCDIR/_main/include
  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  if (test_srcdir != nullptr) {
    return std::filesystem::path(test_srcdir) / "_main" / "include";
  }
  // Fallback for running outside Bazel (e.g., local development)
  // Assume we're in the workspace root
  return std::filesystem::current_path() / "include";
}

auto MakeUniqueTempDir() -> std::filesystem::path {
  static std::random_device rd;
  static std::mt19937 gen(rd());
  static std::uniform_int_distribution<uint64_t> dis;

  // Use TEST_TMPDIR if running under bazel test, otherwise use system temp
  std::filesystem::path base;
  const char* test_tmpdir = std::getenv("TEST_TMPDIR");
  if (test_tmpdir != nullptr) {
    base = test_tmpdir;
  } else {
    base = std::filesystem::temp_directory_path() / "lyra";
  }
  std::filesystem::create_directories(base);

  auto unique_dir = base / std::to_string(dis(gen));
  std::filesystem::create_directories(unique_dir);
  return unique_dir;
}

auto ExecuteCommand(const std::string& cmd) -> std::pair<int, std::string> {
  std::array<char, 128> buffer{};
  std::string result;
  // NOLINTNEXTLINE(misc-include-cleaner): popen/pclose are in <cstdio>
  std::unique_ptr<FILE, decltype(&pclose)> pipe(
      popen(cmd.c_str(), "r"), pclose);
  if (!pipe) {
    return {-1, "popen failed"};
  }
  while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
    result += buffer.data();
  }
  int status = pclose(pipe.release());  // NOLINT(misc-include-cleaner)
  // NOLINTNEXTLINE(misc-include-cleaner): WEXITSTATUS is in <sys/wait.h>
  return {WEXITSTATUS(status), result};
}

}  // namespace

auto Compiler::CompileAndRun(
    const std::vector<std::unique_ptr<mir::Module>>& modules,
    const std::vector<std::string>& variables_to_read)
    // NOLINTNEXTLINE(misc-include-cleaner): CompilerResult is in compiler.hpp
    -> CompilerResult {
  CompilerResult result;

  // Create unique temp directory with design subdirectory for headers
  auto tmp_dir = MakeUniqueTempDir();
  auto design_dir = tmp_dir / "design";
  std::filesystem::create_directories(design_dir);
  auto cpp_path = tmp_dir / "test_main.cpp";
  auto bin_path = tmp_dir / "sim";

  // Generate header for each module
  Codegen codegen;
  for (const auto& mir : modules) {
    std::string generated = codegen.Generate(*mir);
    std::string header_code = "#pragma once\n\n" + generated;
    auto header_path = design_dir / (mir->name + ".hpp");
    std::ofstream out(header_path);
    out << header_code;
  }

  // Use last module as top (modules are in dependency order)
  const auto& top = *modules.back();

  // Build main() that captures display output and prints results
  std::ostringstream main_code;
  main_code << "\nint main() {\n";
  // Initialize global precision only if not default (for
  // $timeunit/$timeprecision)
  if (codegen.GetGlobalPrecisionPower() !=
      common::TimeScale::kDefaultPrecisionPower) {
    main_code << "  lyra::sdk::global_precision_power = "
              << static_cast<int>(codegen.GetGlobalPrecisionPower()) << ";\n";
  }
  main_code << "  " << top.name << " dut;\n";
  // Redirect cout to capture $display output
  main_code << "  std::ostringstream captured;\n";
  main_code << "  auto* old_buf = std::cout.rdbuf(captured.rdbuf());\n";
  main_code << "  auto result = dut.Run();\n";
  main_code << "  std::cout.rdbuf(old_buf);\n";
  // Output captured display with markers
  main_code << "  std::cout << \"__output__=\" << captured.str() "
            << "<< \"__end_output__\" << std::endl;\n";
  for (const auto& var : variables_to_read) {
    main_code << "  std::cout << \"" << var << "=\" << dut." << var
              << " << std::endl;\n";
  }
  main_code
      << "  std::cout << \"__time__=\" << result.final_time << std::endl;\n";
  main_code
      << "  std::cout << \"__stopped__=\" << result.exit_code << std::endl;\n";
  main_code << "  return 0;\n";
  main_code << "}\n";

  // Write test wrapper to separate .cpp file that includes the top header
  std::string wrapper_code;
  {
    std::ostringstream wrapper_stream;
    wrapper_stream << "#include <iostream>\n";
    wrapper_stream << "#include <sstream>\n";
    wrapper_stream << "#include \"design/" << top.name << ".hpp\"\n";
    wrapper_stream << main_code.str();
    wrapper_code = wrapper_stream.str();

    std::ofstream out(cpp_path);
    out << wrapper_code;
  }

  // Debug: print generated code if LYRA_DEBUG_CODEGEN is set
  if (std::getenv("LYRA_DEBUG_CODEGEN") != nullptr) {
    std::cerr << "=== Test Wrapper ===\n" << wrapper_code << "\n=== End ===\n";
  }

  // Compile with SDK include path
  // Include both tmp_dir (for test_main.cpp) and design_dir (for module
  // headers)
  auto sdk_include = GetSdkIncludePath();
  std::string compile_cmd = "clang++ -std=c++23 -I" + sdk_include.string() +
                            " -I" + tmp_dir.string() + " -I" +
                            design_dir.string() + " -o " + bin_path.string() +
                            " " + cpp_path.string() + " 2>&1";
  auto [compile_status, compile_output] = ExecuteCommand(compile_cmd);
  if (compile_status != 0) {
    result.error_message_ = "Compilation failed: " + compile_output;
    return result;
  }

  // Run
  auto [run_status, run_output] = ExecuteCommand(bin_path.string());
  if (run_status != 0) {
    result.error_message_ =
        "Execution failed with status " + std::to_string(run_status);
    return result;
  }

  // Parse output
  // Format: "__output__=...contents...__end_output__\nvar=value\n__time__=N\n"

  // Extract captured output (between __output__= and __end_output__)
  constexpr std::string_view kOutputStart = "__output__=";
  constexpr std::string_view kOutputEnd = "__end_output__";
  auto output_start = run_output.find(kOutputStart);
  auto output_end = run_output.find(kOutputEnd);
  if (output_start != std::string::npos && output_end != std::string::npos) {
    auto content_start = output_start + kOutputStart.size();
    result.captured_output_ =
        run_output.substr(content_start, output_end - content_start);
  }

  // Parse variable values and time
  std::istringstream iss(run_output);
  std::string line;
  while (std::getline(iss, line)) {
    // Skip the output marker line
    if (line.starts_with("__output__=")) {
      continue;
    }
    auto eq_pos = line.find('=');
    if (eq_pos != std::string::npos) {
      std::string name = line.substr(0, eq_pos);
      if (name == "__time__") {
        result.final_time_ = std::stoull(line.substr(eq_pos + 1));
      } else {
        int64_t value = std::stoll(line.substr(eq_pos + 1));
        result.variables_[name] = value;
      }
    }
  }

  result.success_ = true;
  return result;
}

auto CompilerResult::ReadVariable(const std::string& name) const -> int64_t {
  auto it = variables_.find(name);
  if (it == variables_.end()) {
    throw std::runtime_error("Variable not found: " + name);
  }
  return it->second;
}

auto Compiler::RunFromSource(
    const std::string& code, const std::vector<std::string>& variables_to_read)
    -> CompilerResult {
  CompilerResult result;

  // Parse SV
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromString(code);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV";
    return result;
  }

  // Lower to MIR (empty top = all modules in dependency order, last is top)
  const auto& root = compilation->getRoot();
  auto modules = lowering::ast_to_mir::AstToMir(root, "");

  return CompileAndRun(modules, variables_to_read);
}

auto Compiler::RunFromFiles(
    const std::vector<std::string>& paths,
    const std::vector<std::string>& variables_to_read) -> CompilerResult {
  CompilerResult result;

  // Parse SV files
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromFiles(paths);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV files";
    return result;
  }

  // Lower to MIR (empty top = all modules in dependency order, last is top)
  const auto& root = compilation->getRoot();
  auto modules = lowering::ast_to_mir::AstToMir(root, "");

  return CompileAndRun(modules, variables_to_read);
}

}  // namespace lyra::compiler
