#include "tests/utils/cpp_test_runner.hpp"

#include <array>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <memory>
#include <random>
#include <sstream>

#include <slang/ast/Compilation.h>

#include "lyra/codegen/cpp_codegen.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

namespace lyra::test {

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
    base = std::filesystem::temp_directory_path() / "lyra_test";
  }
  std::filesystem::create_directories(base);

  auto unique_dir = base / std::to_string(dis(gen));
  std::filesystem::create_directories(unique_dir);
  return unique_dir;
}

auto ExecuteCommand(const std::string& cmd) -> std::pair<int, std::string> {
  std::array<char, 128> buffer{};
  std::string result;
  std::unique_ptr<FILE, decltype(&pclose)> pipe(
      popen(cmd.c_str(), "r"), pclose);
  if (!pipe) {
    return {-1, "popen failed"};
  }
  while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
    result += buffer.data();
  }
  int status = pclose(pipe.release());
  return {WEXITSTATUS(status), result};
}

}  // namespace

auto CppTestResult::ReadVariable(const std::string& name) const -> int64_t {
  auto it = variables_.find(name);
  if (it == variables_.end()) {
    throw std::runtime_error("Variable not found: " + name);
  }
  return it->second;
}

auto CppTestRunner::RunFromSources(
    const std::vector<SourceFile>& files,
    const std::vector<std::string>& variables_to_read) -> CppTestResult {
  CppTestResult result;

  // Create unique temp directory for this test
  auto tmp_dir = MakeUniqueTempDir();

  // Write SV files to temp directory
  std::vector<std::string> file_paths;
  for (const auto& file : files) {
    auto file_path = tmp_dir / file.name;
    std::ofstream out(file_path);
    out << file.content;
    file_paths.push_back(file_path.string());
  }

  // Parse SV files
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromFiles(file_paths);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV files";
    return result;
  }

  // Lower to MIR
  const auto& root = compilation->getRoot();
  auto mir = lowering::ast_to_mir::AstToMir(root);
  if (!mir) {
    result.error_message_ = "Failed to lower to MIR";
    return result;
  }

  // Generate C++
  codegen::CppCodegen codegen;
  std::string generated = codegen.Generate(*mir);

  // Build main() that prints variables and final time
  std::ostringstream main_code;
  main_code << "\nint main() {\n";
  main_code << "  " << mir->name << " dut;\n";
  main_code << "  auto final_time = dut.RunInitials();\n";
  for (const auto& var : variables_to_read) {
    main_code << "  std::cout << \"" << var << "=\" << dut." << var
              << " << std::endl;\n";
  }
  main_code << "  std::cout << \"__time__=\" << final_time << std::endl;\n";
  main_code << "  return 0;\n";
  main_code << "}\n";

  auto cpp_path = tmp_dir / "test.cpp";
  auto bin_path = tmp_dir / "test";

  // Write complete C++ file (generated code + main)
  {
    std::ofstream out(cpp_path);
    out << "#include <iostream>\n";
    out << generated;
    out << main_code.str();
  }

  // Compile with SDK include path
  auto sdk_include = GetSdkIncludePath();
  std::string compile_cmd = "clang++ -std=c++23 -I" + sdk_include.string() +
                            " -o " + bin_path.string() + " " +
                            cpp_path.string() + " 2>&1";
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

  // Parse output (format: "var=value\n" and "__time__=value\n")
  std::istringstream iss(run_output);
  std::string line;
  while (std::getline(iss, line)) {
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

auto CppTestRunner::RunFromSource(
    const std::string& sv_code,
    const std::vector<std::string>& variables_to_read) -> CppTestResult {
  CppTestResult result;

  // Parse SV
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromString(sv_code);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV";
    return result;
  }

  // Lower to MIR
  const auto& root = compilation->getRoot();
  auto mir = lowering::ast_to_mir::AstToMir(root);
  if (!mir) {
    result.error_message_ = "Failed to lower to MIR";
    return result;
  }

  // Generate C++
  codegen::CppCodegen codegen;
  std::string generated = codegen.Generate(*mir);

  // Build main() that prints variables and final time
  std::ostringstream main_code;
  main_code << "\nint main() {\n";
  main_code << "  " << mir->name << " dut;\n";
  main_code << "  auto final_time = dut.RunInitials();\n";
  for (const auto& var : variables_to_read) {
    main_code << "  std::cout << \"" << var << "=\" << dut." << var
              << " << std::endl;\n";
  }
  main_code << "  std::cout << \"__time__=\" << final_time << std::endl;\n";
  main_code << "  return 0;\n";
  main_code << "}\n";

  // Create unique temp directory for this test
  auto tmp_dir = MakeUniqueTempDir();

  auto cpp_path = tmp_dir / "test.cpp";
  auto bin_path = tmp_dir / "test";

  // Write complete C++ file (generated code + main)
  std::string full_code;
  {
    std::ostringstream code_stream;
    code_stream << "#include <iostream>\n";
    code_stream << generated;
    code_stream << main_code.str();
    full_code = code_stream.str();

    std::ofstream out(cpp_path);
    out << full_code;
  }

  // Debug: print generated code if LYRA_DEBUG_CODEGEN is set
  if (std::getenv("LYRA_DEBUG_CODEGEN") != nullptr) {
    std::cerr << "=== Generated C++ ===\n" << full_code << "\n=== End ===\n";
  }

  // Compile with SDK include path
  auto sdk_include = GetSdkIncludePath();
  std::string compile_cmd = "clang++ -std=c++23 -I" + sdk_include.string() +
                            " -o " + bin_path.string() + " " +
                            cpp_path.string() + " 2>&1";
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

  // Parse output (format: "var=value\n" and "__time__=value\n")
  std::istringstream iss(run_output);
  std::string line;
  while (std::getline(iss, line)) {
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

}  // namespace lyra::test
