#include "lyra/compiler/compiler.hpp"

#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <nlohmann/json.hpp>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>

#include <slang/ast/Compilation.h>

// NOLINTNEXTLINE(misc-include-cleaner): RunSubprocess is only declared here
#include "lyra/common/subprocess.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/compiler/compiler_result.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"

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
  // NOLINTNEXTLINE(cppcoreguidelines-narrowing-conversions)
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

// Generate precompiled header if it doesn't exist, return path to PCH file.
// Returns empty path if PCH generation fails (caller should compile without).
auto GetOrCreatePch(const std::filesystem::path& sdk_include)
    -> std::filesystem::path {
  // Use TEST_TMPDIR if available (Bazel test), else system temp
  std::filesystem::path pch_dir;
  const char* test_tmpdir = std::getenv("TEST_TMPDIR");
  if (test_tmpdir != nullptr) {
    pch_dir = test_tmpdir;
  } else {
    pch_dir = std::filesystem::temp_directory_path() / "lyra";
  }
  std::filesystem::create_directories(pch_dir);

  auto pch_path = pch_dir / "lyra_test.pch";

  // Skip regeneration if PCH already exists
  if (std::filesystem::exists(pch_path)) {
    return pch_path;
  }

  // Create header file for PCH (includes everything test main.cpp needs)
  auto header_path = pch_dir / "lyra_test_pch.hpp";
  {
    std::ofstream out(header_path);
    out << "#pragma once\n";
    out << "#include <iostream>\n";
    out << "#include <sstream>\n";
    out << "#include <lyra/sdk/sdk.hpp>\n";
  }

  // Generate PCH
  auto [status, output] = common::RunSubprocess(
      {"clang++", "-std=c++23", "-x", "c++-header", "-I" + sdk_include.string(),
       "-o", pch_path.string(), header_path.string()});
  if (status != 0) {
    // PCH generation failed - return empty path (will compile without PCH)
    return {};
  }

  return pch_path;
}

}  // namespace

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
auto Compiler::CompileAndRun(
    const std::vector<std::unique_ptr<mir::Module>>& modules,
    const std::vector<std::unique_ptr<mir::Package>>& packages,
    const common::SymbolTable& symbol_table,
    const std::vector<std::string>& variables_to_read,
    const std::vector<std::string>& plusargs) -> CompilerResult {
  CompilerResult result;

  // Create unique temp directory with design subdirectory for headers
  auto tmp_dir = MakeUniqueTempDir();
  auto design_dir = tmp_dir / "design";
  std::filesystem::create_directories(design_dir);
  auto cpp_path = tmp_dir / "test_main.cpp";
  auto bin_path = tmp_dir / "sim";

  Codegen codegen(symbol_table);

  // Generate packages header if there are packages
  bool has_packages = !packages.empty();
  if (has_packages) {
    std::string packages_code = codegen.GeneratePackages(packages);
    auto packages_path = design_dir / "packages.hpp";
    std::ofstream out(packages_path);
    out << packages_code;
  }

  // Generate headers for all modules (handles signature deduplication)
  for (const auto& output : codegen.GenerateAllModules(modules, has_packages)) {
    auto mode = output.append ? std::ios::app : std::ios::out;
    std::ofstream out(design_dir / output.filename, mode);
    out << output.content;
  }

  // Use last module as top (modules are in dependency order)
  const auto& top = *modules.back();

  // Build main() that captures display output and writes JSON result
  auto json_path = tmp_dir / "result.json";
  std::ostringstream main_code;

  // Emit JSON string escape helper (generated code can't use nlohmann/json)
  main_code << R"(
std::string EscapeJsonString(const std::string& s) {
  std::string out;
  out.reserve(s.size() + 16);
  for (unsigned char c : s) {
    switch (c) {
      case '"': out += "\\\""; break;
      case '\\': out += "\\\\"; break;
      case '\n': out += "\\n"; break;
      case '\r': out += "\\r"; break;
      case '\t': out += "\\t"; break;
      default:
        if (c < 0x20 || c > 0x7F) {
          char buf[8];
          snprintf(buf, sizeof(buf), "\\u%04x", c);
          out += buf;
        } else {
          out += static_cast<char>(c);
        }
    }
  }
  return out;
}

)";

  main_code << "int main(int argc, char* argv[]) {\n";
  main_code
      << "  lyra::sdk::InitPlusargs({argv, static_cast<size_t>(argc)});\n";
  // Initialize global precision only if not default (for
  // $timeunit/$timeprecision)
  if (codegen.GetGlobalPrecisionPower() !=
      common::TimeScale::kDefaultPrecisionPower) {
    main_code << "  lyra::sdk::SetGlobalPrecisionPower("
              << static_cast<int>(codegen.GetGlobalPrecisionPower()) << ");\n";
  }
  main_code << "  " << top.name << " dut;\n";
  // Redirect cout to capture $display output
  main_code << "  std::ostringstream captured;\n";
  main_code << "  auto* old_buf = std::cout.rdbuf(captured.rdbuf());\n";
  main_code << "  auto result = dut.Run();\n";
  main_code << "  std::cout.rdbuf(old_buf);\n";

  // Write JSON result with atomic rename
  main_code << "  std::ofstream out(\""
            << (tmp_dir / "result.json.tmp").string() << "\");\n";
  main_code << "  out << \"{\";\n";
  main_code << "  out << \"\\\"schema_version\\\":1,\";\n";
  main_code << "  out << \"\\\"captured_output\\\":\\\"\" << "
               "EscapeJsonString(captured.str()) << \"\\\",\";\n";
  main_code
      << "  out << \"\\\"final_time\\\":\" << result.final_time << \",\";\n";
  main_code
      << "  out << \"\\\"exit_code\\\":\" << result.exit_code << \",\";\n";
  main_code << "  out << \"\\\"variables\\\":{\";\n";

  // Emit variable serialization
  bool first_var = true;
  for (const auto& var : variables_to_read) {
    if (!first_var) {
      main_code << "  out << \",\";\n";
    }
    first_var = false;
    main_code << "  out << \"\\\"" << var << "\\\":\" << std::showpoint << dut."
              << var << " << std::noshowpoint;\n";
  }

  main_code << "  out << \"}\";\n";  // close variables
  main_code << "  out << \"}\";\n";  // close root
  main_code << "  out.close();\n";
  main_code << "  std::rename(\"" << (tmp_dir / "result.json.tmp").string()
            << "\", \"" << json_path.string() << "\");\n";
  main_code << "  return 0;\n";
  main_code << "}\n";

  // Write test wrapper to separate .cpp file that includes the top header
  std::string wrapper_code;
  {
    std::ostringstream wrapper_stream;
    wrapper_stream << "#include <iomanip>\n";
    wrapper_stream << "#include <iostream>\n";
    wrapper_stream << "#include <sstream>\n";
    wrapper_stream << "#include <lyra/sdk/plusargs.hpp>\n";
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

  // Compile with SDK include path and precompiled header for speed
  auto sdk_include = GetSdkIncludePath();
  auto pch_path = GetOrCreatePch(sdk_include);

  std::vector<std::string> compile_argv = {"clang++", "-std=c++23"};
  if (!pch_path.empty()) {
    compile_argv.emplace_back("-include-pch");
    compile_argv.emplace_back(pch_path.string());
  }
  compile_argv.emplace_back("-I" + sdk_include.string());
  compile_argv.emplace_back("-I" + tmp_dir.string());
  compile_argv.emplace_back("-I" + design_dir.string());
  compile_argv.emplace_back("-o");
  compile_argv.emplace_back(bin_path.string());
  compile_argv.emplace_back(cpp_path.string());

  auto [compile_status, compile_output] = common::RunSubprocess(compile_argv);
  if (compile_status != 0) {
    result.error_message_ = "Compilation failed: " + compile_output;
    return result;
  }

  // Run with plusargs
  std::vector<std::string> run_argv = {bin_path.string()};
  for (const auto& arg : plusargs) {
    run_argv.push_back(arg);
  }
  auto [run_status, run_output] = common::RunSubprocess(run_argv);
  if (run_status != 0) {
    result.error_message_ =
        "Execution failed with status " + std::to_string(run_status);
    return result;
  }

  // Read JSON result file
  std::ifstream json_in(json_path);
  if (!json_in) {
    result.error_message_ = "Missing result.json at " + json_path.string();
    return result;
  }

  nlohmann::json j;
  try {
    j = nlohmann::json::parse(json_in);
  } catch (const nlohmann::json::parse_error& e) {
    result.error_message_ = std::string("JSON parse error: ") + e.what();
    return result;
  }

  // Strict parsing: require all expected fields
  if (!j.contains("schema_version")) {
    result.error_message_ = "Harness error: missing schema_version in JSON";
    return result;
  }
  if (!j.contains("captured_output")) {
    result.error_message_ = "Harness error: missing captured_output in JSON";
    return result;
  }
  if (!j.contains("final_time")) {
    result.error_message_ = "Harness error: missing final_time in JSON";
    return result;
  }
  if (!j.contains("variables")) {
    result.error_message_ = "Harness error: missing variables in JSON";
    return result;
  }

  result.captured_output_ = j["captured_output"].get<std::string>();
  result.final_time_ = j["final_time"].get<uint64_t>();

  // Parse variables
  for (const auto& [name, value] : j["variables"].items()) {
    if (value.is_number_float()) {
      result.variables_[name] = value.get<double>();
    } else {
      result.variables_[name] = value.get<int64_t>();
    }
  }

  result.success_ = true;
  return result;
}

auto CompilerResult::ReadVariable(const std::string& name) const -> TestValue {
  auto it = variables_.find(name);
  if (it == variables_.end()) {
    throw std::runtime_error("Variable not found: " + name);
  }
  return it->second;
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
auto Compiler::RunFromSource(
    const std::string& code, const std::vector<std::string>& variables_to_read,
    const std::vector<std::string>& plusargs) -> CompilerResult {
  CompilerResult result;

  // Parse SV
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromString(code);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV";
    return result;
  }

  // Lower to MIR (empty top = all modules in dependency order, last is top)
  auto lowering_result = lowering::ast_to_mir::AstToMir(*compilation, "");

  return CompileAndRun(
      lowering_result.modules, lowering_result.packages,
      lowering_result.symbol_table, variables_to_read, plusargs);
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
auto Compiler::RunFromFiles(
    const std::vector<std::string>& paths,
    const std::vector<std::string>& variables_to_read,
    const std::vector<std::string>& plusargs) -> CompilerResult {
  CompilerResult result;

  // Parse SV files
  frontend::SlangFrontend frontend;
  auto compilation = frontend.LoadFromFiles(paths);
  if (!compilation) {
    result.error_message_ = "Failed to parse SV files";
    return result;
  }

  // Lower to MIR (empty top = all modules in dependency order, last is top)
  auto lowering_result = lowering::ast_to_mir::AstToMir(*compilation, "");

  return CompileAndRun(
      lowering_result.modules, lowering_result.packages,
      lowering_result.symbol_table, variables_to_read, plusargs);
}

}  // namespace lyra::compiler
