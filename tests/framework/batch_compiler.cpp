#include "tests/framework/batch_compiler.hpp"

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <mutex>
#include <nlohmann/json.hpp>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/common/subprocess.hpp"
#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/yaml_loader.hpp"  // NOLINT(misc-include-cleaner)

namespace lyra::test {

namespace {

auto GetSdkIncludePath() -> std::filesystem::path {
  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  if (test_srcdir != nullptr) {
    return std::filesystem::path(test_srcdir) / "_main" / "include";
  }
  return std::filesystem::current_path() / "include";
}

auto MakeUniqueTempDir() -> std::filesystem::path {
  static std::random_device rd;
  // NOLINTNEXTLINE(cppcoreguidelines-narrowing-conversions)
  static std::mt19937 gen(rd());
  static std::uniform_int_distribution<uint64_t> dis;

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

auto GetOrCreatePch(const std::filesystem::path& sdk_include)
    -> std::filesystem::path {
  // Get clang version for cache key
  auto [clang_status, clang_version] =
      common::RunSubprocess({"clang++", "--version"});
  if (clang_status != 0) {
    return {};  // Can't determine version, skip PCH
  }

  // Hash clang version (first line contains version info)
  auto first_line = clang_version.substr(0, clang_version.find('\n'));
  size_t clang_hash = std::hash<std::string>{}(first_line);

  // Hash SDK header content
  auto sdk_header = sdk_include / "lyra" / "sdk" / "sdk.hpp";
  std::ifstream sdk_in(sdk_header);
  std::string sdk_content(
      (std::istreambuf_iterator<char>(sdk_in)),
      std::istreambuf_iterator<char>());
  size_t sdk_hash = std::hash<std::string>{}(sdk_content);

  // Combined cache key
  std::ostringstream key;
  key << std::hex << clang_hash << "_" << sdk_hash;

  std::filesystem::path pch_dir;
  const char* test_tmpdir = std::getenv("TEST_TMPDIR");
  if (test_tmpdir != nullptr) {
    pch_dir = std::filesystem::path(test_tmpdir) / "pch" / key.str();
  } else {
    pch_dir =
        std::filesystem::temp_directory_path() / "lyra" / "pch" / key.str();
  }
  std::filesystem::create_directories(pch_dir);

  auto pch_path = pch_dir / "lyra_batch.pch";
  if (std::filesystem::exists(pch_path)) {
    return pch_path;
  }

  auto header_path = pch_dir / "lyra_batch_pch.hpp";
  {
    std::ofstream out(header_path);
    out << "#pragma once\n";
    // Standard library headers commonly used by generated code
    out << "#include <array>\n";
    out << "#include <cmath>\n";
    out << "#include <filesystem>\n";
    out << "#include <iostream>\n";
    out << "#include <print>\n";
    out << "#include <sstream>\n";
    // SDK headers
    out << "#include <lyra/sdk/sdk.hpp>\n";
    out << "#include <lyra/sdk/plusargs.hpp>\n";
    out << "#include <lyra/sdk/runtime_config.hpp>\n";
  }

  auto [status, output] = common::RunSubprocess(
      {"clang++", "-std=c++23", "-x", "c++-header", "-I" + sdk_include.string(),
       "-o", pch_path.string(), header_path.string()});
  if (status != 0) {
    return {};
  }

  return pch_path;
}

auto ExtractCategory(const std::filesystem::path& yaml_path) -> std::string {
  auto stem = yaml_path.stem().string();
  auto parent = yaml_path.parent_path().filename().string();
  return parent + "_" + stem;
}

auto GetYamlPaths() -> std::vector<std::filesystem::path> {
  if (const char* yaml_path = std::getenv("SV_TEST_YAML")) {
    return {yaml_path};
  }

  const char* test_srcdir = std::getenv("TEST_SRCDIR");
  const char* test_workspace = std::getenv("TEST_WORKSPACE");

  if (test_srcdir == nullptr || test_workspace == nullptr) {
    throw std::runtime_error(
        "Neither SV_TEST_YAML nor TEST_SRCDIR/TEST_WORKSPACE set");
  }

  std::filesystem::path runfiles_dir =
      std::filesystem::path(test_srcdir) / test_workspace;
  std::filesystem::path yaml_dir = runfiles_dir / "tests" / "sv_features";

  std::vector<std::filesystem::path> yaml_paths;
  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(yaml_dir)) {
    if (entry.is_regular_file() && entry.path().extension() == ".yaml") {
      yaml_paths.push_back(entry.path());
    }
  }

  std::ranges::sort(yaml_paths);
  return yaml_paths;
}

auto WriteTempFiles(
    const std::vector<SourceFile>& files, const std::filesystem::path& work_dir)
    -> std::vector<std::string> {
  std::filesystem::create_directories(work_dir);

  std::vector<std::string> paths;
  for (const auto& file : files) {
    auto path = work_dir / file.name;
    std::ofstream out(path);
    out << file.content;
    paths.push_back(path.string());
  }
  return paths;
}

auto FilterSvFiles(const std::vector<std::string>& paths)
    -> std::vector<std::string> {
  std::vector<std::string> sv_paths;
  for (const auto& path : paths) {
    auto ext = std::filesystem::path(path).extension();
    if (ext == ".sv" || ext == ".svh" || ext == ".v" || ext == ".vh") {
      sv_paths.push_back(path);
    }
  }
  return sv_paths;
}

}  // namespace

auto BatchCompiler::Instance() -> BatchCompiler& {
  static BatchCompiler instance;
  return instance;
}

auto BatchCompiler::RunTest(
    const TestCase& test_case,
    const std::vector<std::string>& variables_to_read) -> BatchTestResult {
  EnsurePrepared();

  if (!preparation_error_.empty()) {
    BatchTestResult result;
    result.error_message = "Batch preparation failed: " + preparation_error_;
    return result;
  }

  auto it = name_to_index_.find(test_case.name);
  if (it == name_to_index_.end()) {
    BatchTestResult result;
    result.error_message = "Test not found in batch: " + test_case.name;
    return result;
  }

  return Execute(it->second, variables_to_read);
}

void BatchCompiler::EnsurePrepared() {
  std::call_once(preparation_flag_, [this] {
    std::cerr << "Preparing batch compilation...\n";
    std::cerr.flush();

    std::string current_phase = "initialization";

    try {
      current_phase = "yaml_loading";
      CollectTestsForShard();

      current_phase = "batch_preparation";
      PrepareBatch();

      prepared_ = true;
    } catch (const std::exception& e) {
      std::ostringstream error;
      error << "Batch preparation failed during " << current_phase;
      error << "\n\nError: " << e.what();
      if (!batch_dir_.empty()) {
        error << "\n\nWork directory: " << batch_dir_.string();
      }
      preparation_error_ = error.str();
    }
  });
}

// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
void BatchCompiler::CollectTestsForShard() {
  // Collect ALL tests regardless of sharding - gtest will only run tests
  // assigned to this shard, but we need all tests in the batch because
  // sharding with filters doesn't match our pre-computed distribution.
  std::vector<TestCase> all_cases;
  auto yaml_paths = GetYamlPaths();

  for (const auto& yaml_path : yaml_paths) {
    auto cases = LoadTestCasesFromYaml(yaml_path.string());
    auto category = ExtractCategory(yaml_path);

    for (auto& test_case : cases) {
      test_case.name = category + "_" + test_case.name;
    }

    all_cases.insert(
        all_cases.end(), std::make_move_iterator(cases.begin()),
        std::make_move_iterator(cases.end()));
  }

  for (const auto& test : all_cases) {
    if (!test.skip_codegen) {
      name_to_index_[test.name] = test_cases_.size();
      test_cases_.push_back(test);
    }
  }
}

void BatchCompiler::PrepareBatch() {
  if (test_cases_.empty()) {
    return;
  }

  batch_dir_ = MakeUniqueTempDir();
  auto design_dir = batch_dir_ / "design";
  std::filesystem::create_directories(design_dir);

  for (size_t i = 0; i < test_cases_.size(); ++i) {
    try {
      prepared_tests_.push_back(PrepareTest(test_cases_[i], i));
    } catch (const std::exception& e) {
      throw std::runtime_error(
          "test " + std::to_string(i) + " (" + test_cases_[i].name +
          "): " + e.what());
    }
  }

  std::ostringstream combined;
  for (const auto& pt : prepared_tests_) {
    combined << pt.wrapped_code << "\n";
  }

  auto combined_path = design_dir / "all_tests.hpp";
  {
    std::ofstream out(combined_path);
    out << combined.str();
  }

  auto main_code = GenerateBatchMain();
  auto main_path = batch_dir_ / "batch_main.cpp";
  {
    std::ofstream out(main_path);
    out << main_code;
  }

  Compile();
}

auto BatchCompiler::PrepareTest(const TestCase& test, size_t index)
    -> PreparedTest {
  PreparedTest pt;
  pt.name = test.name;
  pt.namespace_name = "test_" + std::to_string(index);
  pt.plusargs = test.plusargs;
  pt.work_dir = batch_dir_ / ("work_" + std::to_string(index));

  for (const auto& [var, _] : test.expected_values) {
    pt.variables.push_back(var);
  }

  // Always create work directory (needed for current_path in batch execution)
  std::filesystem::create_directories(pt.work_dir);

  std::vector<std::string> sv_paths;

  if (test.IsMultiFile()) {
    auto all_paths = WriteTempFiles(test.files, pt.work_dir);
    sv_paths = FilterSvFiles(all_paths);
  }

  frontend::SlangFrontend frontend;
  std::unique_ptr<slang::ast::Compilation> compilation;

  if (test.IsMultiFile()) {
    frontend::FrontendOptions options;
    options.include_dirs.push_back(pt.work_dir.string());
    compilation = frontend.LoadFromFiles(sv_paths, options);
  } else {
    compilation = frontend.LoadFromString(test.sv_code);
  }

  auto lowering_result = lowering::ast_to_mir::AstToMir(*compilation, "");

  // Get top module name (last module in dependency order)
  pt.top_module_name = lowering_result.modules.back()->name;

  compiler::Codegen codegen;

  pt.wrapped_code = codegen.GenerateBatchTestContent(
      pt.namespace_name, lowering_result.packages, lowering_result.modules,
      pt.top_module_name, pt.variables);
  pt.global_precision_power = codegen.GetGlobalPrecisionPower();

  return pt;
}

auto BatchCompiler::GenerateBatchMain() -> std::string {
  std::ostringstream main;

  main << "#include <cstdio>\n";
  main << "#include <fstream>\n";
  main << "#include <iomanip>\n";
  main << "#include <iostream>\n";
  main << "#include <string>\n";
  main << "#include <string_view>\n";
  main << "#include <vector>\n";
  main << "#include <lyra/sdk/runtime_config.hpp>\n";
  main << "#include \"design/all_tests.hpp\"\n\n";

  // Emit JSON string escape helper
  main << "std::string EscapeJsonString(const std::string& s) {\n";
  main << "  std::string out;\n";
  main << "  out.reserve(s.size() + 16);\n";
  main << "  for (char c : s) {\n";
  main << "    switch (c) {\n";
  main << "      case '\\\"': out += \"\\\\\\\"\"; break;\n";
  main << "      case '\\\\': out += \"\\\\\\\\\"; break;\n";
  main << "      case '\\n': out += \"\\\\n\"; break;\n";
  main << "      case '\\r': out += \"\\\\r\"; break;\n";
  main << "      case '\\t': out += \"\\\\t\"; break;\n";
  main << "      default:\n";
  main << "        if (static_cast<unsigned char>(c) < 0x20 || "
          "static_cast<unsigned char>(c) > 0x7F) {\n";
  main << "          char buf[8];\n";
  main << "          std::snprintf(buf, sizeof(buf), \"\\\\u%04x\", "
          "static_cast<unsigned char>(c));\n";
  main << "          out += buf;\n";
  main << "        } else {\n";
  main << "          out += c;\n";
  main << "        }\n";
  main << "    }\n";
  main << "  }\n";
  main << "  return out;\n";
  main << "}\n\n";

  main << "int main(int argc, char* argv[]) {\n";
  main << "  if (argc < 3) {\n";
  main << "    std::cerr << \"Usage: sim <test_index> <work_dir> "
          "[+plusargs...]\\n\";\n";
  main << "    return 1;\n";
  main << "  }\n\n";

  main << "  int test_index = std::stoi(argv[1]);\n";
  main << "  std::string work_dir = argv[2];\n";
  main << "  std::vector<std::string_view> plusargs;\n";
  main << "  for (int i = 3; i < argc; ++i) {\n";
  main << "    plusargs.emplace_back(argv[i]);\n";
  main << "  }\n";
  main << "  lyra::sdk::TestInvocation invocation{work_dir, plusargs, "
          "argv[0]};\n\n";

  // Generate function pointer array for O(1) dispatch
  main << "  using TestFunc = lyra::sdk::TestResult(*)(const "
          "lyra::sdk::TestInvocation&);\n";
  main << "  constexpr TestFunc kTests[] = {\n";
  for (const auto& pt : prepared_tests_) {
    main << "    " << pt.namespace_name << "::Run,\n";
  }
  main << "  };\n\n";

  main << "  if (test_index < 0 || static_cast<size_t>(test_index) >= "
       << prepared_tests_.size() << ") {\n";
  main << "    std::cerr << \"Unknown test index: \" << test_index << "
          "\"\\n\";\n";
  main << "    return 1;\n";
  main << "  }\n";
  main
      << "  lyra::sdk::TestResult result = kTests[test_index](invocation);\n\n";

  // Write result to JSON file (atomic: write tmp, then rename)
  main << "  std::string tmp_path = work_dir + \"/result.json.tmp\";\n";
  main << "  std::string final_path = work_dir + \"/result.json\";\n";
  main << "  std::ofstream out(tmp_path);\n";
  main << "  out << \"{\";\n";
  main << "  out << \"\\\"schema_version\\\":1,\";\n";
  main << "  out << \"\\\"captured_output\\\":\\\"\" << "
          "EscapeJsonString(result.captured_output) << \"\\\",\";\n";
  main << "  out << \"\\\"final_time\\\":\" << result.final_time << \",\";\n";
  main << "  out << \"\\\"exit_code\\\":\" << result.exit_code << \",\";\n";
  main << "  out << \"\\\"variables\\\":{\";\n";
  main << "  bool first = true;\n";
  main << "  for (const auto& [name, value] : result.variables) {\n";
  main << "    if (!first) out << \",\";\n";
  main << "    first = false;\n";
  main << "    out << \"\\\"\" << name << \"\\\":\";\n";
  main << "    if (std::holds_alternative<double>(value)) {\n";
  main << "      out << std::setprecision(17) << std::get<double>(value);\n";
  main << "    } else {\n";
  main << "      out << std::get<int64_t>(value);\n";
  main << "    }\n";
  main << "  }\n";
  main << "  out << \"}}\";\n";
  main << "  out.close();\n";
  main << "  std::rename(tmp_path.c_str(), final_path.c_str());\n";
  main << "  return 0;\n";
  main << "}\n";

  return main.str();
}

void BatchCompiler::Compile() {
  auto sdk_include = GetSdkIncludePath();
  auto pch_path = GetOrCreatePch(sdk_include);
  auto main_path = batch_dir_ / "batch_main.cpp";
  binary_path_ = batch_dir_ / "sim";

  std::vector<std::string> compile_argv = {"clang++", "-std=c++23"};
  if (!pch_path.empty()) {
    compile_argv.emplace_back("-include-pch");
    compile_argv.emplace_back(pch_path.string());
  }
  compile_argv.emplace_back("-I" + sdk_include.string());
  compile_argv.emplace_back("-I" + batch_dir_.string());
  compile_argv.emplace_back("-o");
  compile_argv.emplace_back(binary_path_.string());
  compile_argv.emplace_back(main_path.string());

  auto [status, output] = common::RunSubprocess(compile_argv);
  if (status != 0) {
    std::ostringstream error;
    error << "clang++ failed (exit " << status << ")\n";
    error << "Output:\n" << output;
    throw std::runtime_error(error.str());
  }
}

auto BatchCompiler::Execute(
    size_t index, [[maybe_unused]] const std::vector<std::string>& variables)
    -> BatchTestResult {
  BatchTestResult result;

  const auto& pt = prepared_tests_[index];

  std::vector<std::string> run_argv = {
      binary_path_.string(), std::to_string(index), pt.work_dir.string()};
  for (const auto& arg : pt.plusargs) {
    run_argv.push_back(arg);
  }

  auto [status, output] = common::RunSubprocess(run_argv);

  // Exit code is primary signal - non-zero means crash
  if (status != 0) {
    result.error_message =
        "Execution failed with status " + std::to_string(status);
    if (!output.empty()) {
      result.error_message += "\nOutput: " + output;
    }
    return result;
  }

  // Read JSON result file
  auto json_path = pt.work_dir / "result.json";
  std::ifstream json_in(json_path);
  if (!json_in) {
    result.error_message =
        "Harness error: missing result.json at " + json_path.string();
    return result;
  }

  try {
    nlohmann::json j = nlohmann::json::parse(json_in);

    // Strict parsing - missing fields = harness failure
    if (!j.contains("schema_version")) {
      result.error_message = "Harness error: missing schema_version in JSON";
      return result;
    }
    if (!j.contains("captured_output") || !j.contains("final_time") ||
        !j.contains("variables")) {
      result.error_message = "Harness error: missing required fields in JSON";
      return result;
    }

    result.captured_output = j["captured_output"].get<std::string>();
    result.final_time = j["final_time"].get<uint64_t>();

    for (const auto& [name, value] : j["variables"].items()) {
      if (value.is_number_float()) {
        result.variables[name] = value.get<double>();
      } else {
        result.variables[name] = value.get<int64_t>();
      }
    }

    result.success = true;
  } catch (const nlohmann::json::exception& e) {
    result.error_message =
        "Harness error: failed to parse result.json: " + std::string(e.what());
  }

  return result;
}

}  // namespace lyra::test
