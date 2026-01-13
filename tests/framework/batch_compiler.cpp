#include "tests/framework/batch_compiler.hpp"

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

#include "lyra/compiler/codegen/codegen.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/lowering/ast_to_mir/ast_to_mir.hpp"
#include "tests/framework/yaml_loader.hpp"

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

auto ExecuteCommand(const std::string& cmd) -> std::pair<int, std::string> {
  std::array<char, 128> buffer{};
  std::string result;
  // NOLINTNEXTLINE(misc-include-cleaner)
  std::unique_ptr<FILE, decltype(&pclose)> pipe(
      popen(cmd.c_str(), "r"), pclose);
  if (!pipe) {
    return {-1, "popen failed"};
  }
  while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
    result += buffer.data();
  }
  // NOLINTNEXTLINE(misc-include-cleaner)
  int status = pclose(pipe.release());
  // NOLINTNEXTLINE(misc-include-cleaner)
  return {WEXITSTATUS(status), result};
}

auto GetOrCreatePch(const std::filesystem::path& sdk_include)
    -> std::filesystem::path {
  std::filesystem::path pch_dir;
  const char* test_tmpdir = std::getenv("TEST_TMPDIR");
  if (test_tmpdir != nullptr) {
    pch_dir = test_tmpdir;
  } else {
    pch_dir = std::filesystem::temp_directory_path() / "lyra";
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
    out << "#include <iostream>\n";
    out << "#include <sstream>\n";
    out << "#include <lyra/sdk/sdk.hpp>\n";
  }

  std::string gen_cmd = "clang++ -std=c++23 -x c++-header -I" +
                        sdk_include.string() + " -o " + pch_path.string() +
                        " " + header_path.string() + " 2>&1";
  auto [status, output] = ExecuteCommand(gen_cmd);
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
  if (prepared_ || preparation_attempted_) {
    return;
  }

  preparation_attempted_ = true;
  try {
    CollectTestsForShard();
    PrepareBatch();
    prepared_ = true;
  } catch (const std::exception& e) {
    preparation_error_ = e.what();
  }
}

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
    prepared_tests_.push_back(PrepareTest(test_cases_[i], i));
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
    auto old_cwd = std::filesystem::current_path();
    std::filesystem::current_path(pt.work_dir);
    compilation = frontend.LoadFromFiles(sv_paths);
    std::filesystem::current_path(old_cwd);
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

  main << "#include <iomanip>\n";
  main << "#include <iostream>\n";
  main << "#include <string>\n";
  main << "#include <string_view>\n";
  main << "#include <vector>\n";
  main << "#include <lyra/sdk/runtime_config.hpp>\n";
  main << "#include \"design/all_tests.hpp\"\n\n";

  main << "int main(int argc, char* argv[]) {\n";
  main << "  if (argc < 3) {\n";
  main << "    std::cerr << \"Usage: sim <test_index> <work_dir> "
          "[+plusargs...]\\n\";\n";
  main << "    return 1;\n";
  main << "  }\n\n";

  main << "  int test_index = std::stoi(argv[1]);\n";
  main << "  std::string_view work_dir = argv[2];\n";
  main << "  std::vector<std::string_view> plusargs;\n";
  main << "  for (int i = 3; i < argc; ++i) {\n";
  main << "    plusargs.emplace_back(argv[i]);\n";
  main << "  }\n";
  main << "  lyra::sdk::TestInvocation invocation{work_dir, plusargs, "
          "argv[0]};\n\n";

  main << "  lyra::sdk::TestResult result;\n";
  main << "  switch (test_index) {\n";

  for (size_t i = 0; i < prepared_tests_.size(); ++i) {
    const auto& pt = prepared_tests_[i];
    main << "    case " << i << ": result = " << pt.namespace_name
         << "::Run(invocation); break;\n";
  }

  main << "    default:\n";
  main << "      std::cerr << \"Unknown test index: \" << test_index << "
          "\"\\n\";\n";
  main << "      return 1;\n";
  main << "  }\n\n";

  // Output result in parseable format
  main << "  std::cout << \"__output__=\" << result.captured_output << "
          "\"__end_output__\" << std::endl;\n";
  main << "  for (const auto& [name, value] : result.variables) {\n";
  main << "    std::cout << name << \"=\";\n";
  main << "    if (std::holds_alternative<double>(value)) {\n";
  main << "      std::cout << std::showpoint << std::get<double>(value) << "
          "std::noshowpoint;\n";
  main << "    } else {\n";
  main << "      std::cout << std::get<int64_t>(value);\n";
  main << "    }\n";
  main << "    std::cout << std::endl;\n";
  main << "  }\n";
  main << "  std::cout << \"__time__=\" << result.final_time << std::endl;\n";
  main << "  std::cout << \"__stopped__=\" << result.exit_code << std::endl;\n";
  main << "  return 0;\n";
  main << "}\n";

  return main.str();
}

void BatchCompiler::Compile() {
  auto sdk_include = GetSdkIncludePath();
  auto pch_path = GetOrCreatePch(sdk_include);
  auto main_path = batch_dir_ / "batch_main.cpp";
  binary_path_ = batch_dir_ / "sim";

  std::string compile_cmd = "clang++ -std=c++23 ";
  if (!pch_path.empty()) {
    compile_cmd += "-include-pch " + pch_path.string() + " ";
  }
  compile_cmd += "-I" + sdk_include.string() + " ";
  compile_cmd += "-I" + batch_dir_.string() + " ";
  compile_cmd += "-o " + binary_path_.string() + " ";
  compile_cmd += main_path.string() + " 2>&1";

  auto [status, output] = ExecuteCommand(compile_cmd);
  if (status != 0) {
    throw std::runtime_error("Batch compilation failed: " + output);
  }
}

auto BatchCompiler::Execute(
    size_t index, [[maybe_unused]] const std::vector<std::string>& variables)
    -> BatchTestResult {
  BatchTestResult result;

  const auto& pt = prepared_tests_[index];

  std::string run_cmd = binary_path_.string();
  run_cmd += " " + std::to_string(index);
  run_cmd += " " + pt.work_dir.string();
  for (const auto& arg : pt.plusargs) {
    run_cmd += " " + arg;
  }

  auto [status, output] = ExecuteCommand(run_cmd);
  if (status != 0) {
    result.error_message =
        "Execution failed with status " + std::to_string(status);
    return result;
  }

  constexpr std::string_view kOutputStart = "__output__=";
  constexpr std::string_view kOutputEnd = "__end_output__";
  auto output_start = output.find(kOutputStart);
  auto output_end = output.find(kOutputEnd);
  if (output_start != std::string::npos && output_end != std::string::npos) {
    auto content_start = output_start + kOutputStart.size();
    result.captured_output =
        output.substr(content_start, output_end - content_start);
  }

  std::istringstream iss(output);
  std::string line;
  while (std::getline(iss, line)) {
    if (line.starts_with("__output__=")) {
      continue;
    }
    auto eq_pos = line.find('=');
    if (eq_pos != std::string::npos) {
      std::string name = line.substr(0, eq_pos);
      std::string value_str = line.substr(eq_pos + 1);
      if (name == "__time__") {
        result.final_time = std::stoull(value_str);
      } else if (name == "__stopped__") {
        // ignore exit code for now
      } else {
        // Parse variable value (int or double)
        if (value_str.find('.') != std::string::npos ||
            value_str.find('e') != std::string::npos ||
            value_str.find('E') != std::string::npos || value_str == "inf" ||
            value_str == "-inf" || value_str == "nan") {
          result.variables[name] = std::stod(value_str);
        } else {
          result.variables[name] = std::stoll(value_str);
        }
      }
    }
  }

  result.success = true;
  return result;
}

}  // namespace lyra::test
