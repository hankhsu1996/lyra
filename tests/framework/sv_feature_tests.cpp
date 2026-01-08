#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <vector>

#include "lyra/compiler/compiler.hpp"
#include "lyra/interpreter/interpreter.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/yaml_loader.hpp"

namespace lyra::test {
namespace {

auto GetYamlPath() -> std::string {
  const char* yaml_path = std::getenv("SV_TEST_YAML");
  if (yaml_path == nullptr) {
    throw std::runtime_error("SV_TEST_YAML environment variable not set");
  }
  return yaml_path;
}

auto WriteTempFiles(const std::vector<SourceFile>& files)
    -> std::vector<std::string> {
  auto tmp_dir = std::filesystem::temp_directory_path() / "lyra_test";
  std::filesystem::create_directories(tmp_dir);

  std::vector<std::string> paths;
  for (const auto& file : files) {
    auto path = tmp_dir / file.name;
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
    auto ext = std::filesystem::path(path).extension().string();
    if (ext == ".sv" || ext == ".v") {
      sv_paths.push_back(path);
    }
  }
  return sv_paths;
}

void AssertOutput(const std::string& actual, const ExpectedOutput& expected) {
  if (expected.IsExact()) {
    EXPECT_EQ(actual, expected.exact.value());
  } else {
    for (const auto& substring : expected.contains) {
      EXPECT_TRUE(actual.find(substring) != std::string::npos)
          << "Expected output to contain: \"" << substring << "\"\n"
          << "Actual output: \"" << actual << "\"";
    }
  }
}

class SvFeatureTest : public testing::TestWithParam<TestCase> {};

TEST_P(SvFeatureTest, Interpreter) {
  const auto& tc = GetParam();

  // Skip interpreter test if flag is set
  if (tc.skip_interpreter) {
    GTEST_SKIP() << "Interpreter skipped";
  }

  interpreter::InterpreterResult result;
  if (tc.IsMultiFile()) {
    auto paths = WriteTempFiles(tc.files);
    auto sv_paths = FilterSvFiles(paths);
    result = interpreter::Interpreter::RunFromFiles(sv_paths);
  } else {
    result = interpreter::Interpreter::RunFromSource(tc.sv_code);
  }

  for (const auto& [var, expected] : tc.expected_values) {
    // Test expected values are always narrow
    EXPECT_EQ(result.ReadVariable(var).AsNarrow().AsInt64(), expected)
        << "Variable: " << var;
  }

  if (tc.expected_time.has_value()) {
    EXPECT_EQ(result.FinalTime(), tc.expected_time.value());
  }

  if (tc.expected_output.has_value()) {
    AssertOutput(result.CapturedOutput(), tc.expected_output.value());
  }
}

TEST_P(SvFeatureTest, CppCodegen) {
  const auto& tc = GetParam();

  // Skip codegen test if flag is set (e.g., for hierarchy tests)
  if (tc.skip_codegen) {
    GTEST_SKIP() << "Codegen skipped (use CLI for hierarchical modules)";
  }

  std::vector<std::string> vars;
  vars.reserve(tc.expected_values.size());
  for (const auto& [var, _] : tc.expected_values) {
    vars.push_back(var);
  }

  compiler::CompilerResult result;
  if (tc.IsMultiFile()) {
    auto paths = WriteTempFiles(tc.files);
    auto sv_paths = FilterSvFiles(paths);
    result = compiler::Compiler::RunFromFiles(sv_paths, vars);
  } else {
    result = compiler::Compiler::RunFromSource(tc.sv_code, vars);
  }
  ASSERT_TRUE(result.Success()) << result.ErrorMessage();

  for (const auto& [var, expected] : tc.expected_values) {
    EXPECT_EQ(result.ReadVariable(var), expected) << "Variable: " << var;
  }

  if (tc.expected_time.has_value()) {
    EXPECT_EQ(result.FinalTime(), tc.expected_time.value());
  }

  if (tc.expected_output.has_value()) {
    AssertOutput(result.CapturedOutput(), tc.expected_output.value());
  }
}

auto LoadTestCases() -> std::vector<TestCase> {
  return LoadTestCasesFromYaml(GetYamlPath());
}

INSTANTIATE_TEST_SUITE_P(
    SvFeatures, SvFeatureTest, testing::ValuesIn(LoadTestCases()),
    [](const testing::TestParamInfo<TestCase>& info) {
      return info.param.name;
    });

}  // namespace
}  // namespace lyra::test

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
