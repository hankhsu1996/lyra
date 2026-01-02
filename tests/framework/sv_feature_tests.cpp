#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <vector>

#include "lyra/driver/driver.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/yaml_loader.hpp"
#include "tests/utils/cpp_test_runner.hpp"

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

class SvFeatureTest : public testing::TestWithParam<TestCase> {};

TEST_P(SvFeatureTest, Interpreter) {
  const auto& tc = GetParam();

  driver::DriverResult result;
  if (tc.IsMultiFile()) {
    auto paths = WriteTempFiles(tc.files);
    result = driver::Driver::RunFromFiles(paths);
  } else {
    result = driver::Driver::RunFromSource(tc.sv_code);
  }

  for (const auto& [var, expected] : tc.expected_values) {
    EXPECT_EQ(result.ReadVariable(var).AsInt64(), expected)
        << "Variable: " << var;
  }
}

TEST_P(SvFeatureTest, CppCodegen) {
  const auto& tc = GetParam();

  std::vector<std::string> vars;
  vars.reserve(tc.expected_values.size());
  for (const auto& [var, _] : tc.expected_values) {
    vars.push_back(var);
  }

  CppTestResult result;
  if (tc.IsMultiFile()) {
    result = CppTestRunner::RunFromSources(tc.files, vars);
  } else {
    result = CppTestRunner::RunFromSource(tc.sv_code, vars);
  }
  ASSERT_TRUE(result.Success()) << result.ErrorMessage();

  for (const auto& [var, expected] : tc.expected_values) {
    EXPECT_EQ(result.ReadVariable(var), expected) << "Variable: " << var;
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
