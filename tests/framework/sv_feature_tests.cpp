#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <ranges>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <variant>
#include <vector>

#include "lyra/compiler/compiler.hpp"
#include "lyra/compiler/compiler_result.hpp"
#include "lyra/interpreter/interpreter.hpp"
#include "lyra/interpreter/interpreter_options.hpp"
#include "lyra/interpreter/interpreter_result.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/yaml_loader.hpp"

namespace lyra::test {
namespace {

// Extract category from YAML path for test name prefixing.
// e.g., "tests/sv_features/operators/binary.yaml" -> "operators_binary"
auto ExtractCategory(const std::filesystem::path& yaml_path) -> std::string {
  auto stem = yaml_path.stem().string();
  auto parent = yaml_path.parent_path().filename().string();
  return parent + "_" + stem;
}

// Get YAML file paths to load test cases from.
// Priority:
// 1. SV_TEST_YAML env var (single file, for backward compatibility)
// 2. Discover all YAML files in runfiles sv_features directory
auto GetYamlPaths() -> std::vector<std::filesystem::path> {
  // Check for single YAML file (backward compatibility)
  if (const char* yaml_path = std::getenv("SV_TEST_YAML")) {
    return {yaml_path};
  }

  // Discover all YAML files from runfiles
  // Bazel sets TEST_SRCDIR to runfiles root
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

  // Sort for deterministic test order
  std::ranges::sort(yaml_paths);
  return yaml_paths;
}

class ScopedCurrentPath {
 public:
  ScopedCurrentPath(const ScopedCurrentPath&) = default;
  ScopedCurrentPath(ScopedCurrentPath&&) = delete;
  auto operator=(const ScopedCurrentPath&) -> ScopedCurrentPath& = default;
  auto operator=(ScopedCurrentPath&&) -> ScopedCurrentPath& = delete;
  explicit ScopedCurrentPath(const std::filesystem::path& path)
      : previous_(std::filesystem::current_path()) {
    std::filesystem::current_path(path);
  }

  ~ScopedCurrentPath() {
    std::filesystem::current_path(previous_);
  }

 private:
  std::filesystem::path previous_;
};

auto WriteTempFiles(
    const std::vector<SourceFile>& files, const std::string& test_name)
    -> std::vector<std::string> {
  // Use unique directory per test to avoid race conditions when tests run in
  // parallel (sharded). Without this, different shards can overwrite each
  // other's files (e.g., mem.hex for $readmemh tests).
  auto tmp_dir =
      std::filesystem::temp_directory_path() / "lyra_test" / test_name;
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
  auto filtered =
      paths | std::views::filter([](const auto& path) {
        auto ext = std::filesystem::path(path).extension();
        return ext == ".sv" || ext == ".svh" || ext == ".v" || ext == ".vh";
      });
  std::vector<std::string> sv_paths;
  std::ranges::copy(filtered, std::back_inserter(sv_paths));
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
  for (const auto& substring : expected.not_contains) {
    EXPECT_TRUE(actual.find(substring) == std::string::npos)
        << "Expected output to NOT contain: \"" << substring << "\"\n"
        << "Actual output: \"" << actual << "\"";
  }
}

class SvFeatureTest : public testing::TestWithParam<TestCase> {};

TEST_P(SvFeatureTest, Interpreter) {
  const auto& test_case = GetParam();

  // Skip interpreter test if flag is set
  if (test_case.skip_interpreter) {
    GTEST_SKIP() << "Interpreter skipped";
  }

  interpreter::InterpreterOptions options;
  options.plusargs = test_case.plusargs;

  interpreter::InterpreterResult result;
  if (test_case.IsMultiFile()) {
    auto paths = WriteTempFiles(test_case.files, test_case.name);
    auto sv_paths = FilterSvFiles(paths);
    ScopedCurrentPath current_dir(
        std::filesystem::path(paths.front()).parent_path());
    result = interpreter::Interpreter::RunFromFiles(sv_paths, "", options);
  } else {
    result =
        interpreter::Interpreter::RunFromSource(test_case.sv_code, "", options);
  }

  for (const auto& [var, expected] : test_case.expected_values) {
    auto actual = result.ReadVariable(var);
    std::visit(
        [&](auto&& exp_val) {
          using T = std::decay_t<decltype(exp_val)>;
          if constexpr (std::is_same_v<T, int64_t>) {
            EXPECT_EQ(actual.AsNarrow().AsInt64(), exp_val)
                << "Variable: " << var;
          } else if constexpr (std::is_same_v<T, double>) {
            // Handle both real (double) and shortreal (float)
            if (actual.IsShortReal()) {
              EXPECT_EQ(static_cast<double>(actual.AsFloat()), exp_val)
                  << "Variable: " << var;
            } else {
              EXPECT_EQ(actual.AsDouble(), exp_val) << "Variable: " << var;
            }
          }
        },
        expected);
  }

  if (test_case.expected_time.has_value()) {
    EXPECT_EQ(result.FinalTime(), test_case.expected_time.value());
  }

  if (test_case.expected_output.has_value()) {
    AssertOutput(result.CapturedOutput(), test_case.expected_output.value());
  }
}

TEST_P(SvFeatureTest, CppCodegen) {
  const auto& test_case = GetParam();

  // Skip codegen test if flag is set (e.g., for hierarchy tests)
  if (test_case.skip_codegen) {
    GTEST_SKIP() << "Codegen skipped (use CLI for hierarchical modules)";
  }

  std::vector<std::string> vars;
  vars.reserve(test_case.expected_values.size());
  for (const auto& [var, _] : test_case.expected_values) {
    vars.push_back(var);
  }

  compiler::CompilerResult result;
  if (test_case.IsMultiFile()) {
    auto paths = WriteTempFiles(test_case.files, test_case.name);
    auto sv_paths = FilterSvFiles(paths);
    ScopedCurrentPath current_dir(
        std::filesystem::path(paths.front()).parent_path());
    result =
        compiler::Compiler::RunFromFiles(sv_paths, vars, test_case.plusargs);
  } else {
    result = compiler::Compiler::RunFromSource(
        test_case.sv_code, vars, test_case.plusargs);
  }
  ASSERT_TRUE(result.Success()) << result.ErrorMessage();

  for (const auto& [var, expected] : test_case.expected_values) {
    auto actual = result.ReadVariable(var);
    std::visit(
        [&](auto&& exp_val) {
          using T = std::decay_t<decltype(exp_val)>;
          if constexpr (std::is_same_v<T, int64_t>) {
            EXPECT_TRUE(std::holds_alternative<int64_t>(actual))
                << "Variable " << var << " expected integer";
            EXPECT_EQ(std::get<int64_t>(actual), exp_val)
                << "Variable: " << var;
          } else if constexpr (std::is_same_v<T, double>) {
            EXPECT_TRUE(std::holds_alternative<double>(actual))
                << "Variable " << var << " expected double";
            EXPECT_EQ(std::get<double>(actual), exp_val) << "Variable: " << var;
          }
        },
        expected);
  }

  if (test_case.expected_time.has_value()) {
    EXPECT_EQ(result.FinalTime(), test_case.expected_time.value());
  }

  if (test_case.expected_output.has_value()) {
    AssertOutput(result.CapturedOutput(), test_case.expected_output.value());
  }
}

auto LoadTestCases() -> std::vector<TestCase> {
  std::vector<TestCase> all_cases;
  auto yaml_paths = GetYamlPaths();

  for (const auto& yaml_path : yaml_paths) {
    auto cases = LoadTestCasesFromYaml(yaml_path.string());
    auto category = ExtractCategory(yaml_path);

    // Prefix test names with category for uniqueness across YAML files
    for (auto& test_case : cases) {
      test_case.name = category + "_" + test_case.name;
    }

    all_cases.insert(
        all_cases.end(), std::make_move_iterator(cases.begin()),
        std::make_move_iterator(cases.end()));
  }

  return all_cases;
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
