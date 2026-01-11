#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <mutex>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/compiler/compiler.hpp"
#include "lyra/frontend/slang_frontend.hpp"
#include "lyra/interpreter/interpreter.hpp"
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

struct CachedCompilation {
  std::unique_ptr<slang::ast::Compilation> compilation;
  std::shared_ptr<slang::SourceManager> source_manager;
  std::filesystem::path working_dir;
};

auto HashCombine(uint64_t seed, uint64_t value) -> uint64_t {
  return seed ^ (value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2));
}

auto HashString(std::string_view value) -> uint64_t {
  return static_cast<uint64_t>(std::hash<std::string_view>{}(value));
}

auto ComputeTestCaseKey(const TestCase& tc) -> uint64_t {
  uint64_t seed = 0;
  if (tc.IsMultiFile()) {
    for (const auto& file : tc.files) {
      seed = HashCombine(seed, HashString(file.name));
      seed = HashCombine(seed, HashString(file.content));
    }
  } else {
    seed = HashCombine(seed, HashString(tc.sv_code));
  }
  return seed;
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

auto WriteTempFiles(const std::vector<SourceFile>& files, uint64_t key)
    -> std::vector<std::string> {
  auto tmp_dir = std::filesystem::temp_directory_path() / "lyra_test" /
                 std::to_string(key);
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

auto CreateCachedCompilation(const TestCase& tc, uint64_t key)
    -> CachedCompilation {
  frontend::SlangFrontend slang_frontend;
  if (tc.IsMultiFile()) {
    auto paths = WriteTempFiles(tc.files, key);
    auto sv_paths = FilterSvFiles(paths);
    auto work_dir = std::filesystem::path(paths.front()).parent_path();
    auto compilation = slang_frontend.LoadFromFiles(sv_paths);
    return CachedCompilation{
        .compilation = std::move(compilation),
        .source_manager = slang_frontend.GetSourceManagerPtr(),
        .working_dir = std::move(work_dir)};
  }
  auto compilation = slang_frontend.LoadFromString(tc.sv_code);
  return CachedCompilation{
      .compilation = std::move(compilation),
      .source_manager = slang_frontend.GetSourceManagerPtr(),
      .working_dir = {}};
}

auto GetCachedCompilation(const TestCase& tc) -> CachedCompilation& {
  static std::mutex cache_mutex;
  static std::unordered_map<uint64_t, CachedCompilation> cache;

  uint64_t key = ComputeTestCaseKey(tc);
  std::scoped_lock lock(cache_mutex);
  auto it = cache.find(key);
  if (it == cache.end()) {
    auto [inserted, _] = cache.emplace(key, CreateCachedCompilation(tc, key));
    return inserted->second;
  }
  return it->second;
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

TEST(SvFeatureTestCache, CacheKeyDiffersOnContent) {
  TestCase first;
  first.sv_code = "module a; endmodule";
  TestCase second;
  second.sv_code = "module b; endmodule";
  EXPECT_NE(ComputeTestCaseKey(first), ComputeTestCaseKey(second));
}

TEST_P(SvFeatureTest, Interpreter) {
  const auto& tc = GetParam();

  // Skip interpreter test if flag is set
  if (tc.skip_interpreter) {
    GTEST_SKIP() << "Interpreter skipped";
  }

  interpreter::InterpreterResult result;
  auto& cached = GetCachedCompilation(tc);
  if (!cached.working_dir.empty()) {
    ScopedCurrentPath current_dir(cached.working_dir);
    result = interpreter::Interpreter::RunWithCompilation(
        *cached.compilation, cached.source_manager);
  } else {
    result = interpreter::Interpreter::RunWithCompilation(
        *cached.compilation, cached.source_manager);
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
  auto& cached = GetCachedCompilation(tc);
  if (!cached.working_dir.empty()) {
    ScopedCurrentPath current_dir(cached.working_dir);
    result = compiler::Compiler::RunWithCompilation(*cached.compilation, vars);
  } else {
    result = compiler::Compiler::RunWithCompilation(*cached.compilation, vars);
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
  std::vector<TestCase> all_cases;
  auto yaml_paths = GetYamlPaths();

  for (const auto& yaml_path : yaml_paths) {
    auto cases = LoadTestCasesFromYaml(yaml_path.string());
    auto category = ExtractCategory(yaml_path);

    // Prefix test names with category for uniqueness across YAML files
    for (auto& tc : cases) {
      tc.name = category + "_" + tc.name;
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
