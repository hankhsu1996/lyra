#include <cstdlib>
#include <exception>
#include <gtest/gtest.h>
#include <iostream>
#include <span>
#include <utility>
#include <vector>

#include "tests/framework/arg_parse.hpp"
#include "tests/framework/runner.hpp"
#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"
#include "tests/framework/test_discovery.hpp"

namespace lyra::test {
namespace {

// Global storage for test cases - avoids copies in lambda captures
std::vector<TestCase>
    g_test_cases;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
BackendKind
    g_backend;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

// Test fixture that references globally stored test case
class SvFeatureTest : public testing::Test {
 public:
  SvFeatureTest(const TestCase* test_case, BackendKind backend)
      : test_case_(test_case), backend_(backend) {
  }

  void TestBody() override {
    RunTestCase(*test_case_, backend_);
  }

 private:
  const TestCase* test_case_;
  BackendKind backend_;
};

// Register tests dynamically using indices into global storage
void RegisterTests() {
  for (size_t i = 0; i < g_test_cases.size(); ++i) {
    const auto& test_case = g_test_cases[i];
    testing::RegisterTest(
        "SvFeatures", test_case.name.c_str(), nullptr, nullptr,
        test_case.source_yaml.c_str(), static_cast<int>(i),
        [i]() -> testing::Test* {
          // NOLINTNEXTLINE(cppcoreguidelines-owning-memory) - gtest owns ptr
          return new SvFeatureTest(&g_test_cases[i], g_backend);
        });
  }
}

}  // namespace
}  // namespace lyra::test

auto main(int argc, char** argv) -> int {
  // Parse our flags, forward the rest to gtest
  auto [args, remaining] =
      lyra::test::ParseArgs(std::span<char*>(argv, static_cast<size_t>(argc)));

  // Null-terminate for gtest (expects argv-style array)
  remaining.push_back(nullptr);
  int remaining_argc = static_cast<int>(remaining.size() - 1);

  // Initialize gtest before registration
  testing::InitGoogleTest(&remaining_argc, remaining.data());

  // Load test configuration and cases
  try {
    auto configuration = lyra::test::GetTestConfiguration(args);
    lyra::test::g_test_cases = lyra::test::LoadTestCases(
        configuration.yaml_paths, configuration.yaml_directory);
    lyra::test::g_backend = configuration.backend;

    // Register tests dynamically with the configured backend
    lyra::test::RegisterTests();
  } catch (const std::exception& e) {
    std::cerr << "Error loading test cases: " << e.what() << "\n";
    return 1;
  }

  return RUN_ALL_TESTS();
}
