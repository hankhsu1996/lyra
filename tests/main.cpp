#include <cstdlib>
#include <exception>
#include <format>
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
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
std::vector<TestCase> g_test_cases;
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
BackendKind g_backend;
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
bool g_force_two_state = false;
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
bool g_share_procs = false;

// Test fixture that references globally stored test case
class SvFeatureTest : public testing::Test {
 public:
  SvFeatureTest(
      const TestCase* test_case, BackendKind backend, bool force_two_state,
      bool share_procs)
      : test_case_(test_case),
        backend_(backend),
        force_two_state_(force_two_state),
        share_procs_(share_procs) {
  }

  void TestBody() override {
    RunTestCase(*test_case_, backend_, force_two_state_, share_procs_);
  }

 private:
  const TestCase* test_case_;
  BackendKind backend_;
  bool force_two_state_;
  bool share_procs_;
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
          return new SvFeatureTest(
              &g_test_cases[i], g_backend, g_force_two_state, g_share_procs);
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

    // Framework-level sharding: deterministic filter over sorted YAML paths
    if (!args.shard_count.empty()) {
      auto shard_count = std::stoi(args.shard_count);
      auto shard_index =
          args.shard_index.empty() ? 0 : std::stoi(args.shard_index);
      if (shard_count < 1) {
        throw std::runtime_error("--shard_count must be >= 1");
      }
      if (shard_index < 0 || shard_index >= shard_count) {
        throw std::runtime_error(
            std::format(
                "--shard_index={} out of range [0, {})", shard_index,
                shard_count));
      }
      auto& paths = configuration.yaml_paths;
      std::vector<std::filesystem::path> filtered;
      for (size_t i = 0; i < paths.size(); ++i) {
        if (static_cast<int>(i % shard_count) == shard_index) {
          filtered.push_back(std::move(paths[i]));
        }
      }
      paths = std::move(filtered);
    }

    lyra::test::g_test_cases = lyra::test::LoadTestCases(
        configuration.yaml_paths, configuration.yaml_directory);
    lyra::test::g_backend = configuration.backend;
    lyra::test::g_force_two_state = configuration.force_two_state;
    lyra::test::g_share_procs = configuration.share_procs;

    // Register tests dynamically with the configured backend
    lyra::test::RegisterTests();
  } catch (const std::exception& e) {
    std::cerr << "Error loading test cases: " << e.what() << "\n";
    return 1;
  }

  return RUN_ALL_TESTS();
}
