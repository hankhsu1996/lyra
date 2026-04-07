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
#include "tests/framework/timing_collector.hpp"

namespace lyra::test {
namespace {

// Global storage for test cases - avoids copies in lambda captures
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
std::vector<TestCase> g_test_cases;
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
BackendKind g_backend;
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
bool g_force_two_state = false;

// Test fixture that references globally stored test case
class SvFeatureTest : public testing::Test {
 public:
  SvFeatureTest(
      const TestCase* test_case, BackendKind backend, bool force_two_state)
      : test_case_(test_case),
        backend_(backend),
        force_two_state_(force_two_state) {
  }

  void TestBody() override {
    RunTestCase(*test_case_, backend_, force_two_state_);
  }

 private:
  const TestCase* test_case_;
  BackendKind backend_;
  bool force_two_state_;
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
              &g_test_cases[i], g_backend, g_force_two_state);
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

  // Enable timing collection if requested
  if (args.timing) {
    lyra::test::SetTimingEnabled(true);
  }

  // Initialize gtest before registration
  testing::InitGoogleTest(&remaining_argc, remaining.data());

  // Build manifest: resolves suite/test_file, loads cases, applies sharding.
  // Sharding is resolved here from CLI args + Bazel env vars, then passed
  // explicitly to BuildManifest (which does not read environment itself).
  try {
    auto shard = lyra::test::ResolveShardSpec(args);
    auto manifest = lyra::test::BuildManifest(args, shard);

    lyra::test::g_test_cases = std::move(manifest.cases);
    lyra::test::g_backend = manifest.backend;
    lyra::test::g_force_two_state = manifest.force_two_state;

    // Disable gtest's internal sharding. Bazel sets GTEST_TOTAL_SHARDS
    // and GTEST_SHARD_INDEX alongside TEST_TOTAL_SHARDS/TEST_SHARD_INDEX.
    // Without clearing these, gtest applies a second layer of sharding
    // on top of the framework's case-level sharding, causing test cases
    // to be silently dropped.
    unsetenv("GTEST_TOTAL_SHARDS");
    unsetenv("GTEST_SHARD_INDEX");

    // Register tests dynamically with the configured backend
    lyra::test::RegisterTests();

    // Invariant: gtest must have registered exactly as many tests as the
    // framework loaded. total_test_count() includes all registered tests
    // regardless of --gtest_filter, so this check is valid even when
    // intentional filtering is applied via BUILD.bazel args.
    auto* unit_test = testing::UnitTest::GetInstance();
    auto registered = static_cast<size_t>(unit_test->total_test_count());
    auto expected = lyra::test::g_test_cases.size();
    if (registered != expected) {
      std::cerr << std::format(
          "Test registration mismatch: gtest has {} tests but framework "
          "loaded {} cases. This indicates a bug in test discovery or "
          "registration, not in filtering.\n",
          registered, expected);
      return 1;
    }
  } catch (const std::exception& e) {
    std::cerr << "Error loading test cases: " << e.what() << "\n";
    return 1;
  }

  int test_result = RUN_ALL_TESTS();

  if (args.timing) {
    lyra::test::GetTimingCollector().PrintSummary();
  }

  return test_result;
}
