#include <cstddef>
#include <filesystem>
#include <gtest/gtest.h>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "tests/framework/runner.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::FilterCases;
using lyra::test::LoadCases;
using lyra::test::LoadSuite;
using lyra::test::RunCase;
using lyra::test::TestCase;

namespace {

struct GoldenEnv {
  std::filesystem::path lyra_exe;
  std::filesystem::path cases_root;
  std::filesystem::path suites_yaml;
};

auto ResolveEnv(Runfiles& rf) -> GoldenEnv {
  GoldenEnv env;
  env.lyra_exe = rf.Rlocation("_main/lyra");
  env.cases_root = rf.Rlocation("_main/tests/cases");
  env.suites_yaml = rf.Rlocation("_main/tests/suites.yaml");
  return env;
}

class CliGoldenTest : public testing::Test {
 public:
  CliGoldenTest(const TestCase& c, const GoldenEnv* env)
      : case_(&c), env_(env) {
  }

 protected:
  void TestBody() override {
    auto result = RunCase(env_->lyra_exe, *case_);
    if (result.mismatch) {
      ADD_FAILURE() << *result.mismatch;
    }
  }

 private:
  const TestCase* case_;
  const GoldenEnv* env_;
};

}  // namespace

auto main(int argc, char** argv) -> int {
  testing::InitGoogleTest(&argc, argv);

  std::string err;
  std::unique_ptr<Runfiles> runfiles{Runfiles::CreateForTest(&err)};
  if (!runfiles) {
    fmt::print(stderr, "failed to create runfiles: {}\n", err);
    return 1;
  }
  // Environment and cases live for the remainder of the process so
  // RegisterTest factories can safely capture references into them.
  static const GoldenEnv kEnv = ResolveEnv(*runfiles);

  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string suite_name = "architecture_reset";
  for (std::size_t i = 1; i + 1 < args.size(); ++i) {
    if (std::string_view(args[i]) == "--suite") {
      suite_name = args[i + 1];
    }
  }

  static const std::vector<TestCase> kCases = [&] {
    auto loaded = LoadCases(kEnv.cases_root);
    auto suite = LoadSuite(kEnv.suites_yaml, suite_name);
    return FilterCases(loaded, suite);
  }();

  // NOLINTBEGIN(cppcoreguidelines-owning-memory) -- gtest's RegisterTest
  // factory API mandates heap-allocated Test subclasses; the test framework
  // takes ownership and destroys them after the test completes.
  for (const auto& c : kCases) {
    testing::RegisterTest(
        "CliGolden", c.id.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* { return new CliGoldenTest(c, &kEnv); });
  }
  // NOLINTEND(cppcoreguidelines-owning-memory)

  return RUN_ALL_TESTS();
}
