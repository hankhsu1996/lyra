#include <cctype>
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
using lyra::test::Suite;
using lyra::test::TestCase;

namespace {

struct CppEnv {
  std::filesystem::path lyra_exe;
  std::filesystem::path cases_root;
  std::filesystem::path suites_yaml;
};

auto ResolveEnv(Runfiles& rf) -> CppEnv {
  CppEnv env;
  env.lyra_exe = rf.Rlocation("_main/lyra");
  env.cases_root = rf.Rlocation("_main/tests/cases");
  env.suites_yaml = rf.Rlocation("_main/tests/suites.yaml");
  return env;
}

class CppTest : public testing::Test {
 public:
  CppTest(const TestCase& c, const CppEnv* env) : case_(&c), env_(env) {
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
  const CppEnv* env_;
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
  static const CppEnv kEnv = ResolveEnv(*runfiles);

  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string suite_name = "architecture_reset";
  for (std::size_t i = 1; i + 1 < args.size(); ++i) {
    if (std::string_view(args[i]) == "--suite") {
      suite_name = args[i + 1];
    }
  }

  const Suite suite = LoadSuite(kEnv.suites_yaml, suite_name);
  static const std::vector<TestCase> kCases =
      FilterCases(LoadCases(kEnv.cases_root), suite);

  if (kCases.empty()) {
    fmt::print(
        stderr,
        "zero cases registered for suite '{}'. "
        "Refusing to report PASS on empty coverage.\n",
        suite_name);
    return 1;
  }

  // The reported group names the backend that ran the case, so a failure in a
  // log says which realization produced it.
  std::string group = suite.backend.value_or("cpp");
  group[0] =
      static_cast<char>(std::toupper(static_cast<unsigned char>(group[0])));

  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  for (const auto& c : kCases) {
    testing::RegisterTest(
        group.c_str(), c.id.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* { return new CppTest(c, &kEnv); });
  }
  // NOLINTEND(cppcoreguidelines-owning-memory)

  return RUN_ALL_TESTS();
}
