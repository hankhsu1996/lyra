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

// The host compiler this corpus was built with. A case can pass under one and
// fail under another -- the runtime is ordinary C++ and its ABI to the
// generated module is hand-declared -- so a log that does not say which one ran
// leaves the reader guessing at the difference between two machines.
auto HostCompiler() -> std::string {
#if defined(__clang_version__)
  return std::string("clang ") + __clang_version__;
#elif defined(__VERSION__)
  return std::string("gcc ") + __VERSION__;
#else
  return "unknown compiler";
#endif
}

struct CorpusEnv {
  std::filesystem::path lyra_exe;
  std::filesystem::path cases_root;
  std::filesystem::path suites_yaml;
};

auto ResolveEnv(Runfiles& rf) -> CorpusEnv {
  CorpusEnv env;
  env.lyra_exe = rf.Rlocation("_main/lyra");
  env.cases_root = rf.Rlocation("_main/tests/cases");
  env.suites_yaml = rf.Rlocation("_main/tests/suites.yaml");
  return env;
}

class CorpusTest : public testing::Test {
 public:
  CorpusTest(const TestCase& c, const CorpusEnv* env) : case_(&c), env_(env) {
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
  const CorpusEnv* env_;
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
  static const CorpusEnv kEnv = ResolveEnv(*runfiles);

  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string suite_name;
  for (std::size_t i = 1; i + 1 < args.size(); ++i) {
    if (std::string_view(args[i]) == "--suite") {
      suite_name = args[i + 1];
    }
  }
  if (suite_name.empty()) {
    fmt::print(stderr, "no suite selected: pass --suite <name>\n");
    return 1;
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

  fmt::print(
      "suite '{}' on the {} backend, built with {}\n", suite_name,
      suite.backend.value_or("cpp"), HostCompiler());

  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  for (const auto& c : kCases) {
    testing::RegisterTest(
        group.c_str(), c.id.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* { return new CorpusTest(c, &kEnv); });
  }
  // NOLINTEND(cppcoreguidelines-owning-memory)

  return RUN_ALL_TESTS();
}
