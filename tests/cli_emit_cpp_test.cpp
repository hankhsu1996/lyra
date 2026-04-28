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
using lyra::test::IsEmitCppCase;
using lyra::test::LoadCases;
using lyra::test::LoadSuite;
using lyra::test::RunCase;
using lyra::test::TestCase;

namespace {

struct EmitCppEnv {
  std::filesystem::path lyra_exe;
  std::filesystem::path cases_root;
  std::filesystem::path suites_yaml;
  lyra::test::CppRunPaths cpp_paths;
};

auto ResolveEnv(Runfiles& rf) -> EmitCppEnv {
  EmitCppEnv env;
  env.lyra_exe = rf.Rlocation("_main/lyra");
  env.cases_root = rf.Rlocation("_main/tests/cases");
  env.suites_yaml = rf.Rlocation("_main/tests/suites.yaml");

  const std::filesystem::path engine_hpp =
      rf.Rlocation("_main/include/lyra/runtime/engine.hpp");
  env.cpp_paths.include_root =
      engine_hpp.parent_path().parent_path().parent_path();

  const std::filesystem::path engine_cpp =
      rf.Rlocation("_main/src/lyra/runtime/engine.cpp");
  const std::filesystem::path base_cpp =
      rf.Rlocation("_main/src/lyra/base/internal_error.cpp");
  env.cpp_paths.runtime_src_dirs = {
      engine_cpp.parent_path(), base_cpp.parent_path()};
  return env;
}

class CliEmitCppTest : public testing::Test {
 public:
  CliEmitCppTest(const TestCase& c, const EmitCppEnv* env)
      : case_(&c), env_(env) {
  }

 protected:
  void TestBody() override {
    auto result = RunCase(env_->lyra_exe, *case_, env_->cpp_paths);
    if (result.mismatch) {
      ADD_FAILURE() << *result.mismatch;
    }
  }

 private:
  const TestCase* case_;
  const EmitCppEnv* env_;
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
  static const EmitCppEnv kEnv = ResolveEnv(*runfiles);

  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string suite_name = "architecture_reset";
  for (std::size_t i = 1; i + 1 < args.size(); ++i) {
    if (std::string_view(args[i]) == "--suite") {
      suite_name = args[i + 1];
    }
  }

  // Only emit-cpp cases (`command: [run, cpp]`) live in this target. They
  // need a host C++ compiler at runtime; the cc_test is tagged
  // `requires-host-cxx` and excluded from `bazel test //...` in CI.
  static const std::vector<TestCase> kCases = [&] {
    auto loaded = LoadCases(kEnv.cases_root);
    auto suite = LoadSuite(kEnv.suites_yaml, suite_name);
    auto filtered = FilterCases(loaded, suite);
    std::erase_if(
        filtered, [](const TestCase& c) { return !IsEmitCppCase(c); });
    return filtered;
  }();

  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  for (const auto& c : kCases) {
    testing::RegisterTest(
        "CliEmitCpp", c.id.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* { return new CliEmitCppTest(c, &kEnv); });
  }
  // NOLINTEND(cppcoreguidelines-owning-memory)

  return RUN_ALL_TESTS();
}
