#include <cstddef>
#include <filesystem>
#include <gtest/gtest.h>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include <fmt/core.h>

#include "lyra/compiler/compile.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/frontend/load.hpp"
#include "tests/diag_framework/diag_matcher.hpp"
#include "tests/diag_framework/expected_diag.hpp"
#include "tests/diag_framework/yaml_parser.hpp"
#include "tests/framework/runner.hpp"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;
using lyra::test::ExpectedDiag;
using lyra::test::FilterCases;
using lyra::test::FormatDiagnosticsForDebug;
using lyra::test::LoadCases;
using lyra::test::LoadExpectedDiagnostics;
using lyra::test::LoadSuite;
using lyra::test::MatchAll;
using lyra::test::MatchContext;
using lyra::test::TestCase;

namespace {

struct StructEnv {
  std::filesystem::path cases_root;
  std::filesystem::path suites_yaml;
};

auto ResolveEnv(Runfiles& rf) -> StructEnv {
  StructEnv env;
  env.cases_root = rf.Rlocation("_main/tests/cases");
  env.suites_yaml = rf.Rlocation("_main/tests/suites.yaml");
  return env;
}

const std::unordered_set<std::string> kAllowedArgs = {"--no-color"};

class DiagStructTest : public testing::Test {
 public:
  explicit DiagStructTest(const TestCase& c) : case_(&c) {
  }

 protected:
  void TestBody() override {
    for (const auto& a : case_->input.extra_args) {
      if (kAllowedArgs.contains(a)) continue;
      ADD_FAILURE() << case_->id
                    << ": diag_struct_tests does not support CLI-only arg: "
                    << a;
      return;
    }

    lyra::frontend::CompilationInput input;
    if (case_->input.top.has_value()) {
      input.top = *case_->input.top;
    }
    for (const auto& f : case_->input.files) {
      input.files.push_back((case_->case_dir / f).string());
    }

    std::vector<ExpectedDiag> expected;
    try {
      expected = LoadExpectedDiagnostics(*case_);
    } catch (const std::exception& e) {
      ADD_FAILURE() << case_->id
                    << ": failed to parse expect.diagnostics: " << e.what();
      return;
    }

    lyra::diag::DiagnosticSink sink;
    auto result = lyra::compiler::Compile(input, sink);

    if (!result.artifacts.parse) {
      ADD_FAILURE() << case_->id
                    << ": compile produced no ParseResult; cannot match Lyra "
                       "diagnostics. sink:\n"
                    << FormatDiagnosticsForDebug(
                           sink.Diagnostics(), MatchContext{
                                                   .mgr = nullptr,
                                                   .case_dir = case_->case_dir,
                                               });
      return;
    }

    if (!result.slang_ok) {
      ADD_FAILURE()
          << case_->id
          << ": frontend produced slang diagnostics; diag_struct_tests "
             "only matches Lyra-owned diagnostics. Use cli_golden_tests "
             "for slang diagnostic cases.";
      return;
    }

    const MatchContext ctx{
        .mgr = &result.artifacts.parse->diag_sources,
        .case_dir = case_->case_dir,
    };

    if (auto mm = MatchAll(sink.Diagnostics(), expected, ctx)) {
      ADD_FAILURE() << case_->id << ": " << *mm;
    }
  }

 private:
  const TestCase* case_;
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
  static const StructEnv kEnv = ResolveEnv(*runfiles);

  const std::span<char* const> args{argv, static_cast<std::size_t>(argc)};
  std::string suite_name = "architecture_diag_struct";
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

  // NOLINTBEGIN(cppcoreguidelines-owning-memory)
  for (const auto& c : kCases) {
    testing::RegisterTest(
        "DiagStruct", c.id.c_str(), nullptr, nullptr, __FILE__, __LINE__,
        [&c]() -> testing::Test* { return new DiagStructTest(c); });
  }
  // NOLINTEND(cppcoreguidelines-owning-memory)

  return RUN_ALL_TESTS();
}
