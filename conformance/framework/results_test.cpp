// What a run leaves for whoever presents it or runs it again: every case once,
// in id order, with the verdict, the reading it came from and each command as
// its own step, the environment the commands read, and what nobody could know
// written as unknown rather than guessed.

#include "conformance/framework/results.hpp"

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/execution.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"
#include "conformance/framework/verdict.hpp"

namespace conformance::framework {
namespace {

using Json = nlohmann::json;

auto Judged(std::string id, Observation seen, Verdict verdict) -> JudgedCase {
  return JudgedCase{
      .id = std::move(id),
      .result = CaseResult{
          .seen = std::move(seen),
          .verdict = std::move(verdict),
          .took = std::chrono::duration<double>(1.5)}};
}

auto ARun() -> Results {
  Observation built_then_ran;
  built_then_ran.end = Ran{.exit_code = 0};
  built_then_ran.checks_passed = true;
  built_then_ran.steps = {
      Step{
          .program = "/bin/tool",
          .arguments = {"--build", "main.sv"},
          .directory = "/scratch/a",
          .outcome =
              ProcessOutcome{
                  .end = Exited{.code = 0},
                  .stdout_text = "built\n",
                  .stderr_text = {}}},
      Step{
          .program = "/scratch/a/program",
          .arguments = {},
          .directory = "/scratch/a",
          .outcome = ProcessOutcome{
              .end = TimedOut{},
              .stdout_text = "All checks passed\n",
              .stderr_text = "\xff not text\n"}}};
  built_then_ran.reports = {ObservedReport{
      .kind = ReportKind::kError,
      .line = 12,
      .time = std::nullopt,
      .scope = std::nullopt,
      .message = std::nullopt,
      .written = "%Error: main.sv:12: x"}};
  built_then_ran.outputs = {WrittenOutput{.name = "dump.vcd", .text = {}}};

  Observation refused;
  refused.end = Rejected{};

  Results results{
      .suite_revision = "0123abcd",
      .suite_directory = "/suite",
      .tool_revision = std::nullopt,
      .tool =
          ToolIdentity{
              .name = "tool",
              .programs = {Program{
                  .role = "tool", .path = "/bin/tool", .version = {}}},
              .options = {"--fast"},
              .environment_read = {"TOOL_HOME"}},
      .environment =
          {EnvironmentVariable{.name = "PATH", .value = "/bin"},
           EnvironmentVariable{.name = "TOOL_HOME", .value = std::nullopt}},
      .host = "Linux 6.6 x86_64",
      .jobs = 4,
      .time_limit = std::chrono::seconds(60),
      .address_limit_mib = std::nullopt,
      .memory_ceiling = std::nullopt,
      .selection = std::nullopt,
      .started = {},
      .ended = {},
      .cases = {}};
  // Finished in the order a pool of workers gives, not the cases' own.
  results.cases.push_back(
      Judged("12_statements/b", std::move(refused), DoesNotConform{"refused"}));
  results.cases.push_back(
      Judged("09_processes/a", std::move(built_then_ran), Conforms{}));
  return results;
}

TEST(Results, ListEveryCaseOnceInIdOrder) {
  const Json run = Json::parse(ToJson(ARun()));
  ASSERT_EQ(run["cases"].size(), 2U);
  EXPECT_EQ(run["cases"][0]["id"], "09_processes/a");
  EXPECT_EQ(run["cases"][1]["id"], "12_statements/b");
}

TEST(Results, StateTheVerdictAndWhy) {
  const Json run = Json::parse(ToJson(ARun()));
  EXPECT_EQ(run["cases"][0]["verdict"], (Json{{"kind", "conforms"}}));
  EXPECT_EQ(
      run["cases"][1]["verdict"],
      (Json{{"kind", "does not conform"}, {"why", "refused"}}));
  EXPECT_EQ(run["cases"][1]["end"], (Json{{"kind", "rejected"}}));
}

TEST(Results, KeepEachCommandAsItsOwnStep) {
  const Json steps = Json::parse(ToJson(ARun()))["cases"][0]["steps"];
  ASSERT_EQ(steps.size(), 2U);
  EXPECT_EQ(steps[0]["program"], "/bin/tool");
  EXPECT_EQ(steps[0]["arguments"], (Json{"--build", "main.sv"}));
  EXPECT_EQ(steps[0]["directory"], "/scratch/a");
  EXPECT_EQ(steps[0]["end"], (Json{{"kind", "exited"}, {"code", 0}}));
  EXPECT_EQ(steps[0]["stdout"], "built\n");
  EXPECT_EQ(steps[1]["end"], (Json{{"kind", "timed out"}}));
}

TEST(Results, WriteBytesThatAreNoTextAsReplacements) {
  const Json steps = Json::parse(ToJson(ARun()))["cases"][0]["steps"];
  EXPECT_EQ(steps[1]["stderr"], "\xef\xbf\xbd not text\n");
}

TEST(Results, WriteWhatNobodyCouldKnowAsUnknown) {
  Results unnamed_host = ARun();
  unnamed_host.host = std::nullopt;
  const Json run = Json::parse(ToJson(unnamed_host));
  EXPECT_TRUE(run["host"].is_null());
  EXPECT_EQ(run["cases"][0]["seconds"], 1.5);
  EXPECT_EQ(run["format_version"], kResultsFormatVersion);
  EXPECT_EQ(run["suite"]["revision"], "0123abcd");
  EXPECT_TRUE(run["tool"]["revision"].is_null());
  EXPECT_TRUE(run["tool"]["programs"][0]["version"].is_null());
  EXPECT_TRUE(run["limits"]["address_limit_mib"].is_null());
  EXPECT_TRUE(run["limits"]["memory_ceiling"].is_null());
  EXPECT_EQ(run["limits"]["time_limit_seconds_per_case"], 60);
  EXPECT_EQ(run["suite"]["directory"], "/suite");
  EXPECT_EQ(
      run["environment"], (Json{{"PATH", "/bin"}, {"TOOL_HOME", nullptr}}));
  EXPECT_TRUE(run["selection"].is_null());
  const Json report = run["cases"][0]["reports"][0];
  EXPECT_EQ(report["kind"], "error");
  EXPECT_EQ(report["line"], 12);
  EXPECT_TRUE(report["time"].is_null());
  EXPECT_TRUE(run["cases"][0]["outputs"][0]["text"].is_null());
}

// Only variables named as read are written down, whatever else the run's
// environment holds.
TEST(Results, KeepOnlyTheEnvironmentNamedAsRead) {
  ASSERT_EQ(setenv("CONFORMANCE_TEST_READ", "kept", 1), 0);
  ASSERT_EQ(setenv("CONFORMANCE_TEST_SECRET", "not kept", 1), 0);
  const std::vector<EnvironmentVariable> environment = EnvironmentOf(
      ToolIdentity{
          .name = "tool",
          .programs = {},
          .options = {},
          .environment_read = {"CONFORMANCE_TEST_READ", "PATH"}});
  unsetenv("CONFORMANCE_TEST_READ");
  unsetenv("CONFORMANCE_TEST_SECRET");
  const auto named = [&](std::string_view name) {
    return std::ranges::count(environment, name, &EnvironmentVariable::name);
  };
  EXPECT_EQ(named("PATH"), 1);
  EXPECT_EQ(named("CONFORMANCE_TEST_READ"), 1);
  EXPECT_EQ(named("CONFORMANCE_TEST_SECRET"), 0);
  const auto read = std::ranges::find(
      environment, "CONFORMANCE_TEST_READ", &EnvironmentVariable::name);
  ASSERT_NE(read, environment.end());
  EXPECT_EQ(read->value, "kept");
}

}  // namespace
}  // namespace conformance::framework
