#include "conformance/framework/results.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cstdlib>
#include <format>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/execution.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/overloaded.hpp"
#include "conformance/framework/process.hpp"
#include "conformance/framework/verdict.hpp"

namespace conformance::framework {

namespace {

using Json = nlohmann::ordered_json;

template <typename T>
auto OrNull(const std::optional<T>& value) -> Json {
  return value ? Json(*value) : Json(nullptr);
}

auto Timestamp(std::chrono::system_clock::time_point when) -> std::string {
  return std::format(
      "{:%FT%TZ}", std::chrono::floor<std::chrono::seconds>(when));
}

auto ToJson(const Verdict& verdict) -> Json {
  return std::visit(
      Overloaded{
          [](const Conforms&) { return Json{{"kind", "conforms"}}; },
          [](const DoesNotConform& v) {
            return Json{{"kind", "does not conform"}, {"why", v.why}};
          },
          [](const Unresolved& v) {
            return Json{{"kind", "unresolved"}, {"why", v.why}};
          },
          [](const NotRun& v) {
            return Json{{"kind", "not run"}, {"why", v.why}};
          }},
      verdict);
}

auto ToJson(const RunEnd& end) -> Json {
  return std::visit(
      Overloaded{
          [](const HarnessFailed& e) {
            return Json{{"kind", "harness failed"}, {"why", e.why}};
          },
          [](const ToolLacks& e) {
            return Json{
                {"kind", "tool lacks"},
                {"use_model", e.use_model},
                {"clause", e.clause}};
          },
          [](const Rejected&) { return Json{{"kind", "rejected"}}; },
          [](const ToolFailed& e) {
            return Json{{"kind", "tool failed"}, {"how", e.how}};
          },
          [](const StoppedAtALimit& e) {
            return Json{{"kind", "stopped at a limit"}, {"limit", e.limit}};
          },
          [](const Ran& e) {
            return Json{{"kind", "ran"}, {"exit_code", e.exit_code}};
          }},
      end);
}

auto ToJson(const ProcessEnd& end) -> Json {
  return std::visit(
      Overloaded{
          [](const CouldNotRun& e) {
            return Json{{"kind", "could not run"}, {"reason", e.reason}};
          },
          [](const Exited& e) {
            return Json{{"kind", "exited"}, {"code", e.code}};
          },
          [](const Signalled& e) {
            return Json{{"kind", "signalled"}, {"signal", e.signal}};
          },
          [](const TimedOut&) { return Json{{"kind", "timed out"}}; },
          [](const TestTimedOut&) { return Json{{"kind", "test timed out"}}; }},
      end);
}

auto ToJson(const ObservedReport& report) -> Json {
  return Json{
      {"kind", SpellingOf(report.kind)},   {"line", OrNull(report.line)},
      {"time", OrNull(report.time)},       {"scope", OrNull(report.scope)},
      {"message", OrNull(report.message)}, {"written", report.written}};
}

auto ToJson(const Step& step) -> Json {
  return Json{{"program", step.program.string()},
              {"arguments", step.arguments},
              {"directory", step.directory.string()},
              {"end", ToJson(step.outcome.end)},
              {"stdout", step.outcome.stdout_text},
              {"stderr", step.outcome.stderr_text}};
}

auto ToJson(const JudgedCase& judged) -> Json {
  const Observation& seen = judged.result.seen;
  Json reports = Json::array();
  for (const ObservedReport& report : seen.reports) {
    reports.push_back(ToJson(report));
  }
  Json outputs = Json::array();
  for (const WrittenOutput& output : seen.outputs) {
    outputs.push_back(
        Json{{"name", output.name}, {"text", OrNull(output.text)}});
  }
  Json steps = Json::array();
  for (const Step& step : seen.steps) {
    steps.push_back(ToJson(step));
  }
  return Json{
      {"id", judged.id},
      {"seconds", judged.result.took.count()},
      {"verdict", ToJson(judged.result.verdict)},
      {"end", ToJson(seen.end)},
      {"checks_passed", seen.checks_passed},
      {"ready_for_the_report", seen.ready_for_the_report},
      {"reports", std::move(reports)},
      {"unplaced", seen.unplaced},
      {"outputs", std::move(outputs)},
      {"steps", std::move(steps)}};
}

auto ToJson(
    const ToolIdentity& tool, const std::optional<std::string>& revision)
    -> Json {
  Json programs = Json::array();
  for (const Program& program : tool.programs) {
    programs.push_back(
        Json{
            {"role", program.role},
            {"path", program.path.string()},
            {"version", OrNull(program.version)}});
  }
  return Json{
      {"name", tool.name},
      {"revision", OrNull(revision)},
      {"programs", std::move(programs)},
      {"options", tool.options}};
}

// What every program's behaviour can follow: where it finds programs and
// libraries, the locale it writes its messages in, and where it puts
// temporary files.
constexpr std::array<std::string_view, 5> kEveryProgramReads = {
    "PATH", "LD_LIBRARY_PATH", "LANG", "LC_ALL", "TMPDIR"};

}  // namespace

auto EnvironmentOf(const ToolIdentity& tool)
    -> std::vector<EnvironmentVariable> {
  std::vector<std::string> names{
      kEveryProgramReads.begin(), kEveryProgramReads.end()};
  for (const std::string& name : tool.environment_read) {
    if (std::ranges::find(names, name) == names.end()) {
      names.push_back(name);
    }
  }
  std::vector<EnvironmentVariable> environment;
  for (std::string& name : names) {
    const char* value = std::getenv(name.c_str());
    environment.push_back(
        EnvironmentVariable{
            .name = std::move(name),
            .value = value == nullptr ? std::nullopt
                                      : std::optional<std::string>(value)});
  }
  return environment;
}

auto ToJson(const Results& results) -> std::string {
  std::vector<const JudgedCase*> ordered;
  for (const JudgedCase& judged : results.cases) {
    ordered.push_back(&judged);
  }
  std::ranges::sort(ordered, {}, &JudgedCase::id);
  Json cases = Json::array();
  for (const JudgedCase* judged : ordered) {
    cases.push_back(ToJson(*judged));
  }
  const Json time_limit =
      results.time_limit ? Json(results.time_limit->count()) : Json(nullptr);
  Json environment = Json::object();
  for (const EnvironmentVariable& variable : results.environment) {
    environment[variable.name] = OrNull(variable.value);
  }
  const Json document{
      {"format_version", kResultsFormatVersion},
      {"suite",
       Json{
           {"revision", OrNull(results.suite_revision)},
           {"directory", results.suite_directory.string()}}},
      {"tool", ToJson(results.tool, results.tool_revision)},
      {"environment", std::move(environment)},
      {"host", OrNull(results.host)},
      {"limits",
       Json{
           {"jobs", results.jobs},
           {"time_limit_seconds_per_case", time_limit},
           {"address_limit_mib", OrNull(results.address_limit_mib)},
           {"memory_ceiling", OrNull(results.memory_ceiling)}}},
      {"selection", OrNull(results.selection)},
      {"started", Timestamp(results.started)},
      {"ended", Timestamp(results.ended)},
      {"cases", std::move(cases)}};
  // A program may write bytes that are no UTF-8, and JSON text is UTF-8, so
  // such a byte is written as the replacement character rather than refusing
  // the whole run.
  return document.dump(2, ' ', false, Json::error_handler_t::replace) + "\n";
}

}  // namespace conformance::framework
