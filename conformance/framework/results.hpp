#pragma once

#include <chrono>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

#include "conformance/framework/execution.hpp"

namespace conformance::framework {

// The version of the layout written below. It moves whenever a field goes or
// its meaning changes, so a reader can tell a file it does not understand from
// one it does.
inline constexpr int kResultsFormatVersion = 1;

// One case a run judged.
struct JudgedCase {
  std::string id;
  CaseResult result;
};

// An environment variable as the run had it, or nothing where it was not set.
struct EnvironmentVariable {
  std::string name;
  std::optional<std::string> value;
};

// The environment variables a run's commands are known to read: those every
// program's behaviour can follow -- where programs and libraries are found,
// the locale its messages are written in, where temporary files go -- and the
// ones the tool itself reads, as this process has them, which is as every
// command it starts has them. Only variables named so are kept, so whatever
// else the environment holds, a credential above all, is never written down.
auto EnvironmentOf(const ToolIdentity& tool)
    -> std::vector<EnvironmentVariable>;

// A whole run of the suite on one tool, as a reader who was not there needs
// it to present the verdicts or to run them again: which text of the suite and
// where it was read from, which tool and how it was invoked, in what
// environment, the limits the caller set, which cases were asked for, and
// every case judged. A fact nobody could know is absent rather than guessed: a
// revision only the caller can state, a version a tool does not say, a limit
// the caller did not set.
struct Results {
  std::optional<std::string> suite_revision;
  // The directory the suite's cases, checks and helpers were read from. A
  // command names a case's files under it, so with the revision it says which
  // file of the suite each path is.
  std::filesystem::path suite_directory;
  // The name the caller gives the configuration this run is of -- the tool and
  // the options every case's build is given -- which a reader is shown in
  // place of either, and by which two runs of one configuration are matched.
  std::optional<std::string> configuration;
  std::optional<std::string> tool_revision;
  ToolIdentity tool;
  std::vector<EnvironmentVariable> environment;
  std::optional<std::string> host;
  int jobs = 1;
  std::optional<std::chrono::seconds> time_limit;
  std::optional<std::uint64_t> address_limit_mib;
  std::optional<std::string> memory_ceiling;
  // The case ids the caller named, or nothing where the whole suite was run.
  std::optional<std::vector<std::string>> selection;
  std::chrono::system_clock::time_point started;
  std::chrono::system_clock::time_point ended;
  std::vector<JudgedCase> cases;
};

// The run as JSON, its cases in id order whatever order they finished in, so
// two runs of one suite line up case by case.
auto ToJson(const Results& results) -> std::string;

}  // namespace conformance::framework
