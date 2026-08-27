#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "conformance_case.hpp"

namespace lyra::test {

// A path through the compiler, named by the artifact it produces rather than by
// how that artifact is run. Execution modes over one emitted form share an
// acceptance surface and so cannot disagree about which programs are accepted,
// which is why they are one path here and not several.
struct ConformancePath {
  std::string name;
  // The arguments that select this path, which is the only place a path is
  // named -- no case states one.
  std::vector<std::string> selector;
  // Whether a run on this path builds a precompiled prelude. One is worth
  // caching across the many cases a shard runs, and a path that builds none has
  // nothing to cache.
  bool caches_prelude = false;
};

// The path of that name, or nothing. Only a path this returns can be run, so a
// misspelled selection fails rather than silently running the default.
auto FindConformancePath(std::string_view name)
    -> std::optional<ConformancePath>;

// One path's record of the cases it does not simply run: each entry pairs a
// case with text the run it produces has to contain. A case that starts passing
// fails until its entry goes, so a record only ever shrinks.
//
// A path keeps two, and they mean different things. The refusals are what the
// path says it cannot do, and they are its coverage report. The defects are
// where it answers wrongly, which is not a limitation but a bug someone has
// diagnosed and chosen to hold rather than fix today. They are separate files
// because the second must not quietly enlarge the first: a wrong answer
// recorded as a refusal would make the coverage report claim ground the path
// does not hold.
class CaseRecord {
 public:
  // Reads a record. A path with nothing to record yields an empty one, whether
  // it keeps no file or a file holding only its own explanation.
  static auto Load(const std::filesystem::path& yaml) -> CaseRecord;

  // The text recorded for a case, or nothing when this record says nothing
  // about it.
  [[nodiscard]] auto Find(std::string_view case_id) const
      -> std::optional<std::string>;

  // Recorded ids matching no case in the corpus. An entry naming a case that
  // was renamed or deleted would otherwise sit unexamined forever.
  [[nodiscard]] auto UnmatchedIds(const std::vector<ConformanceCase>& cases)
      const -> std::vector<std::string>;

  // Ids this record and another both hold. A case is refused or answered
  // wrongly, never both, and two records disagreeing about one case would leave
  // no way to say which outcome the run is held to.
  [[nodiscard]] auto SharedIds(const CaseRecord& other) const
      -> std::vector<std::string>;

 private:
  std::unordered_map<std::string, std::string> entries_;
};

// What one path is held to for the cases it does not simply run.
struct PathRecords {
  CaseRecord refusals;
  CaseRecord defects;
};

// Runs one case on one path and judges it. Nothing on success; on failure, the
// whole account of the run -- what was invoked, how it terminated, and both
// streams -- since a conformance failure is read without re-running it.
auto RunConformanceCase(
    const std::filesystem::path& lyra_exe, const ConformancePath& path,
    const ConformanceCase& test_case, const PathRecords& records)
    -> std::optional<std::string>;

// Elaborates a parked case without lowering or running it. Nothing runs a
// parked case, so nothing else would notice it ceasing to be a legal program --
// a rename elsewhere, a companion that stopped declaring what it declared. This
// holds it to the least a case must be, so that whoever restores it is reading
// what was parked rather than what rotted since.
auto CheckParkedCase(
    const std::filesystem::path& lyra_exe, const ConformanceCase& test_case)
    -> std::optional<std::string>;

}  // namespace lyra::test
