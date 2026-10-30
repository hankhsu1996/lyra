#include "conformance_run.hpp"

#include <chrono>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <tuple>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "cli_fixture.hpp"
#include "conformance/adapters/lyra/reader.hpp"
#include "conformance/framework/case.hpp"
#include "conformance/framework/execution.hpp"
#include "conformance/framework/observation.hpp"
#include "conformance/framework/process.hpp"
#include "conformance/framework/verdict.hpp"
#include "lyra/driver/project_layout.hpp"

namespace lyra::test {

using conformance::adapters::lyra::DesignArguments;
using conformance::adapters::lyra::kInternalErrorReport;
using conformance::adapters::lyra::Tool;
using conformance::framework::Case;
using conformance::framework::Conforms;
using conformance::framework::Describe;
using conformance::framework::DoesNotConform;
using conformance::framework::ExitCodeOf;
using conformance::framework::HarnessFailed;
using conformance::framework::Observation;
using conformance::framework::ProcessOutcome;
using conformance::framework::RejectedBeforeRunning;
using conformance::framework::RunChildProcess;
using conformance::framework::RunOneCase;
using conformance::framework::Transcript;
using conformance::framework::Verdict;

namespace {

// How long elaborating a parked case may take before it is called stuck: a
// guard against one that never ends, well above any honest elaboration.
constexpr std::chrono::seconds kStuckAfter{300};

// What every build of a case on a path is given beyond the design: the path's
// selector, and what the run asks of the compiler itself.
auto BuildOptions(const ConformancePath& path) -> std::vector<std::string> {
  std::vector<std::string> options = path.selector;
  // A refusal is matched against recorded text, and read by whoever the match
  // failed for. Both want the diagnostic as it was written rather than wrapped
  // in the escape sequences that colour it for a terminal.
  options.emplace_back("--no-color");
  // Each case's MIR is checked as the lowering states it and after every
  // rewrite, so a defect is reported against whichever made it rather than
  // only as the unit leaves the last.
  options.emplace_back("--mir-verify-each");
  if (const char* scratch = std::getenv("TEST_TMPDIR");
      scratch != nullptr && *scratch != '\0') {
    // Keeping what a run builds beside the shard's other scratch lets one
    // prepared header serve every case the shard runs, without reaching into
    // a store shared with whatever else is running on the machine.
    options.emplace_back("--cache-dir");
    options.push_back(std::string(scratch) + "/lyra-cache");
  }
  return options;
}

// The case with each foreign source compiled after the header Lyra generates
// for the design's DPI boundary. A case's own C declares what it calls by the
// standard's mapping alone (LRM 35.5.6, Annex H), so a prototype the header
// derives otherwise is a compile error of the case rather than a wrong value
// read across the boundary.
auto WithGeneratedHeader(
    const Case& test_case, const std::filesystem::path& directory)
    -> std::expected<Case, std::string> {
  Case checked = test_case;
  const std::filesystem::path wrappers = directory / "against_generated_header";
  std::error_code created;
  std::filesystem::create_directories(wrappers, created);
  if (created) {
    return std::unexpected(
        std::format(
            "cannot create '{}': {}", wrappers.string(), created.message()));
  }
  for (std::filesystem::path& source : checked.link_sources) {
    const std::filesystem::path wrapper = wrappers / source.filename();
    std::ofstream out(wrapper);
    out << std::format(
        "#include \"{}\"\n#include \"{}\"\n", driver::kDpiAbiHeader,
        source.string());
    if (!out) {
      return std::unexpected(
          std::format("cannot write '{}'", wrapper.string()));
    }
    source = wrapper;
  }
  return checked;
}

// Lyra's reader, with each foreign source compiled after the header Lyra
// generates. That header is Lyra's own and no part of the standard, so the
// check is Lyra's: a reader of its own wrapping the suite's, which every other
// tool is handed unwrapped.
class AgainstGeneratedHeader final : public conformance::framework::Reader {
 public:
  explicit AgainstGeneratedHeader(Tool tool) : lyra_(std::move(tool)) {
  }

  [[nodiscard]] auto Observe(
      const Case& test_case, const std::filesystem::path& directory) const
      -> Observation override {
    const auto checked = WithGeneratedHeader(test_case, directory);
    if (!checked) {
      Observation seen;
      seen.end = HarnessFailed{checked.error()};
      return seen;
    }
    return lyra_.Observe(*checked, directory);
  }

 private:
  conformance::adapters::lyra::Reader lyra_;
};

}  // namespace

auto FindConformancePath(std::string_view name)
    -> std::optional<ConformancePath> {
  if (name == "cpp") {
    return ConformancePath{.name = "cpp", .selector = {}};
  }
  if (name == "llvm") {
    return ConformancePath{.name = "llvm", .selector = {"--backend", "llvm"}};
  }
  return std::nullopt;
}

auto CaseRecord::Load(const std::filesystem::path& yaml) -> CaseRecord {
  CaseRecord record;
  if (!std::filesystem::exists(yaml)) {
    return record;
  }
  const YAML::Node root = YAML::LoadFile(yaml.string());
  // A path with nothing to record keeps a file holding only its own
  // explanation, which parses to nothing at all.
  if (root.IsNull()) {
    return record;
  }
  if (!root.IsMap()) {
    throw std::runtime_error(
        std::format(
            "{}: a record is a map of case id to text the run that case "
            "currently produces contains",
            yaml.string()));
  }
  for (const auto& entry : root) {
    record.entries_.emplace(
        entry.first.as<std::string>(), entry.second.as<std::string>());
  }
  return record;
}

auto CaseRecord::Find(std::string_view case_id) const
    -> std::optional<std::string> {
  const auto found = entries_.find(std::string(case_id));
  if (found == entries_.end()) {
    return std::nullopt;
  }
  return found->second;
}

auto CaseRecord::UnmatchedIds(const std::vector<Case>& cases) const
    -> std::vector<std::string> {
  std::unordered_set<std::string> known;
  known.reserve(cases.size());
  for (const Case& c : cases) {
    known.insert(c.id);
  }
  std::vector<std::string> unmatched;
  for (const auto& [id, text] : entries_) {
    if (!known.contains(id)) {
      unmatched.push_back(id);
    }
  }
  return unmatched;
}

auto CaseRecord::Joined(const CaseRecord& other) const -> CaseRecord {
  CaseRecord joined = *this;
  joined.entries_.insert(other.entries_.begin(), other.entries_.end());
  return joined;
}

auto CaseRecord::SharedIds(const CaseRecord& other) const
    -> std::vector<std::string> {
  std::vector<std::string> shared;
  for (const auto& [id, text] : entries_) {
    if (other.entries_.contains(id)) {
      shared.push_back(id);
    }
  }
  return shared;
}

auto CheckParkedCase(
    const std::filesystem::path& lyra_exe, const Case& test_case)
    -> std::optional<std::string> {
  std::vector<std::string> argv{"check", "--no-color"};
  const std::vector<std::string> design = DesignArguments(test_case);
  argv.insert(argv.end(), design.begin(), design.end());

  const ProcessOutcome outcome = RunChildProcess(lyra_exe, argv, kStuckAfter);
  const auto report = [&](std::string_view what) {
    return std::format(
        "{}\n\ncase: {}\n\n{}", what, test_case.id,
        Transcript(lyra_exe, argv, outcome));
  };

  // A case the standard requires a tool to reject is parked the same way any
  // other is, and elaborating one is supposed to fail.
  const bool elaborated = ExitCodeOf(outcome) == 0;
  if (std::holds_alternative<RejectedBeforeRunning>(test_case.claim)) {
    if (elaborated) {
      return report(
          "the standard requires this program to be rejected, and it "
          "elaborated");
    }
    return std::nullopt;
  }
  if (!elaborated) {
    return report("this parked case no longer elaborates");
  }
  return std::nullopt;
}

// The verdict a case's run on a path gets, held to what the path's records say
// of that case. The verdict is the standard's; the records only say which
// verdict this path is known to give, and a verdict that has moved off them
// fails either way, so a record can only shrink.
auto RunConformanceCase(
    const std::filesystem::path& lyra_exe, const ConformancePath& path,
    const Case& test_case, const PathRecords& records)
    -> std::optional<std::string> {
  const AgainstGeneratedHeader reader(
      Tool{.exe = lyra_exe, .build_options = BuildOptions(path)});
  const auto result = RunOneCase(test_case, reader);
  if (!result) {
    return std::format("{}: {}", test_case.id, result.error());
  }
  const Observation& seen = result->seen;
  const Verdict& verdict = result->verdict;
  const auto report = [&](std::string_view what) {
    return std::format(
        "{}\n\ncase: {}\npath: {}\nverdict: {}\n\n{}", what, test_case.id,
        path.name, Describe(verdict), seen.evidence);
  };
  const auto mentions = [&](std::string_view text) {
    return seen.written.find(text) != std::string::npos;
  };

  // A compiler bug is not a limitation and not a wrong answer, so it is neither
  // of the outcomes a path may be recorded as producing. It fails whatever is
  // recorded, which is what keeps "not implemented yet" and "implemented
  // wrongly" answerable apart.
  if (mentions(kInternalErrorReport)) {
    return report("the run reported a compiler bug");
  }
  const bool conforms = std::holds_alternative<Conforms>(verdict);
  const bool falls_short = std::holds_alternative<DoesNotConform>(verdict);
  for (const auto& [record, kind, drop] :
       {std::tuple{
            &records.refusals, "refusal",
            "this path runs the case now; drop its entry from the path's "
            "refusal record so the coverage it just gained is held from here "
            "on"},
        std::tuple{
            &records.defects, "defect",
            "this path answers this case correctly now; drop its entry from "
            "the path's defect record so the behaviour is held from here "
            "on"}}) {
    const std::optional<std::string> recorded = record->Find(test_case.id);
    if (!recorded) {
      continue;
    }
    if (conforms) {
      return report(drop);
    }
    if (!falls_short || !mentions(*recorded)) {
      return report(
          std::format(
              "this case falls short for a reason other than the recorded {} "
              "'{}'",
              kind, *recorded));
    }
    return std::nullopt;
  }
  if (!conforms) {
    return report("the path does not run this case as the standard requires");
  }
  return std::nullopt;
}

}  // namespace lyra::test
