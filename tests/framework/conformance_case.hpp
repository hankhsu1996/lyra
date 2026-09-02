#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace lyra::test {

// The text a case prints once every check it makes has passed. A case that
// reports success without having run its checks is the failure mode the corpus
// has to exclude, so success is this string appearing rather than the program
// merely surviving.
inline constexpr std::string_view kAllChecksPassed = "All checks passed";

// The source a case is entered through. A case is a directory, and this is the
// file in it that states the claim and makes the checks.
inline constexpr std::string_view kCaseEntrySource = "main.sv";

// What a case's entry source is renamed to when the case is parked, which is
// what a case whose subject cannot be reached at all comes to. The harness does
// not run one, so this name is what keeps it out of the corpus proper.
inline constexpr std::string_view kParkedCaseEntry = "main.sv.deferred";

// What a case claims about the diagnostics its run writes. IEEE 1800 states
// requirements whose whole observable is a message -- LRM 12.4.2 requires a
// violation report where no condition of a qualified if matched, and forbids
// one where an explicit else covers the rest -- and a program cannot read a
// message about itself, so such a claim is stated beside the checks instead of
// being made in SystemVerilog.
//
// Both directions exist because either alone is satisfied by an implementation
// that says nothing at all, or by one that says something everywhere.
struct ReportsUnstated {};

struct ReportsText {
  std::string text;
};

struct ReportsNothing {};

using ReportClaim = std::variant<ReportsUnstated, ReportsText, ReportsNothing>;

// One conformance case: a SystemVerilog program stating what IEEE 1800 requires
// and checking itself against it. A case is a directory, so what belongs to it
// is where a file sits rather than something a directive has to say; the
// directives that remain state what the standard cannot be read from the
// sources alone.
struct ConformanceCase {
  // The case directory's path under the corpus root, which is its identity: the
  // outer directory is the LRM clause it tests and the inner one is the subject
  // within that clause.
  std::string id;
  std::filesystem::path directory;
  // The source stating the claim and making the checks, which a parked case
  // holds under a name the harness does not collect.
  std::filesystem::path entry;
  // The sources compiled ahead of the entry source, in the order given. A
  // reference reaches only the part of a compilation-unit scope declared before
  // it (LRM 3.12.1), so what the case is written against is compiled first.
  std::vector<std::filesystem::path> supporting_sources;
  // Native sources providing the foreign symbols this case's imports name
  // (LRM 35.4).
  std::vector<std::filesystem::path> link_sources;
  // The instances to elaborate. A case that names none elaborates `Top`.
  std::vector<std::string> tops;
  std::vector<std::string> front_end_args;
  // The simulated program's own arguments, where LRM 21.6 plusargs reach a
  // design.
  std::vector<std::string> program_args;
  // Set when IEEE 1800 requires the program to be rejected rather than run,
  // holding text the diagnostic has to contain. Such a case makes no checks and
  // prints no sentinel.
  std::optional<std::string> required_error;
  // What the standard requires this run to report, where a report is the whole
  // of what it requires. A case making no such claim is held to nothing about
  // what it wrote.
  ReportClaim reports;
};

// Every case under `corpus_root`, ordered by id. A directory holding the entry
// source is a case; one that does not is a clause, so the corpus's shape is
// read directly rather than inferred from what the cases mention.
auto LoadConformanceCases(const std::filesystem::path& corpus_root)
    -> std::vector<ConformanceCase>;

// Every parked case under `corpus_root`, ordered by id. Nothing runs one, so
// nothing would notice it ceasing to be a legal program either; collecting them
// is what lets the run hold them to at least that much.
auto LoadParkedCases(const std::filesystem::path& corpus_root)
    -> std::vector<ConformanceCase>;

}  // namespace lyra::test
