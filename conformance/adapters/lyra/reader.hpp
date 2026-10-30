#pragma once

#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/execution.hpp"
#include "conformance/framework/observation.hpp"

namespace conformance::adapters::lyra {

// The compiler under test, and the options every build of a case is given --
// which path it takes, and whatever the run asks of the compiler beyond
// building the program.
struct Tool {
  std::filesystem::path exe;
  std::vector<std::string> build_options;
};

// How Lyra reports the violation of an invariant it established itself. That
// is a fact about its command line, like the exit status and the two streams,
// so a harness reads it from the output rather than linking a compiler library
// to learn it.
inline constexpr std::string_view kInternalErrorReport = "internal error:";

// The design a case states, as Lyra's command line takes it: the instances to
// elaborate, the use model for compilation units, the library map, and the
// sources.
auto DesignArguments(const framework::Case& test_case)
    -> std::vector<std::string>;

// Reads the reports a program Lyra built wrote to its diagnostic stream, which
// is where Lyra writes every report and nothing else, so a line it cannot place
// there is left unplaced rather than passed over as the program's output.
void ReadReports(std::string_view stream, framework::Observation& seen);

// Builds a case's program with Lyra and runs it, reading both as an
// observation. Building and running are two commands, so a program that ran is
// told from one that never did by which command ended it; a build that ended
// is a refusal of the program unless Lyra's words say it ended for a reason of
// its own.
class Reader final : public framework::Reader {
 public:
  explicit Reader(Tool tool);

  [[nodiscard]] auto Observe(
      const framework::Case& test_case,
      const std::filesystem::path& directory) const
      -> framework::Observation override;

 private:
  Tool tool_;
};

}  // namespace conformance::adapters::lyra
