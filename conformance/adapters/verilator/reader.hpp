#pragma once

#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>

#include "conformance/framework/case.hpp"
#include "conformance/framework/execution.hpp"
#include "conformance/framework/observation.hpp"

namespace conformance::adapters::verilator {

// Verilator, as found on the machine running the case. It is a second tool the
// corpus is held up against, never an oracle: what it answers is judged by the
// same claim Lyra's answer is.
struct Tool {
  std::filesystem::path exe;
  // The C compiler a DPI-C source is built with, and the directory holding
  // the standard header Verilator ships for it.
  std::filesystem::path c_compiler;
  std::filesystem::path standard_headers;
};

// Reads the reports in one stream of a program Verilator built. Verilator
// writes its reports among the program's own output, so only a line in the
// shape of one of its reports is read as a report, and the rest is the
// program's.
void ReadReports(std::string_view stream, framework::Observation& seen);

// Verilator and the C compiler named, or the ones on PATH where a name is not
// given, with the directory of the standard header that Verilator says it
// ships. They are the machine's own installs and never build inputs. Why it
// could not be found where it could not.
auto FindTool(
    std::optional<std::filesystem::path> exe,
    std::optional<std::filesystem::path> c_compiler)
    -> std::expected<Tool, std::string>;

// What Verilator says of its own version, on one line.
auto VersionOf(const Tool& tool) -> std::string;

// Builds a case's program with Verilator and runs it, reading both as an
// observation.
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

}  // namespace conformance::adapters::verilator
