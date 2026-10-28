#pragma once

#include <filesystem>
#include <string_view>

#include "conformance_case.hpp"
#include "conformance_observation.hpp"

namespace lyra::test {

// Verilator, as found on the machine running the case. It is a second tool the
// corpus is held up against, never an oracle: what it answers is judged by the
// same claim Lyra's answer is.
struct VerilatorTool {
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
void ReadVerilatorReports(std::string_view stream, Observation& seen);

// Builds a case's program with Verilator and runs it in `directory`, reading
// both as an observation.
auto ObserveWithVerilator(
    const VerilatorTool& tool, const ConformanceCase& test_case,
    const std::filesystem::path& directory) -> Observation;

}  // namespace lyra::test
