#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::test {

struct ExpectedOutput {
  std::optional<std::string> exact;
  std::vector<std::string> contains;
  std::vector<std::string> not_contains;
};

auto StripAnsi(std::string_view s) -> std::string;

auto CheckOutput(
    const std::string& actual, const ExpectedOutput& expected,
    std::string_view context) -> std::optional<std::string>;

struct ProbeMarkerRecord {
  std::string name;
  std::string value;
};

struct ProbeExtractionResult {
  // True when both BEGIN and END sentinels were found in order.
  bool complete = false;
  // True when the BEGIN sentinel was found (regardless of END).
  bool begin_found = false;
  // Parsed `name=value` lines from inside the sentinel block.
  std::vector<ProbeMarkerRecord> records;
  // stdout with the entire sentinel block (BEGIN through END inclusive)
  // removed. When `complete` is false, the original stdout is returned
  // unchanged so the caller can include it in failure context.
  std::string residual_stdout;
};

auto ExtractProbeMarkers(std::string_view stdout_text) -> ProbeExtractionResult;

}  // namespace lyra::test
