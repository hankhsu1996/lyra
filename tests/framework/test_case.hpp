#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

namespace lyra::test {

struct SourceFile {
  std::string name;
  std::string content;
};

struct ExpectedOutput {
  std::optional<std::string> exact;
  std::vector<std::string> contains;
  std::vector<std::string> not_contains;

  [[nodiscard]] auto IsExact() const -> bool {
    return exact.has_value();
  }
};

// Hex string for wide values (>64 bits), stored as lowercase without 0x prefix
// e.g., "ffffffffffffffffffffffffffffffff" for 128-bit all-ones
struct HexValue {
  std::string hex;
};

// Expected value type: integers, floating-point, or hex strings for wide values
// shortreal (float) is stored as double since YAML parsing produces double
using ExpectedValue = std::variant<int64_t, double, HexValue>;

struct TestCase {
  std::string name;
  std::string feature;
  std::string source_yaml;  // Path to YAML file for error reporting
  std::string sv_code;
  std::vector<SourceFile> files;
  std::vector<std::string> plusargs;
  std::map<std::string, ExpectedValue> expected_values;
  std::optional<uint64_t> expected_time;
  std::optional<ExpectedOutput> expected_stdout;
  std::map<std::string, ExpectedOutput> expected_files;
  bool skip_codegen = false;      // Skip codegen test (e.g., for hierarchy)
  bool skip_interpreter = false;  // Skip interpreter test

  [[nodiscard]] auto IsMultiFile() const -> bool {
    return !files.empty();
  }
};

// GTest printer for readable test names
inline void PrintTo(const TestCase& test_case, std::ostream* os) {
  *os << test_case.feature << "/" << test_case.name;
}

}  // namespace lyra::test
