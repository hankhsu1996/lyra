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

// 4-state value with dual-plane encoding for X/Z support in assertions.
// Encoding per bit: unknown=0,value=0→0; unknown=0,value=1→1;
//                   unknown=1,value=0→X; unknown=1,value=1→Z
// Word ordering: word[0] = least significant 64 bits (LSB-first).
// Within a word: bit 0 = LSB.
struct FourStateValue {
  uint32_t width;
  std::vector<uint64_t> value;    // Value/Z bits
  std::vector<uint64_t> unknown;  // Unknown bits (0 = known)
};

// Expected value type: integers, floating-point, hex strings, or 4-state values
// shortreal (float) is stored as double since YAML parsing produces double
using ExpectedValue = std::variant<int64_t, double, HexValue, FourStateValue>;

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
  [[nodiscard]] auto IsMultiFile() const -> bool {
    return !files.empty();
  }
};

// GTest printer for readable test names
inline void PrintTo(const TestCase& test_case, std::ostream* os) {
  *os << test_case.feature << "/" << test_case.name;
}

}  // namespace lyra::test
