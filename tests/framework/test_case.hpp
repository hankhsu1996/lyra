#pragma once

#include <map>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include "tests/framework/test_value.hpp"

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

struct TestCase {
  std::string name;
  std::string feature;
  std::string source_yaml;  // Path to YAML file for error reporting
  std::string sv_code;
  std::vector<SourceFile> files;
  std::vector<std::string> plusargs;
  std::vector<std::string> param_overrides;  // Compile-time param overrides
  std::map<std::string, ExpectedValue> expected_values;
  std::optional<uint64_t> expected_time;
  std::optional<ExpectedOutput> expected_stdout;
  std::map<std::string, ExpectedOutput> expected_files;
  std::optional<ExpectedOutput> expected_error;  // Expected compilation error
  bool pedantic = false;        // Strict LRM compliance mode for this test
  bool trace = false;           // Enable simulation tracing for this test
  bool dump_slot_meta = false;  // Dump slot metadata registry (test-only)
  [[nodiscard]] auto IsMultiFile() const -> bool {
    return !files.empty();
  }
};

// GTest printer for readable test names
inline void PrintTo(const TestCase& test_case, std::ostream* os) {
  *os << test_case.feature << "/" << test_case.name;
}

}  // namespace lyra::test
