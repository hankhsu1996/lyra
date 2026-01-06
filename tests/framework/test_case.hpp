#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

namespace lyra::test {

struct SourceFile {
  std::string name;
  std::string content;
};

struct ExpectedOutput {
  std::optional<std::string> exact;
  std::vector<std::string> contains;

  [[nodiscard]] auto IsExact() const -> bool {
    return exact.has_value();
  }
};

struct TestCase {
  std::string name;
  std::string feature;
  std::string sv_code;
  std::vector<SourceFile> files;
  std::map<std::string, int64_t> expected_values;
  std::optional<uint64_t> expected_time;
  std::optional<ExpectedOutput> expected_output;
  bool skip_codegen = false;  // Skip codegen test (e.g., for hierarchy)

  [[nodiscard]] auto IsMultiFile() const -> bool {
    return !files.empty();
  }
};

// GTest printer for readable test names
inline void PrintTo(const TestCase& test_case, std::ostream* os) {
  *os << test_case.feature << "/" << test_case.name;
}

}  // namespace lyra::test
