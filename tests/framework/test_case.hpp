#pragma once

#include <cstdint>
#include <map>
#include <ostream>
#include <optional>
#include <string>
#include <vector>

namespace lyra::test {

struct SourceFile {
  std::string name;
  std::string content;
};

struct TestCase {
  std::string name;
  std::string feature;
  std::string sv_code;
  std::vector<SourceFile> files;
  std::map<std::string, int64_t> expected_values;
  std::optional<uint64_t> expected_time;

  [[nodiscard]] auto IsMultiFile() const -> bool {
    return !files.empty();
  }
};

// GTest printer for readable test names
inline void PrintTo(const TestCase& test_case, std::ostream* os) {
  *os << test_case.feature << "/" << test_case.name;
}

}  // namespace lyra::test
