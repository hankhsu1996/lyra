#pragma once

#include <cstdint>
#include <map>
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

}  // namespace lyra::test
