#pragma once

#include <string>

namespace lyra::common {

enum class FormatMode {
  kPlain,      // Raw, unindented output for tests and logging
  kContextual  // Indented, structured output for display
};

inline auto Indent(int level, int spaces_per_level = 2) -> std::string {
  // NOLINTNEXTLINE(modernize-return-braced-init-list)
  return std::string(level * spaces_per_level, ' ');
}

}  // namespace lyra::common
