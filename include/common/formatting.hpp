#pragma once

#include <string>

namespace lyra::common {

inline auto Indent(int level, int spaces_per_level = 2) -> std::string {
  return std::string(level * spaces_per_level, ' ');
}

}  // namespace lyra::common
