#pragma once

#include <cstddef>
#include <string>

namespace lyra::projection::cpp {

[[nodiscard]] inline auto Indent(std::size_t level) -> std::string {
  std::string result(level * 2, ' ');
  return result;
}

}  // namespace lyra::projection::cpp
