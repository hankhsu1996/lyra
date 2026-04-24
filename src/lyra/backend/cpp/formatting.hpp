#pragma once

#include <cstddef>
#include <string>

namespace lyra::backend::cpp {

[[nodiscard]] inline auto Indent(std::size_t level) -> std::string {
  std::string result(level * 2, ' ');
  return result;
}

}  // namespace lyra::backend::cpp
