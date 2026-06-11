#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>

namespace lyra::backend::cpp {

[[nodiscard]] inline auto Indent(std::size_t level) -> std::string {
  std::string result(level * 2, ' ');
  return result;
}

// Member name of a static-lifetime body local inside its per-instance
// `_StaticFrame`. Flattening nested blocks into one frame can repeat an SV name
// (same identifier in sibling or nested blocks), so the frame-scope var id is
// appended uniformly to keep every member unique. The same name backs the
// member declaration and every read / write of that local.
[[nodiscard]] inline auto StaticFrameMemberName(
    std::string_view name, std::uint32_t id) -> std::string {
  return std::string(name) + "_" + std::to_string(id);
}

}  // namespace lyra::backend::cpp
