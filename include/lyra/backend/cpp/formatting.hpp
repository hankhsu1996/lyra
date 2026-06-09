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

// Member name for a cross-unit reference slot: the resolved `Var<T>*` the
// constructor points at the child's member. The same name backs the slot's
// declaration, its constructor resolution, and every read / write / subscribe.
[[nodiscard]] inline auto CrossUnitRefSlotName(std::uint32_t slot)
    -> std::string {
  return "xref_" + std::to_string(slot);
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
