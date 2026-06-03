#pragma once

#include <cstddef>
#include <cstdint>
#include <string>

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

}  // namespace lyra::backend::cpp
