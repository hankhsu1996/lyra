#pragma once

#include <cstdint>
#include <vector>

namespace lyra::common {

enum class SelectionStepKind : uint8_t {
  kBranch,
  kArrayEntry,
};

// One constructor-selection step inside the definition.
// construct_index is local to the parent coordinate prefix.
struct SelectionStepDesc {
  SelectionStepKind kind;
  uint32_t construct_index;
  uint32_t alt_index;

  auto operator==(const SelectionStepDesc& other) const -> bool = default;
};

using RepertoireCoord = std::vector<SelectionStepDesc>;

}  // namespace lyra::common
