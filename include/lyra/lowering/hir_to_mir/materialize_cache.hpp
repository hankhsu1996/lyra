#pragma once

#include <cstdint>
#include <unordered_map>

#include "lyra/common/type.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

// Cache for memoizing UseTemp -> PlaceTemp materialization.
// Lifetime: per top-level expression lowering.
struct PlaceMaterializationCache {
  struct Key {
    int32_t temp_id;
    TypeId type;
    auto operator==(const Key&) const -> bool = default;
  };
  struct KeyHash {
    auto operator()(const Key& k) const -> size_t {
      return std::hash<int32_t>{}(k.temp_id) ^
             (std::hash<uint32_t>{}(k.type.value) << 1);
    }
  };
  std::unordered_map<Key, mir::PlaceId, KeyHash> map;
};

}  // namespace lyra::lowering::hir_to_mir
