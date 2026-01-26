#include "lyra/lowering/origin_map.hpp"

#include <utility>

namespace lyra::lowering {

auto OriginMap::Record(MirNode mir_node, HirSource hir_source)
    -> common::OriginId {
  common::OriginId id{static_cast<uint32_t>(entries_.size())};
  entries_.push_back({std::move(mir_node), std::move(hir_source)});
  return id;
}

auto OriginMap::Resolve(common::OriginId id) const
    -> std::optional<OriginEntry> {
  if (!id.IsValid() || id.value >= entries_.size()) {
    return std::nullopt;
  }
  return entries_[id.value];
}

}  // namespace lyra::lowering
