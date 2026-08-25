#pragma once

#include <format>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::base {

// What a pass settled for each entity of another pool, written in whatever
// order it reached them. Keyed by that pool's own id, one value per id, written
// once and read back by id.
//
// Keyed by identity rather than by name: resolving a name to the thing it names
// happened upstream, so every reference already carries the id it resolved to.
//
// This mints nothing; the id space belongs to the pool the entities came from.
// `base::Translation` is the same idea for a pass that walks that pool in id
// order and can therefore be counted from the start. This one cannot be: the
// order it writes in is the order its walk reaches things, so neither the order
// nor the final count is known while it is being written. An id nobody wrote
// for is a defect, and surfaces at the read that wanted it.
template <typename Id, typename T>
class SymbolTable {
 public:
  // Settles what the declaring stage found for `id`. The id space belongs to
  // another pool, so declaring one identity twice is two writers claiming one
  // entity.
  void Define(Id id, T value) {
    if (id.value >= values_.size()) {
      values_.resize(id.value + 1);
    }
    auto& slot = values_[id.value];
    if (slot.has_value()) {
      throw InternalError(
          std::format(
              "SymbolTable::Define: id {} is already declared", id.value));
    }
    slot = std::move(value);
  }

  [[nodiscard]] auto Get(Id id) const -> const T& {
    if (id.value >= values_.size() || !values_[id.value].has_value()) {
      throw InternalError(
          std::format("SymbolTable::Get: id {} was never declared", id.value));
    }
    return *values_[id.value];
  }

 private:
  std::vector<std::optional<T>> values_;
};

}  // namespace lyra::base
