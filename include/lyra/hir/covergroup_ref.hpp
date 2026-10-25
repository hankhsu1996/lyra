#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>

#include "lyra/base/hash.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

struct CovergroupId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CovergroupId&) const -> std::strong_ordering = default;
};

// The name every unit gives one covergroup type (LRM 19.3): the unit declaring
// it and the covergroup's path in that unit, as a class's key is. Inside one
// unit a covergroup is named by its `CovergroupId`, which the unit's covergroup
// registry maps to and from this pair.
struct CovergroupKey {
  std::string unit_name;
  support::DefPath path;

  auto operator<=>(const CovergroupKey&) const
      -> std::strong_ordering = default;
  // Two equal keys display alike, which is all a pool needs of a hash; equality
  // decides which key is which.
  struct Hash {
    auto operator()(const CovergroupKey& key) const -> std::size_t {
      std::size_t seed = std::hash<std::string>{}(key.unit_name);
      base::HashField(seed, support::DisplayOf(key.path));
      return seed;
    }
  };
};

// A method every covergroup has (LRM 19.8), and every coverpoint and cross of
// one where it applies: `get_coverage` and `get_inst_coverage` of each, and
// `sample`, `set_inst_name`, `start` and `stop` of the covergroup.
enum class CovergroupMethod : std::uint8_t {
  kSample,
  kGetCoverage,
  kGetInstCoverage,
  kSetInstName,
  kStart,
  kStop,
};

}  // namespace lyra::hir
