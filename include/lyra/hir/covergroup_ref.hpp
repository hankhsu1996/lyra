#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/base/pool_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

struct CovergroupId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const CovergroupId&) const -> std::strong_ordering = default;
};

// A covergroup type this unit declares (LRM 19.3).
struct LocalCovergroupRef {
  CovergroupId covergroup;

  auto operator==(const LocalCovergroupRef&) const -> bool = default;
};

// A covergroup type another unit declares, named by that unit and the path the
// covergroup has in it, as a class of another unit is.
struct ExternalCovergroupRef {
  std::string unit_name;
  support::DefPath path;

  auto operator==(const ExternalCovergroupRef&) const -> bool = default;
};

using CovergroupRef = std::variant<LocalCovergroupRef, ExternalCovergroupRef>;

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
