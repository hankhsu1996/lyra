#pragma once

#include <cstdint>
#include <variant>

#include "lyra/support/def_path.hpp"

namespace lyra::support {

// A class the lowering builds for its own use, which nothing outside its unit
// names. `disambiguator` is its position among the classes its unit builds so,
// in the order the unit builds them, as the Itanium ABI numbers an unnamed
// type among the unnamed types of its scope and rustc disambiguates a
// definition among the unnamed ones of its parent: a class another unit
// declares here moves none of them.
struct UnnamedClass {
  std::uint32_t disambiguator = 0;

  auto operator==(const UnnamedClass&) const -> bool = default;
};

// How a class is named: by the path another unit reaches it by (LRM 8.3,
// 23.6), or as one nothing outside its unit names.
using ClassName = std::variant<DefPath, UnnamedClass>;

// The path a class is reached by, where another unit reaches it.
[[nodiscard]] inline auto PathOf(const ClassName& name) -> const DefPath* {
  return std::get_if<DefPath>(&name);
}

}  // namespace lyra::support
