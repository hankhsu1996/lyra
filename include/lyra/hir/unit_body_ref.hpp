#pragma once

#include <string>
#include <string_view>

namespace lyra::hir {

// One body of a compiled unit, named by both: the unit, which is what it
// publishes and what every referrer names, and the body it is compiled to,
// which is what an object is made of.
struct UnitBodyRef {
  std::string unit;
  std::string body;

  // The body of a unit compiled to one, which is named as the unit is.
  [[nodiscard]] static auto Sole(std::string_view unit) -> UnitBodyRef {
    return UnitBodyRef{.unit = std::string{unit}, .body = std::string{unit}};
  }

  auto operator==(const UnitBodyRef&) const -> bool = default;
};

}  // namespace lyra::hir
