#pragma once

#include <string>
#include <string_view>

namespace lyra::hir {

// One specialization of a compiled unit, named by both: the unit, which is
// what it publishes and what every referrer names, and what its objects are
// built as -- the same code, the same blocks, the same children -- which is
// what the design's construction tables are stated by.
struct SpecializationRef {
  std::string unit;
  std::string specialization;

  // The specialization of a unit that has one, which is named as the unit is.
  [[nodiscard]] static auto Sole(std::string_view unit) -> SpecializationRef {
    return SpecializationRef{
        .unit = std::string{unit}, .specialization = std::string{unit}};
  }

  auto operator==(const SpecializationRef&) const -> bool = default;
};

}  // namespace lyra::hir
