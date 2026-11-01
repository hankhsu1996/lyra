#pragma once

#include <string>
#include <string_view>

namespace lyra::hir {

// One lowering of a compiled unit, named by both: the unit, which is what it
// publishes and what every referrer names, and the lowering itself, named by
// the hierarchical path of the instance it was lowered for, which is what the
// design's construction tables are stated by.
struct LoweringRef {
  std::string unit;
  std::string lowering;

  // The one lowering of a namespace unit -- a package, a file-set scope, a
  // class specialization's home -- which is named as the unit is.
  [[nodiscard]] static auto OfNamespace(std::string_view unit) -> LoweringRef {
    return LoweringRef{
        .unit = std::string{unit}, .lowering = std::string{unit}};
  }

  auto operator==(const LoweringRef&) const -> bool = default;
};

}  // namespace lyra::hir
