#pragma once

#include <string>
#include <string_view>
#include <vector>

#include "lyra/hir/constant_value.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// The value a parameter holds in one specialization (LRM 6.20), as a constant
// a unit reading the parameter by name folds where it reads it. The parameter
// is named by its own name and the scope of the unit declaring it, `holder`.
struct PublishedParameter {
  std::string name;
  support::DefPath holder;
  ConstantValue value;

  auto operator==(const PublishedParameter&) const -> bool = default;
};

// What a specialization holds that the unit it belongs to does not: the value
// of each parameter it declares. Specializations of one unit publish one
// structure and may hold different values, so these are kept by the
// specialization a reader reached rather than by the unit.
struct SpecializationValues {
  std::vector<PublishedParameter> parameters;

  // The value of the parameter `name` of the scope `holder`, or nothing where
  // the specialization published none for it.
  [[nodiscard]] auto FindParameter(
      std::string_view name, const support::DefPath& holder) const
      -> const ConstantValue* {
    for (const PublishedParameter& parameter : parameters) {
      if (parameter.name == name && parameter.holder == holder) {
        return &parameter.value;
      }
    }
    return nullptr;
  }

  auto operator==(const SpecializationValues&) const -> bool = default;
};

}  // namespace lyra::hir
