#pragma once

#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/integral_constant.hpp"

namespace lyra::hir {

struct ConstantValue;

// A folded elaboration constant: a scalar leaf (integral, real / shortreal /
// realtime as one `double`, or string) or, for an unpacked aggregate, the
// ordered component values. The enclosing type disambiguates a component list
// as a struct's members or an array's elements; the value carries only the
// folded data, the same way an enum member carries an `IntegralConstant`. This
// is the value form of a type-level constant (LRM 7.2.2 member default), not an
// expression -- a data type carries constant values as metadata, never an
// expression.
using ConstantValueData = std::variant<
    IntegralConstant, double, std::string, std::vector<ConstantValue>>;

struct ConstantValue {
  ConstantValueData data;
};

}  // namespace lyra::hir
