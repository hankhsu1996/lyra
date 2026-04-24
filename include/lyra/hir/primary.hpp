#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/value_decl_ref.hpp"

namespace lyra::hir {

struct IntegerLiteral {
  std::int64_t value;
};

using Primary = std::variant<IntegerLiteral, LocalValueRef>;

}  // namespace lyra::hir
