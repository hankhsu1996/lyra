#pragma once

#include <variant>

#include "lyra/hir/value_decl_ref.hpp"

namespace lyra::hir {

using Lvalue = std::variant<LocalValueRef>;

}  // namespace lyra::hir
