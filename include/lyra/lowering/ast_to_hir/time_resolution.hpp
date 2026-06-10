#pragma once

#include <optional>

#include <slang/numeric/Time.h>

#include "lyra/base/time.hpp"

namespace lyra::lowering::ast_to_hir {

auto ResolveTimeResolution(std::optional<slang::TimeScale> ts)
    -> TimeResolution;

}  // namespace lyra::lowering::ast_to_hir
