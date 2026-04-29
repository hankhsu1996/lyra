#pragma once

#include <cstdint>
#include <optional>

#include <slang/numeric/Time.h>

#include "lyra/base/time.hpp"

namespace lyra::lowering::ast_to_hir {

auto TimeScaleValueToPower(slang::TimeScaleValue v) -> std::int8_t;

auto ResolveTimeResolution(std::optional<slang::TimeScale> ts)
    -> TimeResolution;

}  // namespace lyra::lowering::ast_to_hir
