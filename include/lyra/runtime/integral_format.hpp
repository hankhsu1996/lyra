#pragma once

#include <string>

#include "lyra/runtime/format.hpp"

namespace lyra::runtime {

[[nodiscard]] auto FormatIntegral(
    const FormatSpec& spec, const IntegralValueView& value) -> std::string;

}  // namespace lyra::runtime
