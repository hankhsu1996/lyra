#pragma once

#include <string>

#include "lyra/value/format.hpp"

namespace lyra::value {

[[nodiscard]] auto FormatIntegral(
    const FormatSpec& spec, const IntegralValueView& value) -> std::string;

}  // namespace lyra::value
