#pragma once

#include <string>

#include "lyra/mir/type.hpp"

namespace lyra::projection::cpp {

[[nodiscard]] auto RenderTypeAsCpp(const mir::Type& type) -> std::string;

}  // namespace lyra::projection::cpp
