#pragma once

#include <string>

#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

[[nodiscard]] auto RenderTypeAsCpp(const mir::Type& type) -> std::string;

}  // namespace lyra::backend::cpp
