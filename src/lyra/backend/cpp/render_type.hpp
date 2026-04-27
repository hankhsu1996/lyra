#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string;

}  // namespace lyra::backend::cpp
