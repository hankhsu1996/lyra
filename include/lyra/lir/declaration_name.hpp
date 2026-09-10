#pragma once

#include <optional>
#include <string>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The name the declaration `type` names is linked under, program-wide, or
// nothing where the type names no declaration a value is built from. A
// declaration this unit compiles carries the name it was emitted under; one
// another unit declares is composed from the unit and the name a signature
// gave, the same way that unit composed it -- which is what lets the two agree
// with no shared table.
auto DeclarationName(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string>;

}  // namespace lyra::lir
