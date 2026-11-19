#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// `type` as one text, the same in every unit that names it, as clang's
// mangleCanonicalTypeName names a type by what it is rather than by how a
// translation unit spelled it: its kind, each fact that fixes it, and the
// types it is made of, each in turn; a declaration it names by the unit
// defining it and its path there. Two types answer alike exactly where they
// are one type.
[[nodiscard]] auto CanonicalTypeName(const CompilationUnit& unit, TypeId type)
    -> std::string;

}  // namespace lyra::mir
