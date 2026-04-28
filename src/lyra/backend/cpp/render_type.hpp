#pragma once

#include <string>

#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// Renders a MIR type as the corresponding C++ type expression. `owner_class`
// is the class that lexically declares the field whose type is being
// rendered; `ObjectType::target` ids are resolved against its nested classes.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::ClassDecl& owner_class,
    mir::TypeId type_id) -> std::string;

}  // namespace lyra::backend::cpp
