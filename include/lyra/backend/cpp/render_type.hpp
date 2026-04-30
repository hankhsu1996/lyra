#pragma once

#include <string>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// Renders a MIR type as the corresponding C++ type expression. `owner_scope`
// is the structural scope that lexically declares the field whose type is
// being rendered; `ObjectType::target` ids are resolved against its nested
// structural scopes.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    mir::TypeId type_id) -> std::string;

[[nodiscard]] auto RenderPackedShapeLiteral(
    const std::vector<mir::PackedRange>& dims) -> std::string;

}  // namespace lyra::backend::cpp
