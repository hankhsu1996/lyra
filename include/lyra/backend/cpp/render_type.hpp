#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
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
    mir::TypeId type_id) -> diag::Result<std::string>;

// Renders the constructor argument list for a `lyra::value::PackedArray`
// variable declared from a kExplicit packed array type, in the form
// `<bit_width>, <is_signed>, <is_four_state>`.
[[nodiscard]] auto RenderPackedArrayCtorArgs(const mir::PackedArrayType& pa)
    -> std::string;

}  // namespace lyra::backend::cpp
