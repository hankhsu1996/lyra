#pragma once

#include <string>

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// Renders a MIR type as the corresponding C++ type expression. `owner_class`
// is the class that lexically declares the field whose type is being rendered;
// `ObjectType::target` ids are resolved against its nested classes.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::Class& owner_class,
    mir::TypeId type_id) -> std::string;

// Renders the `lyra::value::PackedType` descriptor for a packed array type, in
// the form `lyra::value::PackedType{{{l0, r0}, ...}, <is_signed>,
// <is_four_state>}`. This is the single shape carrier every PackedArray
// construction takes.
[[nodiscard]] auto RenderPackedType(const mir::PackedArrayType& pa)
    -> std::string;

// Renders the emitted C++ class name for a MIR enum type. The name is
// derived from the first TypeAlias declaration targeting `id` in
// `owner_class` (so a `typedef enum {...} foo;` makes the class `foo`);
// when no alias is found, falls back to a numeric internal name.
//
// EnumType itself carries no name (an enum and its typedef are orthogonal:
// an anonymous enum has none, a multi-typedef enum has many). Lookup lives
// here, not on the type.
[[nodiscard]] auto RenderEnumClassName(
    const mir::Class& owner_class, mir::TypeId id) -> std::string;

}  // namespace lyra::backend::cpp
