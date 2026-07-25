#pragma once

#include <string>
#include <string_view>

#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// Renders a MIR type as the corresponding C++ type expression. A nominal type
// name (an enum) is resolved against the unit's own type declarations, the same
// way a struct name is resolved through the unit's struct registry.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string;

// Renders a MIR class reference as the target C++ type expression naming
// that class. Intra-unit refs go through the unit's class registry; external
// refs render as their qualified name.
[[nodiscard]] auto RenderClassRefAsCpp(
    const mir::CompilationUnit& unit, const mir::ClassRef& ref) -> std::string;

// Renders the `lyra::value::PackedType` descriptor for a packed array type, in
// the form `lyra::value::PackedType{{{l0, r0}, ...}, <is_signed>,
// <is_four_state>}`. This is the single shape carrier every PackedArray
// construction takes.
[[nodiscard]] auto RenderPackedType(const mir::PackedArrayType& pa)
    -> std::string;

// Renders the emitted C++ class name for a MIR enum type. The name is the first
// unit type declaration targeting `id` (so a `typedef enum {...} foo;` makes
// the class `foo`); when none exists, falls back to a numeric internal name.
//
// EnumType itself carries no name (an enum and its typedef are orthogonal: an
// anonymous enum has none, a multi-typedef enum has many), so the name is not
// on the type; it is a unit-level fact, resolved by this lookup like a struct
// name.
[[nodiscard]] auto RenderEnumClassName(
    const mir::CompilationUnit& unit, mir::TypeId id) -> std::string;

}  // namespace lyra::backend::cpp
