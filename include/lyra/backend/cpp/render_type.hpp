#pragma once

#include <string>
#include <string_view>

#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::backend::cpp {

// The C ABI type spelling a by-value scalar SV value crosses the DPI-C boundary
// as (LRM 35.5.6, Table H.1): the single carrier-to-C++ mapping both the
// carrier type render and the emitted `extern "C"` foreign declaration read.
// The 2-state named integer carriers are signed to match SV `byte` / `shortint`
// / `int` / `longint`; a 1-bit `bit` / `logic` crosses as `unsigned char`,
// `real` as `double`, a string as a C string, and a `void` return has no
// carrier value.
[[nodiscard]] auto DpiScalarCarrierCppType(support::DpiScalarAbi abi)
    -> std::string_view;

// Renders a MIR type as the corresponding C++ type expression. `owner_class`
// is the class that lexically declares the field whose type is being rendered;
// `ObjectType::target` ids are resolved against its nested classes.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, const mir::Class& owner_class,
    mir::TypeId type_id) -> std::string;

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
