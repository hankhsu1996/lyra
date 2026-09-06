#pragma once

#include <string>
#include <string_view>

#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// The `lyra::support::NetResolution` enumerator naming a net's fold (LRM 6.6),
// as a C++ literal. A net member is value-initialized with it, so the runtime
// net carries its fold as data rather than as a type parameter.
[[nodiscard]] auto NetResolutionCppLiteral(mir::NetResolution resolution)
    -> std::string_view;

// Renders a MIR type as the corresponding C++ type expression. An enum is a
// nominal type over a base integral, so its value renders as that base --
// `lyra::value::PackedArray` -- with no distinct emitted enum type.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string;

// Renders what names bringing a value of this type into existence, which the
// argument list is then applied to. It is the type's own answer and not the
// construction's: a value type spells its own name, while a wrapper that owns
// what it points at spells the entry that allocates and constructs together.
[[nodiscard]] auto RenderTypeConstructionAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string;

// Renders a MIR class reference as the target C++ type expression naming
// that class. Intra-unit refs go through the unit's class registry; external
// refs render as their qualified name.
[[nodiscard]] auto RenderClassRefAsCpp(
    const mir::CompilationUnit& unit, const mir::ClassRef& ref) -> std::string;

}  // namespace lyra::backend::cpp
