#pragma once

#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// Two things MIR states as structure rather than as a value, and that this
// target has to realize with a library type. No MIR type names either of them,
// so neither is reached through the type mapping below -- but each is a library
// type's spelling, and this is where a library type is spelled.

// A body paired with a cleanup that runs on every way out of it: an object
// declared ahead of the body whose destruction runs the cleanup. C++ states an
// extent's exit through a destructor and offers no construct of its own.
[[nodiscard]] auto BodyCleanupExtentCppType() -> std::string_view;

// What an SV class extending nothing (LRM 8.13) is emitted over, so an object
// can answer with a handle to itself (LRM 8.11): realizing that handle as a
// shared owner means the object records which owner refers to it, and this is
// where that record lives.
[[nodiscard]] auto ManagedObjectRootCppType() -> std::string_view;

// The same object under another static view. Every view of a reference is one
// target type, so the target's own cast notation copies it unchanged where what
// a conversion calls for is a new view over the same object; naming that is the
// only way the pair of classes reaches the emitted text.
[[nodiscard]] auto ObjectViewConversionCppName() -> std::string_view;

// Renders a MIR type as the corresponding C++ type expression. An enum is a
// nominal type over a base integral, so its value renders as that base --
// `lyra::value::PackedArray` -- with no distinct emitted enum type.
[[nodiscard]] auto RenderTypeAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id) -> std::string;

// The renders of `types`, in order. A target-language type spelled out of
// others composes these pieces with the punctuation between them, rather than
// walking the components and interleaving the two.
[[nodiscard]] auto RenderEachTypeAsCpp(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> types)
    -> std::vector<std::string>;

// The storage behind a place of this type, named as an lvalue, given the
// place's own render. A pointer names it with the target's own dereference. A
// reference to a lent cell opens that cell (LRM 23.3.3.2). A reference to a
// managed object states which object it names rather than being it, so the
// object is reached first -- which is where the class the reading point assumes
// is written down, and where reaching through a reference that names no object
// is caught (LRM 8.4). A type this backend states no access for is refused
// rather than answered, so a place it was never asked about cannot be given a
// plausible one.
//
// This is the one entry that names a runtime library's access protocol; an
// entry that emits a value composes punctuation around its answer and never
// spells the protocol itself.
[[nodiscard]] auto RenderPlaceAccessAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId type_id,
    std::string_view place) -> std::string;

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
