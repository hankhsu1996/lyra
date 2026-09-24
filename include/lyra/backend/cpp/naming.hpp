#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/minted_entry.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type_descriptor_id.hpp"

namespace lyra::backend::cpp {

// Every name the compiler makes up starts with `sv_`, and a source name that
// starts with it is escaped rather than written as is. SystemVerilog reserves
// no identifier for the compiler, so without the prefix a made-up name could
// collide with one the design declares.
inline constexpr std::string_view kMintedPrefix = "sv_";

// Words C++ does not accept as a name: keywords, the alternative operator
// spellings, and identifiers with a special meaning ([lex.key], [lex.name]
// table 4). SystemVerilog reserves none of them, so a design may declare any.
// The last group would compile as a name and is listed anyway: escaping one
// only makes it read worse, while missing one breaks the build.
inline constexpr auto kCppReservedWords = std::to_array<std::string_view>(
    {"alignas",
     "alignof",
     "and",
     "and_eq",
     "asm",
     "auto",
     "bitand",
     "bitor",
     "bool",
     "break",
     "case",
     "catch",
     "char",
     "char16_t",
     "char32_t",
     "char8_t",
     "class",
     "co_await",
     "co_return",
     "co_yield",
     "compl",
     "concept",
     "const",
     "const_cast",
     "consteval",
     "constexpr",
     "constinit",
     "continue",
     "contract_assert",
     "decltype",
     "default",
     "delete",
     "do",
     "double",
     "dynamic_cast",
     "else",
     "enum",
     "explicit",
     "export",
     "extern",
     "false",
     "final",
     "float",
     "for",
     "friend",
     "goto",
     "if",
     "import",
     "inline",
     "int",
     "long",
     "module",
     "mutable",
     "namespace",
     "new",
     "noexcept",
     "not",
     "not_eq",
     "nullptr",
     "operator",
     "or",
     "or_eq",
     "override",
     "post",
     "pre",
     "private",
     "protected",
     "public",
     "register",
     "reinterpret_cast",
     "requires",
     "return",
     "short",
     "signed",
     "sizeof",
     "static",
     "static_assert",
     "static_cast",
     "struct",
     "switch",
     "template",
     "this",
     "thread_local",
     "throw",
     "true",
     "try",
     "typedef",
     "typeid",
     "typename",
     "union",
     "unsigned",
     "using",
     "virtual",
     "void",
     "volatile",
     "wchar_t",
     "while",
     "xor",
     "xor_eq"});

// How each kind of name is spelled in the emitted C++. Every site that writes a
// name takes it from here, so a declaration and its uses cannot spell it
// differently. These types are written straight into the output; a site that
// needs the characters as a string, such as a file path, converts them first.
// Each holds views into the MIR or into constant words, which outlive the
// writing.

// A name from the source, as a C++ identifier. SystemVerilog allows any
// printable non-space character in an escaped identifier (LRM 5.6.1), while C++
// allows letters, digits and underscores and reserves some words. A name C++
// accepts, and that does not start with `sv_`, is written as is: `count` stays
// `count`. Anything else becomes `sv_esc_` and its bytes in hex, so `\a.b `
// becomes `sv_esc_612e62`. Replacing the bad characters instead (`a.b` ->
// `a_b`) could turn two different source names into one C++ name, and the
// escaped form can collide with neither a plain name nor a made-up one.
//
// Names C++ reserves to the implementation but still compiles -- a double
// underscore, an underscore before a capital -- are left plain.
struct SourceName {
  std::string_view name;
};

// A name made from a kind and a position: `sv_local_3`. Where the source gave
// the declaration an identifier it is appended, `sv_local_3_count`, so the
// output reads like the design while the position alone keeps two
// declarations apart.
struct MintedName {
  std::string_view what;
  std::uint32_t ordinal;
  std::optional<std::string_view> source;
};

// A made-up name with no position, `sv_create`, for a declaration there is only
// ever one of where it stands.
struct MintedWord {
  std::string_view word;
};

// A name written exactly as given, because something outside this compiler
// already spells it that way.
struct VerbatimName {
  std::string_view text;
};

// Any of the above, for a declaration whose kind decides which.
using CppName = std::variant<SourceName, MintedName, MintedWord, VerbatimName>;

void WriteOne(TargetText& out, SourceName name);
void WriteOne(TargetText& out, const MintedName& name);
void WriteOne(TargetText& out, MintedWord name);
void WriteOne(TargetText& out, VerbatimName name);
void WriteOne(TargetText& out, const CppName& name);

[[nodiscard]] inline auto ToCppName(std::string_view name) -> SourceName {
  return SourceName{.name = name};
}

// The namespace a unit's declarations live in, named after the unit: unit `Top`
// is `namespace Top`. Sites call this rather than spelling the unit name, so
// the reader can tell a namespace from a class.
[[nodiscard]] inline auto UnitNamespaceOf(std::string_view unit_name)
    -> SourceName {
  return ToCppName(unit_name);
}

// The qualifier for a reference into a unit's namespace, `::Top`, used even
// from inside the unit itself. It starts at the global scope because the class
// at the root of the unit's hierarchy is also called `Top`, and inside that
// class a plain `Top` names the class, not the namespace.
struct UnitScope {
  std::string_view unit_name;
};

void WriteOne(TargetText& out, UnitScope scope);

[[nodiscard]] inline auto CppUnitScope(std::string_view unit_name)
    -> UnitScope {
  return UnitScope{.unit_name = unit_name};
}

// The files a unit is emitted as. Both the unit writing a file and every unit
// including it compute the name from here, so a unit can include a file of
// another unit without seeing how that unit was emitted. A file name is a
// string rather than output text, because what uses it first is the code that
// writes the file to disk.

// `Top.hpp`: what another unit includes. It includes the opening file and every
// class file below, in an order that compiles.
[[nodiscard]] inline auto UnitSignatureFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".hpp");
}

// `Top.opening.hpp`: forward declarations, and the cells and functions the
// unit's namespace declares. It includes nothing of the program, so any class
// file, of this unit or another, can include it first.
[[nodiscard]] inline auto UnitOpeningFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".opening.hpp");
}

// `Top.Base.hpp`: one class another unit may name, alone in its file. It
// includes the file of each class it derives from, so whichever file is
// included first, every class is defined after its bases and only once.
[[nodiscard]] inline auto UnitClassFileOf(
    std::string_view unit_name, const CppName& class_name) -> std::string {
  return TextOf(ToCppName(unit_name), ".", class_name, ".hpp");
}

// `Top.cpp`: the translation unit defining everything the headers declare.
[[nodiscard]] inline auto UnitCodeFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".cpp");
}

[[nodiscard]] inline auto MintedCppName(
    std::string_view what, std::uint32_t ordinal) -> MintedName {
  return MintedName{.what = what, .ordinal = ordinal, .source = std::nullopt};
}

[[nodiscard]] inline auto MintedCppNameWith(
    std::string_view what, std::uint32_t ordinal,
    std::optional<std::string_view> name) -> MintedName {
  return MintedName{.what = what, .ordinal = ordinal, .source = name};
}

// The name of a virtual method another unit's class declared, read from what
// this unit was told about that class. A virtual call to it and an override of
// it must spell it the same way, so both take it from here.
[[nodiscard]] inline auto CppExternalBehaviorName(
    const mir::CompilationUnit& unit, std::string_view unit_name,
    std::string_view class_name, mir::BehaviorOrdinal ordinal) -> SourceName {
  const mir::ExternalClass* introducer =
      mir::FindExternalClass(unit.external_classes, unit_name, class_name);
  if (introducer == nullptr || ordinal.value >= introducer->behaviors.size()) {
    throw InternalError(
        "backend::cpp: a behavior is named that no consumed promise describes");
  }
  return ToCppName(introducer->behaviors[ordinal.value]);
}

// The name of a function a class owns -- a method, a process, or a lifecycle
// body. A named one keeps its name; an unnamed one is `sv_body_<n>`. The
// declaration, the definition and every call take it from here.
//
// An override takes the name of the method it overrides, from the class that
// declared it: C++ matches an override to its virtual by name, so under any
// other name it would declare a second virtual instead.
//
// It takes a class rather than a unit's namespace, because a function of a
// namespace can also be reached by a foreign linkage name or as a fixed entry,
// which this does not spell.
[[nodiscard]] inline auto CppClassCallableName(
    const mir::CompilationUnit& unit, const mir::Class& cls,
    mir::CallableId body) -> CppName {
  const auto declared_here = [&]() -> CppName {
    const std::optional<std::string_view> name =
        mir::NameOf(cls.named_callables, body);
    if (name.has_value()) {
      return ToCppName(*name);
    }
    return MintedCppName("body", body.value);
  };
  const std::optional<mir::VirtualDispatchRole>& role =
      cls.callables.Get(body).virtual_dispatch;
  if (!role.has_value()) {
    return declared_here();
  }
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> CppName {
            return declared_here();
          },
          [&](const mir::OverridesIntraUnitSlot& taken) -> CppName {
            return CppClassCallableName(
                unit, unit.GetClass(taken.slot_owner), taken.slot_id);
          },
          [&](const mir::OverridesExternalSlot& taken) -> CppName {
            return CppExternalBehaviorName(
                unit, taken.unit_name, taken.class_name, taken.ordinal);
          }},
      *role);
}

// The name of a class field: `sv_field_<slot>`, plus the source name where
// there is one, `sv_field_1_v`. The source name alone would not do: a C++ class
// has one name space for fields and methods, and a scope's class holds both
// the cell `x` and the method another unit calls to reach it, which that unit
// spells `x`.
//
// The class declaring the field and another unit reading it both call this,
// each with the slot and the source name it knows, so the two agree.
[[nodiscard]] inline auto CppFieldNameOf(
    mir::FieldId slot, std::optional<std::string_view> name) -> MintedName {
  return MintedCppNameWith("field", slot.value, name);
}

[[nodiscard]] inline auto CppFieldName(
    std::span<const mir::NamedField> named, mir::FieldId slot) -> MintedName {
  return CppFieldNameOf(slot, mir::NameOf(named, slot));
}

// The name of a local: `sv_local_<slot>`, plus the source name where there is
// one. The source name alone would not do: two sibling scopes may each declare
// an `n` -- two `matches` patterns in one block each bind their own (LRM
// 12.6.1) -- and a function's locals are all declared in one C++ scope.
[[nodiscard]] inline auto CppLocalName(
    std::span<const mir::NamedLocal> named, mir::LocalId local) -> MintedName {
  return MintedCppNameWith("local", local.value, mir::NameOf(named, local));
}

// The names of a class's static properties and a unit's static variables. The
// ones the source declared keep their names; the rest were added by lowering,
// for state that outlives one call, and are `sv_cell_<n>` or
// `sv_variable_<n>`.
[[nodiscard]] inline auto CppStaticPropertyName(
    std::span<const mir::NamedStaticProperty> named, mir::StaticPropertyId slot)
    -> CppName {
  const std::optional<std::string_view> name = mir::NameOf(named, slot);
  if (name.has_value()) {
    return ToCppName(*name);
  }
  return MintedCppName("cell", slot.value);
}

[[nodiscard]] inline auto CppStaticVariableName(
    std::span<const mir::NamedStaticVariable> named,
    mir::StaticVariableId variable) -> CppName {
  const std::optional<std::string_view> name = mir::NameOf(named, variable);
  if (name.has_value()) {
    return ToCppName(*name);
  }
  return MintedCppName("variable", variable.value);
}

// The name of a class: its declared name, or `sv_scope_<n>` for a scope of the
// design hierarchy, which has none.
[[nodiscard]] inline auto CppClassName(const mir::Class& cls, mir::ClassId id)
    -> CppName {
  if (cls.name.has_value()) {
    return ToCppName(*cls.name);
  }
  return MintedCppName("scope", id.value);
}

// The names of a struct the compiler made to hold a scope's variables, and of
// its fields. The source names neither, so both are positions:
// `sv_scope_storage_<n>` and `sv_field_<slot>`.
[[nodiscard]] inline auto CppStructName(mir::StructId id) -> MintedName {
  return MintedCppName("scope_storage", id.value);
}

[[nodiscard]] inline auto CppStructFieldName(mir::FieldId slot) -> MintedName {
  return CppFieldNameOf(slot, std::nullopt);
}

// The name of a closure capture, `sv_capture_<n>`. A capture shares the
// lambda's scope with its parameters and locals, which carry source names, so
// it gets a name no source name can produce.
[[nodiscard]] inline auto CppClosureCaptureName(mir::FieldId slot)
    -> MintedName {
  return MintedCppName("capture", slot.value);
}

// The name of the run-time description of a type, `sv_type_<n>`: a position in
// the unit's list of descriptions, since the source names none.
[[nodiscard]] inline auto CppTypeDescriptorName(
    mir::TypeDescriptorId descriptor) -> MintedName {
  return MintedCppName("type", descriptor.value);
}

// The name of a constant value the unit uses, `sv_const_<n>`: a position in the
// unit's list of constants, since the source wrote the value and no name.
[[nodiscard]] inline auto CppIntegralConstantName(mir::IntegralConstantId c)
    -> MintedName {
  return MintedCppName("const", c.value);
}

// The names of a class's runtime callbacks and its static constants,
// `sv_adapter_<n>` and `sv_constant_<n>`: positions in the class, since the
// source declares neither.
[[nodiscard]] inline auto CppAbiAdapterName(mir::AbiAdapterId id)
    -> MintedName {
  return MintedCppName("adapter", id.value);
}

[[nodiscard]] inline auto CppStaticConstantName(mir::StaticConstantId id)
    -> MintedName {
  return MintedCppName("constant", id.value);
}

// The name of a class's object record, `sv_object_record`. Another unit names
// it as `Class::sv_object_record`, and that unit cannot know this class's
// constant positions, so it gets a fixed word instead of a position.
[[nodiscard]] inline auto CppObjectRecordName() -> MintedWord {
  return MintedWord{.word = "object_record"};
}

// The static member through which the runtime's allocation finds a class's
// object record. The runtime reads a member of this exact name, so the name
// is the runtime's, and this is the one place the emitter spells it.
[[nodiscard]] inline auto CppClassRecordHookName() -> std::string_view {
  return "kClassRecord";
}

// A DPI-C linkage name, written exactly as given. It is a C identifier (LRM
// 35.4) that the user's own C code also spells, so it cannot be escaped or
// prefixed.
[[nodiscard]] inline auto CppForeignSymbolName(std::string_view linkage_name)
    -> VerbatimName {
  return VerbatimName{.text = linkage_name};
}

// The name of one of a unit's fixed entries, such as `sv_create`. The source
// names none of them, but another unit calls them, so each has a fixed word
// both units write.
[[nodiscard]] inline auto CppMintedEntryName(mir::MintedEntry entry)
    -> MintedWord {
  switch (entry) {
    case mir::MintedEntry::kInstallStorage:
      return MintedWord{.word = "install_namespace_storage"};
    case mir::MintedEntry::kInitializeStorage:
      return MintedWord{.word = "initialize_namespace_storage"};
    case mir::MintedEntry::kMakeObject:
      return MintedWord{.word = "create"};
  }
  throw InternalError("backend::cpp: unknown minted entry");
}

// The name of a function in a unit's namespace, chosen by how other code
// reaches it: its DPI-C linkage name, its source name, its fixed entry word, or
// `sv_body_<n>` if nothing outside the unit reaches it.
[[nodiscard]] inline auto CppUnitCallableName(
    const mir::CompilationUnit& unit, mir::CallableId body) -> CppName {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName& r) -> CppName {
            return CppForeignSymbolName(r.name);
          },
          [](const mir::ReachedByName& r) -> CppName {
            return ToCppName(r.name);
          },
          [](const mir::ReachedByMintedEntry& r) -> CppName {
            return CppMintedEntryName(r.entry);
          },
          [](const mir::ReachedByPosition& r) -> CppName {
            return MintedCppName("body", r.slot.value);
          }},
      mir::NamespaceReachOf(unit, body));
}

}  // namespace lyra::backend::cpp
