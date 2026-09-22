#pragma once

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

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

// The prefix every name the compiler mints for the emitted C++ carries, and
// which no rendered source name can begin with. It is what lets a synthesized
// declaration be named at all: SystemVerilog reserves no spelling to the
// compiler, so a word minted without one is a word a program may also declare.
inline constexpr std::string_view kMintedPrefix = "sv_";

// The words C++ refuses as a name: its keywords, the alternative spellings of
// its operators, and the identifiers it gives a meaning of their own
// ([lex.key], [lex.name] table 4). SystemVerilog reserves none of them, so each
// is a name a design may declare. The last group is admissible as an ordinary
// name and is listed anyway, because being escaped costs a name only how it
// reads while being missed costs a program that does not build.
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

// A source name spelled as a C++ identifier. SystemVerilog admits every
// printable character but white space in an identifier (LRM 5.6.1), while C++
// admits letters, digits and underscores and refuses some of what those spell,
// so a name C++ will not take is escaped rather than repaired: mapping what it
// refuses onto what it allows maps two declarations that differ onto one token,
// and the emitted unit then declares one of them twice.
//
// A name renders as itself exactly when C++ would accept it as a name and it
// does not begin with the minted prefix; everything else renders as that
// prefix, an escape marker, and its bytes in hex. The two images are disjoint,
// so no two source names meet and nothing the compiler mints is reachable from
// one.
//
// What C++ accepts as a name is narrower than what its identifier characters
// spell, so a word list answers half of that question. The spellings it
// reserves to an implementation rather than refusing -- a name carrying a
// double underscore, or an underscore before a capital -- stay plain: such a
// name compiles, and the compiler already spells names of its own that way.
[[nodiscard]] inline auto ToCppName(std::string_view name) -> std::string {
  const auto is_word = [](char c) {
    return std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '_';
  };
  const bool plain =
      !name.empty() && std::isdigit(static_cast<unsigned char>(name[0])) == 0 &&
      !name.starts_with(kMintedPrefix) && std::ranges::all_of(name, is_word) &&
      !std::ranges::contains(kCppReservedWords, name);
  if (plain) {
    return std::string{name};
  }
  std::string out{kMintedPrefix};
  out += "esc_";
  for (const char c : name) {
    out += std::format("{:02x}", static_cast<unsigned char>(c));
  }
  return out;
}

// The name of the namespace a unit's declarations live in, as the header that
// opens it writes it. That a unit's emitted peer is a namespace is one
// decision, answered here rather than at each site that spells it -- which also
// tells a reader which of the identifiers around it is a namespace and which is
// a class.
[[nodiscard]] inline auto UnitNamespaceOf(std::string_view unit_name)
    -> std::string {
  return ToCppName(unit_name);
}

// The prefix every reference into a unit's namespace qualifies by, its own
// unit's included. It starts at the global scope because a class carries its
// own name inside its body and a unit's namespace carries the same identifier
// as the class at the root of that unit's hierarchy: from inside such a class
// the unqualified prefix names the class, and the namespace behind it becomes
// unreachable.
[[nodiscard]] inline auto CppUnitScope(std::string_view unit_name)
    -> std::string {
  return std::format("::{}", UnitNamespaceOf(unit_name));
}

// The files a unit's emission produces. The same agreement the namespace needs
// applies to every one of them, one level out -- whoever writes a file writes
// its name, and whoever reads it writes the same name in an include, with no
// list between them saying where anything was put. That is what lets a unit
// name a file of another unit it has never seen emitted.

// What a referrer names, and the whole of what this unit promised: it reads the
// opening file and every class file below, in an order that satisfies them.
[[nodiscard]] inline auto UnitSignatureFileOf(std::string_view unit_name)
    -> std::string {
  return std::format("{}.hpp", ToCppName(unit_name));
}

// What the unit's declarations open with: the names they will use, and the
// cells and bodies its namespace declares. Nothing of the program has to be
// read before it, which is what lets a class written anywhere reach back to it.
[[nodiscard]] inline auto UnitOpeningFileOf(std::string_view unit_name)
    -> std::string {
  return std::format("{}.opening.hpp", ToCppName(unit_name));
}

// One class the unit promised. Each takes a file of its own: a class is written
// after the one it rests on and cannot be written twice, so a reader entering
// files in any order reaches each class through the one file that has it, and
// the order among the files is the order among the classes -- which a program
// always has, since a class may not rest on itself.
//
// `cpp_class_name` is already spelled for the target, because what a class is
// called there is decided in one place and a class the source never named still
// has to be written somewhere. A unit always has a source name, so this maps
// that one itself.
[[nodiscard]] inline auto UnitClassFileOf(
    std::string_view unit_name, std::string_view cpp_class_name)
    -> std::string {
  return std::format("{}.{}.hpp", ToCppName(unit_name), cpp_class_name);
}

// The translation unit realizing everything above.
[[nodiscard]] inline auto UnitCodeFileOf(std::string_view unit_name)
    -> std::string {
  return std::format("{}.cpp", ToCppName(unit_name));
}

// The C++ identifier a declaration the compiler synthesized is emitted under.
// `what` says which kind it is and `ordinal` which one, because such a
// declaration has no source name to take one from.
[[nodiscard]] inline auto MintedCppName(
    std::string_view what, std::uint32_t ordinal) -> std::string {
  return std::format("{}{}_{}", kMintedPrefix, what, ordinal);
}

// The same, with the identifier the source gave that declaration after the
// position, so the emitted text still reads like the design without the
// position ever ceasing to be what separates two declarations. A declaration
// the source never named carries the position alone. One composition, so every
// party spelling such a name arrives at the same one.
[[nodiscard]] inline auto MintedCppNameWith(
    std::string_view what, std::uint32_t ordinal,
    std::optional<std::string_view> name) -> std::string {
  const std::string minted = MintedCppName(what, ordinal);
  return name.has_value() ? std::format("{}_{}", minted, ToCppName(*name))
                          : minted;
}

// The C++ identifier a behavior another unit's class introduced is emitted
// under, read out of the promise this unit consumed about that class. A call
// dispatching on one and a body taking one over spell the same behavior, so
// both read it here.
[[nodiscard]] inline auto CppExternalBehaviorName(
    const mir::CompilationUnit& unit, std::string_view unit_name,
    std::string_view class_name, mir::BehaviorOrdinal ordinal) -> std::string {
  const mir::ExternalClass* introducer =
      mir::FindExternalClass(unit.external_classes, unit_name, class_name);
  if (introducer == nullptr || ordinal.value >= introducer->behaviors.size()) {
    throw InternalError(
        "backend::cpp: a behavior is named that no consumed promise describes");
  }
  return ToCppName(introducer->behaviors[ordinal.value]);
}

// The C++ identifier one body a class owns is emitted under -- a method, a
// process, or a lifecycle body. One the source named takes that name; one
// nothing names takes a minted name over the position it sits at, which no
// source name reaches. One answer, so the declaration, the definition, and
// every call spell it alike.
//
// It takes the class rather than the relation because a unit's namespace holds
// the same kind of relation over the same kind of id, and a body of one is
// reached in ways a class's never is -- a foreign linkage name, or which
// bring-up entry it is. Naming one from here would answer with a spelling
// nothing links under.
//
// A body taking over a dispatch slot answers with the identifier the class that
// introduced the slot gave it: this target resolves a takeover by name, so a
// body under any other spelling introduces a second slot instead of filling the
// one it was built for.
[[nodiscard]] inline auto CppClassCallableName(
    const mir::CompilationUnit& unit, const mir::Class& cls,
    mir::CallableId body) -> std::string {
  const auto declared_here = [&] {
    const std::optional<std::string_view> name =
        mir::NameOf(cls.named_callables, body);
    return name.has_value() ? ToCppName(*name)
                            : MintedCppName("body", body.value);
  };
  const std::optional<mir::VirtualDispatchRole>& role =
      cls.callables.Get(body).virtual_dispatch;
  if (!role.has_value()) {
    return declared_here();
  }
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> std::string {
            return declared_here();
          },
          [&](const mir::OverridesIntraUnitSlot& taken) -> std::string {
            return CppClassCallableName(
                unit, unit.GetClass(taken.slot_owner), taken.slot_id);
          },
          [&](const mir::OverridesExternalSlot& taken) -> std::string {
            return CppExternalBehaviorName(
                unit, taken.unit_name, taken.class_name, taken.ordinal);
          }},
      *role);
}

// The C++ identifier one of a class's fields is emitted under. The slot has to
// lead, for the reason it leads for a body local: this target holds everything
// a class declares in one name space whatever kind each of them is, and a class
// holds storage beside the behaviors it takes over, whose identifiers are the
// identifiers of the members they answer with. So a source identifier does not
// decide a C++ one, and a slot, being a position, is distinct by being one.
//
// Both parties spelling storage come here -- the class that declares it, and a
// referrer reading the identifier out of the promise it consumed -- each with
// the slot and whatever identifier it holds, so the two cannot come apart.
[[nodiscard]] inline auto CppFieldNameOf(
    mir::FieldId slot, std::optional<std::string_view> name) -> std::string {
  return MintedCppNameWith("field", slot.value, name);
}

[[nodiscard]] inline auto CppFieldName(
    std::span<const mir::NamedField> named, mir::FieldId slot) -> std::string {
  return CppFieldNameOf(slot, mir::NameOf(named, slot));
}

// The C++ identifier one of a body's locals is emitted under. The position has
// to lead, because nothing else separates them: a body's locals are one flat
// arena spanning every scope the source nested inside it, and SystemVerilog
// lets sibling scopes reuse an identifier -- two `matches` patterns in one
// block each binding `n` are two variables, each scoped to its own statement
// (LRM 12.6.1) -- while this target has one scope to declare them in. So a
// source identifier does not decide a C++ one, and a slot, being a position, is
// distinct by being one.
[[nodiscard]] inline auto CppLocalName(
    std::span<const mir::NamedLocal> named, mir::LocalId local) -> std::string {
  return MintedCppNameWith("local", local.value, mir::NameOf(named, local));
}

// The C++ identifiers a class's and a unit's type-associated cells are emitted
// under. The source declares some of each pool and the lowering keeps the rest
// for bodies that outlive an activation; only the first kind answers to an
// identifier.
[[nodiscard]] inline auto CppStaticPropertyName(
    std::span<const mir::NamedStaticProperty> named, mir::StaticPropertyId slot)
    -> std::string {
  const std::optional<std::string_view> name = mir::NameOf(named, slot);
  return name.has_value() ? ToCppName(*name)
                          : MintedCppName("cell", slot.value);
}

[[nodiscard]] inline auto CppStaticVariableName(
    std::span<const mir::NamedStaticVariable> named,
    mir::StaticVariableId variable) -> std::string {
  const std::optional<std::string_view> name = mir::NameOf(named, variable);
  return name.has_value() ? ToCppName(*name)
                          : MintedCppName("variable", variable.value);
}

// The C++ identifier a class is emitted under. A class the source declared
// takes its declared name; a scope of the design hierarchy takes a minted name
// over the identity its unit's registry gave it.
[[nodiscard]] inline auto CppClassName(const mir::Class& cls, mir::ClassId id)
    -> std::string {
  return cls.name.has_value() ? ToCppName(*cls.name)
                              : MintedCppName("scope", id.value);
}

// The C++ identifier a gathered-scope aggregate, and one member of one, are
// emitted under. The source declares no such aggregate, so neither it nor any
// member of it answers to an identifier and a position is the whole of what
// names one.
[[nodiscard]] inline auto CppStructName(mir::StructId id) -> std::string {
  return MintedCppName("scope_storage", id.value);
}

[[nodiscard]] inline auto CppStructFieldName(mir::FieldId slot) -> std::string {
  return CppFieldNameOf(slot, std::nullopt);
}

// The C++ identifier one of a closure's captures is emitted under. A capture is
// realized as a lambda capture and shares the lambda's scope with the closure's
// per-invocation parameters and body locals, so what separates it from them has
// to be something no source identifier can reach: its position in the closure.
[[nodiscard]] inline auto CppClosureCaptureName(mir::FieldId slot)
    -> std::string {
  return MintedCppName("capture", slot.value);
}

// The C++ identifier the run-time description of a type is emitted under. It
// describes a type rather than standing for a declaration, so no source
// identifier reaches it and its position in the unit's own pool is the whole of
// its identity.
[[nodiscard]] inline auto CppTypeDescriptorName(
    mir::TypeDescriptorId descriptor) -> std::string {
  return MintedCppName("type", descriptor.value);
}

// The C++ identifier a constant of the unit is emitted under. The source wrote
// the value, never a name for it, so its position in the unit's own pool is the
// whole of its identity -- the same answer the description of a type takes.
[[nodiscard]] inline auto CppIntegralConstantName(mir::IntegralConstantId c)
    -> std::string {
  return MintedCppName("const", c.value);
}

// The C++ identifiers a class's runtime-callback adapters and its compile-time
// constants are emitted under. The source declares neither, so each is named
// over the position it sits at in the arena its class owns -- the same identity
// every reference to one already carries.
[[nodiscard]] inline auto CppAbiAdapterName(mir::AbiAdapterId id)
    -> std::string {
  return MintedCppName("adapter", id.value);
}

[[nodiscard]] inline auto CppStaticConstantName(mir::StaticConstantId id)
    -> std::string {
  return MintedCppName("constant", id.value);
}

// The C++ identifier a class's object record is emitted under. It is the one
// constant of a class that another unit spells, and a position counted in this
// unit's arena is not something that unit can count -- so it is named off the
// class, which both sides already agree on.
[[nodiscard]] inline auto CppObjectRecordName() -> std::string {
  return std::format("{}object_record", kMintedPrefix);
}

// The identifier a class states its object record under for the allocation to
// read. It is the runtime's own name for what a class of the source language
// says about its objects, so the two sides agree on it by both reading one
// spelling rather than by each writing the same characters.
[[nodiscard]] inline auto CppClassRecordHookName() -> std::string_view {
  return "kClassRecord";
}

// The C++ identifier a DPI-C linkage name is emitted under. It is an identifier
// of C rather than of SystemVerilog (LRM 35.4), already spelled the way the
// foreign side must see it, so it crosses as itself -- and it must, since the
// user's own C source names it and nothing maps that.
[[nodiscard]] inline auto CppForeignSymbolName(std::string_view linkage_name)
    -> std::string {
  return std::string{linkage_name};
}

// The C++ identifier a body of a unit that answers to no name is emitted under.
// Another unit's emitted text names it and the source declares no name for it,
// so both ends compose it from which of them it is.
[[nodiscard]] inline auto CppMintedEntryName(mir::MintedEntry entry)
    -> std::string {
  switch (entry) {
    case mir::MintedEntry::kInstallStorage:
      return std::format("{}install_namespace_storage", kMintedPrefix);
    case mir::MintedEntry::kInitializeStorage:
      return std::format("{}initialize_namespace_storage", kMintedPrefix);
    case mir::MintedEntry::kMakeObject:
      return std::format("{}create", kMintedPrefix);
  }
  throw InternalError("backend::cpp: unknown minted entry");
}

// The C++ identifier one body of a unit's namespace is emitted under. Another
// unit's emitted text may name it, so what reaches it decides the spelling
// where anything does; a body nothing reaches from outside takes a minted name
// over the position it sits at, the way a class's own unnamed bodies already
// do.
[[nodiscard]] inline auto CppUnitCallableName(
    const mir::CompilationUnit& unit, mir::CallableId body) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName& r) {
            return CppForeignSymbolName(r.name);
          },
          [](const mir::ReachedByName& r) { return ToCppName(r.name); },
          [](const mir::ReachedByMintedEntry& r) {
            return CppMintedEntryName(r.entry);
          },
          [](const mir::ReachedByPosition& r) {
            return MintedCppName("body", r.slot.value);
          }},
      mir::NamespaceReachOf(unit, body));
}

}  // namespace lyra::backend::cpp
