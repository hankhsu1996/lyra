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
#include "lyra/mir/local.hpp"
#include "lyra/mir/namespace_storage_phase.hpp"
#include "lyra/mir/static_constant_id.hpp"
#include "lyra/mir/struct_id.hpp"

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

// The namespace a unit's declarations live in. That a unit's emitted peer is a
// namespace is one decision, and two kinds of site spell it: the header that
// opens the unit's own, and every reference that qualifies into another's. They
// have to agree or the reference resolves to nothing, so the mapping is
// answered here rather than at each of them -- which also tells a reader which
// of the identifiers around it is a namespace and which is a class.
[[nodiscard]] inline auto UnitNamespaceOf(std::string_view unit_name)
    -> std::string {
  return ToCppName(unit_name);
}

// The C++ identifier a declaration the compiler synthesized is emitted under.
// `what` says which kind it is and `ordinal` which one, because such a
// declaration has no source name to take one from.
[[nodiscard]] inline auto MintedCppName(
    std::string_view what, std::uint32_t ordinal) -> std::string {
  return std::format("{}{}_{}", kMintedPrefix, what, ordinal);
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
[[nodiscard]] inline auto CppClassCallableName(
    const mir::Class& cls, mir::CallableId body) -> std::string {
  const std::optional<std::string_view> name =
      mir::NameOf(cls.named_callables, body);
  return name.has_value() ? ToCppName(*name)
                          : MintedCppName("body", body.value);
}

// The C++ identifier one of a class's fields is emitted under, given the names
// its class answers. Storage the source named takes that name; storage nothing
// names takes a minted name over the slot it sits at, which no source name
// reaches.
[[nodiscard]] inline auto CppFieldName(
    std::span<const mir::NamedField> named, mir::FieldId slot) -> std::string {
  const std::optional<std::string_view> name = mir::NameOf(named, slot);
  return name.has_value() ? ToCppName(*name)
                          : MintedCppName("field", slot.value);
}

// The C++ identifier one of a body's locals is emitted under. Every one of them
// is minted over the position it sits at, and a local the source named carries
// that identifier after the position, so the emitted body still reads like the
// design without the position ever ceasing to be what separates two locals.
//
// The position has to lead, because nothing else separates them. A body's
// locals are one flat arena spanning every scope the source nested inside it,
// and SystemVerilog lets sibling scopes reuse an identifier -- two `matches`
// patterns in one block each binding `n` are two variables, each scoped to its
// own statement (LRM 12.6.1) -- while this target has one scope to declare them
// in. So a source identifier does not decide a C++ one, and a slot, being a
// position, is distinct by being one.
[[nodiscard]] inline auto CppLocalName(
    std::span<const mir::NamedLocal> named, mir::LocalId local) -> std::string {
  const std::string minted = MintedCppName("local", local.value);
  const std::optional<std::string_view> name = mir::NameOf(named, local);
  return name.has_value() ? std::format("{}_{}", minted, ToCppName(*name))
                          : minted;
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
  return MintedCppName("field", slot.value);
}

// The C++ identifier one of a closure's captures is emitted under. A capture is
// realized as a lambda capture and shares the lambda's scope with the closure's
// per-invocation parameters and body locals, so what separates it from them has
// to be something no source identifier can reach: its position in the closure.
[[nodiscard]] inline auto CppClosureCaptureName(mir::FieldId slot)
    -> std::string {
  return MintedCppName("capture", slot.value);
}

// The C++ identifier the run-time description of a packed type is emitted
// under. It describes a type rather than standing for a declaration, so no
// source identifier reaches it and the type's own position is what names it.
[[nodiscard]] inline auto CppPackedTypeName(mir::TypeId integral)
    -> std::string {
  return MintedCppName("packed_type", integral.value);
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

// The C++ identifier one of the two bodies bringing up a unit's namespace is
// emitted under. Another unit's emitted text names it, and the source declares
// no name for it, so both ends compose it from which of the two it is.
[[nodiscard]] inline auto CppStorageEntryName(mir::NamespaceStoragePhase phase)
    -> std::string {
  switch (phase) {
    case mir::NamespaceStoragePhase::kInstall:
      return std::format("{}install_namespace_storage", kMintedPrefix);
    case mir::NamespaceStoragePhase::kInitialize:
      return std::format("{}initialize_namespace_storage", kMintedPrefix);
  }
  throw InternalError("backend::cpp: unknown namespace storage phase");
}

// The C++ identifier one body of a unit's namespace is emitted under. Another
// unit's emitted text may name it, so what reaches it decides the spelling
// rather than the position it happens to sit at.
[[nodiscard]] inline auto CppUnitCallableName(
    const mir::CompilationUnit& unit, mir::CallableId body) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName& r) {
            return std::string{r.name};
          },
          [](const mir::ReachedByName& r) { return ToCppName(r.name); },
          [](const mir::ReachedByStoragePhase& r) {
            return CppStorageEntryName(r.phase);
          }},
      mir::NamespaceReachOf(unit, body));
}

}  // namespace lyra::backend::cpp
